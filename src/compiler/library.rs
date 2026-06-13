use std::fs;
use std::io::{self, Write};
use std::path::{Path, PathBuf};
use std::sync::LazyLock;

use chrono::{DateTime, Datelike, Timelike, Utc};
use ecow::eco_format;
use rayon::iter::{IntoParallelRefIterator, ParallelIterator};
use serde::Serialize;
use typst::diag::{FileError, FileResult, SourceDiagnostic, SourceResult, Warned};
use typst::foundations::{Bytes, Datetime, Dict, Duration, IntoValue, Smart};
use typst::layout::PageRanges;
use typst::syntax::{FileId, RootedPath, Span, VirtualPath, VirtualRoot};
use typst::text::{Font, FontBook};
use typst::utils::LazyHash;
use typst::{Library, LibraryExt, World};
use typst_bundle::{Bundle, BundleOptions, VirtualFs};
use typst_html::HtmlOptions;
use typst_kit::datetime::Time;
use typst_kit::diagnostics::DiagnosticWorld;
use typst_kit::downloader::SystemDownloader;
use typst_kit::files::{FileLoader, FileStore, FsRoot};
use typst_kit::fonts::FontStore;
use typst_kit::packages::{FsPackages, SystemPackages, UniversePackages};
use typst_kit::timer::Timer;
use typst_pdf::{PdfOptions, PdfStandards, Timestamp};
use typst_render::RenderOptions;
use typst_svg::SvgOptions;
use typst_utils::Scalar;

use crate::args::{
    CompileOutputPath, DepsFormat, DiagnosticFormat, FontArgs, PackageArgs, PdfStandard,
};
use crate::compiler::{CompilerBackend, forced_feature_names, site_inputs};
use crate::config::BuildConfig;
use crate::error::StrResult;

pub struct LibraryCompiler;

impl CompilerBackend for LibraryCompiler {
    fn compile_bundle(build_config: &BuildConfig, entrypoint: &str) -> StrResult<()> {
        compile_bundle(build_config, entrypoint)
    }
}

fn compile_bundle(build_config: &BuildConfig, entrypoint: &str) -> StrResult<()> {
    let mut config = LibraryCompileConfig::new(build_config)?;
    let mut world = LibraryWorld::new(build_config, entrypoint)?;
    let mut timer = Timer::new_or_placeholder(build_config.typst.timings.clone());

    timer
        .record(&mut world, |world| compile_once(world, &mut config))
        .map_err(|err| eco_format!("{err}"))?
}

struct LibraryCompileConfig {
    warnings: Vec<SourceDiagnostic>,
    pretty: bool,
    pages: Option<PageRanges>,
    creation_timestamp: Option<DateTime<Utc>>,
    diagnostic_format: DiagnosticFormat,
    open: Option<Option<String>>,
    pdf_standards: PdfStandards,
    tagged: bool,
    deps: Option<CompileOutputPath>,
    deps_format: DepsFormat,
    ppi: f64,
    output_directory: PathBuf,
}

impl LibraryCompileConfig {
    fn new(build_config: &BuildConfig) -> StrResult<Self> {
        let typst = &build_config.typst;
        let pages = typst.pages.as_ref().map(|export_ranges| {
            PageRanges::new(export_ranges.iter().map(|range| range.0.clone()).collect())
        });

        let mut warnings = Vec::new();
        let tagged = !typst.no_pdf_tags && pages.is_none();
        if pages.is_some() && !typst.no_pdf_tags {
            let mut warning =
                SourceDiagnostic::warning(Span::detached(), "using --pages implies --no-pdf-tags");
            warning.hint("the resulting PDF will be inaccessible");
            warning.hint("add --no-pdf-tags to silence this warning");
            warnings.push(warning);
        }
        validate_pdf_tags(
            tagged,
            typst.no_pdf_tags,
            pages.is_some(),
            &typst.pdf_standard,
        )?;

        let pdf_standards = PdfStandards::new(
            &typst
                .pdf_standard
                .iter()
                .copied()
                .map(Into::into)
                .collect::<Vec<_>>(),
        )
        .map_err(|err| eco_format!("{}", err.message()))?;

        let mut deps = typst.deps.clone();
        let mut deps_format = typst.deps_format;
        if let Some(path) = &typst.make_deps
            && deps.is_none()
        {
            deps = Some(CompileOutputPath::Path(path.clone()));
            deps_format = DepsFormat::Make;
            warnings.push(SourceDiagnostic::warning(
                Span::detached(),
                "--make-deps is deprecated, use --deps and --deps-format instead",
            ));
        }

        if matches!(deps, Some(CompileOutputPath::Stdout)) {
            // The bundle itself is always a directory, so stdout dependencies
            // are valid in Weibian's compile mode.
        }

        let creation_timestamp = typst
            .creation_timestamp
            .map(|time| {
                chrono::DateTime::from_timestamp(time, 0)
                    .ok_or_else(|| eco_format!("creation timestamp is out of range"))
            })
            .transpose()?;

        Ok(Self {
            warnings,
            pretty: typst.pretty,
            pages,
            creation_timestamp,
            diagnostic_format: typst.process.diagnostic_format,
            open: typst.open.clone(),
            pdf_standards,
            tagged,
            deps,
            deps_format,
            ppi: typst.ppi,
            output_directory: build_config.output_directory.clone(),
        })
    }
}

fn validate_pdf_tags(
    tagged: bool,
    no_pdf_tags: bool,
    has_pages: bool,
    standards: &[PdfStandard],
) -> StrResult<()> {
    if tagged {
        return Ok(());
    }

    for standard in standards {
        let name = match standard {
            PdfStandard::A_1a => "PDF/A-1a",
            PdfStandard::A_2a => "PDF/A-2a",
            PdfStandard::A_3a => "PDF/A-3a",
            PdfStandard::UA_1 => "PDF/UA-1",
            _ => continue,
        };
        if no_pdf_tags {
            return Err(eco_format!(
                "cannot disable PDF tags when exporting a {name} document"
            ));
        }
        if has_pages {
            return Err(eco_format!(
                "cannot disable PDF tags when exporting a {name} document"
            ));
        }
    }

    Ok(())
}

fn compile_once(world: &mut LibraryWorld, config: &mut LibraryCompileConfig) -> StrResult<()> {
    let Warned {
        output,
        mut warnings,
    } = typst::compile::<Bundle>(world);
    warnings.extend(config.warnings.iter().cloned());

    let output = output.and_then(|bundle| export_bundle(&bundle, config));

    match &output {
        Ok(_) => {
            print_diagnostics(world, &[], &warnings, config.diagnostic_format)?;
            open_output(config)?;
        }
        Err(errors) => {
            print_diagnostics(world, errors, &warnings, config.diagnostic_format)?;
        }
    }

    if let Some(dest) = &config.deps {
        write_deps(world, dest, config.deps_format, output.as_deref().ok())
            .map_err(|err| eco_format!("failed to create dependency file ({err})"))?;
    }

    output
        .map(|_| ())
        .map_err(|_| eco_format!("typst compile failed"))
}

fn export_bundle(bundle: &Bundle, config: &LibraryCompileConfig) -> SourceResult<Vec<PathBuf>> {
    let options = BundleOptions {
        html: HtmlOptions {
            pretty: config.pretty,
        },
        pdf: pdf_options(config),
        png: png_options(config),
        svg: SvgOptions {
            render_bleed: false,
            pretty: config.pretty,
        },
    };

    let fs = typst_bundle::export(bundle, &options)?;
    write_virtual_fs(&config.output_directory, &fs).map_err(|err| {
        vec![SourceDiagnostic::error(
            Span::detached(),
            eco_format!("failed to write bundle ({err})"),
        )]
        .into()
    })
}

fn write_virtual_fs(root: &Path, fs: &VirtualFs) -> StrResult<Vec<PathBuf>> {
    fs::create_dir_all(root)
        .map_err(|err| eco_format!("failed to create output directory ({err})"))?;

    fs.par_iter()
        .map(|(path, data)| {
            let realized = path.realize(root);

            if let Some(parent) = realized.parent() {
                fs::create_dir_all(parent)
                    .map_err(|err| eco_format!("failed to create directory ({err})"))?;
            }

            fs::write(&realized, data)
                .map_err(|err| eco_format!("failed to write file ({err})"))?;
            Ok(realized)
        })
        .collect()
}

fn pdf_options(config: &LibraryCompileConfig) -> PdfOptions {
    let timestamp = match config.creation_timestamp {
        Some(timestamp) => convert_datetime(timestamp).map(Timestamp::new_utc),
        None => {
            let local_datetime = chrono::Local::now();
            convert_datetime(local_datetime).and_then(|datetime| {
                Timestamp::new_local(datetime, local_datetime.offset().local_minus_utc() / 60)
            })
        }
    };

    PdfOptions {
        ident: Smart::Auto,
        creator: Smart::Auto,
        timestamp,
        page_ranges: config.pages.clone(),
        standards: config.pdf_standards.clone(),
        tagged: config.tagged,
        pretty: config.pretty,
    }
}

fn png_options(config: &LibraryCompileConfig) -> RenderOptions {
    RenderOptions {
        pixel_per_pt: Scalar::new(config.ppi / 72.0),
        render_bleed: false,
    }
}

fn convert_datetime<Tz: chrono::TimeZone>(date_time: chrono::DateTime<Tz>) -> Option<Datetime> {
    Datetime::from_ymd_hms(
        date_time.year(),
        date_time.month().try_into().ok()?,
        date_time.day().try_into().ok()?,
        date_time.hour().try_into().ok()?,
        date_time.minute().try_into().ok()?,
        date_time.second().try_into().ok()?,
    )
}

struct LibraryWorld {
    workdir: Option<PathBuf>,
    library: LazyHash<Library>,
    fonts: LazyLock<FontStore, Box<dyn Fn() -> FontStore + Send + Sync>>,
    files: FileStore<WeibianFiles>,
    now: Time,
}

impl LibraryWorld {
    fn new(build_config: &BuildConfig, entrypoint: &str) -> StrResult<Self> {
        if let Some(jobs) = build_config.typst.process.jobs {
            rayon::ThreadPoolBuilder::new()
                .num_threads(jobs)
                .use_current_thread()
                .build_global()
                .ok();
        }

        let library = {
            let inputs: Dict = build_config
                .typst
                .inputs
                .iter()
                .cloned()
                .chain(site_inputs(build_config))
                .map(|(key, value)| (key.as_str().into(), value.as_str().into_value()))
                .collect();
            let features = parse_features()?;

            Library::builder()
                .with_inputs(inputs)
                .with_features(features)
                .build()
        };

        let now = match build_config.typst.creation_timestamp {
            Some(timestamp) => Time::fixed_timestamp(timestamp)
                .map_err(|_| eco_format!("creation timestamp is out of range"))?,
            None => Time::system(),
        };

        let root = build_config.input_directory.canonicalize().map_err(|err| {
            eco_format!(
                "failed to canonicalize input directory {}: {err}",
                build_config.input_directory.display()
            )
        })?;

        let font_args = build_config.typst.font.clone();
        Ok(Self {
            workdir: std::env::current_dir().ok(),
            library: LazyHash::new(library),
            fonts: LazyLock::new(Box::new(move || discover_fonts(&font_args))),
            files: FileStore::new(WeibianFiles::new(
                root,
                entrypoint.as_bytes().to_vec(),
                &build_config.typst.package,
            )),
            now,
        })
    }

    fn root(&self) -> &Path {
        self.files.loader().project.path()
    }

    fn workdir(&self) -> &Path {
        self.workdir.as_deref().unwrap_or(Path::new("."))
    }

    fn dependencies(&mut self) -> impl Iterator<Item = PathBuf> + '_ {
        let (loader, deps) = self.files.dependencies();
        deps.filter_map(|id| loader.resolve(id).ok())
    }
}

impl World for LibraryWorld {
    fn library(&self) -> &LazyHash<Library> {
        &self.library
    }

    fn book(&self) -> &LazyHash<FontBook> {
        self.fonts.book()
    }

    fn main(&self) -> FileId {
        self.files.loader().main
    }

    fn source(&self, id: FileId) -> FileResult<typst::syntax::Source> {
        self.files.source(id)
    }

    fn file(&self, id: FileId) -> FileResult<Bytes> {
        self.files.file(id)
    }

    fn font(&self, index: usize) -> Option<Font> {
        self.fonts.font(index)
    }

    fn today(&self, offset: Option<Duration>) -> Option<Datetime> {
        self.now.today(offset)
    }
}

impl DiagnosticWorld for LibraryWorld {
    fn name(&self, id: FileId) -> String {
        if id == self.files.loader().main {
            return "<weibian-entrypoint>".into();
        }

        let vpath = id.vpath();
        match id.root() {
            VirtualRoot::Project => {
                let rooted = vpath.realize(self.root());
                pathdiff::diff_paths(rooted, self.workdir())
                    .map(|path| path.to_string_lossy().into_owned())
                    .unwrap_or_else(|| vpath.get_without_slash().into())
            }
            VirtualRoot::Package(package) => {
                format!("{package}{}", vpath.get_with_slash())
            }
        }
    }
}

struct WeibianFiles {
    main: FileId,
    project: FsRoot,
    packages: SystemPackages,
    entrypoint: Bytes,
}

impl WeibianFiles {
    fn new(root: PathBuf, entrypoint: Vec<u8>, package: &PackageArgs) -> Self {
        let main = FileId::unique(RootedPath::new(
            VirtualRoot::Project,
            VirtualPath::new("<weibian-entrypoint>").expect("synthetic path is valid"),
        ));

        Self {
            main,
            project: FsRoot::new(root),
            packages: system_packages(package),
            entrypoint: Bytes::new(entrypoint),
        }
    }

    fn resolve(&self, id: FileId) -> FileResult<PathBuf> {
        if id == self.main {
            return Err(FileError::NotFound("<weibian-entrypoint>".into()));
        }
        Ok(self.root(id)?.resolve(id.vpath()))
    }

    fn root(&self, id: FileId) -> FileResult<FsRoot> {
        Ok(match id.root() {
            VirtualRoot::Project => self.project.clone(),
            VirtualRoot::Package(spec) => self.packages.obtain(spec)?,
        })
    }
}

impl FileLoader for WeibianFiles {
    fn load(&self, id: FileId) -> FileResult<Bytes> {
        if id == self.main {
            Ok(self.entrypoint.clone())
        } else {
            self.root(id)?.load(id.vpath())
        }
    }
}

fn parse_features() -> StrResult<typst::Features> {
    forced_feature_names(std::env::var("TYPST_FEATURES").ok().as_deref())
        .into_iter()
        .map(|feature| match feature.as_str() {
            "html" => Ok(typst::Feature::Html),
            "bundle" => Ok(typst::Feature::Bundle),
            "a11y-extras" => Ok(typst::Feature::A11yExtras),
            _ => Err(eco_format!("unsupported Typst feature `{feature}`")),
        })
        .collect::<StrResult<Vec<_>>>()
        .map(|features| features.into_iter().collect())
}

fn discover_fonts(args: &FontArgs) -> FontStore {
    let mut fonts = FontStore::new();

    if !args.ignore_system_fonts {
        fonts.extend(typst_kit::fonts::system());
    }

    if !args.ignore_embedded_fonts {
        fonts.extend(typst_kit::fonts::embedded());
    }

    for path in &args.font_paths {
        fonts.extend(typst_kit::fonts::scan(path));
    }

    fonts
}

fn system_packages(args: &PackageArgs) -> SystemPackages {
    let user_agent = format!("typst/{}", typst::utils::version().raw());
    SystemPackages::from_parts(
        args.package_path
            .clone()
            .map(FsPackages::new)
            .or_else(FsPackages::system_data),
        args.package_cache_path
            .clone()
            .map(FsPackages::new)
            .or_else(FsPackages::system_cache),
        UniversePackages::new(SystemDownloader::new(user_agent)),
    )
}

fn print_diagnostics(
    world: &LibraryWorld,
    errors: &[SourceDiagnostic],
    warnings: &[SourceDiagnostic],
    format: DiagnosticFormat,
) -> StrResult<()> {
    typst_kit::diagnostics::emit(
        &mut crate::terminal::out(),
        world,
        errors.iter().chain(warnings),
        match format {
            DiagnosticFormat::Human => typst_kit::diagnostics::DiagnosticFormat::Human,
            DiagnosticFormat::Short => typst_kit::diagnostics::DiagnosticFormat::Short,
        },
    )
    .map_err(|err| eco_format!("failed to print diagnostics ({err})"))
}

fn open_output(config: &mut LibraryCompileConfig) -> StrResult<()> {
    let Some(viewer) = config.open.take() else {
        return Ok(());
    };

    let path = config
        .output_directory
        .canonicalize()
        .map_err(|err| eco_format!("failed to canonicalize path ({err})"))?;

    if let Some(viewer) = viewer {
        open::with_detached(path.as_os_str(), &viewer)
            .map_err(|err| eco_format!("failed to open file with {viewer} ({err})"))
    } else {
        open::that_detached(path.as_os_str()).map_err(|err| {
            let openers = open::commands(path.as_os_str())
                .iter()
                .map(|command| command.get_program().to_string_lossy())
                .collect::<Vec<_>>()
                .join(", ");
            eco_format!(
                "failed to open file with any of these resource openers: {openers} ({err})",
            )
        })
    }
}

fn write_deps(
    world: &mut LibraryWorld,
    dest: &CompileOutputPath,
    format: DepsFormat,
    outputs: Option<&[PathBuf]>,
) -> io::Result<()> {
    match format {
        DepsFormat::Json => write_deps_json(world, dest, outputs)?,
        DepsFormat::Zero => write_deps_zero(world, dest)?,
        DepsFormat::Make => {
            if let Some(outputs) = outputs {
                write_deps_make(world, dest, outputs)?;
            }
        }
    }
    Ok(())
}

fn write_deps_json(
    world: &mut LibraryWorld,
    dest: &CompileOutputPath,
    outputs: Option<&[PathBuf]>,
) -> io::Result<()> {
    let to_string = |dep: PathBuf, kind| {
        dep.into_os_string().into_string().map_err(|dep| {
            io::Error::new(
                io::ErrorKind::InvalidData,
                format!("{kind} {dep:?} is not valid UTF-8"),
            )
        })
    };

    let inputs = relative_dependencies(world)?
        .map(|dep| to_string(dep, "input"))
        .collect::<Result<_, _>>()?;
    let outputs = outputs
        .map(|outputs| {
            outputs
                .iter()
                .cloned()
                .map(|output| to_string(output, "output"))
                .collect::<Result<_, _>>()
        })
        .transpose()?;

    #[derive(Serialize)]
    struct Deps {
        inputs: Vec<String>,
        outputs: Option<Vec<String>>,
    }

    serde_json::to_writer(open_compile_output(dest)?, &Deps { inputs, outputs })?;
    Ok(())
}

fn write_deps_zero(world: &mut LibraryWorld, dest: &CompileOutputPath) -> io::Result<()> {
    let mut dest = open_compile_output(dest)?;
    for dep in relative_dependencies(world)? {
        dest.write_all(dep.as_os_str().as_encoded_bytes())?;
        dest.write_all(b"\0")?;
    }
    Ok(())
}

fn write_deps_make(
    world: &mut LibraryWorld,
    dest: &CompileOutputPath,
    outputs: &[PathBuf],
) -> io::Result<()> {
    let mut buffer = Vec::new();
    for (i, output) in outputs.iter().enumerate() {
        let Some(string) = output.to_str() else {
            continue;
        };
        if i != 0 {
            buffer.write_all(b" ")?;
        }
        buffer.write_all(munge(string).as_bytes())?;
    }

    let mut dest = open_compile_output(dest)?;
    dest.write_all(&buffer)?;
    dest.write_all(b":")?;

    for dep in relative_dependencies(world)? {
        let Some(string) = dep.to_str() else { continue };
        dest.write_all(b" ")?;
        dest.write_all(munge(string).as_bytes())?;
    }
    dest.write_all(b"\n")?;

    Ok(())
}

fn relative_dependencies(
    world: &mut LibraryWorld,
) -> io::Result<impl Iterator<Item = PathBuf> + '_> {
    let root = world.root().to_owned();
    let current_dir = std::env::current_dir()?;
    let relative_root = pathdiff::diff_paths(&root, &current_dir).unwrap_or_else(|| root.clone());
    Ok(world.dependencies().map(move |dependency| {
        dependency
            .strip_prefix(&root)
            .map_or_else(|_| dependency.clone(), |path| relative_root.join(path))
    }))
}

fn open_compile_output(output: &CompileOutputPath) -> io::Result<OpenCompileOutput<'_>> {
    match output {
        CompileOutputPath::Stdout => Ok(OpenCompileOutput::Stdout(io::stdout().lock())),
        CompileOutputPath::Path(path) => fs::File::create(path).map(OpenCompileOutput::File),
    }
}

enum OpenCompileOutput<'a> {
    Stdout(io::StdoutLock<'a>),
    File(fs::File),
}

impl Write for OpenCompileOutput<'_> {
    fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
        match self {
            Self::Stdout(stdout) => stdout.write(buf),
            Self::File(file) => file.write(buf),
        }
    }

    fn flush(&mut self) -> io::Result<()> {
        match self {
            Self::Stdout(stdout) => stdout.flush(),
            Self::File(file) => file.flush(),
        }
    }
}

fn munge(s: &str) -> String {
    let mut res = String::with_capacity(s.len());
    let mut slashes = 0;
    for c in s.chars() {
        match c {
            '\\' => slashes += 1,
            '$' => {
                res.push('$');
                slashes = 0;
            }
            ':' => {
                res.push('\\');
                slashes = 0;
            }
            ' ' | '\t' => {
                for _ in 0..slashes + 1 {
                    res.push('\\');
                }
                slashes = 0;
            }
            '#' => {
                res.push('\\');
                slashes = 0;
            }
            _ => slashes = 0,
        };
        res.push(c);
    }
    res
}

impl From<PdfStandard> for typst_pdf::PdfStandard {
    fn from(standard: PdfStandard) -> Self {
        match standard {
            PdfStandard::V_1_4 => typst_pdf::PdfStandard::V_1_4,
            PdfStandard::V_1_5 => typst_pdf::PdfStandard::V_1_5,
            PdfStandard::V_1_6 => typst_pdf::PdfStandard::V_1_6,
            PdfStandard::V_1_7 => typst_pdf::PdfStandard::V_1_7,
            PdfStandard::V_2_0 => typst_pdf::PdfStandard::V_2_0,
            PdfStandard::A_1b => typst_pdf::PdfStandard::A_1b,
            PdfStandard::A_1a => typst_pdf::PdfStandard::A_1a,
            PdfStandard::A_2b => typst_pdf::PdfStandard::A_2b,
            PdfStandard::A_2u => typst_pdf::PdfStandard::A_2u,
            PdfStandard::A_2a => typst_pdf::PdfStandard::A_2a,
            PdfStandard::A_3b => typst_pdf::PdfStandard::A_3b,
            PdfStandard::A_3u => typst_pdf::PdfStandard::A_3u,
            PdfStandard::A_3a => typst_pdf::PdfStandard::A_3a,
            PdfStandard::A_4 => typst_pdf::PdfStandard::A_4,
            PdfStandard::A_4f => typst_pdf::PdfStandard::A_4f,
            PdfStandard::A_4e => typst_pdf::PdfStandard::A_4e,
            PdfStandard::UA_1 => typst_pdf::PdfStandard::Ua_1,
        }
    }
}

#[cfg(test)]
mod tests {
    use std::fs;
    use std::path::{Path, PathBuf};
    use std::time::{SystemTime, UNIX_EPOCH};

    use crate::args::{CompilerBackendKind, TypstCompileArgs};
    use crate::bundle::{collect_asset_files, collect_typst_sources, render_entrypoint};
    use crate::config::{BuildConfig, InputFilters, SiteSettings};

    use super::LibraryCompiler;
    use crate::compiler::CompilerBackend;

    fn temp_project(name: &str) -> PathBuf {
        let stamp = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .expect("system clock before unix epoch")
            .as_nanos();
        let root = std::env::temp_dir().join(format!(
            "weibian-library-{name}-{}-{stamp}",
            std::process::id()
        ));
        fs::create_dir_all(&root).expect("failed to create temp project");
        root
    }

    fn write(path: &Path, contents: impl AsRef<[u8]>) {
        if let Some(parent) = path.parent() {
            fs::create_dir_all(parent).expect("failed to create parent directory");
        }
        fs::write(path, contents).expect("failed to write test file");
    }

    #[test]
    fn library_backend_writes_bundle_documents_and_assets() {
        let root = temp_project("bundle");
        let input = root.join("typ");
        let public = input.join("public");
        let output = root.join("dist");
        write(&input.join("main.typ"), "#document(\"index.html\")[Hello]");
        write(&public.join("css/site.css"), "body { color: black; }");

        let filters = InputFilters::new(&["**/*.typ".into()], &[]).expect("filters should build");
        let build_config = BuildConfig {
            input_directory: input.clone(),
            input_filters: filters,
            public_directory: public.clone(),
            output_directory: output.clone(),
            compiler_backend: CompilerBackendKind::Library,
            site: SiteSettings {
                domain: None,
                root_dir: "/".into(),
                trailing_slash: false,
            },
            typst: TypstCompileArgs::default(),
        };
        let sources = collect_typst_sources(&input, &build_config.input_filters)
            .expect("sources should collect");
        let assets = collect_asset_files(&input, &public).expect("assets should collect");
        let entrypoint = render_entrypoint(&sources, &assets);

        LibraryCompiler::compile_bundle(&build_config, &entrypoint)
            .expect("library backend should compile bundle");

        assert!(output.join("index.html").exists());
        assert_eq!(
            fs::read_to_string(output.join("css/site.css")).expect("asset should be readable"),
            "body { color: black; }"
        );
    }
}
