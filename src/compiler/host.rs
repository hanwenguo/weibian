use std::ffi::OsString;
use std::io::{self, Write};
use std::path::Path;
use std::process::{Command, Stdio};

use ecow::eco_format;

use crate::args::{CompileOutputPath, TypstCompileArgs};
use crate::compiler::{CompilerBackend, forced_features, site_inputs};
use crate::config::BuildConfig;
use crate::error::StrResult;

pub struct HostCompiler;

impl CompilerBackend for HostCompiler {
    fn compile_bundle(build_config: &BuildConfig, entrypoint: &str) -> StrResult<()> {
        compile_bundle(build_config, entrypoint)
    }
}

fn compile_bundle(build_config: &BuildConfig, entrypoint: &str) -> StrResult<()> {
    let features = std::env::var("TYPST_FEATURES").ok();
    let args = build_typst_args(build_config, features.as_deref());
    let compiler = host_compiler_path(build_config);
    let mut child = Command::new(compiler)
        .args(&args)
        .stdin(Stdio::piped())
        .stdout(Stdio::inherit())
        .stderr(Stdio::inherit())
        .spawn()
        .map_err(|err| {
            eco_format!(
                "failed to run Typst host compiler {}: {err}",
                compiler.display()
            )
        })?;

    if let Some(mut stdin) = child.stdin.take() {
        match stdin.write_all(entrypoint.as_bytes()) {
            Ok(()) => {}
            Err(err) if err.kind() == io::ErrorKind::BrokenPipe => {}
            Err(err) => {
                return Err(eco_format!(
                    "failed to write generated Typst entrypoint: {err}"
                ));
            }
        }
    }

    let status = child
        .wait()
        .map_err(|err| eco_format!("failed to wait for typst: {err}"))?;
    if !status.success() {
        return Err(eco_format!("typst compile failed with status {status}"));
    }

    Ok(())
}

pub fn host_compiler_path(build_config: &BuildConfig) -> &Path {
    &build_config.host_compiler
}

pub fn build_typst_args(build_config: &BuildConfig, env_features: Option<&str>) -> Vec<OsString> {
    let typst = &build_config.typst;
    let mut args = Vec::new();
    args.push(OsString::from("compile"));

    push_forwarded_typst_args(&mut args, typst);

    args.push(OsString::from("--root"));
    args.push(build_config.input_directory.as_os_str().to_owned());
    args.push(OsString::from("--format"));
    args.push(OsString::from("bundle"));
    args.push(OsString::from("--features"));
    args.push(OsString::from(forced_features(env_features)));

    for (key, value) in site_inputs(build_config) {
        args.push(OsString::from("--input"));
        args.push(OsString::from(format!("{key}={value}")));
    }

    args.push(OsString::from("-"));
    args.push(build_config.output_directory.as_os_str().to_owned());
    args
}

fn push_forwarded_typst_args(args: &mut Vec<OsString>, typst: &TypstCompileArgs) {
    for (key, value) in &typst.inputs {
        args.push(OsString::from("--input"));
        args.push(OsString::from(format!("{key}={value}")));
    }

    for font_path in &typst.font.font_paths {
        args.push(OsString::from("--font-path"));
        args.push(font_path.as_os_str().to_owned());
    }

    if typst.font.ignore_system_fonts {
        args.push(OsString::from("--ignore-system-fonts"));
    }

    if typst.font.ignore_embedded_fonts {
        args.push(OsString::from("--ignore-embedded-fonts"));
    }

    push_optional_path(
        args,
        "--package-path",
        typst.package.package_path.as_deref(),
    );
    push_optional_path(
        args,
        "--package-cache-path",
        typst.package.package_cache_path.as_deref(),
    );

    if let Some(timestamp) = typst.creation_timestamp {
        args.push(OsString::from("--creation-timestamp"));
        args.push(OsString::from(timestamp.to_string()));
    }

    if typst.pretty {
        args.push(OsString::from("--pretty"));
    }

    if let Some(pages) = &typst.pages {
        args.push(OsString::from("--pages"));
        args.push(OsString::from(
            pages
                .iter()
                .map(ToString::to_string)
                .collect::<Vec<_>>()
                .join(","),
        ));
    }

    if !typst.pdf_standard.is_empty() {
        args.push(OsString::from("--pdf-standard"));
        args.push(OsString::from(
            typst
                .pdf_standard
                .iter()
                .map(ToString::to_string)
                .collect::<Vec<_>>()
                .join(","),
        ));
    }

    if typst.no_pdf_tags {
        args.push(OsString::from("--no-pdf-tags"));
    }

    if typst.ppi != 144.0 {
        args.push(OsString::from("--ppi"));
        args.push(OsString::from(typst.ppi.to_string()));
    }

    push_optional_path(args, "--make-deps", typst.make_deps.as_deref());

    if let Some(deps) = &typst.deps {
        args.push(OsString::from("--deps"));
        args.push(output_path_arg(deps));
    }

    args.push(OsString::from("--deps-format"));
    args.push(OsString::from(typst.deps_format.to_string()));

    if let Some(jobs) = typst.process.jobs {
        args.push(OsString::from("--jobs"));
        args.push(OsString::from(jobs.to_string()));
    }

    args.push(OsString::from("--diagnostic-format"));
    args.push(OsString::from(typst.process.diagnostic_format.to_string()));

    if let Some(open) = &typst.open {
        args.push(OsString::from("--open"));
        if let Some(viewer) = open {
            args.push(OsString::from(viewer));
        }
    }

    push_optional_path(args, "--timings", typst.timings.as_deref());
}

fn push_optional_path(args: &mut Vec<OsString>, flag: &str, path: Option<&Path>) {
    if let Some(path) = path {
        args.push(OsString::from(flag));
        args.push(path.as_os_str().to_owned());
    }
}

fn output_path_arg(output: &CompileOutputPath) -> OsString {
    match output {
        CompileOutputPath::Stdout => OsString::from("-"),
        CompileOutputPath::Path(path) => path.as_os_str().to_owned(),
    }
}

#[cfg(test)]
mod tests {
    use std::path::{Path, PathBuf};

    use crate::args::{
        CompileOutputPath, CompilerBackendKind, DepsFormat, DiagnosticFormat, FontArgs,
        PackageArgs, ProcessArgs, TypstCompileArgs,
    };
    use crate::config::{BuildConfig, InputFilters, SiteSettings};

    use super::{build_typst_args, host_compiler_path};

    #[test]
    fn build_typst_args_forces_bundle_controls_and_forwards_user_options() {
        let build_config = BuildConfig {
            input_directory: PathBuf::from("typ"),
            input_filters: InputFilters::new(&[], &[]).expect("filters should build"),
            public_directory: PathBuf::from("typ/public"),
            output_directory: PathBuf::from("dist"),
            compiler_backend: CompilerBackendKind::Library,
            host_compiler: PathBuf::from("/custom/typst"),
            site: SiteSettings {
                domain: Some("https://example.com".into()),
                root_dir: "/notes/".into(),
                trailing_slash: true,
            },
            typst: TypstCompileArgs {
                inputs: vec![("custom".into(), "value".into())],
                font: FontArgs {
                    font_paths: vec![PathBuf::from("fonts")],
                    ignore_system_fonts: true,
                    ignore_embedded_fonts: true,
                },
                package: PackageArgs {
                    package_path: Some(PathBuf::from("packages")),
                    package_cache_path: Some(PathBuf::from("cache/packages")),
                },
                creation_timestamp: Some(1_700_000_000),
                pretty: true,
                pages: None,
                pdf_standard: vec![],
                no_pdf_tags: true,
                ppi: 192.0,
                make_deps: Some(PathBuf::from("deps.mk")),
                deps: Some(CompileOutputPath::Stdout),
                deps_format: DepsFormat::Make,
                process: ProcessArgs {
                    jobs: Some(2),
                    diagnostic_format: DiagnosticFormat::Short,
                },
                open: Some(Some("viewer".into())),
                timings: Some(PathBuf::from("timings.json")),
            },
        };

        let args = build_typst_args(&build_config, Some("html,a11y-extras"));
        let args: Vec<_> = args
            .iter()
            .map(|arg| arg.to_string_lossy().into_owned())
            .collect();

        assert_eq!(
            args,
            [
                "compile",
                "--input",
                "custom=value",
                "--font-path",
                "fonts",
                "--ignore-system-fonts",
                "--ignore-embedded-fonts",
                "--package-path",
                "packages",
                "--package-cache-path",
                "cache/packages",
                "--creation-timestamp",
                "1700000000",
                "--pretty",
                "--no-pdf-tags",
                "--ppi",
                "192",
                "--make-deps",
                "deps.mk",
                "--deps",
                "-",
                "--deps-format",
                "make",
                "--jobs",
                "2",
                "--diagnostic-format",
                "short",
                "--open",
                "viewer",
                "--timings",
                "timings.json",
                "--root",
                "typ",
                "--format",
                "bundle",
                "--features",
                "a11y-extras,bundle,html",
                "--input",
                "wb-domain=https://example.com",
                "--input",
                "wb-root-dir=/notes/",
                "--input",
                "wb-trailing-slash=true",
                "-",
                "dist",
            ]
        );
    }

    #[test]
    fn host_compiler_path_uses_build_config_path() {
        let build_config = BuildConfig {
            input_directory: PathBuf::from("typ"),
            input_filters: InputFilters::new(&[], &[]).expect("filters should build"),
            public_directory: PathBuf::from("typ/public"),
            output_directory: PathBuf::from("dist"),
            compiler_backend: CompilerBackendKind::Host,
            host_compiler: PathBuf::from("/custom/typst"),
            site: SiteSettings {
                domain: None,
                root_dir: "/".into(),
                trailing_slash: false,
            },
            typst: TypstCompileArgs::default(),
        };

        assert_eq!(
            host_compiler_path(&build_config),
            Path::new("/custom/typst")
        );
    }
}
