#[cfg(any(feature = "library-compiler", test))]
use std::collections::BTreeSet;
#[cfg(any(feature = "library-compiler", test))]
use std::fs;
#[cfg(feature = "library-compiler")]
use std::io::{self, Write};
#[cfg(feature = "library-compiler")]
use std::net::SocketAddr;
#[cfg(any(feature = "library-compiler", test))]
use std::path::PathBuf;
#[cfg(feature = "library-compiler")]
use std::time::Duration;

#[cfg(feature = "library-compiler")]
use codespan_reporting::term::{self, termcolor};
use ecow::eco_format;
#[cfg(feature = "library-compiler")]
use termcolor::WriteColor;
#[cfg(feature = "library-compiler")]
use typst_kit::server::HttpServer;
#[cfg(feature = "library-compiler")]
use typst_kit::watcher::Watcher;

use crate::args::{CompilerBackendKind, WatchCommand};
#[cfg(feature = "library-compiler")]
use crate::compile::prepare_compile;
#[cfg(feature = "library-compiler")]
use crate::compiler::library::{CompileOutcome, LibraryCompileSession};
use crate::config::{BuildConfig, WeibianConfig};
use crate::error::StrResult;

pub fn watch(command: &WatchCommand, config: &WeibianConfig) -> StrResult<()> {
    let build_config = BuildConfig::from(&command.args, config)?;
    if !cfg!(feature = "library-compiler") {
        return watch_with_library(&build_config, command);
    }

    match build_config.compiler_backend {
        CompilerBackendKind::Library => watch_with_library(&build_config, command),
        CompilerBackendKind::Host => Err(eco_format!(
            "watch mode is only implemented for the library compiler backend"
        )),
    }
}

#[cfg(feature = "library-compiler")]
fn watch_with_library(build_config: &BuildConfig, command: &WatchCommand) -> StrResult<()> {
    let prepared = prepare_compile(build_config)?;
    let server = if command.server.no_serve {
        None
    } else {
        Some(HttpServer::new(
            &build_config.input_directory.display().to_string(),
            command.server.port,
            !command.server.no_reload,
        )?)
    };
    let server_addr = server.as_ref().map(HttpServer::addr);

    let mut session =
        LibraryCompileSession::new_with_server(build_config, &prepared.entrypoint, server)?;
    let mut watcher = Watcher::new(Some(build_config.output_directory.clone()))?;

    compile_and_report(build_config, server_addr, &mut session)?;

    loop {
        let dependencies = session.dependencies().collect::<Vec<_>>();
        watcher.update(collect_watch_paths(build_config, dependencies)?)?;
        watcher.wait()?;

        print_status(Status::Compiling.print_header(build_config, server_addr))?;

        match prepare_compile(build_config) {
            Ok(prepared) => {
                session.replace_entrypoint(&prepared.entrypoint);
                session.reset();
                compile_and_report_without_header(&mut session)?;
                comemo::evict(10);
            }
            Err(err) => {
                print_status(Status::Error.print_message())?;
                print_status(print_watch_error(&err))?;
            }
        }
    }
}

#[cfg(not(feature = "library-compiler"))]
fn watch_with_library(_build_config: &BuildConfig, _command: &WatchCommand) -> StrResult<()> {
    Err(eco_format!(
        "watch mode requires a binary built with the `library-compiler` feature"
    ))
}

#[cfg(any(feature = "library-compiler", test))]
fn collect_watch_paths(
    build_config: &BuildConfig,
    dependencies: impl IntoIterator<Item = PathBuf>,
) -> StrResult<Vec<PathBuf>> {
    let mut paths = BTreeSet::new();

    collect_existing_dirs(build_config, &build_config.input_directory, &mut paths)?;

    for dependency in dependencies {
        if is_output_path(build_config, &dependency) {
            continue;
        }
        paths.insert(dependency);
    }

    Ok(paths.into_iter().collect())
}

#[cfg(any(feature = "library-compiler", test))]
fn collect_existing_dirs(
    build_config: &BuildConfig,
    dir: &std::path::Path,
    paths: &mut BTreeSet<PathBuf>,
) -> StrResult<()> {
    if !dir.exists() {
        return Ok(());
    }

    if dir != build_config.input_directory && is_output_path(build_config, dir) {
        return Ok(());
    }

    paths.insert(dir.to_path_buf());

    let entries = fs::read_dir(dir)
        .map_err(|err| eco_format!("failed to read watch directory {}: {err}", dir.display()))?;
    for entry in entries {
        let entry =
            entry.map_err(|err| eco_format!("failed to read watch directory entry: {err}"))?;
        let path = entry.path();
        let file_type = entry
            .file_type()
            .map_err(|err| eco_format!("failed to read file type for {}: {err}", path.display()))?;
        if file_type.is_dir() {
            collect_existing_dirs(build_config, &path, paths)?;
        }
    }

    Ok(())
}

#[cfg(any(feature = "library-compiler", test))]
fn is_output_path(build_config: &BuildConfig, path: &std::path::Path) -> bool {
    path.starts_with(&build_config.output_directory)
}

#[cfg(feature = "library-compiler")]
fn compile_and_report(
    build_config: &BuildConfig,
    server_addr: Option<SocketAddr>,
    session: &mut LibraryCompileSession,
) -> StrResult<()> {
    print_status(Status::Compiling.print_header(build_config, server_addr))?;
    compile_and_report_without_header(session)
}

#[cfg(feature = "library-compiler")]
fn compile_and_report_without_header(session: &mut LibraryCompileSession) -> StrResult<()> {
    let started = std::time::Instant::now();
    match session.compile_once() {
        Ok(CompileOutcome { had_warnings }) => {
            let duration = started.elapsed();
            if had_warnings {
                print_status(Status::PartialSuccess(duration).print_message())?;
            } else {
                print_status(Status::Success(duration).print_message())?;
            }
        }
        Err(_) => {
            print_status(Status::Error.print_message())?;
        }
    }

    Ok(())
}

#[cfg(feature = "library-compiler")]
fn print_status(result: io::Result<()>) -> StrResult<()> {
    result.map_err(|err| eco_format!("failed to write watch status ({err})"))
}

#[cfg(feature = "library-compiler")]
fn print_watch_error(message: &str) -> io::Result<()> {
    let styles = term::Styles::default();

    let mut output = crate::terminal::out();
    output.set_color(&styles.header_error)?;
    write!(output, "error")?;

    output.reset()?;
    writeln!(output, ": {message}")?;
    output.flush()
}

#[cfg(feature = "library-compiler")]
enum Status {
    Compiling,
    Success(Duration),
    PartialSuccess(Duration),
    Error,
}

#[cfg(feature = "library-compiler")]
impl Status {
    fn print_header(
        &self,
        build_config: &BuildConfig,
        server_addr: Option<SocketAddr>,
    ) -> io::Result<()> {
        let color = self.color();
        let mut out = crate::terminal::out();

        out.clear_screen()?;
        out.set_color(&color)?;
        write!(out, "watching")?;
        out.reset()?;
        writeln!(out, " {}", build_config.input_directory.display())?;

        out.set_color(&color)?;
        write!(out, "writing to")?;
        out.reset()?;
        writeln!(out, " {}", build_config.output_directory.display())?;

        if let Some(addr) = server_addr {
            out.set_color(&color)?;
            write!(out, "serving at")?;
            out.reset()?;
            writeln!(out, " http://{addr}")?;
        }

        writeln!(out)?;
        self.write_message(&mut out)?;
        writeln!(out)?;
        out.flush()
    }

    fn print_message(&self) -> io::Result<()> {
        let mut out = crate::terminal::out();
        self.write_message(&mut out)?;
        writeln!(out)?;
        out.flush()
    }

    fn write_message(&self, out: &mut impl WriteColor) -> io::Result<()> {
        let timestamp = chrono::offset::Local::now().format("%H:%M:%S");
        write!(out, "[{timestamp}] {}", self.message())
    }

    fn message(&self) -> String {
        match *self {
            Self::Compiling => "compiling ...".into(),
            Self::Success(duration) => {
                format!(
                    "compiled successfully in {}",
                    typst::utils::format_duration(duration)
                )
            }
            Self::PartialSuccess(duration) => {
                format!(
                    "compiled with warnings in {}",
                    typst::utils::format_duration(duration)
                )
            }
            Self::Error => "compiled with errors".into(),
        }
    }

    fn color(&self) -> termcolor::ColorSpec {
        let styles = term::Styles::default();
        match self {
            Self::Error => styles.header_error,
            Self::PartialSuccess(_) => styles.header_warning,
            _ => styles.header_note,
        }
    }
}

#[cfg(test)]
mod tests {
    use std::fs;
    use std::path::Path;
    use std::time::{SystemTime, UNIX_EPOCH};

    use crate::args::{
        CompileArgs, CompilerBackendKind, ServerArgs, SiteArgs, TypstCompileArgs, WatchCommand,
    };
    use crate::config::{BuildConfig, InputFilters, SiteSettings, WeibianConfig};

    use super::{collect_watch_paths, watch};

    fn temp_project(name: &str) -> std::path::PathBuf {
        let stamp = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .expect("system clock before unix epoch")
            .as_nanos();
        let root = std::env::temp_dir().join(format!(
            "weibian-watch-{name}-{}-{stamp}",
            std::process::id()
        ));
        fs::create_dir_all(&root).expect("failed to create temp project");
        root
    }

    fn build_config(root: &Path) -> BuildConfig {
        BuildConfig {
            input_directory: root.join("typ"),
            input_filters: InputFilters::new(&[], &[]).expect("filters should build"),
            public_directory: root.join("typ/public"),
            output_directory: root.join("dist"),
            compiler_backend: CompilerBackendKind::Library,
            host_compiler: std::path::PathBuf::from("typst"),
            site: SiteSettings {
                domain: None,
                root_dir: "/".into(),
                trailing_slash: false,
            },
            typst: TypstCompileArgs::default(),
        }
    }

    #[cfg(feature = "library-compiler")]
    #[test]
    fn watch_rejects_host_compiler_backend() {
        let command = WatchCommand {
            args: CompileArgs {
                input: None,
                output: None,
                public: None,
                compiler: Some(CompilerBackendKind::Host),
                host_compiler: None,
                site: SiteArgs {
                    domain: None,
                    root_dir: None,
                    trailing_slash: None,
                },
                typst: TypstCompileArgs::default(),
            },
            server: ServerArgs::default(),
        };

        let err =
            watch(&command, &WeibianConfig::default()).expect_err("host watch should be rejected");

        assert!(err.contains("only implemented for the library compiler"));
    }

    #[cfg(not(feature = "library-compiler"))]
    #[test]
    fn watch_requires_library_compiler_feature() {
        let command = WatchCommand {
            args: CompileArgs {
                input: None,
                output: None,
                public: None,
                compiler: Some(CompilerBackendKind::Host),
                host_compiler: None,
                site: SiteArgs {
                    domain: None,
                    root_dir: None,
                    trailing_slash: None,
                },
                typst: TypstCompileArgs::default(),
            },
            server: ServerArgs::default(),
        };

        let err = watch(&command, &WeibianConfig::default())
            .expect_err("watch should require the library compiler feature");

        assert!(err.contains("requires a binary built with the `library-compiler` feature"));
    }

    #[test]
    fn collect_watch_paths_includes_input_dirs_and_dependencies_but_not_output() {
        let root = temp_project("paths");
        let build = build_config(&root);
        fs::create_dir_all(build.input_directory.join("blog"))
            .expect("failed to create input subtree");
        fs::create_dir_all(build.public_directory.join("css"))
            .expect("failed to create public subtree");
        fs::create_dir_all(&build.output_directory).expect("failed to create output dir");
        let dependency = build.input_directory.join("main.typ");
        fs::write(&dependency, "#document(\"index.html\")[Hi]")
            .expect("failed to write dependency");

        let paths = collect_watch_paths(
            &build,
            [
                dependency.clone(),
                build.output_directory.join("index.html"),
            ],
        )
        .expect("watch paths should collect");

        assert!(paths.contains(&build.input_directory));
        assert!(paths.contains(&build.input_directory.join("blog")));
        assert!(paths.contains(&build.public_directory));
        assert!(paths.contains(&build.public_directory.join("css")));
        assert!(paths.contains(&dependency));
        assert!(
            !paths
                .iter()
                .any(|path| path.starts_with(&build.output_directory))
        );
    }

    #[test]
    fn collect_watch_paths_skips_output_directory_inside_input_directory() {
        let root = temp_project("nested-output");
        let mut build = build_config(&root);
        build.output_directory = build.input_directory.join("dist");
        fs::create_dir_all(build.output_directory.join("assets"))
            .expect("failed to create output subtree");
        fs::create_dir_all(build.input_directory.join("notes"))
            .expect("failed to create input subtree");

        let paths = collect_watch_paths(&build, [build.output_directory.join("index.html")])
            .expect("watch paths should collect");

        assert!(paths.contains(&build.input_directory));
        assert!(paths.contains(&build.input_directory.join("notes")));
        assert!(
            !paths
                .iter()
                .any(|path| path.starts_with(&build.output_directory))
        );
    }
}
