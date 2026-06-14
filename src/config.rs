use std::fmt;
use std::path::{Path, PathBuf};

use ecow::eco_format;
use figment::Figment;
use figment::providers::{Format, Toml};
use globset::{Glob, GlobSet, GlobSetBuilder};
use serde::de::{self, SeqAccess, Visitor};
use serde::{Deserialize, Deserializer};

use crate::args::{CompileArgs, CompilerBackendKind, TypstCompileArgs};
use crate::error::StrResult;

const DEFAULT_CONFIG_PATH: &str = "weibian.toml";

#[derive(Debug, Default, Deserialize)]
pub struct WeibianConfig {
    #[serde(default, alias = "directories")]
    pub files: FilesConfig,

    #[serde(default)]
    pub compiler: CompilerConfig,

    #[serde(default)]
    pub site: SiteConfig,
}

#[derive(Debug, Default, Deserialize)]
pub struct FilesConfig {
    pub input_dir: Option<PathBuf>,
    pub output_dir: Option<PathBuf>,
    pub public_dir: Option<PathBuf>,
    #[serde(default, deserialize_with = "deserialize_glob_list")]
    pub include: Vec<String>,
    #[serde(default, deserialize_with = "deserialize_glob_list")]
    pub exclude: Vec<String>,
}

#[derive(Debug, Default, Deserialize)]
pub struct SiteConfig {
    pub domain: Option<String>,
    pub root_dir: Option<String>,
    pub trailing_slash: Option<bool>,
}

#[derive(Debug, Default, Deserialize)]
pub struct CompilerConfig {
    pub backend: Option<CompilerBackendKind>,
    pub path: Option<PathBuf>,
}
#[derive(Debug, Clone)]
pub struct SiteSettings {
    pub domain: Option<String>,
    pub root_dir: String,
    pub trailing_slash: bool,
}

#[derive(Debug, Clone)]
pub struct InputFilters {
    include: GlobSet,
    exclude: GlobSet,
    include_all: bool,
    has_exclude: bool,
}

impl InputFilters {
    pub fn new(include: &[String], exclude: &[String]) -> StrResult<Self> {
        let include_set = build_globset(include, "include")?;
        let exclude_set = build_globset(exclude, "exclude")?;
        Ok(Self {
            include: include_set,
            exclude: exclude_set,
            include_all: include.is_empty(),
            has_exclude: !exclude.is_empty(),
        })
    }

    pub fn allows(&self, relative_path: &Path) -> bool {
        if self.exclude.is_match(relative_path) {
            return false;
        }
        if self.include_all {
            return true;
        }
        self.include.is_match(relative_path)
    }

    pub fn has_filters(&self) -> bool {
        self.has_exclude || !self.include_all
    }
}

/// A preprocessed `CompileCommand` with config defaults applied.
#[derive(Debug, Clone)]
pub struct BuildConfig {
    pub input_directory: PathBuf,
    pub input_filters: InputFilters,
    pub public_directory: PathBuf,
    pub output_directory: PathBuf,
    pub compiler_backend: CompilerBackendKind,
    pub host_compiler: PathBuf,
    pub site: SiteSettings,
    pub typst: TypstCompileArgs,
}

pub fn load_config(config_path: Option<&Path>) -> StrResult<WeibianConfig> {
    let (path, is_default) = match config_path {
        Some(path) => (path.to_path_buf(), false),
        None => (PathBuf::from(DEFAULT_CONFIG_PATH), true),
    };

    if !path.exists() {
        if is_default {
            return Ok(WeibianConfig::default());
        }
        return Err(eco_format!("config file {} does not exist", path.display()));
    }

    Figment::new()
        .merge(Toml::file(&path))
        .extract::<WeibianConfig>()
        .map_err(|err| eco_format!("failed to load config {}: {err}", path.display()))
}

impl BuildConfig {
    pub fn from(args: &CompileArgs, config: &WeibianConfig) -> StrResult<Self> {
        let input_directory =
            resolve_dir(args.input.as_ref(), config.files.input_dir.as_ref(), "typ");
        let input_filters = InputFilters::new(&config.files.include, &config.files.exclude)?;
        let public_directory = resolve_public_dir(
            &input_directory,
            args.public.as_ref(),
            config.files.public_dir.as_ref(),
        )?;
        let output_directory = resolve_dir(
            args.output.as_ref(),
            config.files.output_dir.as_ref(),
            "dist",
        );

        let domain = args
            .site
            .domain
            .clone()
            .or_else(|| config.site.domain.clone());
        let root_dir = normalize_root_dir(
            args.site
                .root_dir
                .as_deref()
                .or(config.site.root_dir.as_deref()),
        );
        let trailing_slash = args
            .site
            .trailing_slash
            .unwrap_or(config.site.trailing_slash.unwrap_or(false));
        let compiler_backend = args
            .compiler
            .or(config.compiler.backend)
            .unwrap_or_else(CompilerBackendKind::compiled_default);
        let host_compiler = args
            .host_compiler
            .clone()
            .or_else(|| config.compiler.path.clone())
            .unwrap_or_else(|| PathBuf::from("typst"));
        Ok(Self {
            input_directory,
            input_filters,
            public_directory,
            output_directory,
            compiler_backend,
            host_compiler,
            site: SiteSettings {
                domain,
                root_dir,
                trailing_slash,
            },
            typst: args.typst.clone(),
        })
    }
}

fn build_globset(patterns: &[String], label: &str) -> StrResult<GlobSet> {
    let mut builder = GlobSetBuilder::new();
    for pattern in patterns {
        let glob = Glob::new(pattern)
            .map_err(|err| eco_format!("invalid {label} glob \"{pattern}\": {err}"))?;
        builder.add(glob);
    }
    builder
        .build()
        .map_err(|err| eco_format!("failed to build {label} glob set: {err}"))
}

fn deserialize_glob_list<'de, D>(deserializer: D) -> Result<Vec<String>, D::Error>
where
    D: Deserializer<'de>,
{
    struct GlobListVisitor;

    impl<'de> Visitor<'de> for GlobListVisitor {
        type Value = Vec<String>;

        fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
            formatter.write_str("a string or a list of strings")
        }

        fn visit_str<E>(self, value: &str) -> Result<Self::Value, E>
        where
            E: de::Error,
        {
            Ok(vec![value.to_string()])
        }

        fn visit_string<E>(self, value: String) -> Result<Self::Value, E>
        where
            E: de::Error,
        {
            Ok(vec![value])
        }

        fn visit_seq<A>(self, mut seq: A) -> Result<Self::Value, A::Error>
        where
            A: SeqAccess<'de>,
        {
            let mut values = Vec::new();
            while let Some(value) = seq.next_element::<String>()? {
                values.push(value);
            }
            Ok(values)
        }
    }

    deserializer.deserialize_any(GlobListVisitor)
}

fn resolve_dir(cli: Option<&PathBuf>, config: Option<&PathBuf>, default: &str) -> PathBuf {
    cli.cloned()
        .or_else(|| config.cloned())
        .unwrap_or_else(|| PathBuf::from(default))
}

fn resolve_public_dir(
    input_directory: &Path,
    cli: Option<&PathBuf>,
    config: Option<&PathBuf>,
) -> StrResult<PathBuf> {
    let raw = cli
        .cloned()
        .or_else(|| config.cloned())
        .unwrap_or_else(|| PathBuf::from("public"));
    let public_directory = if raw.is_absolute() {
        raw
    } else {
        input_directory.join(raw)
    };

    if public_directory.exists() {
        let input = input_directory.canonicalize().map_err(|err| {
            eco_format!(
                "failed to canonicalize input directory {}: {err}",
                input_directory.display()
            )
        })?;
        let public = public_directory.canonicalize().map_err(|err| {
            eco_format!(
                "failed to canonicalize public directory {}: {err}",
                public_directory.display()
            )
        })?;
        if !public.starts_with(&input) {
            return Err(eco_format!(
                "public directory {} must be inside input directory {}",
                public_directory.display(),
                input_directory.display()
            ));
        }
    }

    Ok(public_directory)
}

fn normalize_root_dir(raw: Option<&str>) -> String {
    let mut root = raw.unwrap_or("/").trim().to_string();
    if root.is_empty() {
        root = "/".to_string();
    }
    if !root.starts_with('/') {
        root.insert(0, '/');
    }
    if !root.ends_with('/') {
        root.push('/');
    }
    root
}

#[cfg(test)]
mod tests {
    use std::fs;
    use std::path::{Path, PathBuf};
    use std::time::{SystemTime, UNIX_EPOCH};

    use crate::args::{CompileArgs, CompilerBackendKind, SiteArgs, TypstCompileArgs};

    use super::{BuildConfig, FilesConfig, WeibianConfig, load_config};

    struct CurrentDirGuard(PathBuf);

    impl CurrentDirGuard {
        fn enter(path: &Path) -> Self {
            let original = std::env::current_dir().expect("failed to get current dir");
            std::env::set_current_dir(path).expect("failed to set current dir");
            Self(original)
        }
    }

    impl Drop for CurrentDirGuard {
        fn drop(&mut self) {
            std::env::set_current_dir(&self.0).expect("failed to restore current dir");
        }
    }

    fn temp_project(name: &str) -> PathBuf {
        let stamp = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .expect("system clock before unix epoch")
            .as_nanos();
        let root = std::env::temp_dir().join(format!(
            "weibian-config-{name}-{}-{stamp}",
            std::process::id()
        ));
        fs::create_dir_all(&root).expect("failed to create temp project");
        root
    }

    fn compile_args(input: Option<PathBuf>, public: Option<PathBuf>) -> CompileArgs {
        CompileArgs {
            input,
            public,
            output: None,
            compiler: None,
            host_compiler: None,
            site: SiteArgs {
                domain: None,
                root_dir: None,
                trailing_slash: None,
            },
            typst: TypstCompileArgs::default(),
        }
    }

    #[test]
    fn default_config_path_reads_weibian_toml_from_cwd() {
        let root = temp_project("default-config-path");
        fs::write(
            root.join("weibian.toml"),
            r#"
                [files]
                input_dir = "notes"
                output_dir = "site"
            "#,
        )
        .expect("failed to write config");

        let _cwd = CurrentDirGuard::enter(&root);
        let config = load_config(None).expect("default config should load");

        assert_eq!(config.files.input_dir.as_deref(), Some(Path::new("notes")));
        assert_eq!(config.files.output_dir.as_deref(), Some(Path::new("site")));
    }

    #[test]
    fn relative_public_dir_resolves_under_input_dir() {
        let root = temp_project("relative-public");
        let input = root.join("typ");
        fs::create_dir_all(input.join("public")).expect("failed to create public dir");

        let config = WeibianConfig {
            files: FilesConfig {
                input_dir: Some(input.clone()),
                output_dir: Some(root.join("dist")),
                public_dir: Some(Path::new("public").into()),
                include: vec![],
                exclude: vec![],
            },
            compiler: Default::default(),
            site: Default::default(),
        };

        let build = BuildConfig::from(&compile_args(None, None), &config)
            .expect("build config should resolve");

        assert_eq!(build.public_directory, input.join("public"));
    }

    #[test]
    fn existing_public_dir_outside_input_dir_is_rejected() {
        let root = temp_project("outside-public");
        let input = root.join("typ");
        let public = root.join("public");
        fs::create_dir_all(&input).expect("failed to create input dir");
        fs::create_dir_all(&public).expect("failed to create public dir");

        let config = WeibianConfig {
            files: FilesConfig {
                input_dir: Some(input),
                output_dir: Some(root.join("dist")),
                public_dir: Some(public),
                include: vec![],
                exclude: vec![],
            },
            compiler: Default::default(),
            site: Default::default(),
        };

        let err = BuildConfig::from(&compile_args(None, None), &config)
            .expect_err("outside public dir should fail");

        assert!(err.contains("must be inside input directory"));
    }

    #[test]
    fn cli_compiler_backend_overrides_config_backend() {
        let root = temp_project("compiler-cli");
        let input = root.join("typ");
        fs::create_dir_all(input.join("public")).expect("failed to create public dir");

        let mut args = compile_args(None, None);
        args.compiler = Some(CompilerBackendKind::Host);

        let config = WeibianConfig {
            files: FilesConfig {
                input_dir: Some(input),
                output_dir: Some(root.join("dist")),
                public_dir: None,
                include: vec![],
                exclude: vec![],
            },
            compiler: super::CompilerConfig {
                backend: Some(CompilerBackendKind::Library),
                path: None,
            },
            site: Default::default(),
        };

        let build = BuildConfig::from(&args, &config).expect("build config should resolve");

        assert_eq!(build.compiler_backend, CompilerBackendKind::Host);
    }

    #[test]
    fn config_compiler_backend_is_used_when_cli_is_absent() {
        let root = temp_project("compiler-config");
        let input = root.join("typ");
        fs::create_dir_all(input.join("public")).expect("failed to create public dir");

        let config = WeibianConfig {
            files: FilesConfig {
                input_dir: Some(input),
                output_dir: Some(root.join("dist")),
                public_dir: None,
                include: vec![],
                exclude: vec![],
            },
            compiler: super::CompilerConfig {
                backend: Some(CompilerBackendKind::Host),
                path: None,
            },
            site: Default::default(),
        };

        let build = BuildConfig::from(&compile_args(None, None), &config)
            .expect("build config should resolve");

        assert_eq!(build.compiler_backend, CompilerBackendKind::Host);
    }

    #[test]
    fn compiled_default_compiler_backend_is_used_when_unspecified() {
        let root = temp_project("compiler-default");
        let input = root.join("typ");
        fs::create_dir_all(input.join("public")).expect("failed to create public dir");

        let config = WeibianConfig {
            files: FilesConfig {
                input_dir: Some(input),
                output_dir: Some(root.join("dist")),
                public_dir: None,
                include: vec![],
                exclude: vec![],
            },
            compiler: Default::default(),
            site: Default::default(),
        };

        let build = BuildConfig::from(&compile_args(None, None), &config)
            .expect("build config should resolve");

        assert_eq!(
            build.compiler_backend,
            CompilerBackendKind::compiled_default()
        );
    }

    #[test]
    fn cli_host_compiler_path_overrides_config_path() {
        let root = temp_project("compiler-path");
        let input = root.join("typ");
        fs::create_dir_all(input.join("public")).expect("failed to create public dir");

        let mut args = compile_args(None, None);
        args.host_compiler = Some(PathBuf::from("/cli/typst"));

        let config = WeibianConfig {
            files: FilesConfig {
                input_dir: Some(input),
                output_dir: Some(root.join("dist")),
                public_dir: None,
                include: vec![],
                exclude: vec![],
            },
            compiler: super::CompilerConfig {
                backend: Some(CompilerBackendKind::Host),
                path: Some(PathBuf::from("/config/typst")),
            },
            site: Default::default(),
        };

        let build = BuildConfig::from(&args, &config).expect("build config should resolve");

        assert_eq!(build.host_compiler, PathBuf::from("/cli/typst"));
    }
}
