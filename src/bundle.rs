use std::fs;
use std::path::{Component, Path};

use ecow::eco_format;

use crate::config::InputFilters;
use crate::error::StrResult;

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct TypstSource {
    pub root_relative: String,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct AssetFile {
    pub output_path: String,
    pub root_relative_input: String,
}

pub fn collect_typst_sources(
    input_dir: &Path,
    filters: &InputFilters,
) -> StrResult<Vec<TypstSource>> {
    let mut sources = Vec::new();
    let mut stack = vec![input_dir.to_path_buf()];

    while let Some(dir) = stack.pop() {
        let entries = fs::read_dir(&dir).map_err(|err| {
            eco_format!("failed to read input directory {}: {err}", dir.display())
        })?;

        for entry in entries {
            let entry =
                entry.map_err(|err| eco_format!("failed to read input directory entry: {err}"))?;
            let path = entry.path();
            let file_type = entry.file_type().map_err(|err| {
                eco_format!("failed to read file type for {}: {err}", path.display())
            })?;

            if file_type.is_dir() {
                stack.push(path);
                continue;
            }

            if !file_type.is_file() {
                continue;
            }

            if path.extension().and_then(|ext| ext.to_str()) != Some("typ") {
                continue;
            }

            let relative = path.strip_prefix(input_dir).map_err(|_| {
                eco_format!(
                    "source path {} is not inside input directory {}",
                    path.display(),
                    input_dir.display()
                )
            })?;
            if !filters.allows(relative) {
                continue;
            }

            sources.push(TypstSource {
                root_relative: root_relative_path(relative)?,
            });
        }
    }

    sources.sort_by(|left, right| left.root_relative.cmp(&right.root_relative));
    Ok(sources)
}

pub fn collect_asset_files(input_dir: &Path, public_dir: &Path) -> StrResult<Vec<AssetFile>> {
    if !public_dir.exists() {
        return Ok(Vec::new());
    }

    let mut assets = Vec::new();
    let mut stack = vec![public_dir.to_path_buf()];

    while let Some(dir) = stack.pop() {
        let entries = fs::read_dir(&dir).map_err(|err| {
            eco_format!("failed to read public directory {}: {err}", dir.display())
        })?;

        for entry in entries {
            let entry =
                entry.map_err(|err| eco_format!("failed to read public directory entry: {err}"))?;
            let path = entry.path();
            let file_type = entry.file_type().map_err(|err| {
                eco_format!("failed to read file type for {}: {err}", path.display())
            })?;

            if file_type.is_dir() {
                stack.push(path);
                continue;
            }

            if !file_type.is_file() {
                continue;
            }

            let public_relative = path.strip_prefix(public_dir).map_err(|_| {
                eco_format!(
                    "asset path {} is not inside public directory {}",
                    path.display(),
                    public_dir.display()
                )
            })?;
            let input_relative = path.strip_prefix(input_dir).map_err(|_| {
                eco_format!(
                    "asset path {} is not inside input directory {}",
                    path.display(),
                    input_dir.display()
                )
            })?;

            assets.push(AssetFile {
                output_path: relative_path(public_relative)?,
                root_relative_input: root_relative_path(input_relative)?,
            });
        }
    }

    assets.sort_by(|left, right| {
        left.output_path
            .cmp(&right.output_path)
            .then_with(|| left.root_relative_input.cmp(&right.root_relative_input))
    });
    Ok(assets)
}

pub fn render_entrypoint(sources: &[TypstSource], assets: &[AssetFile]) -> String {
    let mut entrypoint = String::new();

    for source in sources {
        entrypoint.push_str("#include ");
        entrypoint.push_str(&typst_string(&source.root_relative));
        entrypoint.push('\n');
    }

    if !sources.is_empty() && !assets.is_empty() {
        entrypoint.push('\n');
    }

    for asset in assets {
        entrypoint.push_str("#asset(");
        entrypoint.push_str(&typst_string(&asset.output_path));
        entrypoint.push_str(", read(");
        entrypoint.push_str(&typst_string(&asset.root_relative_input));
        entrypoint.push_str(", encoding: none))\n");
    }

    entrypoint
}

fn root_relative_path(path: &Path) -> StrResult<String> {
    let relative = relative_path(path)?;
    Ok(format!("/{relative}"))
}

fn relative_path(path: &Path) -> StrResult<String> {
    let mut parts = Vec::new();
    for component in path.components() {
        match component {
            Component::Normal(part) => {
                let part = part
                    .to_str()
                    .ok_or_else(|| eco_format!("path {} is not valid UTF-8", path.display()))?;
                parts.push(part.to_string());
            }
            Component::CurDir => {}
            _ => {
                return Err(eco_format!(
                    "path {} cannot be represented as a Typst root-relative path",
                    path.display()
                ));
            }
        }
    }
    Ok(parts.join("/"))
}

fn typst_string(value: &str) -> String {
    let mut escaped = String::with_capacity(value.len() + 2);
    escaped.push('"');
    for ch in value.chars() {
        match ch {
            '\\' => escaped.push_str("\\\\"),
            '"' => escaped.push_str("\\\""),
            '\n' => escaped.push_str("\\n"),
            '\r' => escaped.push_str("\\r"),
            '\t' => escaped.push_str("\\t"),
            _ => escaped.push(ch),
        }
    }
    escaped.push('"');
    escaped
}

#[cfg(test)]
mod tests {
    use std::fs;
    use std::path::Path;
    use std::time::{SystemTime, UNIX_EPOCH};

    use crate::config::InputFilters;

    use super::{
        AssetFile, TypstSource, collect_asset_files, collect_typst_sources, render_entrypoint,
    };

    fn temp_project(name: &str) -> std::path::PathBuf {
        let stamp = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .expect("system clock before unix epoch")
            .as_nanos();
        let root =
            std::env::temp_dir().join(format!("weibian-{name}-{}-{stamp}", std::process::id()));
        fs::create_dir_all(&root).expect("failed to create temp project");
        root
    }

    fn write(path: &Path, contents: &str) {
        if let Some(parent) = path.parent() {
            fs::create_dir_all(parent).expect("failed to create parent directory");
        }
        fs::write(path, contents).expect("failed to write test file");
    }

    #[test]
    fn collect_typst_sources_is_filtered_relative_to_input_dir_and_sorted() {
        let root = temp_project("sources");
        let input = root.join("typ");
        write(&input.join("index.typ"), "#include \"a.typ\"");
        write(&input.join("z.typ"), "#let z = 1");
        write(&input.join("blog/a.typ"), "#let a = 1");
        write(&input.join("_template/template.typ"), "#let t = 1");
        write(&input.join("notes/not-typ.txt"), "ignored");

        let filters = InputFilters::new(&["**/*.typ".into()], &["_template/*.typ".into()])
            .expect("filters should build");
        let sources = collect_typst_sources(&input, &filters).expect("sources should collect");
        let relative: Vec<_> = sources
            .iter()
            .map(|source| source.root_relative.as_str())
            .collect();

        assert_eq!(relative, ["/blog/a.typ", "/index.typ", "/z.typ"]);
    }

    #[test]
    fn collect_asset_files_uses_public_relative_output_and_root_relative_reads() {
        let root = temp_project("assets");
        let input = root.join("typ");
        let public = input.join("public");
        write(&public.join("css/site.css"), "body {}");
        write(&public.join("favicon.ico"), "icon");

        let assets = collect_asset_files(&input, &public).expect("assets should collect");
        let pairs: Vec<_> = assets
            .iter()
            .map(|asset| {
                (
                    asset.output_path.as_str(),
                    asset.root_relative_input.as_str(),
                )
            })
            .collect();

        assert_eq!(
            pairs,
            [
                ("css/site.css", "/public/css/site.css"),
                ("favicon.ico", "/public/favicon.ico")
            ]
        );
    }

    #[test]
    fn collect_asset_files_treats_missing_public_dir_as_no_assets() {
        let root = temp_project("missing-assets");
        let input = root.join("typ");
        fs::create_dir_all(&input).expect("failed to create input dir");

        let assets =
            collect_asset_files(&input, &input.join("public")).expect("assets should collect");

        assert!(assets.is_empty());
    }

    #[test]
    fn render_entrypoint_includes_sources_and_assets_with_escaped_paths() {
        let sources = vec![
            TypstSource {
                root_relative: "/blog/a.typ".into(),
            },
            TypstSource {
                root_relative: "/quote\"note.typ".into(),
            },
        ];
        let assets = vec![AssetFile {
            output_path: "css/site.css".into(),
            root_relative_input: "/public/css/site.css".into(),
        }];

        let entrypoint = render_entrypoint(&sources, &assets);

        assert_eq!(
            entrypoint,
            "#include \"/blog/a.typ\"\n\
             #include \"/quote\\\"note.typ\"\n\
             \n\
             #asset(\"css/site.css\", read(\"/public/css/site.css\", encoding: none))\n"
        );
    }
}
