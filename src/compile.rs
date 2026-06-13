use ecow::eco_format;

use crate::args::CompileCommand;
use crate::bundle::{collect_asset_files, collect_typst_sources, render_entrypoint};
use crate::config::{BuildConfig, WeibianConfig};
use crate::error::StrResult;

pub struct PreparedCompile {
    pub entrypoint: String,
}

pub fn compile(command: &CompileCommand, config: &WeibianConfig) -> StrResult<()> {
    let build_config = BuildConfig::from(&command.args, config)?;
    let prepared = prepare_compile(&build_config)?;

    crate::compiler::compile_bundle(&build_config, &prepared.entrypoint)
}

pub fn prepare_compile(build_config: &BuildConfig) -> StrResult<PreparedCompile> {
    let sources =
        collect_typst_sources(&build_config.input_directory, &build_config.input_filters)?;
    if sources.is_empty() {
        if build_config.input_filters.has_filters() {
            return Err(eco_format!(
                "no .typ files matched input include/exclude patterns in input directory {}",
                build_config.input_directory.display()
            ));
        }
        return Err(eco_format!(
            "no .typ files found in input directory {}",
            build_config.input_directory.display()
        ));
    }

    let assets = collect_asset_files(
        &build_config.input_directory,
        &build_config.public_directory,
    )?;
    let entrypoint = render_entrypoint(&sources, &assets);

    Ok(PreparedCompile { entrypoint })
}
