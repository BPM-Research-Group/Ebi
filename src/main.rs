use anyhow::{Context, Result};
use clap::crate_version;
use env_logger::Builder;
use log::LevelFilter;

use ebi;
use ebi::ebi_framework::ebi_command::EBI_COMMANDS;

pub fn main() -> Result<()> {

    //enable debugging
    Builder::new().filter_level(LevelFilter::Trace).init();

    let command = EBI_COMMANDS.build_cli();
    let command = command.version(crate_version!());
    let cli_matches = command.get_matches();
    
    log::info!("Ebi starting");

    EBI_COMMANDS.execute(&cli_matches).context("Executing Ebi")
}