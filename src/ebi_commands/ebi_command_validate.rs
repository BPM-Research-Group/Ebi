
use std::path::PathBuf;

use crate::{ebi_info, ebi_input_output::EbiInputType, ebi_objects::ebi_object::{EbiObject, EbiObjectType, EbiTraitObject}, ebi_traits::ebi_trait::EbiTrait, export::{EbiOutput, EbiOutputType}, file_handler::EbiFileHandler, import, math::fraction::Fraction};
use anyhow::{anyhow, Result, Ok, Context};
use clap::{value_parser, Arg, ArgAction, ArgMatches, Command};

use super::ebi_command::EbiCommand;

#[macro_export]
macro_rules! ebi_validate {
    () => {"validate"};
}

pub const EBI_VALIDATE: EbiCommand = EbiCommand::Command {
    name_short: "vali",
    name_long: Some(ebi_validate!()),
    explanation_short: concat!(concat!("Attempt to parse any file supported by Ebi. If you do not know the type the file should have, try `Ebi ", ebi_info!(), "`.")),
    explanation_long: None,
    latex_link: None,
    cli_command: Some(|command| {
        command.arg(
            Arg::new("file")
            .action(ArgAction::Set)
            .value_name("FILE")
            .help("The file to be parsed.")
            .required(true)
            .value_parser(value_parser!(PathBuf))
        )
    }),
    exact_arithmetic: true,
    input_types: &[ &[ &EbiInputType::FileHandler ] ],
    input_names: &[ "TYPE" ],
    input_helps: &[ "The type for which parsing should be attempted." ],
    execute: |mut inputs, cli_matches| {
        let file_handler = inputs.remove(0).to_type::<EbiFileHandler>()?;
        
        if let Some(file) = cli_matches.get_one::<PathBuf>("file") {
            let mut reader = import::get_reader_file(file).context("Could not get reader for file.")?;
            import::validate_object_of(&mut reader, &file_handler).context("Validating the file.")?;
            return Ok(EbiOutput::String(format!("Object is a valid {}.", file_handler.name)));
        } else {
            return Err(anyhow!("No input file given."))
        }
    },
    output: &EbiOutputType::String,
    
};