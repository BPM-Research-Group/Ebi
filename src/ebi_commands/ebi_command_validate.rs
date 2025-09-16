use std::path::PathBuf;
use anyhow::{anyhow, Context, Ok};
use clap::{value_parser, Arg, ArgAction};

use crate::{ebi_framework::{ebi_command::EbiCommand, ebi_file_handler::EbiFileHandler, ebi_input::{self, EbiInputType}, ebi_output::{EbiOutput, EbiOutputType}}, ebi_info};

#[macro_export]
macro_rules! ebi_validate {
    () => {"validate"};
}

pub const EBI_VALIDATE: EbiCommand = EbiCommand::Command {
    name_short: "vali",
    name_long: Some(ebi_validate!()),
    library_name: "ebi_commands::ebi_command_validate::EBI_VALIDATE",
    explanation_short: concat!(concat!("Attempt to parse any file supported by Ebi, and return a parsing error if necessary.
        If you do not know the type the file should have, try `Ebi ", ebi_info!(), "`.")),
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
        
        if let Some(file) = cli_matches.unwrap().get_one::<PathBuf>("file") {
            let mut reader = ebi_input::get_reader_file(file).context("Could not get reader for file.")?;
            ebi_input::validate_object_of(&mut reader, &file_handler).with_context(|| "validating the file")?;
            return Ok(EbiOutput::String(format!("Object is a valid {}.", file_handler.name)));
        } else {
            return Err(anyhow!("no input file given"))
        }
    },
    output_type: &EbiOutputType::String,
    
};