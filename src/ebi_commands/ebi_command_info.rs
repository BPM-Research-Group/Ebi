use std::{io::Write, path::PathBuf};
use anyhow::{anyhow, Context, Result};
use clap::{builder::Str, value_parser, Arg, ArgAction};
use flate2::write;
use fraction::BigFraction;
use inflector::Inflector;

use crate::{ebi_input_output::{EbiInput, EbiInputType}, ebi_objects::ebi_object::{EbiObject, EbiObjectType, EbiTraitObject}, ebi_traits::ebi_trait::EbiTrait, export::{EbiOutput, EbiOutputType}, file_handler::{self, EBI_FILE_HANDLERS}, import};

use super::ebi_command::EbiCommand;

#[macro_export]
macro_rules! ebi_info {
    () => {"info"};
}

pub const EBI_INFO: EbiCommand = EbiCommand::Command { 
    name_short: ebi_info!(), 
    name_long: Some("information"), 
    explanation_short: "Show information about an object.", 
    explanation_long: None, 
    latex_link: None, 
    cli_command: None,
    exact_arithmetic: true, 
    input_types: &[ &[ &EbiInputType::AnyObject ] ], 
    input_names: &[ "FILE" ], 
    input_helps: &[ "Any file supported by Ebi." ], 
    execute: |mut inputs, _| {
        if let EbiInput::Object(object, file_handler) = inputs.remove(0) {
            let mut f = vec![];
            
            writeln!(f, "Object was recognised as {} {} (.{}).", object.get_type().get_article(), object.get_type(), file_handler.file_extension);

            //object-specific info
            object.info(&mut f)?;

            //show applicable commands
            let commands = file_handler.get_applicable_commands();
            let mut paths = if commands.len() > 11 {
                commands.iter().take(10).map(|path| EbiCommand::path_to_string(path)).collect::<Vec<_>>()
            } else {
                commands.iter().map(|path| EbiCommand::path_to_string(path)).collect::<Vec<_>>()
            };
            paths.sort();
            if commands.len() > 11 {
                paths.push(format!(".. ({} more)", commands.len() - 10));
            }
            writeln!(f, "{} {} can be used in:", object.get_type().get_article().to_string().to_sentence_case(), object.get_type())?;

            writeln!(f, "\t{}", paths.join("\n\t"));

            return Ok(EbiOutput::String(String::from_utf8(f).unwrap()));
        }
    
        unreachable!()
    }, 
    output: &EbiOutputType::String
};

pub trait Infoable {
    fn info(&self, f: &mut impl std::io::Write) -> Result<()>;
}

impl Infoable for String {
    fn info(&self, f: &mut impl std::io::Write) -> Result<()> {
        Ok(writeln!(f, "Length\t{}", self.len())?)
    }
}

impl Infoable for BigFraction {
    fn info(&self, f: &mut impl std::io::Write) -> Result<()> {
        match self {
            fraction::GenericFraction::Rational(sign, ratio) => write!(f, "{} bits / {} bits", ratio.numer().bits(), ratio.denom().bits())?,
            fraction::GenericFraction::Infinity(sign) => write!(f, "{} infinity", sign.to_string())?,
            fraction::GenericFraction::NaN => write!(f, "NaN")?,
        }
        Ok(write!(f, "")?)
    }
}