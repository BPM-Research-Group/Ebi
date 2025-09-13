use ebi_objects::Infoable;
use inflector::Inflector;
use std::io::Write;

use crate::ebi_framework::{
    ebi_command::EbiCommand,
    ebi_input::{EbiInput, EbiInputType},
    ebi_output::{EbiOutput, EbiOutputType},
};

#[macro_export]
macro_rules! ebi_info {
    () => {
        "info"
    };
}

pub const EBI_INFO: EbiCommand = EbiCommand::Command {
    name_short: ebi_info!(),
    name_long: Some("information"),
    library_name: "ebi_commands::ebi_command_info::EBI_INFO",
    explanation_short: "Show information about a file.",
    explanation_long: None,
    latex_link: None,
    cli_command: None,
    exact_arithmetic: true,
    input_types: &[&[&EbiInputType::AnyObject]],
    input_names: &["FILE"],
    input_helps: &["Any file supported by Ebi."],
    execute: |mut inputs, _| {
        if let EbiInput::Object(object, file_handler) = inputs.remove(0) {
            let mut f = vec![];

            writeln!(
                f,
                "Object was recognised as {} {} (.{}).",
                object.get_type().get_article(),
                object.get_type(),
                file_handler.file_extension
            )?;

            //object-specific info
            object.info(&mut f)?;

            //show applicable commands
            let commands = file_handler.get_applicable_commands();
            let mut paths = if commands.len() > 31 {
                commands
                    .iter()
                    .take(10)
                    .map(|path| EbiCommand::path_to_string(path))
                    .collect::<Vec<_>>()
            } else {
                commands
                    .iter()
                    .map(|path| EbiCommand::path_to_string(path))
                    .collect::<Vec<_>>()
            };
            paths.sort();
            if commands.len() > 31 {
                paths.push(format!(".. ({} more)", commands.len() - 30));
            }
            writeln!(
                f,
                "{} {} can be used in:",
                object
                    .get_type()
                    .get_article()
                    .to_string()
                    .to_sentence_case(),
                object.get_type()
            )?;

            writeln!(f, "\t{}", paths.join("\n\t"))?;

            return Ok(EbiOutput::String(String::from_utf8(f).unwrap()));
        }

        unreachable!()
    },
    output_type: &EbiOutputType::String,
};
