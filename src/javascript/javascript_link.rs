use crate::ebi_framework::{
    ebi_command::EbiCommand,
    ebi_input::{EbiInput, attempt_parse},
    ebi_output::{self, EbiOutputType},
};
use ebi_objects::anyhow::{Context, Error, Result, anyhow};
use itertools::Itertools;
use wasm_bindgen::prelude::*;

#[wasm_bindgen]
extern "C" {
    pub fn ebi_error(s: &str, command_name: &str);

    pub fn ebi_output(s: &str, command_name: &str);

    pub fn ebi_log(s: &str, command_name: &str);
}

pub fn read_inputs(
    string_inputs: Vec<String>,
    command: &EbiCommand,
) -> Result<(Vec<EbiInput>, &EbiOutputType)> {
    if let EbiCommand::Command {
        input_types: input_typess,
        input_names,
        output_type,
        ..
    } = command
    {
        //read the inputs
        let mut inputs = vec![];
        for ((input_types, input_name), string_input) in input_typess
            .iter()
            .zip(input_names.iter())
            .zip(string_inputs.into_iter())
        {
            //read input
            inputs.push(
                attempt_parse(input_types, string_input)
                    .with_context(|| format!("Reading parameter {}.", input_name))?,
            );
        }
        Ok((inputs, output_type))
    } else {
        Err(anyhow!("Command expected."))
    }
}

pub(crate) fn execute_javascript_command(
    command: &EbiCommand,
    string_inputs: Vec<String>,
    command_name: &str,
) {
    let number_of_inputs = string_inputs.len();
    let (inputs, output_type) = match read_inputs(string_inputs, command)
        .with_context(|| anyhow!("Reading {} inputs.", number_of_inputs))
    {
        Ok(t) => t,
        Err(e) => return ebi_error(&print_error(e), command_name),
    };

    // Execute the command.
    let result = match command
        .execute_with_inputs(inputs)
        .with_context(|| anyhow!("Executing command {}", command.long_name()))
    {
        Ok(result) => result,
        Err(e) => return ebi_error(&print_error(e), command_name),
    };

    let exporter = EbiCommand::select_exporter(output_type, None, None).unwrap();
    match ebi_output::export_to_string(result, exporter) {
        Ok(string) => return ebi_output(&string, command_name),
        Err(e) => return ebi_error(&print_error(e), command_name),
    }
}

pub fn print_error(e: Error) -> String {
    format!(
        "Error: {e}<br>Caused by:<br>{}",
        e.chain()
            .enumerate()
            .skip(1)
            .map(|(i, cause)| format!("{i}: {cause}"))
            .join("<br>")
    )
}
