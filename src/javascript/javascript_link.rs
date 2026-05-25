use crate::{
    ebi_framework::{
        ebi_command::EbiCommand,
        ebi_input::{EbiInput, attempt_parse},
        ebi_output::{self, EbiOutputType},
    },
    multiple_reader::MultipleReader,
};
use ebi_objects::anyhow::{Context, Error, Result, anyhow};
use itertools::Itertools;
use js_sys::Uint8Array;
use std::path::PathBuf;
use wasm_bindgen::prelude::*;

#[wasm_bindgen]
extern "C" {
    pub fn ebi_error(s: &str, command_name: &str);

    pub fn ebi_output(s: &str, command_name: &str, file_extension: &str);

    pub fn ebi_log(s: &str, command_name: &str);
}

#[wasm_bindgen(getter_with_clone)]
pub struct JavascriptInput {
    // an enum would be better, but that's seemingly not supported yet by wasm.
    pub textual: Option<String>,
    pub binary: Option<Uint8Array>,
}

#[wasm_bindgen]
impl JavascriptInput {
    #[wasm_bindgen(constructor)]
    pub fn new() -> Self {
        Self {
            textual: None,
            binary: None,
        }
    }

    pub fn set_textual(&mut self, string: String) {
        self.textual = Some(string);
        self.binary = None;
    }

    pub fn set_binary(&mut self, binary: Uint8Array) {
        self.textual = None;
        self.binary = Some(binary);
    }

    pub fn clone(&self) -> Self {
        Self {
            textual: self.textual.clone(),
            binary: self.binary.clone(),
        }
    }
}

pub fn read_inputs(
    javascript_inputs: Vec<JavascriptInput>,
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
        for ((input_types, input_name), javascript_input) in input_typess
            .iter()
            .zip(input_names.iter())
            .zip(javascript_inputs.into_iter())
        {
            //read input
            let reader = match (javascript_input.textual, javascript_input.binary) {
                (Some(string_input), None) => MultipleReader::String(string_input),
                (None, Some(array)) => MultipleReader::Bytes(array.to_vec()),
                _ => {
                    return Err(anyhow!(
                        "No inputs or both textual and binary inputs given."
                    ));
                }
            };

            inputs.push(
                attempt_parse(input_types, reader)
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
    javascript_inputs: Vec<JavascriptInput>,
    command_name: &str,
    exporter_file_extension: &str,
) {
    let (inputs, output_type) =
        match read_inputs(javascript_inputs, command).with_context(|| anyhow!("Reading inputs.")) {
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

    let mut p = PathBuf::new();
    p.set_file_name("file");
    p.set_extension(exporter_file_extension);
    let exporter = EbiCommand::select_exporter(output_type, Some(&p), None).unwrap();
    match ebi_output::export_to_string(result, &exporter) {
        Ok(string) => return ebi_output(&string, command_name, exporter.get_extension()),
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
