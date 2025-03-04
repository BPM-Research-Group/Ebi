use anyhow::{anyhow, Result};
use pyo3::prelude::*;

use super::ebi_command::EBI_COMMANDS;
use super::ebi_input::EbiInputType;
use super::ebi_output::EbiOutput;
use super::ebi_trait::EbiTrait;
use super::prom_link::attempt_parse;
use crate::ebi_commands::ebi_command_analyse::EBI_ANALYSE_COMPLETENESS;
use crate::ebi_framework::ebi_command::EbiCommand;
use crate::ebi_framework::ebi_output;
use crate::ebi_framework::{ebi_input::EbiInput, ebi_object::EbiObjectType};
use crate::ebi_traits::ebi_trait_event_log::EbiTraitEventLog;

impl EbiCommand {
    /// Executes the command using the provided inputs without reading from CLI.
    /// Returns the resulting object.
    pub fn execute_with_inputs(&self, inputs: Vec<EbiInput>) -> Result<EbiOutput> {
        match self {
            EbiCommand::Command {
                execute,
                output_type,
                ..
            } => {
                // Call the command’s execute closure directly.
                // Passing None for ArgMatches since we don’t need any CLI options.
                let result = (execute)(inputs, None)?;
                if &&result.get_type() != output_type {
                    return Err(anyhow!("Output type {} does not match the declared output of {}.", result.get_type(), output_type))
                }
                Ok(result)
            }
            _ => Err(anyhow!("Not a command variant.")),
        }
    }
}

#[pyfunction]
fn ebi_analyse_completeness(event_log: String) -> PyResult<String> {
    //let event_log_type = EbiInputType::Object(EbiObjectType::EventLog);
    let event_log_type = EbiInputType::Trait(EbiTrait::EventLog);

    // Use the same approach as in the Java integration: pass a slice of input types.
    let input = attempt_parse(&[&event_log_type], event_log).map_err(|e| {
        pyo3::exceptions::PyException::new_err(format!("Error reading event log: {}", e))
    })?;

    let inputs = vec![input];


    let command: &&EbiCommand = &&EBI_ANALYSE_COMPLETENESS;

    // Execute the command.
    let result = command
        .execute_with_inputs(inputs)
        .map_err(|e| pyo3::exceptions::PyException::new_err(format!("Command error: {}", e)))?;

    let exporter = EbiCommand::select_exporter(&result.get_type(), None);

    // If the exporter is binary, convert the bytes to a UTF-8 string.
    let output_string = if exporter.is_binary() {
        let bytes = ebi_output::export_to_bytes(result, exporter)
            .map_err(|e| pyo3::exceptions::PyException::new_err(format!("Export error: {}", e)))?;
        String::from_utf8(bytes).map_err(|e| {
            pyo3::exceptions::PyException::new_err(format!("UTF8 conversion error: {}", e))
        })?
    } else {
        ebi_output::export_to_string(result, exporter)
            .map_err(|e| pyo3::exceptions::PyException::new_err(format!("Export error: {}", e)))?
    };

    Ok(output_string)
}

/// A Python module implemented in Rust. The name of this function must match
/// the `lib.name` setting in the `Cargo.toml`, else Python will not be able to
/// import the module.
#[pymodule]
fn ebi(_py: Python<'_>, m: &PyModule) -> PyResult<()> {
    m.add_function(wrap_pyfunction!(ebi_analyse_completeness, m)?)?;
    Ok(())
}
