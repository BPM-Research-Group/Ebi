use anyhow::{anyhow, Result};
use pyo3::prelude::*;
use pyo3::types::{PyAny, PyList};
use polars::prelude::*;
use pyo3::exceptions::PyValueError;
use std::io::Cursor;

use super::ebi_input::EbiInputType;
use super::ebi_output::EbiOutput;
use super::ebi_trait::EbiTrait;
use super::prom_link::attempt_parse;
use crate::ebi_commands::ebi_command_analyse::EBI_ANALYSE_COMPLETENESS;
use crate::ebi_framework::ebi_command::EbiCommand;
use crate::ebi_framework::ebi_output;
use crate::ebi_framework::ebi_input::EbiInput;

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


#[pyfunction]
pub fn get_log_length(event_log: &PyAny) -> PyResult<usize> {
    let list = event_log.getattr("_list")?.downcast::<PyList>()?;
    Ok(list.len())
}

fn extract_dataframe(ipc_bytes: &[u8]) -> Result<DataFrame, PolarsError> {
    let cursor = Cursor::new(ipc_bytes);
    // Use the IPC reader to deserialize the data into a DataFrame.
    polars::io::ipc::IpcReader::new(cursor).finish()
}

fn count_unique_traces(df: &DataFrame, column: &str) -> Result<usize, PolarsError> {
    // Retrieve the specified column.
    let trace_column = df.column(column)?;
    // Calculate the unique values in the column.
    let unique_traces = trace_column.unique()?;
    Ok(unique_traces.len())
}

#[pyfunction]
fn count_traces(ipc_bytes: &[u8]) -> PyResult<usize> {
    // First, extract the DataFrame from the IPC bytes.
    let df = extract_dataframe(ipc_bytes)
        .map_err(|e| PyValueError::new_err(format!("Failed to extract DataFrame: {}", e)))?;
    // Then, count unique traces in the "trace" column.
    let count = count_unique_traces(&df, "case:concept:name")
        .map_err(|e| PyValueError::new_err(format!("Failed to count unique traces: {}", e)))?;
    Ok(count)
}

#[pyfunction]
fn rust_head(ipc_bytes: &[u8]) -> PyResult<String> {
    // Create a cursor for the provided IPC bytes.
    let cursor = Cursor::new(ipc_bytes);
    
    // Deserialize the IPC bytes into a Polars DataFrame.
    let df = polars::io::ipc::IpcReader::new(cursor)
        .finish()
        .map_err(|e| PyValueError::new_err(format!("Failed to extract DataFrame: {}", e)))?;
    
    // Get the first 5 rows.
    let head_df = df.head(Some(5));
    
    // Convert the head DataFrame to a string.
    // The Display implementation for DataFrame prints a nice table.
    Ok(format!("{}", head_df))
}

/// A Python module implemented in Rust. The name of this function must match
/// the `lib.name` setting in the `Cargo.toml`, else Python will not be able to
/// import the module.
#[pymodule]
fn ebi(_py: Python<'_>, m: &PyModule) -> PyResult<()> {
    m.add_function(wrap_pyfunction!(ebi_analyse_completeness, m)?)?;
    m.add_function(wrap_pyfunction!(get_log_length, m)?)?;
    m.add_function(wrap_pyfunction!(count_traces, m)?)?;
    m.add_function(wrap_pyfunction!(rust_head, m)?)?;
    Ok(())
}
