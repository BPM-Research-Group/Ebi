use anyhow::{anyhow, Result};
use pyo3::prelude::*;
use pyo3::types::{PyAny, PyDict, PyList};
use polars::prelude::*;
use pyo3::exceptions::PyValueError;
use std::io::Cursor;
use std::collections::HashMap;

use super::ebi_input::EbiInputType;
use super::ebi_output::EbiOutput;
use super::ebi_trait::EbiTrait;
use super::prom_link::attempt_parse;
use crate::ebi_commands::ebi_command_analyse::EBI_ANALYSE_COMPLETENESS;
use crate::ebi_framework::ebi_command::EbiCommand;
use crate::ebi_framework::{ebi_output, ebi_input::EbiInput, ebi_object::EbiObject};
use crate::ebi_objects::event_log::EventLog;
use crate::ebi_framework::infoable::Infoable;
use process_mining::event_log::{event_log_struct::{EventLogClassifier, to_attributes}, Attributes, AttributeValue};
use process_mining::event_log::{EventLog as ProcessMiningEventLog, Trace, Event};



pub trait ImportableFromPM4Py {
    /// Imports a PM4Py EventLog (as PyAny) and returns an EbiObject.
    fn import_from_pm4py(event_log: &PyAny) -> PyResult<EbiObject>;
}

impl ImportableFromPM4Py for EventLog {
    fn import_from_pm4py(event_log: &PyAny) -> PyResult<EbiObject> {
        // Extract the list of traces from the PM4Py event log.
        let py_traces = event_log.getattr("_list")?.downcast::<PyList>()?;
        let mut traces = Vec::new();
        for py_trace in py_traces.iter() {
            let trace = trace_from_py(py_trace)?;
            traces.push(trace);
        }
        // Get log-level attributes, if available.
        let attributes = if let Ok(attr_obj) = event_log.getattr("_attributes") {
            convert_py_dict_to_attributes(attr_obj)?
        } else {
            vec!()
        };
        // Create a ProcessMiningEventLog instance.
        let pm_log = ProcessMiningEventLog { attributes, traces, extensions: None, classifiers: None, global_trace_attrs: None, global_event_attrs: None };
        // For classifier, we use a default one.
        let classifier = EventLogClassifier::default();
        let event_log_rust = EventLog::new(pm_log, classifier);
        Ok(EbiObject::EventLog(event_log_rust))
    }
}

// A helper to convert a Python dict into our Attributes.
fn convert_py_dict_to_attributes(py_obj: &PyAny) -> PyResult<Attributes> {
    let dict = py_obj.downcast::<PyDict>()?;
    let mut map = HashMap::new();
    for (key, value) in dict.iter() {
        let key_str: String = key.extract()?;
        // For simplicity, we assume every attribute is a string.
        let value_str: String = value.str()?.to_str()?.to_owned();
        map.insert(key_str, AttributeValue::String(value_str));
    }
    Ok(to_attributes(map))
}


// Build an Event from a PyAny that represents a PM4Py Event.
pub fn event_from_py(py_event: &PyAny) -> PyResult<Event> {
    // PM4Py Event stores its data in _dict
    let dict_obj = py_event.getattr("_dict")?;
    let attributes = convert_py_dict_to_attributes(dict_obj)?;
    Ok(Event { attributes })
}

// Build a Trace from a PyAny that represents a PM4Py Trace.
pub fn trace_from_py(py_trace: &PyAny) -> PyResult<Trace> {
    let attributes = if let Ok(attr_obj) = py_trace.getattr("_attributes") {
        convert_py_dict_to_attributes(attr_obj)?
    } else {
        vec!()
    };
    let py_events = py_trace.getattr("_list")?.downcast::<PyList>()?;
    let mut events = Vec::new();
    for py_event in py_events.iter() {
        let event = event_from_py(py_event)?;
        events.push(event);
    }
    Ok(Trace { attributes, events })
}

// ========= EbiCommand execution if inputs are given ==========

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

// ================= Python bindings - Test functionality ===================

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

/// internal function that creates a Polars DataFrame from IPC bytes
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
fn dataframe_head(ipc_bytes: &[u8]) -> PyResult<String> {
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

#[pyfunction]
pub fn eventlog_head(event_log: &PyAny) -> PyResult<String> {
    // Access the `_list` attribute of the event log and downcast it to PyList.
    let list = event_log.getattr("_list")?.downcast::<PyList>()?;

    // Iterate over the first 5 items (or fewer if the list is smaller).
    let mut result = String::new();
    for (i, item) in list.iter().enumerate().take(5) {
        // Convert each item to a string representation.
        let trace = item.str()?.to_str()?;
        result.push_str(&format!("Trace {}: {}\n", i + 1, trace));
    }

    Ok(result)
}

/// test function to import a PM4Py event log and return its info
#[pyfunction]
pub fn import_event_log(event_log: &PyAny) -> PyResult<String> {
    let ebi_obj = EventLog::import_from_pm4py(event_log)?;
    if let EbiObject::EventLog(log) = ebi_obj {
        let mut output = Vec::new();
        log.info(&mut output)
            .map_err(|e| pyo3::exceptions::PyValueError::new_err(format!("Failed to get info: {}", e)))?;
        let info_str = String::from_utf8(output)
            .map_err(|e| pyo3::exceptions::PyValueError::new_err(format!("UTF-8 error: {}", e)))?;
        Ok(info_str)
    } else {
        Err(pyo3::exceptions::PyValueError::new_err("Expected an EventLog"))
    }
}




/// A Python module implemented in Rust. The name of this function must match
/// the `lib.name` setting in the `Cargo.toml`, else Python will not be able to
/// import the module.
#[pymodule]
fn ebi(_py: Python<'_>, m: &PyModule) -> PyResult<()> {
    m.add_function(wrap_pyfunction!(ebi_analyse_completeness, m)?)?;
    m.add_function(wrap_pyfunction!(get_log_length, m)?)?;
    m.add_function(wrap_pyfunction!(count_traces, m)?)?;
    m.add_function(wrap_pyfunction!(dataframe_head, m)?)?;
    m.add_function(wrap_pyfunction!(eventlog_head, m)?)?;
    m.add_function(wrap_pyfunction!(import_event_log, m)?)?;
    Ok(())
}
