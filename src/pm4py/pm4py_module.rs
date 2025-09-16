#![allow(unsafe_op_in_unsafe_fn)]
use ebi_objects::{EventLog, LabelledPetriNet};
use pyo3::{pyfunction, pymodule, types::PyModule, wrap_pyfunction, PyAny, PyResult, Python};

use crate::{
    ebi_commands::{
        ebi_command_analyse::EBI_ANALYSE_COMPLETENESS, ebi_command_visualise::{EBI_VISUALISE_GRAPH, EBI_VISUALISE_TEXT},
    },
    ebi_framework::{ebi_command::EbiCommand, ebi_output},
    pm4py::pm4py_link::ImportableFromPM4Py,
};

// ================= Python bindings - Test functionality ===================
/// Actual Ebi function exposed to Python.
/// This function takes a PM4Py event log and returns the result of the completeness analysis.
/// Function chosen as a first example
#[pyfunction]
fn analyse_completeness(event_log: &PyAny) -> PyResult<String> {
    let command: &&EbiCommand = &&EBI_ANALYSE_COMPLETENESS;

    let input_types = match **command {
        EbiCommand::Command { input_types, .. } => input_types,
        _ => {
            return Err(pyo3::exceptions::PyValueError::new_err(
                "Expected a command.",
            ));
        }
    };

    let input = EventLog::import_from_pm4py(event_log, input_types[0])?;

    let inputs = vec![input];

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