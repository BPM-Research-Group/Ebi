use pyo3::prelude::*;
use pyo3::types::PyAny;
use super::pm4py_link::ImportableFromPM4Py;
use crate::ebi_commands::ebi_command_analyse::EBI_ANALYSE_COMPLETENESS;
use crate::ebi_commands::ebi_command_visualise::{EBI_VISUALISE_TEXT, EBI_VISUALISE_SVG};
use crate::ebi_framework::ebi_command::EbiCommand;
use crate::ebi_framework::ebi_output;
use crate::ebi_objects::event_log::EventLog;
use crate::ebi_objects::labelled_petri_net::LabelledPetriNet;
use crate::ebi_objects::stochastic_labelled_petri_net::StochasticLabelledPetriNet;


// ================= Python bindings - Test functionality ===================
/// Actual Ebi function exposed to Python.
/// This function takes a PM4Py event log and returns the result of the completeness analysis.
/// Function chosen as a first example
#[pyfunction]
fn analyse_completeness(event_log: &PyAny) -> PyResult<String> {
    let command: &&EbiCommand = &&EBI_ANALYSE_COMPLETENESS;

    let input_types = match **command {
        EbiCommand::Command { input_types, .. } => input_types,
        _ => return Err(pyo3::exceptions::PyValueError::new_err("Expected a command.")),
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

#[pyfunction]
fn visualise_text(arg0: &PyAny) -> PyResult<String> {
    let command: &&EbiCommand = &&EBI_VISUALISE_TEXT;
    let input_types = match **command {
        EbiCommand::Command { input_types, .. } => input_types,
        _ => return Err(pyo3::exceptions::PyValueError::new_err("Expected a command.")),
    };
    let input0 = [
        EventLog::import_from_pm4py,
        StochasticLabelledPetriNet::import_from_pm4py,
        LabelledPetriNet::import_from_pm4py,
    ]
    .iter()
    .find_map(|importer| importer(arg0, input_types[0]).ok())
    .ok_or_else(|| pyo3::exceptions::PyValueError::new_err("Could not import argument 0"))?;
    let inputs = vec![input0];

    // Execute the command.
    let result = command.execute_with_inputs(inputs)
        .map_err(|e| pyo3::exceptions::PyException::new_err(format!("Command error: {}", e)))?;
    let exporter = EbiCommand::select_exporter(&result.get_type(), None);

    let output_string = if exporter.is_binary() {
        let bytes = ebi_output::export_to_bytes(result, exporter)
            .map_err(|e| pyo3::exceptions::PyException::new_err(format!("Export error: {}", e)))?;
        String::from_utf8(bytes)
            .map_err(|e| pyo3::exceptions::PyException::new_err(format!("UTF8 conversion error: {}", e)))?
    } else {
        ebi_output::export_to_string(result, exporter)
            .map_err(|e| pyo3::exceptions::PyException::new_err(format!("Export error: {}", e)))?
    };

    Ok(output_string)
}

#[pyfunction]
fn visualise_svg(object: &PyAny) -> PyResult<String> {
    let command: &&EbiCommand = &&EBI_VISUALISE_SVG;

    let input_types = match **command {
        EbiCommand::Command { input_types, .. } => input_types,
        _ => return Err(pyo3::exceptions::PyValueError::new_err("Expected a command.")),
    };

    let input = [
        StochasticLabelledPetriNet::import_from_pm4py,
        LabelledPetriNet::import_from_pm4py,
    ]
        .iter()
        .find_map(|importer| importer(object, input_types[0]).ok())
        .ok_or_else(|| {
            pyo3::exceptions::PyValueError::new_err("Object could not be imported by any handler")
        })?;

    let inputs = vec![input];


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
    m.add_function(wrap_pyfunction!(analyse_completeness, m)?)?;
    m.add_function(wrap_pyfunction!(visualise_text, m)?)?;
    m.add_function(wrap_pyfunction!(visualise_svg, m)?)?;
    Ok(())
}
