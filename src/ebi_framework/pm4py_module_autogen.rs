use pyo3::prelude::*;
use pyo3::types::PyAny;
use super::pm4py_link::{ImportableFromPM4Py, IMPORTERS, ExportableToPM4Py};
use crate::ebi_framework::{ebi_command::EbiCommand, ebi_output};
use crate::ebi_objects::{event_log::EventLog, labelled_petri_net::LabelledPetriNet, stochastic_labelled_petri_net::StochasticLabelledPetriNet, process_tree::ProcessTree};
use crate::ebi_commands::ebi_command_analyse::EBI_ANALYSE_ALL;
use crate::ebi_commands::ebi_command_analyse::EBI_ANALYSE_COMPLETENESS;
use crate::ebi_commands::ebi_command_analyse::EBI_ANALYSE_COVERAGE;
use crate::ebi_commands::ebi_command_analyse::EBI_ANALYSE_DIRECTLY_FOLLOWS_EDGE_DIFFERENCE;
use crate::ebi_commands::ebi_command_analyse::EBI_ANALYSE_MEDOID;
use crate::ebi_commands::ebi_command_analyse::EBI_ANALYSE_MINPROB;
use crate::ebi_commands::ebi_command_analyse::EBI_ANALYSE_MODE;
use crate::ebi_commands::ebi_command_analyse::EBI_ANALYSE_MOSTLIKELY;
use crate::ebi_commands::ebi_command_analyse::EBI_ANALYSE_VARIETY;
use crate::ebi_commands::ebi_command_analyse_non_stochastic::EBI_ANALYSE_NON_STOCHASTIC_ALIGNMENT;
use crate::ebi_commands::ebi_command_analyse_non_stochastic::EBI_ANALYSE_NON_STOCHASTIC_ANY_TRACES;
use crate::ebi_commands::ebi_command_analyse_non_stochastic::EBI_ANALYSE_NON_STOCHASTIC_BOUNDED;
use crate::ebi_commands::ebi_command_analyse_non_stochastic::EBI_ANALYSE_NON_STOCHASTIC_CLUSTER;
use crate::ebi_commands::ebi_command_analyse_non_stochastic::EBI_ANALYSE_NON_STOCHASTIC_EXECUTIONS;
use crate::ebi_commands::ebi_command_analyse_non_stochastic::EBI_ANALYSE_NON_STOCHASTIC_INFINITELY_MANY_TRACES;
use crate::ebi_commands::ebi_command_analyse_non_stochastic::EBI_ANALYSE_NON_STOCHASTIC_MEDOID;
use crate::ebi_commands::ebi_command_association::ASSOCIATION_ATTRIBUTES;
use crate::ebi_commands::ebi_command_association::ASSOCIATION_ATTRIBUTE;
use crate::ebi_commands::ebi_command_conformance::CONFORMANCE_EMSC;
use crate::ebi_commands::ebi_command_conformance::CONFORMANCE_EMSC_SAMPLE;
use crate::ebi_commands::ebi_command_conformance::CONFORMANCE_ER;
use crate::ebi_commands::ebi_command_conformance::CONFORMANCE_JSSC;
use crate::ebi_commands::ebi_command_conformance::CONFORMANCE_JSSC_SAMPLE;
use crate::ebi_commands::ebi_command_conformance::CONFORMANCE_UEMSC;
use crate::ebi_commands::ebi_command_convert::EBI_CONVERT_SLANG;
use crate::ebi_commands::ebi_command_convert::EBI_CONVERT_LPN;
use crate::ebi_commands::ebi_command_convert::EBI_CONVERT_SDFA;
use crate::ebi_commands::ebi_command_discover::EBI_DISCOVER_ALIGNMENTS;
use crate::ebi_commands::ebi_command_discover::EBI_DISCOVER_DIRECTLY_FOLLOWS;
use crate::ebi_commands::ebi_command_discover::EBI_DISCOVER_OCCURRENCE;
use crate::ebi_commands::ebi_command_discover::EBI_DISCOVER_UNIFORM;
use crate::ebi_commands::ebi_command_discover_non_stochastic::EBI_DISCOVER_NON_STOCHASTIC_DIRECTLY_FOLLOWS;
use crate::ebi_commands::ebi_command_discover_non_stochastic::EBI_DISCOVER_NON_STOCHASTIC_FLOWER_DFA;
use crate::ebi_commands::ebi_command_discover_non_stochastic::EBI_DISCOVER_NON_STOCHASTIC_FLOWER_TREE;
use crate::ebi_commands::ebi_command_discover_non_stochastic::EBI_DISCOVER_NON_STOCHASTIC_TREE_DFA;
use crate::ebi_commands::ebi_command_discover_non_stochastic::EBI_DISCOVER_NON_STOCHASTIC_TREE_TREE;
use crate::ebi_commands::ebi_command_info::EBI_INFO;
use crate::ebi_commands::ebi_command_itself::EBI_ITSELF_GENERATE_PM4PY;
use crate::ebi_commands::ebi_command_itself::EBI_ITSELF_GRAPH;
use crate::ebi_commands::ebi_command_itself::EBI_ITSELF_JAVA;
use crate::ebi_commands::ebi_command_itself::EBI_ITSELF_LOGO;
use crate::ebi_commands::ebi_command_itself::EBI_ITSELF_MANUAL;
use crate::ebi_commands::ebi_command_probability::EBI_PROBABILITY_EXPLAIN_TRACE;
use crate::ebi_commands::ebi_command_probability::EBI_PROBABILITY_MODEL;
use crate::ebi_commands::ebi_command_probability::EBI_PROBABILITY_TRACE;
use crate::ebi_commands::ebi_command_sample::EBI_SAMPLE;
use crate::ebi_commands::ebi_command_test::TEST_LOG_ATTRIBUTE;
use crate::ebi_commands::ebi_command_validate::EBI_VALIDATE;
use crate::ebi_commands::ebi_command_visualise::EBI_VISUALISE_PDF;
use crate::ebi_commands::ebi_command_visualise::EBI_VISUALISE_SVG;
use crate::ebi_commands::ebi_command_visualise::EBI_VISUALISE_TEXT;

#[pyfunction]
fn analyse_all_traces(arg0: &PyAny) -> PyResult<String> {
    let command: &&EbiCommand = &&EBI_ANALYSE_ALL;
    let input_types = match **command {
        EbiCommand::Command { input_types, .. } => input_types,
        _ => return Err(pyo3::exceptions::PyValueError::new_err("Expected a command.")),
    };
    let input0 = IMPORTERS
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
fn analyse_completeness(py: Python<'_>, arg0: &PyAny) -> PyResult<PyObject> {
    let command: &&EbiCommand = &&EBI_ANALYSE_COMPLETENESS;
    let input_types = match **command {
        EbiCommand::Command { input_types, .. } => input_types,
        _ => return Err(pyo3::exceptions::PyValueError::new_err("Expected a command.")),
    };
    let input0 = IMPORTERS
    .iter()
    .find_map(|importer| importer(arg0, input_types[0]).ok())
    .ok_or_else(|| pyo3::exceptions::PyValueError::new_err("Could not import argument 0"))?;
    let inputs = vec![input0];

    // Execute the command.
    let result = command.execute_with_inputs(inputs)
        .map_err(|e| pyo3::exceptions::PyException::new_err(format!("Command error: {}", e)))?
        .export_to_pm4py(py)
        .map_err(|e| pyo3::exceptions::PyException::new_err(format!("Export error: {}", e)))?;
        
    Ok(result)
}

#[pyfunction]
fn analyse_coverage(arg0: &PyAny, arg1: &PyAny) -> PyResult<String> {
    let command: &&EbiCommand = &&EBI_ANALYSE_COVERAGE;
    let input_types = match **command {
        EbiCommand::Command { input_types, .. } => input_types,
        _ => return Err(pyo3::exceptions::PyValueError::new_err("Expected a command.")),
    };
    let input0 = IMPORTERS
    .iter()
    .find_map(|importer| importer(arg0, input_types[0]).ok())
    .ok_or_else(|| pyo3::exceptions::PyValueError::new_err("Could not import argument 0"))?;
    let input1 = IMPORTERS
    .iter()
    .find_map(|importer| importer(arg1, input_types[1]).ok())
    .ok_or_else(|| pyo3::exceptions::PyValueError::new_err("Could not import argument 1"))?;
    let inputs = vec![input0, input1];

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
fn analyse_directly_follows_edge_difference(arg0: &PyAny, arg1: &PyAny) -> PyResult<String> {
    let command: &&EbiCommand = &&EBI_ANALYSE_DIRECTLY_FOLLOWS_EDGE_DIFFERENCE;
    let input_types = match **command {
        EbiCommand::Command { input_types, .. } => input_types,
        _ => return Err(pyo3::exceptions::PyValueError::new_err("Expected a command.")),
    };
    let input0 = IMPORTERS
    .iter()
    .find_map(|importer| importer(arg0, input_types[0]).ok())
    .ok_or_else(|| pyo3::exceptions::PyValueError::new_err("Could not import argument 0"))?;
    let input1 = IMPORTERS
    .iter()
    .find_map(|importer| importer(arg1, input_types[1]).ok())
    .ok_or_else(|| pyo3::exceptions::PyValueError::new_err("Could not import argument 1"))?;
    let inputs = vec![input0, input1];

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
fn analyse_medoid(arg0: &PyAny, arg1: &PyAny) -> PyResult<String> {
    let command: &&EbiCommand = &&EBI_ANALYSE_MEDOID;
    let input_types = match **command {
        EbiCommand::Command { input_types, .. } => input_types,
        _ => return Err(pyo3::exceptions::PyValueError::new_err("Expected a command.")),
    };
    let input0 = IMPORTERS
    .iter()
    .find_map(|importer| importer(arg0, input_types[0]).ok())
    .ok_or_else(|| pyo3::exceptions::PyValueError::new_err("Could not import argument 0"))?;
    let input1 = IMPORTERS
    .iter()
    .find_map(|importer| importer(arg1, input_types[1]).ok())
    .ok_or_else(|| pyo3::exceptions::PyValueError::new_err("Could not import argument 1"))?;
    let inputs = vec![input0, input1];

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
fn analyse_minimum_probability_traces(arg0: &PyAny, arg1: &PyAny) -> PyResult<String> {
    let command: &&EbiCommand = &&EBI_ANALYSE_MINPROB;
    let input_types = match **command {
        EbiCommand::Command { input_types, .. } => input_types,
        _ => return Err(pyo3::exceptions::PyValueError::new_err("Expected a command.")),
    };
    let input0 = IMPORTERS
    .iter()
    .find_map(|importer| importer(arg0, input_types[0]).ok())
    .ok_or_else(|| pyo3::exceptions::PyValueError::new_err("Could not import argument 0"))?;
    let input1 = IMPORTERS
    .iter()
    .find_map(|importer| importer(arg1, input_types[1]).ok())
    .ok_or_else(|| pyo3::exceptions::PyValueError::new_err("Could not import argument 1"))?;
    let inputs = vec![input0, input1];

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
fn analyse_mode(arg0: &PyAny) -> PyResult<String> {
    let command: &&EbiCommand = &&EBI_ANALYSE_MODE;
    let input_types = match **command {
        EbiCommand::Command { input_types, .. } => input_types,
        _ => return Err(pyo3::exceptions::PyValueError::new_err("Expected a command.")),
    };
    let input0 = IMPORTERS
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
fn analyse_most_likely_traces(arg0: &PyAny, arg1: &PyAny) -> PyResult<String> {
    let command: &&EbiCommand = &&EBI_ANALYSE_MOSTLIKELY;
    let input_types = match **command {
        EbiCommand::Command { input_types, .. } => input_types,
        _ => return Err(pyo3::exceptions::PyValueError::new_err("Expected a command.")),
    };
    let input0 = IMPORTERS
    .iter()
    .find_map(|importer| importer(arg0, input_types[0]).ok())
    .ok_or_else(|| pyo3::exceptions::PyValueError::new_err("Could not import argument 0"))?;
    let input1 = IMPORTERS
    .iter()
    .find_map(|importer| importer(arg1, input_types[1]).ok())
    .ok_or_else(|| pyo3::exceptions::PyValueError::new_err("Could not import argument 1"))?;
    let inputs = vec![input0, input1];

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
fn analyse_variety(arg0: &PyAny) -> PyResult<String> {
    let command: &&EbiCommand = &&EBI_ANALYSE_VARIETY;
    let input_types = match **command {
        EbiCommand::Command { input_types, .. } => input_types,
        _ => return Err(pyo3::exceptions::PyValueError::new_err("Expected a command.")),
    };
    let input0 = IMPORTERS
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
fn analyse_non_stochastic_alignment(arg0: &PyAny, arg1: &PyAny) -> PyResult<String> {
    let command: &&EbiCommand = &&EBI_ANALYSE_NON_STOCHASTIC_ALIGNMENT;
    let input_types = match **command {
        EbiCommand::Command { input_types, .. } => input_types,
        _ => return Err(pyo3::exceptions::PyValueError::new_err("Expected a command.")),
    };
    let input0 = IMPORTERS
    .iter()
    .find_map(|importer| importer(arg0, input_types[0]).ok())
    .ok_or_else(|| pyo3::exceptions::PyValueError::new_err("Could not import argument 0"))?;
    let input1 = IMPORTERS
    .iter()
    .find_map(|importer| importer(arg1, input_types[1]).ok())
    .ok_or_else(|| pyo3::exceptions::PyValueError::new_err("Could not import argument 1"))?;
    let inputs = vec![input0, input1];

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
fn analyse_non_stochastic_any_traces(arg0: &PyAny) -> PyResult<String> {
    let command: &&EbiCommand = &&EBI_ANALYSE_NON_STOCHASTIC_ANY_TRACES;
    let input_types = match **command {
        EbiCommand::Command { input_types, .. } => input_types,
        _ => return Err(pyo3::exceptions::PyValueError::new_err("Expected a command.")),
    };
    let input0 = IMPORTERS
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
fn analyse_non_stochastic_bounded(arg0: &PyAny) -> PyResult<String> {
    let command: &&EbiCommand = &&EBI_ANALYSE_NON_STOCHASTIC_BOUNDED;
    let input_types = match **command {
        EbiCommand::Command { input_types, .. } => input_types,
        _ => return Err(pyo3::exceptions::PyValueError::new_err("Expected a command.")),
    };
    let input0 = IMPORTERS
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
fn analyse_non_stochastic_cluster(arg0: &PyAny, arg1: &PyAny) -> PyResult<String> {
    let command: &&EbiCommand = &&EBI_ANALYSE_NON_STOCHASTIC_CLUSTER;
    let input_types = match **command {
        EbiCommand::Command { input_types, .. } => input_types,
        _ => return Err(pyo3::exceptions::PyValueError::new_err("Expected a command.")),
    };
    let input0 = IMPORTERS
    .iter()
    .find_map(|importer| importer(arg0, input_types[0]).ok())
    .ok_or_else(|| pyo3::exceptions::PyValueError::new_err("Could not import argument 0"))?;
    let input1 = IMPORTERS
    .iter()
    .find_map(|importer| importer(arg1, input_types[1]).ok())
    .ok_or_else(|| pyo3::exceptions::PyValueError::new_err("Could not import argument 1"))?;
    let inputs = vec![input0, input1];

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
fn analyse_non_stochastic_executions(arg0: &PyAny, arg1: &PyAny) -> PyResult<String> {
    let command: &&EbiCommand = &&EBI_ANALYSE_NON_STOCHASTIC_EXECUTIONS;
    let input_types = match **command {
        EbiCommand::Command { input_types, .. } => input_types,
        _ => return Err(pyo3::exceptions::PyValueError::new_err("Expected a command.")),
    };
    let input0 = IMPORTERS
    .iter()
    .find_map(|importer| importer(arg0, input_types[0]).ok())
    .ok_or_else(|| pyo3::exceptions::PyValueError::new_err("Could not import argument 0"))?;
    let input1 = IMPORTERS
    .iter()
    .find_map(|importer| importer(arg1, input_types[1]).ok())
    .ok_or_else(|| pyo3::exceptions::PyValueError::new_err("Could not import argument 1"))?;
    let inputs = vec![input0, input1];

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
fn analyse_non_stochastic_infinitely_many_traces(arg0: &PyAny) -> PyResult<String> {
    let command: &&EbiCommand = &&EBI_ANALYSE_NON_STOCHASTIC_INFINITELY_MANY_TRACES;
    let input_types = match **command {
        EbiCommand::Command { input_types, .. } => input_types,
        _ => return Err(pyo3::exceptions::PyValueError::new_err("Expected a command.")),
    };
    let input0 = IMPORTERS
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
fn analyse_non_stochastic_medoid(arg0: &PyAny, arg1: &PyAny) -> PyResult<String> {
    let command: &&EbiCommand = &&EBI_ANALYSE_NON_STOCHASTIC_MEDOID;
    let input_types = match **command {
        EbiCommand::Command { input_types, .. } => input_types,
        _ => return Err(pyo3::exceptions::PyValueError::new_err("Expected a command.")),
    };
    let input0 = IMPORTERS
    .iter()
    .find_map(|importer| importer(arg0, input_types[0]).ok())
    .ok_or_else(|| pyo3::exceptions::PyValueError::new_err("Could not import argument 0"))?;
    let input1 = IMPORTERS
    .iter()
    .find_map(|importer| importer(arg1, input_types[1]).ok())
    .ok_or_else(|| pyo3::exceptions::PyValueError::new_err("Could not import argument 1"))?;
    let inputs = vec![input0, input1];

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
fn association_all_trace_attributes(arg0: &PyAny) -> PyResult<String> {
    let command: &&EbiCommand = &&ASSOCIATION_ATTRIBUTES;
    let input_types = match **command {
        EbiCommand::Command { input_types, .. } => input_types,
        _ => return Err(pyo3::exceptions::PyValueError::new_err("Expected a command.")),
    };
    let input0 = IMPORTERS
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
fn association_trace_attribute(arg0: &PyAny, arg1: &PyAny) -> PyResult<String> {
    let command: &&EbiCommand = &&ASSOCIATION_ATTRIBUTE;
    let input_types = match **command {
        EbiCommand::Command { input_types, .. } => input_types,
        _ => return Err(pyo3::exceptions::PyValueError::new_err("Expected a command.")),
    };
    let input0 = IMPORTERS
    .iter()
    .find_map(|importer| importer(arg0, input_types[0]).ok())
    .ok_or_else(|| pyo3::exceptions::PyValueError::new_err("Could not import argument 0"))?;
    let input1 = IMPORTERS
    .iter()
    .find_map(|importer| importer(arg1, input_types[1]).ok())
    .ok_or_else(|| pyo3::exceptions::PyValueError::new_err("Could not import argument 1"))?;
    let inputs = vec![input0, input1];

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
fn conformance_earth_movers_stochastic_conformance(arg0: &PyAny, arg1: &PyAny) -> PyResult<String> {
    let command: &&EbiCommand = &&CONFORMANCE_EMSC;
    let input_types = match **command {
        EbiCommand::Command { input_types, .. } => input_types,
        _ => return Err(pyo3::exceptions::PyValueError::new_err("Expected a command.")),
    };
    let input0 = IMPORTERS
    .iter()
    .find_map(|importer| importer(arg0, input_types[0]).ok())
    .ok_or_else(|| pyo3::exceptions::PyValueError::new_err("Could not import argument 0"))?;
    let input1 = IMPORTERS
    .iter()
    .find_map(|importer| importer(arg1, input_types[1]).ok())
    .ok_or_else(|| pyo3::exceptions::PyValueError::new_err("Could not import argument 1"))?;
    let inputs = vec![input0, input1];

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
fn conformance_earth_movers_stochastic_conformance_sample(arg0: &PyAny, arg1: &PyAny, arg2: &PyAny) -> PyResult<String> {
    let command: &&EbiCommand = &&CONFORMANCE_EMSC_SAMPLE;
    let input_types = match **command {
        EbiCommand::Command { input_types, .. } => input_types,
        _ => return Err(pyo3::exceptions::PyValueError::new_err("Expected a command.")),
    };
    let input0 = IMPORTERS
    .iter()
    .find_map(|importer| importer(arg0, input_types[0]).ok())
    .ok_or_else(|| pyo3::exceptions::PyValueError::new_err("Could not import argument 0"))?;
    let input1 = IMPORTERS
    .iter()
    .find_map(|importer| importer(arg1, input_types[1]).ok())
    .ok_or_else(|| pyo3::exceptions::PyValueError::new_err("Could not import argument 1"))?;
    let input2 = IMPORTERS
    .iter()
    .find_map(|importer| importer(arg2, input_types[2]).ok())
    .ok_or_else(|| pyo3::exceptions::PyValueError::new_err("Could not import argument 2"))?;
    let inputs = vec![input0, input1, input2];

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
fn conformance_entropic_relevance(arg0: &PyAny, arg1: &PyAny) -> PyResult<String> {
    let command: &&EbiCommand = &&CONFORMANCE_ER;
    let input_types = match **command {
        EbiCommand::Command { input_types, .. } => input_types,
        _ => return Err(pyo3::exceptions::PyValueError::new_err("Expected a command.")),
    };
    let input0 = IMPORTERS
    .iter()
    .find_map(|importer| importer(arg0, input_types[0]).ok())
    .ok_or_else(|| pyo3::exceptions::PyValueError::new_err("Could not import argument 0"))?;
    let input1 = IMPORTERS
    .iter()
    .find_map(|importer| importer(arg1, input_types[1]).ok())
    .ok_or_else(|| pyo3::exceptions::PyValueError::new_err("Could not import argument 1"))?;
    let inputs = vec![input0, input1];

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
fn conformance_jensen_shannon(arg0: &PyAny, arg1: &PyAny) -> PyResult<String> {
    let command: &&EbiCommand = &&CONFORMANCE_JSSC;
    let input_types = match **command {
        EbiCommand::Command { input_types, .. } => input_types,
        _ => return Err(pyo3::exceptions::PyValueError::new_err("Expected a command.")),
    };
    let input0 = IMPORTERS
    .iter()
    .find_map(|importer| importer(arg0, input_types[0]).ok())
    .ok_or_else(|| pyo3::exceptions::PyValueError::new_err("Could not import argument 0"))?;
    let input1 = IMPORTERS
    .iter()
    .find_map(|importer| importer(arg1, input_types[1]).ok())
    .ok_or_else(|| pyo3::exceptions::PyValueError::new_err("Could not import argument 1"))?;
    let inputs = vec![input0, input1];

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
fn conformance_jensen_shannon_sample(arg0: &PyAny, arg1: &PyAny, arg2: &PyAny) -> PyResult<String> {
    let command: &&EbiCommand = &&CONFORMANCE_JSSC_SAMPLE;
    let input_types = match **command {
        EbiCommand::Command { input_types, .. } => input_types,
        _ => return Err(pyo3::exceptions::PyValueError::new_err("Expected a command.")),
    };
    let input0 = IMPORTERS
    .iter()
    .find_map(|importer| importer(arg0, input_types[0]).ok())
    .ok_or_else(|| pyo3::exceptions::PyValueError::new_err("Could not import argument 0"))?;
    let input1 = IMPORTERS
    .iter()
    .find_map(|importer| importer(arg1, input_types[1]).ok())
    .ok_or_else(|| pyo3::exceptions::PyValueError::new_err("Could not import argument 1"))?;
    let input2 = IMPORTERS
    .iter()
    .find_map(|importer| importer(arg2, input_types[2]).ok())
    .ok_or_else(|| pyo3::exceptions::PyValueError::new_err("Could not import argument 2"))?;
    let inputs = vec![input0, input1, input2];

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
fn conformance_unit_earth_movers_stochastic_conformance(arg0: &PyAny, arg1: &PyAny) -> PyResult<String> {
    let command: &&EbiCommand = &&CONFORMANCE_UEMSC;
    let input_types = match **command {
        EbiCommand::Command { input_types, .. } => input_types,
        _ => return Err(pyo3::exceptions::PyValueError::new_err("Expected a command.")),
    };
    let input0 = IMPORTERS
    .iter()
    .find_map(|importer| importer(arg0, input_types[0]).ok())
    .ok_or_else(|| pyo3::exceptions::PyValueError::new_err("Could not import argument 0"))?;
    let input1 = IMPORTERS
    .iter()
    .find_map(|importer| importer(arg1, input_types[1]).ok())
    .ok_or_else(|| pyo3::exceptions::PyValueError::new_err("Could not import argument 1"))?;
    let inputs = vec![input0, input1];

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
fn convert_finite_stochastic_language(arg0: &PyAny) -> PyResult<String> {
    let command: &&EbiCommand = &&EBI_CONVERT_SLANG;
    let input_types = match **command {
        EbiCommand::Command { input_types, .. } => input_types,
        _ => return Err(pyo3::exceptions::PyValueError::new_err("Expected a command.")),
    };
    let input0 = IMPORTERS
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
fn convert_labelled_petri_net(arg0: &PyAny) -> PyResult<String> {
    let command: &&EbiCommand = &&EBI_CONVERT_LPN;
    let input_types = match **command {
        EbiCommand::Command { input_types, .. } => input_types,
        _ => return Err(pyo3::exceptions::PyValueError::new_err("Expected a command.")),
    };
    let input0 = IMPORTERS
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
fn convert_stochastic_finite_deterministic_automaton(arg0: &PyAny) -> PyResult<String> {
    let command: &&EbiCommand = &&EBI_CONVERT_SDFA;
    let input_types = match **command {
        EbiCommand::Command { input_types, .. } => input_types,
        _ => return Err(pyo3::exceptions::PyValueError::new_err("Expected a command.")),
    };
    let input0 = IMPORTERS
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
fn discover_alignments(arg0: &PyAny, arg1: &PyAny) -> PyResult<String> {
    let command: &&EbiCommand = &&EBI_DISCOVER_ALIGNMENTS;
    let input_types = match **command {
        EbiCommand::Command { input_types, .. } => input_types,
        _ => return Err(pyo3::exceptions::PyValueError::new_err("Expected a command.")),
    };
    let input0 = IMPORTERS
    .iter()
    .find_map(|importer| importer(arg0, input_types[0]).ok())
    .ok_or_else(|| pyo3::exceptions::PyValueError::new_err("Could not import argument 0"))?;
    let input1 = IMPORTERS
    .iter()
    .find_map(|importer| importer(arg1, input_types[1]).ok())
    .ok_or_else(|| pyo3::exceptions::PyValueError::new_err("Could not import argument 1"))?;
    let inputs = vec![input0, input1];

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
fn discover_directly_follows_graph(arg0: &PyAny, arg1: &PyAny) -> PyResult<String> {
    let command: &&EbiCommand = &&EBI_DISCOVER_DIRECTLY_FOLLOWS;
    let input_types = match **command {
        EbiCommand::Command { input_types, .. } => input_types,
        _ => return Err(pyo3::exceptions::PyValueError::new_err("Expected a command.")),
    };
    let input0 = IMPORTERS
    .iter()
    .find_map(|importer| importer(arg0, input_types[0]).ok())
    .ok_or_else(|| pyo3::exceptions::PyValueError::new_err("Could not import argument 0"))?;
    let input1 = IMPORTERS
    .iter()
    .find_map(|importer| importer(arg1, input_types[1]).ok())
    .ok_or_else(|| pyo3::exceptions::PyValueError::new_err("Could not import argument 1"))?;
    let inputs = vec![input0, input1];

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
fn discover_occurrence(arg0: &PyAny, arg1: &PyAny) -> PyResult<String> {
    let command: &&EbiCommand = &&EBI_DISCOVER_OCCURRENCE;
    let input_types = match **command {
        EbiCommand::Command { input_types, .. } => input_types,
        _ => return Err(pyo3::exceptions::PyValueError::new_err("Expected a command.")),
    };
    let input0 = IMPORTERS
    .iter()
    .find_map(|importer| importer(arg0, input_types[0]).ok())
    .ok_or_else(|| pyo3::exceptions::PyValueError::new_err("Could not import argument 0"))?;
    let input1 = IMPORTERS
    .iter()
    .find_map(|importer| importer(arg1, input_types[1]).ok())
    .ok_or_else(|| pyo3::exceptions::PyValueError::new_err("Could not import argument 1"))?;
    let inputs = vec![input0, input1];

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
fn discover_uniform(arg0: &PyAny) -> PyResult<String> {
    let command: &&EbiCommand = &&EBI_DISCOVER_UNIFORM;
    let input_types = match **command {
        EbiCommand::Command { input_types, .. } => input_types,
        _ => return Err(pyo3::exceptions::PyValueError::new_err("Expected a command.")),
    };
    let input0 = IMPORTERS
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
fn discover_non_stochastic_directly_follows_model(arg0: &PyAny, arg1: &PyAny) -> PyResult<String> {
    let command: &&EbiCommand = &&EBI_DISCOVER_NON_STOCHASTIC_DIRECTLY_FOLLOWS;
    let input_types = match **command {
        EbiCommand::Command { input_types, .. } => input_types,
        _ => return Err(pyo3::exceptions::PyValueError::new_err("Expected a command.")),
    };
    let input0 = IMPORTERS
    .iter()
    .find_map(|importer| importer(arg0, input_types[0]).ok())
    .ok_or_else(|| pyo3::exceptions::PyValueError::new_err("Could not import argument 0"))?;
    let input1 = IMPORTERS
    .iter()
    .find_map(|importer| importer(arg1, input_types[1]).ok())
    .ok_or_else(|| pyo3::exceptions::PyValueError::new_err("Could not import argument 1"))?;
    let inputs = vec![input0, input1];

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
fn discover_non_stochastic_flower_deterministic_finite_automaton(arg0: &PyAny) -> PyResult<String> {
    let command: &&EbiCommand = &&EBI_DISCOVER_NON_STOCHASTIC_FLOWER_DFA;
    let input_types = match **command {
        EbiCommand::Command { input_types, .. } => input_types,
        _ => return Err(pyo3::exceptions::PyValueError::new_err("Expected a command.")),
    };
    let input0 = IMPORTERS
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
fn discover_non_stochastic_flower_process_tree(arg0: &PyAny) -> PyResult<String> {
    let command: &&EbiCommand = &&EBI_DISCOVER_NON_STOCHASTIC_FLOWER_TREE;
    let input_types = match **command {
        EbiCommand::Command { input_types, .. } => input_types,
        _ => return Err(pyo3::exceptions::PyValueError::new_err("Expected a command.")),
    };
    let input0 = IMPORTERS
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
fn discover_non_stochastic_prefix_tree_deterministic_finite_automaton(arg0: &PyAny) -> PyResult<String> {
    let command: &&EbiCommand = &&EBI_DISCOVER_NON_STOCHASTIC_TREE_DFA;
    let input_types = match **command {
        EbiCommand::Command { input_types, .. } => input_types,
        _ => return Err(pyo3::exceptions::PyValueError::new_err("Expected a command.")),
    };
    let input0 = IMPORTERS
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
fn discover_non_stochastic_prefix_tree_process_tree(arg0: &PyAny) -> PyResult<String> {
    let command: &&EbiCommand = &&EBI_DISCOVER_NON_STOCHASTIC_TREE_TREE;
    let input_types = match **command {
        EbiCommand::Command { input_types, .. } => input_types,
        _ => return Err(pyo3::exceptions::PyValueError::new_err("Expected a command.")),
    };
    let input0 = IMPORTERS
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
fn information(arg0: &PyAny) -> PyResult<String> {
    let command: &&EbiCommand = &&EBI_INFO;
    let input_types = match **command {
        EbiCommand::Command { input_types, .. } => input_types,
        _ => return Err(pyo3::exceptions::PyValueError::new_err("Expected a command.")),
    };
    let input0 = IMPORTERS
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
fn itself_generate_pm4py() -> PyResult<String> {
    let command: &&EbiCommand = &&EBI_ITSELF_GENERATE_PM4PY;
    let input_types = match **command {
        EbiCommand::Command { input_types, .. } => input_types,
        _ => return Err(pyo3::exceptions::PyValueError::new_err("Expected a command.")),
    };
    let inputs = vec![];

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
fn itself_graph() -> PyResult<String> {
    let command: &&EbiCommand = &&EBI_ITSELF_GRAPH;
    let input_types = match **command {
        EbiCommand::Command { input_types, .. } => input_types,
        _ => return Err(pyo3::exceptions::PyValueError::new_err("Expected a command.")),
    };
    let inputs = vec![];

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
fn itself_java() -> PyResult<String> {
    let command: &&EbiCommand = &&EBI_ITSELF_JAVA;
    let input_types = match **command {
        EbiCommand::Command { input_types, .. } => input_types,
        _ => return Err(pyo3::exceptions::PyValueError::new_err("Expected a command.")),
    };
    let inputs = vec![];

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
fn itself_logo() -> PyResult<String> {
    let command: &&EbiCommand = &&EBI_ITSELF_LOGO;
    let input_types = match **command {
        EbiCommand::Command { input_types, .. } => input_types,
        _ => return Err(pyo3::exceptions::PyValueError::new_err("Expected a command.")),
    };
    let inputs = vec![];

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
fn itself_manual() -> PyResult<String> {
    let command: &&EbiCommand = &&EBI_ITSELF_MANUAL;
    let input_types = match **command {
        EbiCommand::Command { input_types, .. } => input_types,
        _ => return Err(pyo3::exceptions::PyValueError::new_err("Expected a command.")),
    };
    let inputs = vec![];

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
fn probability_explain_trace(arg0: &PyAny, arg1: &PyAny) -> PyResult<String> {
    let command: &&EbiCommand = &&EBI_PROBABILITY_EXPLAIN_TRACE;
    let input_types = match **command {
        EbiCommand::Command { input_types, .. } => input_types,
        _ => return Err(pyo3::exceptions::PyValueError::new_err("Expected a command.")),
    };
    let input0 = IMPORTERS
    .iter()
    .find_map(|importer| importer(arg0, input_types[0]).ok())
    .ok_or_else(|| pyo3::exceptions::PyValueError::new_err("Could not import argument 0"))?;
    let input1 = IMPORTERS
    .iter()
    .find_map(|importer| importer(arg1, input_types[1]).ok())
    .ok_or_else(|| pyo3::exceptions::PyValueError::new_err("Could not import argument 1"))?;
    let inputs = vec![input0, input1];

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
fn probability_model(arg0: &PyAny, arg1: &PyAny) -> PyResult<String> {
    let command: &&EbiCommand = &&EBI_PROBABILITY_MODEL;
    let input_types = match **command {
        EbiCommand::Command { input_types, .. } => input_types,
        _ => return Err(pyo3::exceptions::PyValueError::new_err("Expected a command.")),
    };
    let input0 = IMPORTERS
    .iter()
    .find_map(|importer| importer(arg0, input_types[0]).ok())
    .ok_or_else(|| pyo3::exceptions::PyValueError::new_err("Could not import argument 0"))?;
    let input1 = IMPORTERS
    .iter()
    .find_map(|importer| importer(arg1, input_types[1]).ok())
    .ok_or_else(|| pyo3::exceptions::PyValueError::new_err("Could not import argument 1"))?;
    let inputs = vec![input0, input1];

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
fn probability_trace(arg0: &PyAny) -> PyResult<String> {
    let command: &&EbiCommand = &&EBI_PROBABILITY_TRACE;
    let input_types = match **command {
        EbiCommand::Command { input_types, .. } => input_types,
        _ => return Err(pyo3::exceptions::PyValueError::new_err("Expected a command.")),
    };
    let input0 = IMPORTERS
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
fn sample(arg0: &PyAny, arg1: &PyAny) -> PyResult<String> {
    let command: &&EbiCommand = &&EBI_SAMPLE;
    let input_types = match **command {
        EbiCommand::Command { input_types, .. } => input_types,
        _ => return Err(pyo3::exceptions::PyValueError::new_err("Expected a command.")),
    };
    let input0 = IMPORTERS
    .iter()
    .find_map(|importer| importer(arg0, input_types[0]).ok())
    .ok_or_else(|| pyo3::exceptions::PyValueError::new_err("Could not import argument 0"))?;
    let input1 = IMPORTERS
    .iter()
    .find_map(|importer| importer(arg1, input_types[1]).ok())
    .ok_or_else(|| pyo3::exceptions::PyValueError::new_err("Could not import argument 1"))?;
    let inputs = vec![input0, input1];

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
fn test_log_categorical_attribute(arg0: &PyAny, arg1: &PyAny) -> PyResult<String> {
    let command: &&EbiCommand = &&TEST_LOG_ATTRIBUTE;
    let input_types = match **command {
        EbiCommand::Command { input_types, .. } => input_types,
        _ => return Err(pyo3::exceptions::PyValueError::new_err("Expected a command.")),
    };
    let input0 = IMPORTERS
    .iter()
    .find_map(|importer| importer(arg0, input_types[0]).ok())
    .ok_or_else(|| pyo3::exceptions::PyValueError::new_err("Could not import argument 0"))?;
    let input1 = IMPORTERS
    .iter()
    .find_map(|importer| importer(arg1, input_types[1]).ok())
    .ok_or_else(|| pyo3::exceptions::PyValueError::new_err("Could not import argument 1"))?;
    let inputs = vec![input0, input1];

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
fn validate(arg0: &PyAny) -> PyResult<String> {
    let command: &&EbiCommand = &&EBI_VALIDATE;
    let input_types = match **command {
        EbiCommand::Command { input_types, .. } => input_types,
        _ => return Err(pyo3::exceptions::PyValueError::new_err("Expected a command.")),
    };
    let input0 = IMPORTERS
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
fn visualise_pdf(arg0: &PyAny) -> PyResult<String> {
    let command: &&EbiCommand = &&EBI_VISUALISE_PDF;
    let input_types = match **command {
        EbiCommand::Command { input_types, .. } => input_types,
        _ => return Err(pyo3::exceptions::PyValueError::new_err("Expected a command.")),
    };
    let input0 = IMPORTERS
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
fn visualise_svg(py: Python<'_>, arg0: &PyAny) -> PyResult<PyObject> {
    let command: &&EbiCommand = &&EBI_VISUALISE_SVG;
    let input_types = match **command {
        EbiCommand::Command { input_types, .. } => input_types,
        _ => return Err(pyo3::exceptions::PyValueError::new_err("Expected a command.")),
    };
    let input0 = IMPORTERS
    .iter()
    .find_map(|importer| importer(arg0, input_types[0]).ok())
    .ok_or_else(|| pyo3::exceptions::PyValueError::new_err("Could not import argument 0"))?;
    let inputs = vec![input0];

    // Execute the command.
    let result = command.execute_with_inputs(inputs)
        .map_err(|e| pyo3::exceptions::PyException::new_err(format!("Command error: {}", e)))?
        .export_to_pm4py(py)
        .map_err(|e| pyo3::exceptions::PyException::new_err(format!("Export error: {}", e)))?;

    Ok(result)
}

#[pyfunction]
fn visualise_text(arg0: &PyAny) -> PyResult<String> {
    let command: &&EbiCommand = &&EBI_VISUALISE_TEXT;
    let input_types = match **command {
        EbiCommand::Command { input_types, .. } => input_types,
        _ => return Err(pyo3::exceptions::PyValueError::new_err("Expected a command.")),
    };
    let input0 = IMPORTERS
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



#[pymodule]
fn ebi(_py: Python<'_>, m: &PyModule) -> PyResult<()> {    m.add_function(wrap_pyfunction!(analyse_all_traces, m)?)?;
    m.add_function(wrap_pyfunction!(analyse_completeness, m)?)?;
    m.add_function(wrap_pyfunction!(analyse_coverage, m)?)?;
    m.add_function(wrap_pyfunction!(analyse_directly_follows_edge_difference, m)?)?;
    m.add_function(wrap_pyfunction!(analyse_medoid, m)?)?;
    m.add_function(wrap_pyfunction!(analyse_minimum_probability_traces, m)?)?;
    m.add_function(wrap_pyfunction!(analyse_mode, m)?)?;
    m.add_function(wrap_pyfunction!(analyse_most_likely_traces, m)?)?;
    m.add_function(wrap_pyfunction!(analyse_variety, m)?)?;
    m.add_function(wrap_pyfunction!(analyse_non_stochastic_alignment, m)?)?;
    m.add_function(wrap_pyfunction!(analyse_non_stochastic_any_traces, m)?)?;
    m.add_function(wrap_pyfunction!(analyse_non_stochastic_bounded, m)?)?;
    m.add_function(wrap_pyfunction!(analyse_non_stochastic_cluster, m)?)?;
    m.add_function(wrap_pyfunction!(analyse_non_stochastic_executions, m)?)?;
    m.add_function(wrap_pyfunction!(analyse_non_stochastic_infinitely_many_traces, m)?)?;
    m.add_function(wrap_pyfunction!(analyse_non_stochastic_medoid, m)?)?;
    m.add_function(wrap_pyfunction!(association_all_trace_attributes, m)?)?;
    m.add_function(wrap_pyfunction!(association_trace_attribute, m)?)?;
    m.add_function(wrap_pyfunction!(conformance_earth_movers_stochastic_conformance, m)?)?;
    m.add_function(wrap_pyfunction!(conformance_earth_movers_stochastic_conformance_sample, m)?)?;
    m.add_function(wrap_pyfunction!(conformance_entropic_relevance, m)?)?;
    m.add_function(wrap_pyfunction!(conformance_jensen_shannon, m)?)?;
    m.add_function(wrap_pyfunction!(conformance_jensen_shannon_sample, m)?)?;
    m.add_function(wrap_pyfunction!(conformance_unit_earth_movers_stochastic_conformance, m)?)?;
    m.add_function(wrap_pyfunction!(convert_finite_stochastic_language, m)?)?;
    m.add_function(wrap_pyfunction!(convert_labelled_petri_net, m)?)?;
    m.add_function(wrap_pyfunction!(convert_stochastic_finite_deterministic_automaton, m)?)?;
    m.add_function(wrap_pyfunction!(discover_alignments, m)?)?;
    m.add_function(wrap_pyfunction!(discover_directly_follows_graph, m)?)?;
    m.add_function(wrap_pyfunction!(discover_occurrence, m)?)?;
    m.add_function(wrap_pyfunction!(discover_uniform, m)?)?;
    m.add_function(wrap_pyfunction!(discover_non_stochastic_directly_follows_model, m)?)?;
    m.add_function(wrap_pyfunction!(discover_non_stochastic_flower_deterministic_finite_automaton, m)?)?;
    m.add_function(wrap_pyfunction!(discover_non_stochastic_flower_process_tree, m)?)?;
    m.add_function(wrap_pyfunction!(discover_non_stochastic_prefix_tree_deterministic_finite_automaton, m)?)?;
    m.add_function(wrap_pyfunction!(discover_non_stochastic_prefix_tree_process_tree, m)?)?;
    m.add_function(wrap_pyfunction!(information, m)?)?;
    m.add_function(wrap_pyfunction!(itself_generate_pm4py, m)?)?;
    m.add_function(wrap_pyfunction!(itself_graph, m)?)?;
    m.add_function(wrap_pyfunction!(itself_java, m)?)?;
    m.add_function(wrap_pyfunction!(itself_logo, m)?)?;
    m.add_function(wrap_pyfunction!(itself_manual, m)?)?;
    m.add_function(wrap_pyfunction!(probability_explain_trace, m)?)?;
    m.add_function(wrap_pyfunction!(probability_model, m)?)?;
    m.add_function(wrap_pyfunction!(probability_trace, m)?)?;
    m.add_function(wrap_pyfunction!(sample, m)?)?;
    m.add_function(wrap_pyfunction!(test_log_categorical_attribute, m)?)?;
    m.add_function(wrap_pyfunction!(validate, m)?)?;
    m.add_function(wrap_pyfunction!(visualise_pdf, m)?)?;
    m.add_function(wrap_pyfunction!(visualise_svg, m)?)?;
    m.add_function(wrap_pyfunction!(visualise_text, m)?)?;
    Ok(())
}


