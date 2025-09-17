#![allow(unsafe_op_in_unsafe_fn)]
#![allow(unused_variables)]

use pyo3::prelude::*;
use pyo3::types::PyAny;
use super::pm4py_link::{import_or_load, ExportableToPM4Py};
use crate::ebi_framework::ebi_command::EbiCommand;
use crate::ebi_commands::ebi_command_analyse::EBI_ANALYSE_ALL;
use crate::ebi_commands::ebi_command_analyse::EBI_ANALYSE_COMPLETENESS;
use crate::ebi_commands::ebi_command_analyse::EBI_ANALYSE_COVERAGE;
use crate::ebi_commands::ebi_command_analyse::EBI_ANALYSE_DIRECTLY_FOLLOWS_EDGE_DIFFERENCE;
use crate::ebi_commands::ebi_command_analyse::EBI_ANALYSE_MEDOID;
use crate::ebi_commands::ebi_command_analyse::EBI_ANALYSE_MINPROB;
use crate::ebi_commands::ebi_command_analyse::EBI_ANALYSE_MODE;
use crate::ebi_commands::ebi_command_analyse::EBI_ANALYSE_MOSTLIKELY;
use crate::ebi_commands::ebi_command_analyse::EBI_ANALYSE_VARIETY;
use crate::ebi_commands::ebi_command_analyse_non_stochastic::EBI_ANALYSE_NON_STOCHASTIC_ACTIVITIES;
use crate::ebi_commands::ebi_command_analyse_non_stochastic::EBI_ANALYSE_NON_STOCHASTIC_ANY_TRACES;
use crate::ebi_commands::ebi_command_analyse_non_stochastic::EBI_ANALYSE_NON_STOCHASTIC_BOUNDED;
use crate::ebi_commands::ebi_command_analyse_non_stochastic::EBI_ANALYSE_NON_STOCHASTIC_CLUSTER;
use crate::ebi_commands::ebi_command_analyse_non_stochastic::EBI_ANALYSE_NON_STOCHASTIC_EXECUTIONS;
use crate::ebi_commands::ebi_command_analyse_non_stochastic::EBI_ANALYSE_NON_STOCHASTIC_INFINITELY_MANY_TRACES;
use crate::ebi_commands::ebi_command_analyse_non_stochastic::EBI_ANALYSE_NON_STOCHASTIC_MEDOID;
use crate::ebi_commands::ebi_command_association::ASSOCIATION_ATTRIBUTES;
use crate::ebi_commands::ebi_command_association::ASSOCIATION_ATTRIBUTE;
use crate::ebi_commands::ebi_command_conformance::CONFORMANCE_CSSC;
use crate::ebi_commands::ebi_command_conformance::CONFORMANCE_EMSC;
use crate::ebi_commands::ebi_command_conformance::CONFORMANCE_EMSC_SAMPLE;
use crate::ebi_commands::ebi_command_conformance::CONFORMANCE_ER;
use crate::ebi_commands::ebi_command_conformance::CONFORMANCE_HSC;
use crate::ebi_commands::ebi_command_conformance::CONFORMANCE_JSSC;
use crate::ebi_commands::ebi_command_conformance::CONFORMANCE_JSSC_SAMPLE;
use crate::ebi_commands::ebi_command_conformance::CONFORMANCE_UEMSC;
use crate::ebi_commands::ebi_command_conformance_non_stochastic::EBI_CONFORMANCE_NON_STOCHASTIC_ALIGNMENTS;
use crate::ebi_commands::ebi_command_conformance_non_stochastic::EBI_CONFORMANCE_NON_STOCHASTIC_ESCAPING_EDGES_PRECISION;
use crate::ebi_commands::ebi_command_conformance_non_stochastic::EBI_CONFORMANCE_NON_STOCHASTIC_SET_ALIGNMENTS;
use crate::ebi_commands::ebi_command_conformance_non_stochastic::EBI_CONFORMANCE_NON_STOCHASTIC_TRACE_FITNESS;
use crate::ebi_commands::ebi_command_convert::EBI_CONVERT_SLANG;
use crate::ebi_commands::ebi_command_convert::EBI_CONVERT_LPN;
use crate::ebi_commands::ebi_command_convert::EBI_CONVERT_SDFA;
use crate::ebi_commands::ebi_command_discover::EBI_DISCOVER_ALIGNMENTS;
use crate::ebi_commands::ebi_command_discover::EBI_DISCOVER_DIRECTLY_FOLLOWS;
use crate::ebi_commands::ebi_command_discover::EBI_DISCOVER_OCCURRENCE;
use crate::ebi_commands::ebi_command_discover::EBI_DISCOVER_UNIFORM;
use crate::ebi_commands::ebi_command_discover_non_stochastic::EBI_DISCOVER_NON_STOCHASTIC_FLOWER_DFA;
use crate::ebi_commands::ebi_command_discover_non_stochastic::EBI_DISCOVER_NON_STOCHASTIC_FLOWER_TREE;
use crate::ebi_commands::ebi_command_discover_non_stochastic::EBI_DISCOVER_NON_STOCHASTIC_TREE_DFA;
use crate::ebi_commands::ebi_command_discover_non_stochastic::EBI_DISCOVER_NON_STOCHASTIC_TREE_TREE;
use crate::ebi_commands::ebi_command_info::EBI_INFO;
use crate::ebi_commands::ebi_command_itself::EBI_ITSELF_GRAPH;
use crate::ebi_commands::ebi_command_itself::EBI_ITSELF_HTML;
use crate::ebi_commands::ebi_command_itself::EBI_ITSELF_JAVA;
use crate::ebi_commands::ebi_command_itself::EBI_ITSELF_LOGO;
use crate::ebi_commands::ebi_command_itself::EBI_ITSELF_MANUAL;
use crate::ebi_commands::ebi_command_itself::EBI_ITSELF_PM4PY;
use crate::ebi_commands::ebi_command_probability::EBI_PROBABILITY_EXPLAIN_TRACE;
use crate::ebi_commands::ebi_command_probability::EBI_PROBABILITY_LOG;
use crate::ebi_commands::ebi_command_probability::EBI_PROBABILITY_TRACE;
use crate::ebi_commands::ebi_command_sample::EBI_SAMPLE_FOLDS;
use crate::ebi_commands::ebi_command_sample::EBI_SAMPLE_TRACES;
use crate::ebi_commands::ebi_command_test::EBI_BOOTSTRAP_TEST;
use crate::ebi_commands::ebi_command_test::EBI_TEST_LOG_ATTRIBUTE;
use crate::ebi_commands::ebi_command_validate::EBI_VALIDATE;
use crate::ebi_commands::ebi_command_visualise::EBI_VISUALISE_GRAPH;
use crate::ebi_commands::ebi_command_visualise::EBI_VISUALISE_TEXT;

#[pyfunction]
fn analyse_all_traces(py: Python<'_>, arg0: &PyAny) -> PyResult<PyObject> {
    let command: &&EbiCommand = &&EBI_ANALYSE_ALL;
    let input_types = match **command {
        EbiCommand::Command { input_types, .. } => input_types,
        _ => return Err(pyo3::exceptions::PyValueError::new_err("Expected a command.")),
    };
    let input0 = import_or_load(arg0, input_types[0], 0)
            .map_err(|e| pyo3::exceptions::PyValueError::new_err(format!("Could not import argument 0: {}", e)))?;
        let inputs = vec![input0];

    // Execute the command.
    let result = command.execute_with_inputs(inputs)
        .map_err(|e| pyo3::exceptions::PyException::new_err(format!("Command error: {}", e)))?
        .export_to_pm4py(py)
        .map_err(|e| pyo3::exceptions::PyException::new_err(format!("Export error: {}", e)))?;

    Ok(result)
}

#[pyfunction]
fn analyse_completeness(py: Python<'_>, arg0: &PyAny) -> PyResult<PyObject> {
    let command: &&EbiCommand = &&EBI_ANALYSE_COMPLETENESS;
    let input_types = match **command {
        EbiCommand::Command { input_types, .. } => input_types,
        _ => return Err(pyo3::exceptions::PyValueError::new_err("Expected a command.")),
    };
    let input0 = import_or_load(arg0, input_types[0], 0)
            .map_err(|e| pyo3::exceptions::PyValueError::new_err(format!("Could not import argument 0: {}", e)))?;
        let inputs = vec![input0];

    // Execute the command.
    let result = command.execute_with_inputs(inputs)
        .map_err(|e| pyo3::exceptions::PyException::new_err(format!("Command error: {}", e)))?
        .export_to_pm4py(py)
        .map_err(|e| pyo3::exceptions::PyException::new_err(format!("Export error: {}", e)))?;

    Ok(result)
}

#[pyfunction]
fn analyse_coverage(py: Python<'_>, arg0: &PyAny, arg1: &PyAny) -> PyResult<PyObject> {
    let command: &&EbiCommand = &&EBI_ANALYSE_COVERAGE;
    let input_types = match **command {
        EbiCommand::Command { input_types, .. } => input_types,
        _ => return Err(pyo3::exceptions::PyValueError::new_err("Expected a command.")),
    };
    let input0 = import_or_load(arg0, input_types[0], 0)
            .map_err(|e| pyo3::exceptions::PyValueError::new_err(format!("Could not import argument 0: {}", e)))?;
        let input1 = import_or_load(arg1, input_types[1], 1)
            .map_err(|e| pyo3::exceptions::PyValueError::new_err(format!("Could not import argument 1: {}", e)))?;
        let inputs = vec![input0, input1];

    // Execute the command.
    let result = command.execute_with_inputs(inputs)
        .map_err(|e| pyo3::exceptions::PyException::new_err(format!("Command error: {}", e)))?
        .export_to_pm4py(py)
        .map_err(|e| pyo3::exceptions::PyException::new_err(format!("Export error: {}", e)))?;

    Ok(result)
}

#[pyfunction]
fn analyse_directly_follows_edge_difference(py: Python<'_>, arg0: &PyAny, arg1: &PyAny) -> PyResult<PyObject> {
    let command: &&EbiCommand = &&EBI_ANALYSE_DIRECTLY_FOLLOWS_EDGE_DIFFERENCE;
    let input_types = match **command {
        EbiCommand::Command { input_types, .. } => input_types,
        _ => return Err(pyo3::exceptions::PyValueError::new_err("Expected a command.")),
    };
    let input0 = import_or_load(arg0, input_types[0], 0)
            .map_err(|e| pyo3::exceptions::PyValueError::new_err(format!("Could not import argument 0: {}", e)))?;
        let input1 = import_or_load(arg1, input_types[1], 1)
            .map_err(|e| pyo3::exceptions::PyValueError::new_err(format!("Could not import argument 1: {}", e)))?;
        let inputs = vec![input0, input1];

    // Execute the command.
    let result = command.execute_with_inputs(inputs)
        .map_err(|e| pyo3::exceptions::PyException::new_err(format!("Command error: {}", e)))?
        .export_to_pm4py(py)
        .map_err(|e| pyo3::exceptions::PyException::new_err(format!("Export error: {}", e)))?;

    Ok(result)
}

#[pyfunction]
fn analyse_medoid(py: Python<'_>, arg0: &PyAny, arg1: &PyAny) -> PyResult<PyObject> {
    let command: &&EbiCommand = &&EBI_ANALYSE_MEDOID;
    let input_types = match **command {
        EbiCommand::Command { input_types, .. } => input_types,
        _ => return Err(pyo3::exceptions::PyValueError::new_err("Expected a command.")),
    };
    let input0 = import_or_load(arg0, input_types[0], 0)
            .map_err(|e| pyo3::exceptions::PyValueError::new_err(format!("Could not import argument 0: {}", e)))?;
        let input1 = import_or_load(arg1, input_types[1], 1)
            .map_err(|e| pyo3::exceptions::PyValueError::new_err(format!("Could not import argument 1: {}", e)))?;
        let inputs = vec![input0, input1];

    // Execute the command.
    let result = command.execute_with_inputs(inputs)
        .map_err(|e| pyo3::exceptions::PyException::new_err(format!("Command error: {}", e)))?
        .export_to_pm4py(py)
        .map_err(|e| pyo3::exceptions::PyException::new_err(format!("Export error: {}", e)))?;

    Ok(result)
}

#[pyfunction]
fn analyse_minimum_probability_traces(py: Python<'_>, arg0: &PyAny, arg1: &PyAny) -> PyResult<PyObject> {
    let command: &&EbiCommand = &&EBI_ANALYSE_MINPROB;
    let input_types = match **command {
        EbiCommand::Command { input_types, .. } => input_types,
        _ => return Err(pyo3::exceptions::PyValueError::new_err("Expected a command.")),
    };
    let input0 = import_or_load(arg0, input_types[0], 0)
            .map_err(|e| pyo3::exceptions::PyValueError::new_err(format!("Could not import argument 0: {}", e)))?;
        let input1 = import_or_load(arg1, input_types[1], 1)
            .map_err(|e| pyo3::exceptions::PyValueError::new_err(format!("Could not import argument 1: {}", e)))?;
        let inputs = vec![input0, input1];

    // Execute the command.
    let result = command.execute_with_inputs(inputs)
        .map_err(|e| pyo3::exceptions::PyException::new_err(format!("Command error: {}", e)))?
        .export_to_pm4py(py)
        .map_err(|e| pyo3::exceptions::PyException::new_err(format!("Export error: {}", e)))?;

    Ok(result)
}

#[pyfunction]
fn analyse_mode(py: Python<'_>, arg0: &PyAny) -> PyResult<PyObject> {
    let command: &&EbiCommand = &&EBI_ANALYSE_MODE;
    let input_types = match **command {
        EbiCommand::Command { input_types, .. } => input_types,
        _ => return Err(pyo3::exceptions::PyValueError::new_err("Expected a command.")),
    };
    let input0 = import_or_load(arg0, input_types[0], 0)
            .map_err(|e| pyo3::exceptions::PyValueError::new_err(format!("Could not import argument 0: {}", e)))?;
        let inputs = vec![input0];

    // Execute the command.
    let result = command.execute_with_inputs(inputs)
        .map_err(|e| pyo3::exceptions::PyException::new_err(format!("Command error: {}", e)))?
        .export_to_pm4py(py)
        .map_err(|e| pyo3::exceptions::PyException::new_err(format!("Export error: {}", e)))?;

    Ok(result)
}

#[pyfunction]
fn analyse_most_likely_traces(py: Python<'_>, arg0: &PyAny, arg1: &PyAny) -> PyResult<PyObject> {
    let command: &&EbiCommand = &&EBI_ANALYSE_MOSTLIKELY;
    let input_types = match **command {
        EbiCommand::Command { input_types, .. } => input_types,
        _ => return Err(pyo3::exceptions::PyValueError::new_err("Expected a command.")),
    };
    let input0 = import_or_load(arg0, input_types[0], 0)
            .map_err(|e| pyo3::exceptions::PyValueError::new_err(format!("Could not import argument 0: {}", e)))?;
        let input1 = import_or_load(arg1, input_types[1], 1)
            .map_err(|e| pyo3::exceptions::PyValueError::new_err(format!("Could not import argument 1: {}", e)))?;
        let inputs = vec![input0, input1];

    // Execute the command.
    let result = command.execute_with_inputs(inputs)
        .map_err(|e| pyo3::exceptions::PyException::new_err(format!("Command error: {}", e)))?
        .export_to_pm4py(py)
        .map_err(|e| pyo3::exceptions::PyException::new_err(format!("Export error: {}", e)))?;

    Ok(result)
}

#[pyfunction]
fn analyse_variety(py: Python<'_>, arg0: &PyAny) -> PyResult<PyObject> {
    let command: &&EbiCommand = &&EBI_ANALYSE_VARIETY;
    let input_types = match **command {
        EbiCommand::Command { input_types, .. } => input_types,
        _ => return Err(pyo3::exceptions::PyValueError::new_err("Expected a command.")),
    };
    let input0 = import_or_load(arg0, input_types[0], 0)
            .map_err(|e| pyo3::exceptions::PyValueError::new_err(format!("Could not import argument 0: {}", e)))?;
        let inputs = vec![input0];

    // Execute the command.
    let result = command.execute_with_inputs(inputs)
        .map_err(|e| pyo3::exceptions::PyException::new_err(format!("Command error: {}", e)))?
        .export_to_pm4py(py)
        .map_err(|e| pyo3::exceptions::PyException::new_err(format!("Export error: {}", e)))?;

    Ok(result)
}

#[pyfunction]
fn analyse_non_stochastic_activities(py: Python<'_>, arg0: &PyAny) -> PyResult<PyObject> {
    let command: &&EbiCommand = &&EBI_ANALYSE_NON_STOCHASTIC_ACTIVITIES;
    let input_types = match **command {
        EbiCommand::Command { input_types, .. } => input_types,
        _ => return Err(pyo3::exceptions::PyValueError::new_err("Expected a command.")),
    };
    let input0 = import_or_load(arg0, input_types[0], 0)
            .map_err(|e| pyo3::exceptions::PyValueError::new_err(format!("Could not import argument 0: {}", e)))?;
        let inputs = vec![input0];

    // Execute the command.
    let result = command.execute_with_inputs(inputs)
        .map_err(|e| pyo3::exceptions::PyException::new_err(format!("Command error: {}", e)))?
        .export_to_pm4py(py)
        .map_err(|e| pyo3::exceptions::PyException::new_err(format!("Export error: {}", e)))?;

    Ok(result)
}

#[pyfunction]
fn analyse_non_stochastic_any_traces(py: Python<'_>, arg0: &PyAny) -> PyResult<PyObject> {
    let command: &&EbiCommand = &&EBI_ANALYSE_NON_STOCHASTIC_ANY_TRACES;
    let input_types = match **command {
        EbiCommand::Command { input_types, .. } => input_types,
        _ => return Err(pyo3::exceptions::PyValueError::new_err("Expected a command.")),
    };
    let input0 = import_or_load(arg0, input_types[0], 0)
            .map_err(|e| pyo3::exceptions::PyValueError::new_err(format!("Could not import argument 0: {}", e)))?;
        let inputs = vec![input0];

    // Execute the command.
    let result = command.execute_with_inputs(inputs)
        .map_err(|e| pyo3::exceptions::PyException::new_err(format!("Command error: {}", e)))?
        .export_to_pm4py(py)
        .map_err(|e| pyo3::exceptions::PyException::new_err(format!("Export error: {}", e)))?;

    Ok(result)
}

#[pyfunction]
fn analyse_non_stochastic_bounded(py: Python<'_>, arg0: &PyAny) -> PyResult<PyObject> {
    let command: &&EbiCommand = &&EBI_ANALYSE_NON_STOCHASTIC_BOUNDED;
    let input_types = match **command {
        EbiCommand::Command { input_types, .. } => input_types,
        _ => return Err(pyo3::exceptions::PyValueError::new_err("Expected a command.")),
    };
    let input0 = import_or_load(arg0, input_types[0], 0)
            .map_err(|e| pyo3::exceptions::PyValueError::new_err(format!("Could not import argument 0: {}", e)))?;
        let inputs = vec![input0];

    // Execute the command.
    let result = command.execute_with_inputs(inputs)
        .map_err(|e| pyo3::exceptions::PyException::new_err(format!("Command error: {}", e)))?
        .export_to_pm4py(py)
        .map_err(|e| pyo3::exceptions::PyException::new_err(format!("Export error: {}", e)))?;

    Ok(result)
}

#[pyfunction]
fn analyse_non_stochastic_cluster(py: Python<'_>, arg0: &PyAny, arg1: &PyAny) -> PyResult<PyObject> {
    let command: &&EbiCommand = &&EBI_ANALYSE_NON_STOCHASTIC_CLUSTER;
    let input_types = match **command {
        EbiCommand::Command { input_types, .. } => input_types,
        _ => return Err(pyo3::exceptions::PyValueError::new_err("Expected a command.")),
    };
    let input0 = import_or_load(arg0, input_types[0], 0)
            .map_err(|e| pyo3::exceptions::PyValueError::new_err(format!("Could not import argument 0: {}", e)))?;
        let input1 = import_or_load(arg1, input_types[1], 1)
            .map_err(|e| pyo3::exceptions::PyValueError::new_err(format!("Could not import argument 1: {}", e)))?;
        let inputs = vec![input0, input1];

    // Execute the command.
    let result = command.execute_with_inputs(inputs)
        .map_err(|e| pyo3::exceptions::PyException::new_err(format!("Command error: {}", e)))?
        .export_to_pm4py(py)
        .map_err(|e| pyo3::exceptions::PyException::new_err(format!("Export error: {}", e)))?;

    Ok(result)
}

#[pyfunction]
fn analyse_non_stochastic_executions(py: Python<'_>, arg0: &PyAny, arg1: &PyAny) -> PyResult<PyObject> {
    let command: &&EbiCommand = &&EBI_ANALYSE_NON_STOCHASTIC_EXECUTIONS;
    let input_types = match **command {
        EbiCommand::Command { input_types, .. } => input_types,
        _ => return Err(pyo3::exceptions::PyValueError::new_err("Expected a command.")),
    };
    let input0 = import_or_load(arg0, input_types[0], 0)
            .map_err(|e| pyo3::exceptions::PyValueError::new_err(format!("Could not import argument 0: {}", e)))?;
        let input1 = import_or_load(arg1, input_types[1], 1)
            .map_err(|e| pyo3::exceptions::PyValueError::new_err(format!("Could not import argument 1: {}", e)))?;
        let inputs = vec![input0, input1];

    // Execute the command.
    let result = command.execute_with_inputs(inputs)
        .map_err(|e| pyo3::exceptions::PyException::new_err(format!("Command error: {}", e)))?
        .export_to_pm4py(py)
        .map_err(|e| pyo3::exceptions::PyException::new_err(format!("Export error: {}", e)))?;

    Ok(result)
}

#[pyfunction]
fn analyse_non_stochastic_infinitely_many_traces(py: Python<'_>, arg0: &PyAny) -> PyResult<PyObject> {
    let command: &&EbiCommand = &&EBI_ANALYSE_NON_STOCHASTIC_INFINITELY_MANY_TRACES;
    let input_types = match **command {
        EbiCommand::Command { input_types, .. } => input_types,
        _ => return Err(pyo3::exceptions::PyValueError::new_err("Expected a command.")),
    };
    let input0 = import_or_load(arg0, input_types[0], 0)
            .map_err(|e| pyo3::exceptions::PyValueError::new_err(format!("Could not import argument 0: {}", e)))?;
        let inputs = vec![input0];

    // Execute the command.
    let result = command.execute_with_inputs(inputs)
        .map_err(|e| pyo3::exceptions::PyException::new_err(format!("Command error: {}", e)))?
        .export_to_pm4py(py)
        .map_err(|e| pyo3::exceptions::PyException::new_err(format!("Export error: {}", e)))?;

    Ok(result)
}

#[pyfunction]
fn analyse_non_stochastic_medoid(py: Python<'_>, arg0: &PyAny, arg1: &PyAny) -> PyResult<PyObject> {
    let command: &&EbiCommand = &&EBI_ANALYSE_NON_STOCHASTIC_MEDOID;
    let input_types = match **command {
        EbiCommand::Command { input_types, .. } => input_types,
        _ => return Err(pyo3::exceptions::PyValueError::new_err("Expected a command.")),
    };
    let input0 = import_or_load(arg0, input_types[0], 0)
            .map_err(|e| pyo3::exceptions::PyValueError::new_err(format!("Could not import argument 0: {}", e)))?;
        let input1 = import_or_load(arg1, input_types[1], 1)
            .map_err(|e| pyo3::exceptions::PyValueError::new_err(format!("Could not import argument 1: {}", e)))?;
        let inputs = vec![input0, input1];

    // Execute the command.
    let result = command.execute_with_inputs(inputs)
        .map_err(|e| pyo3::exceptions::PyException::new_err(format!("Command error: {}", e)))?
        .export_to_pm4py(py)
        .map_err(|e| pyo3::exceptions::PyException::new_err(format!("Export error: {}", e)))?;

    Ok(result)
}

#[pyfunction]
fn association_all_trace_attributes(py: Python<'_>, arg0: &PyAny, arg1: &PyAny) -> PyResult<PyObject> {
    let command: &&EbiCommand = &&ASSOCIATION_ATTRIBUTES;
    let input_types = match **command {
        EbiCommand::Command { input_types, .. } => input_types,
        _ => return Err(pyo3::exceptions::PyValueError::new_err("Expected a command.")),
    };
    let input0 = import_or_load(arg0, input_types[0], 0)
            .map_err(|e| pyo3::exceptions::PyValueError::new_err(format!("Could not import argument 0: {}", e)))?;
        let input1 = import_or_load(arg1, input_types[1], 1)
            .map_err(|e| pyo3::exceptions::PyValueError::new_err(format!("Could not import argument 1: {}", e)))?;
        let inputs = vec![input0, input1];

    // Execute the command.
    let result = command.execute_with_inputs(inputs)
        .map_err(|e| pyo3::exceptions::PyException::new_err(format!("Command error: {}", e)))?
        .export_to_pm4py(py)
        .map_err(|e| pyo3::exceptions::PyException::new_err(format!("Export error: {}", e)))?;

    Ok(result)
}

#[pyfunction]
fn association_trace_attribute(py: Python<'_>, arg0: &PyAny, arg1: &PyAny, arg2: &PyAny) -> PyResult<PyObject> {
    let command: &&EbiCommand = &&ASSOCIATION_ATTRIBUTE;
    let input_types = match **command {
        EbiCommand::Command { input_types, .. } => input_types,
        _ => return Err(pyo3::exceptions::PyValueError::new_err("Expected a command.")),
    };
    let input0 = import_or_load(arg0, input_types[0], 0)
            .map_err(|e| pyo3::exceptions::PyValueError::new_err(format!("Could not import argument 0: {}", e)))?;
        let input1 = import_or_load(arg1, input_types[1], 1)
            .map_err(|e| pyo3::exceptions::PyValueError::new_err(format!("Could not import argument 1: {}", e)))?;
        let input2 = import_or_load(arg2, input_types[2], 2)
            .map_err(|e| pyo3::exceptions::PyValueError::new_err(format!("Could not import argument 2: {}", e)))?;
        let inputs = vec![input0, input1, input2];

    // Execute the command.
    let result = command.execute_with_inputs(inputs)
        .map_err(|e| pyo3::exceptions::PyException::new_err(format!("Command error: {}", e)))?
        .export_to_pm4py(py)
        .map_err(|e| pyo3::exceptions::PyException::new_err(format!("Export error: {}", e)))?;

    Ok(result)
}

#[pyfunction]
fn conformance_chi_square_stochastic_conformance(py: Python<'_>, arg0: &PyAny, arg1: &PyAny) -> PyResult<PyObject> {
    let command: &&EbiCommand = &&CONFORMANCE_CSSC;
    let input_types = match **command {
        EbiCommand::Command { input_types, .. } => input_types,
        _ => return Err(pyo3::exceptions::PyValueError::new_err("Expected a command.")),
    };
    let input0 = import_or_load(arg0, input_types[0], 0)
            .map_err(|e| pyo3::exceptions::PyValueError::new_err(format!("Could not import argument 0: {}", e)))?;
        let input1 = import_or_load(arg1, input_types[1], 1)
            .map_err(|e| pyo3::exceptions::PyValueError::new_err(format!("Could not import argument 1: {}", e)))?;
        let inputs = vec![input0, input1];

    // Execute the command.
    let result = command.execute_with_inputs(inputs)
        .map_err(|e| pyo3::exceptions::PyException::new_err(format!("Command error: {}", e)))?
        .export_to_pm4py(py)
        .map_err(|e| pyo3::exceptions::PyException::new_err(format!("Export error: {}", e)))?;

    Ok(result)
}

#[pyfunction]
fn conformance_earth_movers_stochastic_conformance(py: Python<'_>, arg0: &PyAny, arg1: &PyAny) -> PyResult<PyObject> {
    let command: &&EbiCommand = &&CONFORMANCE_EMSC;
    let input_types = match **command {
        EbiCommand::Command { input_types, .. } => input_types,
        _ => return Err(pyo3::exceptions::PyValueError::new_err("Expected a command.")),
    };
    let input0 = import_or_load(arg0, input_types[0], 0)
            .map_err(|e| pyo3::exceptions::PyValueError::new_err(format!("Could not import argument 0: {}", e)))?;
        let input1 = import_or_load(arg1, input_types[1], 1)
            .map_err(|e| pyo3::exceptions::PyValueError::new_err(format!("Could not import argument 1: {}", e)))?;
        let inputs = vec![input0, input1];

    // Execute the command.
    let result = command.execute_with_inputs(inputs)
        .map_err(|e| pyo3::exceptions::PyException::new_err(format!("Command error: {}", e)))?
        .export_to_pm4py(py)
        .map_err(|e| pyo3::exceptions::PyException::new_err(format!("Export error: {}", e)))?;

    Ok(result)
}

#[pyfunction]
fn conformance_earth_movers_stochastic_conformance_sample(py: Python<'_>, arg0: &PyAny, arg1: &PyAny, arg2: &PyAny) -> PyResult<PyObject> {
    let command: &&EbiCommand = &&CONFORMANCE_EMSC_SAMPLE;
    let input_types = match **command {
        EbiCommand::Command { input_types, .. } => input_types,
        _ => return Err(pyo3::exceptions::PyValueError::new_err("Expected a command.")),
    };
    let input0 = import_or_load(arg0, input_types[0], 0)
            .map_err(|e| pyo3::exceptions::PyValueError::new_err(format!("Could not import argument 0: {}", e)))?;
        let input1 = import_or_load(arg1, input_types[1], 1)
            .map_err(|e| pyo3::exceptions::PyValueError::new_err(format!("Could not import argument 1: {}", e)))?;
        let input2 = import_or_load(arg2, input_types[2], 2)
            .map_err(|e| pyo3::exceptions::PyValueError::new_err(format!("Could not import argument 2: {}", e)))?;
        let inputs = vec![input0, input1, input2];

    // Execute the command.
    let result = command.execute_with_inputs(inputs)
        .map_err(|e| pyo3::exceptions::PyException::new_err(format!("Command error: {}", e)))?
        .export_to_pm4py(py)
        .map_err(|e| pyo3::exceptions::PyException::new_err(format!("Export error: {}", e)))?;

    Ok(result)
}

#[pyfunction]
fn conformance_entropic_relevance(py: Python<'_>, arg0: &PyAny, arg1: &PyAny) -> PyResult<PyObject> {
    let command: &&EbiCommand = &&CONFORMANCE_ER;
    let input_types = match **command {
        EbiCommand::Command { input_types, .. } => input_types,
        _ => return Err(pyo3::exceptions::PyValueError::new_err("Expected a command.")),
    };
    let input0 = import_or_load(arg0, input_types[0], 0)
            .map_err(|e| pyo3::exceptions::PyValueError::new_err(format!("Could not import argument 0: {}", e)))?;
        let input1 = import_or_load(arg1, input_types[1], 1)
            .map_err(|e| pyo3::exceptions::PyValueError::new_err(format!("Could not import argument 1: {}", e)))?;
        let inputs = vec![input0, input1];

    // Execute the command.
    let result = command.execute_with_inputs(inputs)
        .map_err(|e| pyo3::exceptions::PyException::new_err(format!("Command error: {}", e)))?
        .export_to_pm4py(py)
        .map_err(|e| pyo3::exceptions::PyException::new_err(format!("Export error: {}", e)))?;

    Ok(result)
}

#[pyfunction]
fn conformance_hellinger_stochastic_conformance(py: Python<'_>, arg0: &PyAny, arg1: &PyAny) -> PyResult<PyObject> {
    let command: &&EbiCommand = &&CONFORMANCE_HSC;
    let input_types = match **command {
        EbiCommand::Command { input_types, .. } => input_types,
        _ => return Err(pyo3::exceptions::PyValueError::new_err("Expected a command.")),
    };
    let input0 = import_or_load(arg0, input_types[0], 0)
            .map_err(|e| pyo3::exceptions::PyValueError::new_err(format!("Could not import argument 0: {}", e)))?;
        let input1 = import_or_load(arg1, input_types[1], 1)
            .map_err(|e| pyo3::exceptions::PyValueError::new_err(format!("Could not import argument 1: {}", e)))?;
        let inputs = vec![input0, input1];

    // Execute the command.
    let result = command.execute_with_inputs(inputs)
        .map_err(|e| pyo3::exceptions::PyException::new_err(format!("Command error: {}", e)))?
        .export_to_pm4py(py)
        .map_err(|e| pyo3::exceptions::PyException::new_err(format!("Export error: {}", e)))?;

    Ok(result)
}

#[pyfunction]
fn conformance_jensen_shannon(py: Python<'_>, arg0: &PyAny, arg1: &PyAny) -> PyResult<PyObject> {
    let command: &&EbiCommand = &&CONFORMANCE_JSSC;
    let input_types = match **command {
        EbiCommand::Command { input_types, .. } => input_types,
        _ => return Err(pyo3::exceptions::PyValueError::new_err("Expected a command.")),
    };
    let input0 = import_or_load(arg0, input_types[0], 0)
            .map_err(|e| pyo3::exceptions::PyValueError::new_err(format!("Could not import argument 0: {}", e)))?;
        let input1 = import_or_load(arg1, input_types[1], 1)
            .map_err(|e| pyo3::exceptions::PyValueError::new_err(format!("Could not import argument 1: {}", e)))?;
        let inputs = vec![input0, input1];

    // Execute the command.
    let result = command.execute_with_inputs(inputs)
        .map_err(|e| pyo3::exceptions::PyException::new_err(format!("Command error: {}", e)))?
        .export_to_pm4py(py)
        .map_err(|e| pyo3::exceptions::PyException::new_err(format!("Export error: {}", e)))?;

    Ok(result)
}

#[pyfunction]
fn conformance_jensen_shannon_sample(py: Python<'_>, arg0: &PyAny, arg1: &PyAny, arg2: &PyAny) -> PyResult<PyObject> {
    let command: &&EbiCommand = &&CONFORMANCE_JSSC_SAMPLE;
    let input_types = match **command {
        EbiCommand::Command { input_types, .. } => input_types,
        _ => return Err(pyo3::exceptions::PyValueError::new_err("Expected a command.")),
    };
    let input0 = import_or_load(arg0, input_types[0], 0)
            .map_err(|e| pyo3::exceptions::PyValueError::new_err(format!("Could not import argument 0: {}", e)))?;
        let input1 = import_or_load(arg1, input_types[1], 1)
            .map_err(|e| pyo3::exceptions::PyValueError::new_err(format!("Could not import argument 1: {}", e)))?;
        let input2 = import_or_load(arg2, input_types[2], 2)
            .map_err(|e| pyo3::exceptions::PyValueError::new_err(format!("Could not import argument 2: {}", e)))?;
        let inputs = vec![input0, input1, input2];

    // Execute the command.
    let result = command.execute_with_inputs(inputs)
        .map_err(|e| pyo3::exceptions::PyException::new_err(format!("Command error: {}", e)))?
        .export_to_pm4py(py)
        .map_err(|e| pyo3::exceptions::PyException::new_err(format!("Export error: {}", e)))?;

    Ok(result)
}

#[pyfunction]
fn conformance_unit_earth_movers_stochastic_conformance(py: Python<'_>, arg0: &PyAny, arg1: &PyAny) -> PyResult<PyObject> {
    let command: &&EbiCommand = &&CONFORMANCE_UEMSC;
    let input_types = match **command {
        EbiCommand::Command { input_types, .. } => input_types,
        _ => return Err(pyo3::exceptions::PyValueError::new_err("Expected a command.")),
    };
    let input0 = import_or_load(arg0, input_types[0], 0)
            .map_err(|e| pyo3::exceptions::PyValueError::new_err(format!("Could not import argument 0: {}", e)))?;
        let input1 = import_or_load(arg1, input_types[1], 1)
            .map_err(|e| pyo3::exceptions::PyValueError::new_err(format!("Could not import argument 1: {}", e)))?;
        let inputs = vec![input0, input1];

    // Execute the command.
    let result = command.execute_with_inputs(inputs)
        .map_err(|e| pyo3::exceptions::PyException::new_err(format!("Command error: {}", e)))?
        .export_to_pm4py(py)
        .map_err(|e| pyo3::exceptions::PyException::new_err(format!("Export error: {}", e)))?;

    Ok(result)
}

#[pyfunction]
fn conformance_non_stochastic_alignments(py: Python<'_>, arg0: &PyAny, arg1: &PyAny) -> PyResult<PyObject> {
    let command: &&EbiCommand = &&EBI_CONFORMANCE_NON_STOCHASTIC_ALIGNMENTS;
    let input_types = match **command {
        EbiCommand::Command { input_types, .. } => input_types,
        _ => return Err(pyo3::exceptions::PyValueError::new_err("Expected a command.")),
    };
    let input0 = import_or_load(arg0, input_types[0], 0)
            .map_err(|e| pyo3::exceptions::PyValueError::new_err(format!("Could not import argument 0: {}", e)))?;
        let input1 = import_or_load(arg1, input_types[1], 1)
            .map_err(|e| pyo3::exceptions::PyValueError::new_err(format!("Could not import argument 1: {}", e)))?;
        let inputs = vec![input0, input1];

    // Execute the command.
    let result = command.execute_with_inputs(inputs)
        .map_err(|e| pyo3::exceptions::PyException::new_err(format!("Command error: {}", e)))?
        .export_to_pm4py(py)
        .map_err(|e| pyo3::exceptions::PyException::new_err(format!("Export error: {}", e)))?;

    Ok(result)
}

#[pyfunction]
fn conformance_non_stochastic_escaping_edges_precision(py: Python<'_>, arg0: &PyAny, arg1: &PyAny) -> PyResult<PyObject> {
    let command: &&EbiCommand = &&EBI_CONFORMANCE_NON_STOCHASTIC_ESCAPING_EDGES_PRECISION;
    let input_types = match **command {
        EbiCommand::Command { input_types, .. } => input_types,
        _ => return Err(pyo3::exceptions::PyValueError::new_err("Expected a command.")),
    };
    let input0 = import_or_load(arg0, input_types[0], 0)
            .map_err(|e| pyo3::exceptions::PyValueError::new_err(format!("Could not import argument 0: {}", e)))?;
        let input1 = import_or_load(arg1, input_types[1], 1)
            .map_err(|e| pyo3::exceptions::PyValueError::new_err(format!("Could not import argument 1: {}", e)))?;
        let inputs = vec![input0, input1];

    // Execute the command.
    let result = command.execute_with_inputs(inputs)
        .map_err(|e| pyo3::exceptions::PyException::new_err(format!("Command error: {}", e)))?
        .export_to_pm4py(py)
        .map_err(|e| pyo3::exceptions::PyException::new_err(format!("Export error: {}", e)))?;

    Ok(result)
}

#[pyfunction]
fn conformance_non_stochastic_set_alignments(py: Python<'_>, arg0: &PyAny, arg1: &PyAny) -> PyResult<PyObject> {
    let command: &&EbiCommand = &&EBI_CONFORMANCE_NON_STOCHASTIC_SET_ALIGNMENTS;
    let input_types = match **command {
        EbiCommand::Command { input_types, .. } => input_types,
        _ => return Err(pyo3::exceptions::PyValueError::new_err("Expected a command.")),
    };
    let input0 = import_or_load(arg0, input_types[0], 0)
            .map_err(|e| pyo3::exceptions::PyValueError::new_err(format!("Could not import argument 0: {}", e)))?;
        let input1 = import_or_load(arg1, input_types[1], 1)
            .map_err(|e| pyo3::exceptions::PyValueError::new_err(format!("Could not import argument 1: {}", e)))?;
        let inputs = vec![input0, input1];

    // Execute the command.
    let result = command.execute_with_inputs(inputs)
        .map_err(|e| pyo3::exceptions::PyException::new_err(format!("Command error: {}", e)))?
        .export_to_pm4py(py)
        .map_err(|e| pyo3::exceptions::PyException::new_err(format!("Export error: {}", e)))?;

    Ok(result)
}

#[pyfunction]
fn conformance_non_stochastic_trace_fitness(py: Python<'_>, arg0: &PyAny) -> PyResult<PyObject> {
    let command: &&EbiCommand = &&EBI_CONFORMANCE_NON_STOCHASTIC_TRACE_FITNESS;
    let input_types = match **command {
        EbiCommand::Command { input_types, .. } => input_types,
        _ => return Err(pyo3::exceptions::PyValueError::new_err("Expected a command.")),
    };
    let input0 = import_or_load(arg0, input_types[0], 0)
            .map_err(|e| pyo3::exceptions::PyValueError::new_err(format!("Could not import argument 0: {}", e)))?;
        let inputs = vec![input0];

    // Execute the command.
    let result = command.execute_with_inputs(inputs)
        .map_err(|e| pyo3::exceptions::PyException::new_err(format!("Command error: {}", e)))?
        .export_to_pm4py(py)
        .map_err(|e| pyo3::exceptions::PyException::new_err(format!("Export error: {}", e)))?;

    Ok(result)
}

#[pyfunction]
fn convert_finite_stochastic_language(py: Python<'_>, arg0: &PyAny) -> PyResult<PyObject> {
    let command: &&EbiCommand = &&EBI_CONVERT_SLANG;
    let input_types = match **command {
        EbiCommand::Command { input_types, .. } => input_types,
        _ => return Err(pyo3::exceptions::PyValueError::new_err("Expected a command.")),
    };
    let input0 = import_or_load(arg0, input_types[0], 0)
            .map_err(|e| pyo3::exceptions::PyValueError::new_err(format!("Could not import argument 0: {}", e)))?;
        let inputs = vec![input0];

    // Execute the command.
    let result = command.execute_with_inputs(inputs)
        .map_err(|e| pyo3::exceptions::PyException::new_err(format!("Command error: {}", e)))?
        .export_to_pm4py(py)
        .map_err(|e| pyo3::exceptions::PyException::new_err(format!("Export error: {}", e)))?;

    Ok(result)
}

#[pyfunction]
fn convert_labelled_petri_net(py: Python<'_>, arg0: &PyAny) -> PyResult<PyObject> {
    let command: &&EbiCommand = &&EBI_CONVERT_LPN;
    let input_types = match **command {
        EbiCommand::Command { input_types, .. } => input_types,
        _ => return Err(pyo3::exceptions::PyValueError::new_err("Expected a command.")),
    };
    let input0 = import_or_load(arg0, input_types[0], 0)
            .map_err(|e| pyo3::exceptions::PyValueError::new_err(format!("Could not import argument 0: {}", e)))?;
        let inputs = vec![input0];

    // Execute the command.
    let result = command.execute_with_inputs(inputs)
        .map_err(|e| pyo3::exceptions::PyException::new_err(format!("Command error: {}", e)))?
        .export_to_pm4py(py)
        .map_err(|e| pyo3::exceptions::PyException::new_err(format!("Export error: {}", e)))?;

    Ok(result)
}

#[pyfunction]
fn convert_stochastic_finite_deterministic_automaton(py: Python<'_>, arg0: &PyAny) -> PyResult<PyObject> {
    let command: &&EbiCommand = &&EBI_CONVERT_SDFA;
    let input_types = match **command {
        EbiCommand::Command { input_types, .. } => input_types,
        _ => return Err(pyo3::exceptions::PyValueError::new_err("Expected a command.")),
    };
    let input0 = import_or_load(arg0, input_types[0], 0)
            .map_err(|e| pyo3::exceptions::PyValueError::new_err(format!("Could not import argument 0: {}", e)))?;
        let inputs = vec![input0];

    // Execute the command.
    let result = command.execute_with_inputs(inputs)
        .map_err(|e| pyo3::exceptions::PyException::new_err(format!("Command error: {}", e)))?
        .export_to_pm4py(py)
        .map_err(|e| pyo3::exceptions::PyException::new_err(format!("Export error: {}", e)))?;

    Ok(result)
}

#[pyfunction]
fn discover_alignments(py: Python<'_>, arg0: &PyAny, arg1: &PyAny) -> PyResult<PyObject> {
    let command: &&EbiCommand = &&EBI_DISCOVER_ALIGNMENTS;
    let input_types = match **command {
        EbiCommand::Command { input_types, .. } => input_types,
        _ => return Err(pyo3::exceptions::PyValueError::new_err("Expected a command.")),
    };
    let input0 = import_or_load(arg0, input_types[0], 0)
            .map_err(|e| pyo3::exceptions::PyValueError::new_err(format!("Could not import argument 0: {}", e)))?;
        let input1 = import_or_load(arg1, input_types[1], 1)
            .map_err(|e| pyo3::exceptions::PyValueError::new_err(format!("Could not import argument 1: {}", e)))?;
        let inputs = vec![input0, input1];

    // Execute the command.
    let result = command.execute_with_inputs(inputs)
        .map_err(|e| pyo3::exceptions::PyException::new_err(format!("Command error: {}", e)))?
        .export_to_pm4py(py)
        .map_err(|e| pyo3::exceptions::PyException::new_err(format!("Export error: {}", e)))?;

    Ok(result)
}

#[pyfunction]
fn discover_directly_follows_graph(py: Python<'_>, arg0: &PyAny, arg1: &PyAny) -> PyResult<PyObject> {
    let command: &&EbiCommand = &&EBI_DISCOVER_DIRECTLY_FOLLOWS;
    let input_types = match **command {
        EbiCommand::Command { input_types, .. } => input_types,
        _ => return Err(pyo3::exceptions::PyValueError::new_err("Expected a command.")),
    };
    let input0 = import_or_load(arg0, input_types[0], 0)
            .map_err(|e| pyo3::exceptions::PyValueError::new_err(format!("Could not import argument 0: {}", e)))?;
        let input1 = import_or_load(arg1, input_types[1], 1)
            .map_err(|e| pyo3::exceptions::PyValueError::new_err(format!("Could not import argument 1: {}", e)))?;
        let inputs = vec![input0, input1];

    // Execute the command.
    let result = command.execute_with_inputs(inputs)
        .map_err(|e| pyo3::exceptions::PyException::new_err(format!("Command error: {}", e)))?
        .export_to_pm4py(py)
        .map_err(|e| pyo3::exceptions::PyException::new_err(format!("Export error: {}", e)))?;

    Ok(result)
}

#[pyfunction]
fn discover_occurrence(py: Python<'_>, arg0: &PyAny, arg1: &PyAny) -> PyResult<PyObject> {
    let command: &&EbiCommand = &&EBI_DISCOVER_OCCURRENCE;
    let input_types = match **command {
        EbiCommand::Command { input_types, .. } => input_types,
        _ => return Err(pyo3::exceptions::PyValueError::new_err("Expected a command.")),
    };
    let input0 = import_or_load(arg0, input_types[0], 0)
            .map_err(|e| pyo3::exceptions::PyValueError::new_err(format!("Could not import argument 0: {}", e)))?;
        let input1 = import_or_load(arg1, input_types[1], 1)
            .map_err(|e| pyo3::exceptions::PyValueError::new_err(format!("Could not import argument 1: {}", e)))?;
        let inputs = vec![input0, input1];

    // Execute the command.
    let result = command.execute_with_inputs(inputs)
        .map_err(|e| pyo3::exceptions::PyException::new_err(format!("Command error: {}", e)))?
        .export_to_pm4py(py)
        .map_err(|e| pyo3::exceptions::PyException::new_err(format!("Export error: {}", e)))?;

    Ok(result)
}

#[pyfunction]
fn discover_uniform(py: Python<'_>, arg0: &PyAny) -> PyResult<PyObject> {
    let command: &&EbiCommand = &&EBI_DISCOVER_UNIFORM;
    let input_types = match **command {
        EbiCommand::Command { input_types, .. } => input_types,
        _ => return Err(pyo3::exceptions::PyValueError::new_err("Expected a command.")),
    };
    let input0 = import_or_load(arg0, input_types[0], 0)
            .map_err(|e| pyo3::exceptions::PyValueError::new_err(format!("Could not import argument 0: {}", e)))?;
        let inputs = vec![input0];

    // Execute the command.
    let result = command.execute_with_inputs(inputs)
        .map_err(|e| pyo3::exceptions::PyException::new_err(format!("Command error: {}", e)))?
        .export_to_pm4py(py)
        .map_err(|e| pyo3::exceptions::PyException::new_err(format!("Export error: {}", e)))?;

    Ok(result)
}

#[pyfunction]
fn discover_non_stochastic_flower_deterministic_finite_automaton(py: Python<'_>, arg0: &PyAny) -> PyResult<PyObject> {
    let command: &&EbiCommand = &&EBI_DISCOVER_NON_STOCHASTIC_FLOWER_DFA;
    let input_types = match **command {
        EbiCommand::Command { input_types, .. } => input_types,
        _ => return Err(pyo3::exceptions::PyValueError::new_err("Expected a command.")),
    };
    let input0 = import_or_load(arg0, input_types[0], 0)
            .map_err(|e| pyo3::exceptions::PyValueError::new_err(format!("Could not import argument 0: {}", e)))?;
        let inputs = vec![input0];

    // Execute the command.
    let result = command.execute_with_inputs(inputs)
        .map_err(|e| pyo3::exceptions::PyException::new_err(format!("Command error: {}", e)))?
        .export_to_pm4py(py)
        .map_err(|e| pyo3::exceptions::PyException::new_err(format!("Export error: {}", e)))?;

    Ok(result)
}

#[pyfunction]
fn discover_non_stochastic_flower_process_tree(py: Python<'_>, arg0: &PyAny) -> PyResult<PyObject> {
    let command: &&EbiCommand = &&EBI_DISCOVER_NON_STOCHASTIC_FLOWER_TREE;
    let input_types = match **command {
        EbiCommand::Command { input_types, .. } => input_types,
        _ => return Err(pyo3::exceptions::PyValueError::new_err("Expected a command.")),
    };
    let input0 = import_or_load(arg0, input_types[0], 0)
            .map_err(|e| pyo3::exceptions::PyValueError::new_err(format!("Could not import argument 0: {}", e)))?;
        let inputs = vec![input0];

    // Execute the command.
    let result = command.execute_with_inputs(inputs)
        .map_err(|e| pyo3::exceptions::PyException::new_err(format!("Command error: {}", e)))?
        .export_to_pm4py(py)
        .map_err(|e| pyo3::exceptions::PyException::new_err(format!("Export error: {}", e)))?;

    Ok(result)
}

#[pyfunction]
fn discover_non_stochastic_prefix_tree_deterministic_finite_automaton(py: Python<'_>, arg0: &PyAny) -> PyResult<PyObject> {
    let command: &&EbiCommand = &&EBI_DISCOVER_NON_STOCHASTIC_TREE_DFA;
    let input_types = match **command {
        EbiCommand::Command { input_types, .. } => input_types,
        _ => return Err(pyo3::exceptions::PyValueError::new_err("Expected a command.")),
    };
    let input0 = import_or_load(arg0, input_types[0], 0)
            .map_err(|e| pyo3::exceptions::PyValueError::new_err(format!("Could not import argument 0: {}", e)))?;
        let inputs = vec![input0];

    // Execute the command.
    let result = command.execute_with_inputs(inputs)
        .map_err(|e| pyo3::exceptions::PyException::new_err(format!("Command error: {}", e)))?
        .export_to_pm4py(py)
        .map_err(|e| pyo3::exceptions::PyException::new_err(format!("Export error: {}", e)))?;

    Ok(result)
}

#[pyfunction]
fn discover_non_stochastic_prefix_tree_process_tree(py: Python<'_>, arg0: &PyAny) -> PyResult<PyObject> {
    let command: &&EbiCommand = &&EBI_DISCOVER_NON_STOCHASTIC_TREE_TREE;
    let input_types = match **command {
        EbiCommand::Command { input_types, .. } => input_types,
        _ => return Err(pyo3::exceptions::PyValueError::new_err("Expected a command.")),
    };
    let input0 = import_or_load(arg0, input_types[0], 0)
            .map_err(|e| pyo3::exceptions::PyValueError::new_err(format!("Could not import argument 0: {}", e)))?;
        let inputs = vec![input0];

    // Execute the command.
    let result = command.execute_with_inputs(inputs)
        .map_err(|e| pyo3::exceptions::PyException::new_err(format!("Command error: {}", e)))?
        .export_to_pm4py(py)
        .map_err(|e| pyo3::exceptions::PyException::new_err(format!("Export error: {}", e)))?;

    Ok(result)
}

#[pyfunction]
fn information(py: Python<'_>, arg0: &PyAny) -> PyResult<PyObject> {
    let command: &&EbiCommand = &&EBI_INFO;
    let input_types = match **command {
        EbiCommand::Command { input_types, .. } => input_types,
        _ => return Err(pyo3::exceptions::PyValueError::new_err("Expected a command.")),
    };
    let input0 = import_or_load(arg0, input_types[0], 0)
            .map_err(|e| pyo3::exceptions::PyValueError::new_err(format!("Could not import argument 0: {}", e)))?;
        let inputs = vec![input0];

    // Execute the command.
    let result = command.execute_with_inputs(inputs)
        .map_err(|e| pyo3::exceptions::PyException::new_err(format!("Command error: {}", e)))?
        .export_to_pm4py(py)
        .map_err(|e| pyo3::exceptions::PyException::new_err(format!("Export error: {}", e)))?;

    Ok(result)
}

#[pyfunction]
fn itself_graph(py: Python<'_>, ) -> PyResult<PyObject> {
    let command: &&EbiCommand = &&EBI_ITSELF_GRAPH;
    let input_types = match **command {
        EbiCommand::Command { input_types, .. } => input_types,
        _ => return Err(pyo3::exceptions::PyValueError::new_err("Expected a command.")),
    };
    let inputs = vec![];

    // Execute the command.
    let result = command.execute_with_inputs(inputs)
        .map_err(|e| pyo3::exceptions::PyException::new_err(format!("Command error: {}", e)))?
        .export_to_pm4py(py)
        .map_err(|e| pyo3::exceptions::PyException::new_err(format!("Export error: {}", e)))?;

    Ok(result)
}

#[pyfunction]
fn itself_html(py: Python<'_>, ) -> PyResult<PyObject> {
    let command: &&EbiCommand = &&EBI_ITSELF_HTML;
    let input_types = match **command {
        EbiCommand::Command { input_types, .. } => input_types,
        _ => return Err(pyo3::exceptions::PyValueError::new_err("Expected a command.")),
    };
    let inputs = vec![];

    // Execute the command.
    let result = command.execute_with_inputs(inputs)
        .map_err(|e| pyo3::exceptions::PyException::new_err(format!("Command error: {}", e)))?
        .export_to_pm4py(py)
        .map_err(|e| pyo3::exceptions::PyException::new_err(format!("Export error: {}", e)))?;

    Ok(result)
}

#[pyfunction]
fn itself_java(py: Python<'_>, ) -> PyResult<PyObject> {
    let command: &&EbiCommand = &&EBI_ITSELF_JAVA;
    let input_types = match **command {
        EbiCommand::Command { input_types, .. } => input_types,
        _ => return Err(pyo3::exceptions::PyValueError::new_err("Expected a command.")),
    };
    let inputs = vec![];

    // Execute the command.
    let result = command.execute_with_inputs(inputs)
        .map_err(|e| pyo3::exceptions::PyException::new_err(format!("Command error: {}", e)))?
        .export_to_pm4py(py)
        .map_err(|e| pyo3::exceptions::PyException::new_err(format!("Export error: {}", e)))?;

    Ok(result)
}

#[pyfunction]
fn itself_logo(py: Python<'_>, ) -> PyResult<PyObject> {
    let command: &&EbiCommand = &&EBI_ITSELF_LOGO;
    let input_types = match **command {
        EbiCommand::Command { input_types, .. } => input_types,
        _ => return Err(pyo3::exceptions::PyValueError::new_err("Expected a command.")),
    };
    let inputs = vec![];

    // Execute the command.
    let result = command.execute_with_inputs(inputs)
        .map_err(|e| pyo3::exceptions::PyException::new_err(format!("Command error: {}", e)))?
        .export_to_pm4py(py)
        .map_err(|e| pyo3::exceptions::PyException::new_err(format!("Export error: {}", e)))?;

    Ok(result)
}

#[pyfunction]
fn itself_manual(py: Python<'_>, ) -> PyResult<PyObject> {
    let command: &&EbiCommand = &&EBI_ITSELF_MANUAL;
    let input_types = match **command {
        EbiCommand::Command { input_types, .. } => input_types,
        _ => return Err(pyo3::exceptions::PyValueError::new_err("Expected a command.")),
    };
    let inputs = vec![];

    // Execute the command.
    let result = command.execute_with_inputs(inputs)
        .map_err(|e| pyo3::exceptions::PyException::new_err(format!("Command error: {}", e)))?
        .export_to_pm4py(py)
        .map_err(|e| pyo3::exceptions::PyException::new_err(format!("Export error: {}", e)))?;

    Ok(result)
}

#[pyfunction]
fn itself_pm4py(py: Python<'_>, ) -> PyResult<PyObject> {
    let command: &&EbiCommand = &&EBI_ITSELF_PM4PY;
    let input_types = match **command {
        EbiCommand::Command { input_types, .. } => input_types,
        _ => return Err(pyo3::exceptions::PyValueError::new_err("Expected a command.")),
    };
    let inputs = vec![];

    // Execute the command.
    let result = command.execute_with_inputs(inputs)
        .map_err(|e| pyo3::exceptions::PyException::new_err(format!("Command error: {}", e)))?
        .export_to_pm4py(py)
        .map_err(|e| pyo3::exceptions::PyException::new_err(format!("Export error: {}", e)))?;

    Ok(result)
}

#[pyfunction]
fn probability_explain_trace(py: Python<'_>, arg0: &PyAny, arg1: &PyAny) -> PyResult<PyObject> {
    let command: &&EbiCommand = &&EBI_PROBABILITY_EXPLAIN_TRACE;
    let input_types = match **command {
        EbiCommand::Command { input_types, .. } => input_types,
        _ => return Err(pyo3::exceptions::PyValueError::new_err("Expected a command.")),
    };
    let input0 = import_or_load(arg0, input_types[0], 0)
            .map_err(|e| pyo3::exceptions::PyValueError::new_err(format!("Could not import argument 0: {}", e)))?;
        let input1 = import_or_load(arg1, input_types[1], 1)
            .map_err(|e| pyo3::exceptions::PyValueError::new_err(format!("Could not import argument 1: {}", e)))?;
        let inputs = vec![input0, input1];

    // Execute the command.
    let result = command.execute_with_inputs(inputs)
        .map_err(|e| pyo3::exceptions::PyException::new_err(format!("Command error: {}", e)))?
        .export_to_pm4py(py)
        .map_err(|e| pyo3::exceptions::PyException::new_err(format!("Export error: {}", e)))?;

    Ok(result)
}

#[pyfunction]
fn probability_log(py: Python<'_>, arg0: &PyAny, arg1: &PyAny) -> PyResult<PyObject> {
    let command: &&EbiCommand = &&EBI_PROBABILITY_LOG;
    let input_types = match **command {
        EbiCommand::Command { input_types, .. } => input_types,
        _ => return Err(pyo3::exceptions::PyValueError::new_err("Expected a command.")),
    };
    let input0 = import_or_load(arg0, input_types[0], 0)
            .map_err(|e| pyo3::exceptions::PyValueError::new_err(format!("Could not import argument 0: {}", e)))?;
        let input1 = import_or_load(arg1, input_types[1], 1)
            .map_err(|e| pyo3::exceptions::PyValueError::new_err(format!("Could not import argument 1: {}", e)))?;
        let inputs = vec![input0, input1];

    // Execute the command.
    let result = command.execute_with_inputs(inputs)
        .map_err(|e| pyo3::exceptions::PyException::new_err(format!("Command error: {}", e)))?
        .export_to_pm4py(py)
        .map_err(|e| pyo3::exceptions::PyException::new_err(format!("Export error: {}", e)))?;

    Ok(result)
}

#[pyfunction]
fn probability_trace(py: Python<'_>, arg0: &PyAny) -> PyResult<PyObject> {
    let command: &&EbiCommand = &&EBI_PROBABILITY_TRACE;
    let input_types = match **command {
        EbiCommand::Command { input_types, .. } => input_types,
        _ => return Err(pyo3::exceptions::PyValueError::new_err("Expected a command.")),
    };
    let input0 = import_or_load(arg0, input_types[0], 0)
            .map_err(|e| pyo3::exceptions::PyValueError::new_err(format!("Could not import argument 0: {}", e)))?;
        let inputs = vec![input0];

    // Execute the command.
    let result = command.execute_with_inputs(inputs)
        .map_err(|e| pyo3::exceptions::PyException::new_err(format!("Command error: {}", e)))?
        .export_to_pm4py(py)
        .map_err(|e| pyo3::exceptions::PyException::new_err(format!("Export error: {}", e)))?;

    Ok(result)
}

#[pyfunction]
fn sample_folds(py: Python<'_>, arg0: &PyAny, arg1: &PyAny, arg2: &PyAny, arg3: &PyAny) -> PyResult<PyObject> {
    let command: &&EbiCommand = &&EBI_SAMPLE_FOLDS;
    let input_types = match **command {
        EbiCommand::Command { input_types, .. } => input_types,
        _ => return Err(pyo3::exceptions::PyValueError::new_err("Expected a command.")),
    };
    let input0 = import_or_load(arg0, input_types[0], 0)
            .map_err(|e| pyo3::exceptions::PyValueError::new_err(format!("Could not import argument 0: {}", e)))?;
        let input1 = import_or_load(arg1, input_types[1], 1)
            .map_err(|e| pyo3::exceptions::PyValueError::new_err(format!("Could not import argument 1: {}", e)))?;
        let input2 = import_or_load(arg2, input_types[2], 2)
            .map_err(|e| pyo3::exceptions::PyValueError::new_err(format!("Could not import argument 2: {}", e)))?;
        let input3 = import_or_load(arg3, input_types[3], 3)
            .map_err(|e| pyo3::exceptions::PyValueError::new_err(format!("Could not import argument 3: {}", e)))?;
        let inputs = vec![input0, input1, input2, input3];

    // Execute the command.
    let result = command.execute_with_inputs(inputs)
        .map_err(|e| pyo3::exceptions::PyException::new_err(format!("Command error: {}", e)))?
        .export_to_pm4py(py)
        .map_err(|e| pyo3::exceptions::PyException::new_err(format!("Export error: {}", e)))?;

    Ok(result)
}

#[pyfunction]
fn sample_traces(py: Python<'_>, arg0: &PyAny, arg1: &PyAny) -> PyResult<PyObject> {
    let command: &&EbiCommand = &&EBI_SAMPLE_TRACES;
    let input_types = match **command {
        EbiCommand::Command { input_types, .. } => input_types,
        _ => return Err(pyo3::exceptions::PyValueError::new_err("Expected a command.")),
    };
    let input0 = import_or_load(arg0, input_types[0], 0)
            .map_err(|e| pyo3::exceptions::PyValueError::new_err(format!("Could not import argument 0: {}", e)))?;
        let input1 = import_or_load(arg1, input_types[1], 1)
            .map_err(|e| pyo3::exceptions::PyValueError::new_err(format!("Could not import argument 1: {}", e)))?;
        let inputs = vec![input0, input1];

    // Execute the command.
    let result = command.execute_with_inputs(inputs)
        .map_err(|e| pyo3::exceptions::PyException::new_err(format!("Command error: {}", e)))?
        .export_to_pm4py(py)
        .map_err(|e| pyo3::exceptions::PyException::new_err(format!("Export error: {}", e)))?;

    Ok(result)
}

#[pyfunction]
fn test_bootstrap_test(py: Python<'_>, arg0: &PyAny, arg1: &PyAny, arg2: &PyAny, arg3: &PyAny) -> PyResult<PyObject> {
    let command: &&EbiCommand = &&EBI_BOOTSTRAP_TEST;
    let input_types = match **command {
        EbiCommand::Command { input_types, .. } => input_types,
        _ => return Err(pyo3::exceptions::PyValueError::new_err("Expected a command.")),
    };
    let input0 = import_or_load(arg0, input_types[0], 0)
            .map_err(|e| pyo3::exceptions::PyValueError::new_err(format!("Could not import argument 0: {}", e)))?;
        let input1 = import_or_load(arg1, input_types[1], 1)
            .map_err(|e| pyo3::exceptions::PyValueError::new_err(format!("Could not import argument 1: {}", e)))?;
        let input2 = import_or_load(arg2, input_types[2], 2)
            .map_err(|e| pyo3::exceptions::PyValueError::new_err(format!("Could not import argument 2: {}", e)))?;
        let input3 = import_or_load(arg3, input_types[3], 3)
            .map_err(|e| pyo3::exceptions::PyValueError::new_err(format!("Could not import argument 3: {}", e)))?;
        let inputs = vec![input0, input1, input2, input3];

    // Execute the command.
    let result = command.execute_with_inputs(inputs)
        .map_err(|e| pyo3::exceptions::PyException::new_err(format!("Command error: {}", e)))?
        .export_to_pm4py(py)
        .map_err(|e| pyo3::exceptions::PyException::new_err(format!("Export error: {}", e)))?;

    Ok(result)
}

#[pyfunction]
fn test_log_categorical_attribute(py: Python<'_>, arg0: &PyAny, arg1: &PyAny, arg2: &PyAny, arg3: &PyAny) -> PyResult<PyObject> {
    let command: &&EbiCommand = &&EBI_TEST_LOG_ATTRIBUTE;
    let input_types = match **command {
        EbiCommand::Command { input_types, .. } => input_types,
        _ => return Err(pyo3::exceptions::PyValueError::new_err("Expected a command.")),
    };
    let input0 = import_or_load(arg0, input_types[0], 0)
            .map_err(|e| pyo3::exceptions::PyValueError::new_err(format!("Could not import argument 0: {}", e)))?;
        let input1 = import_or_load(arg1, input_types[1], 1)
            .map_err(|e| pyo3::exceptions::PyValueError::new_err(format!("Could not import argument 1: {}", e)))?;
        let input2 = import_or_load(arg2, input_types[2], 2)
            .map_err(|e| pyo3::exceptions::PyValueError::new_err(format!("Could not import argument 2: {}", e)))?;
        let input3 = import_or_load(arg3, input_types[3], 3)
            .map_err(|e| pyo3::exceptions::PyValueError::new_err(format!("Could not import argument 3: {}", e)))?;
        let inputs = vec![input0, input1, input2, input3];

    // Execute the command.
    let result = command.execute_with_inputs(inputs)
        .map_err(|e| pyo3::exceptions::PyException::new_err(format!("Command error: {}", e)))?
        .export_to_pm4py(py)
        .map_err(|e| pyo3::exceptions::PyException::new_err(format!("Export error: {}", e)))?;

    Ok(result)
}

#[pyfunction]
fn validate(py: Python<'_>, arg0: &PyAny) -> PyResult<PyObject> {
    let command: &&EbiCommand = &&EBI_VALIDATE;
    let input_types = match **command {
        EbiCommand::Command { input_types, .. } => input_types,
        _ => return Err(pyo3::exceptions::PyValueError::new_err("Expected a command.")),
    };
    let input0 = import_or_load(arg0, input_types[0], 0)
            .map_err(|e| pyo3::exceptions::PyValueError::new_err(format!("Could not import argument 0: {}", e)))?;
        let inputs = vec![input0];

    // Execute the command.
    let result = command.execute_with_inputs(inputs)
        .map_err(|e| pyo3::exceptions::PyException::new_err(format!("Command error: {}", e)))?
        .export_to_pm4py(py)
        .map_err(|e| pyo3::exceptions::PyException::new_err(format!("Export error: {}", e)))?;

    Ok(result)
}

#[pyfunction]
fn visualise_graph(py: Python<'_>, arg0: &PyAny) -> PyResult<PyObject> {
    let command: &&EbiCommand = &&EBI_VISUALISE_GRAPH;
    let input_types = match **command {
        EbiCommand::Command { input_types, .. } => input_types,
        _ => return Err(pyo3::exceptions::PyValueError::new_err("Expected a command.")),
    };
    let input0 = import_or_load(arg0, input_types[0], 0)
            .map_err(|e| pyo3::exceptions::PyValueError::new_err(format!("Could not import argument 0: {}", e)))?;
        let inputs = vec![input0];

    // Execute the command.
    let result = command.execute_with_inputs(inputs)
        .map_err(|e| pyo3::exceptions::PyException::new_err(format!("Command error: {}", e)))?
        .export_to_pm4py(py)
        .map_err(|e| pyo3::exceptions::PyException::new_err(format!("Export error: {}", e)))?;

    Ok(result)
}

#[pyfunction]
fn visualise_text(py: Python<'_>, arg0: &PyAny) -> PyResult<PyObject> {
    let command: &&EbiCommand = &&EBI_VISUALISE_TEXT;
    let input_types = match **command {
        EbiCommand::Command { input_types, .. } => input_types,
        _ => return Err(pyo3::exceptions::PyValueError::new_err("Expected a command.")),
    };
    let input0 = import_or_load(arg0, input_types[0], 0)
            .map_err(|e| pyo3::exceptions::PyValueError::new_err(format!("Could not import argument 0: {}", e)))?;
        let inputs = vec![input0];

    // Execute the command.
    let result = command.execute_with_inputs(inputs)
        .map_err(|e| pyo3::exceptions::PyException::new_err(format!("Command error: {}", e)))?
        .export_to_pm4py(py)
        .map_err(|e| pyo3::exceptions::PyException::new_err(format!("Export error: {}", e)))?;

    Ok(result)
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
    m.add_function(wrap_pyfunction!(analyse_non_stochastic_activities, m)?)?;
    m.add_function(wrap_pyfunction!(analyse_non_stochastic_any_traces, m)?)?;
    m.add_function(wrap_pyfunction!(analyse_non_stochastic_bounded, m)?)?;
    m.add_function(wrap_pyfunction!(analyse_non_stochastic_cluster, m)?)?;
    m.add_function(wrap_pyfunction!(analyse_non_stochastic_executions, m)?)?;
    m.add_function(wrap_pyfunction!(analyse_non_stochastic_infinitely_many_traces, m)?)?;
    m.add_function(wrap_pyfunction!(analyse_non_stochastic_medoid, m)?)?;
    m.add_function(wrap_pyfunction!(association_all_trace_attributes, m)?)?;
    m.add_function(wrap_pyfunction!(association_trace_attribute, m)?)?;
    m.add_function(wrap_pyfunction!(conformance_chi_square_stochastic_conformance, m)?)?;
    m.add_function(wrap_pyfunction!(conformance_earth_movers_stochastic_conformance, m)?)?;
    m.add_function(wrap_pyfunction!(conformance_earth_movers_stochastic_conformance_sample, m)?)?;
    m.add_function(wrap_pyfunction!(conformance_entropic_relevance, m)?)?;
    m.add_function(wrap_pyfunction!(conformance_hellinger_stochastic_conformance, m)?)?;
    m.add_function(wrap_pyfunction!(conformance_jensen_shannon, m)?)?;
    m.add_function(wrap_pyfunction!(conformance_jensen_shannon_sample, m)?)?;
    m.add_function(wrap_pyfunction!(conformance_unit_earth_movers_stochastic_conformance, m)?)?;
    m.add_function(wrap_pyfunction!(conformance_non_stochastic_alignments, m)?)?;
    m.add_function(wrap_pyfunction!(conformance_non_stochastic_escaping_edges_precision, m)?)?;
    m.add_function(wrap_pyfunction!(conformance_non_stochastic_set_alignments, m)?)?;
    m.add_function(wrap_pyfunction!(conformance_non_stochastic_trace_fitness, m)?)?;
    m.add_function(wrap_pyfunction!(convert_finite_stochastic_language, m)?)?;
    m.add_function(wrap_pyfunction!(convert_labelled_petri_net, m)?)?;
    m.add_function(wrap_pyfunction!(convert_stochastic_finite_deterministic_automaton, m)?)?;
    m.add_function(wrap_pyfunction!(discover_alignments, m)?)?;
    m.add_function(wrap_pyfunction!(discover_directly_follows_graph, m)?)?;
    m.add_function(wrap_pyfunction!(discover_occurrence, m)?)?;
    m.add_function(wrap_pyfunction!(discover_uniform, m)?)?;
    m.add_function(wrap_pyfunction!(discover_non_stochastic_flower_deterministic_finite_automaton, m)?)?;
    m.add_function(wrap_pyfunction!(discover_non_stochastic_flower_process_tree, m)?)?;
    m.add_function(wrap_pyfunction!(discover_non_stochastic_prefix_tree_deterministic_finite_automaton, m)?)?;
    m.add_function(wrap_pyfunction!(discover_non_stochastic_prefix_tree_process_tree, m)?)?;
    m.add_function(wrap_pyfunction!(information, m)?)?;
    m.add_function(wrap_pyfunction!(itself_graph, m)?)?;
    m.add_function(wrap_pyfunction!(itself_html, m)?)?;
    m.add_function(wrap_pyfunction!(itself_java, m)?)?;
    m.add_function(wrap_pyfunction!(itself_logo, m)?)?;
    m.add_function(wrap_pyfunction!(itself_manual, m)?)?;
    m.add_function(wrap_pyfunction!(itself_pm4py, m)?)?;
    m.add_function(wrap_pyfunction!(probability_explain_trace, m)?)?;
    m.add_function(wrap_pyfunction!(probability_log, m)?)?;
    m.add_function(wrap_pyfunction!(probability_trace, m)?)?;
    m.add_function(wrap_pyfunction!(sample_folds, m)?)?;
    m.add_function(wrap_pyfunction!(sample_traces, m)?)?;
    m.add_function(wrap_pyfunction!(test_bootstrap_test, m)?)?;
    m.add_function(wrap_pyfunction!(test_log_categorical_attribute, m)?)?;
    m.add_function(wrap_pyfunction!(validate, m)?)?;
    m.add_function(wrap_pyfunction!(visualise_graph, m)?)?;
    m.add_function(wrap_pyfunction!(visualise_text, m)?)?;
    Ok(())
}

