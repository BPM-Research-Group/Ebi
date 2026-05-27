#![allow(unsafe_op_in_unsafe_fn)]
#![allow(unused_variables)]

// This file has been automatically generated. Manual changes will be overridden.

use pyo3::prelude::*;
use pyo3::types::PyAny;
use super::{python_link::import_or_load, python_export::ExportableToPM4Py};
use crate::ebi_framework::ebi_command::EbiCommand;

#[pyfunction]
fn analyse_all_traces(py: Python<'_>, arg0: &Bound<'_, PyAny>) -> PyResult<Py<PyAny>> {
    ebi_objects::ebi_arithmetic::exact::set_exact_globally(true);
    let command: &&EbiCommand = &&crate::ebi_commands::ebi_command_analyse::EBI_ANALYSE_ALL;
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
fn analyse_completeness(py: Python<'_>, arg0: &Bound<'_, PyAny>) -> PyResult<Py<PyAny>> {
    ebi_objects::ebi_arithmetic::exact::set_exact_globally(true);
    let command: &&EbiCommand = &&crate::ebi_commands::ebi_command_analyse::EBI_ANALYSE_COMPLETENESS;
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
fn analyse_coverage(py: Python<'_>, arg0: &Bound<'_, PyAny>, arg1: &Bound<'_, PyAny>) -> PyResult<Py<PyAny>> {
    ebi_objects::ebi_arithmetic::exact::set_exact_globally(true);
    let command: &&EbiCommand = &&crate::ebi_commands::ebi_command_analyse::EBI_ANALYSE_COVERAGE;
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
fn analyse_directly_follows_edge_difference(py: Python<'_>, arg0: &Bound<'_, PyAny>, arg1: &Bound<'_, PyAny>) -> PyResult<Py<PyAny>> {
    ebi_objects::ebi_arithmetic::exact::set_exact_globally(true);
    let command: &&EbiCommand = &&crate::ebi_commands::ebi_command_analyse::EBI_ANALYSE_DIRECTLY_FOLLOWS_EDGE_DIFFERENCE;
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
fn analyse_entropy(py: Python<'_>, arg0: &Bound<'_, PyAny>) -> PyResult<Py<PyAny>> {
    ebi_objects::ebi_arithmetic::exact::set_exact_globally(true);
    let command: &&EbiCommand = &&crate::ebi_commands::ebi_command_analyse::EBI_ANALYSE_ENTROPY;
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
fn analyse_medoid(py: Python<'_>, arg0: &Bound<'_, PyAny>, arg1: &Bound<'_, PyAny>) -> PyResult<Py<PyAny>> {
    ebi_objects::ebi_arithmetic::exact::set_exact_globally(true);
    let command: &&EbiCommand = &&crate::ebi_commands::ebi_command_analyse::EBI_ANALYSE_MEDOID;
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
fn analyse_minimum_probability_traces(py: Python<'_>, arg0: &Bound<'_, PyAny>, arg1: &Bound<'_, PyAny>) -> PyResult<Py<PyAny>> {
    ebi_objects::ebi_arithmetic::exact::set_exact_globally(true);
    let command: &&EbiCommand = &&crate::ebi_commands::ebi_command_analyse::EBI_ANALYSE_MINPROB;
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
fn analyse_mode(py: Python<'_>, arg0: &Bound<'_, PyAny>) -> PyResult<Py<PyAny>> {
    ebi_objects::ebi_arithmetic::exact::set_exact_globally(true);
    let command: &&EbiCommand = &&crate::ebi_commands::ebi_command_analyse::EBI_ANALYSE_MODE;
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
fn analyse_most_likely_traces(py: Python<'_>, arg0: &Bound<'_, PyAny>, arg1: &Bound<'_, PyAny>) -> PyResult<Py<PyAny>> {
    ebi_objects::ebi_arithmetic::exact::set_exact_globally(true);
    let command: &&EbiCommand = &&crate::ebi_commands::ebi_command_analyse::EBI_ANALYSE_MOSTLIKELY;
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
fn analyse_variety(py: Python<'_>, arg0: &Bound<'_, PyAny>) -> PyResult<Py<PyAny>> {
    ebi_objects::ebi_arithmetic::exact::set_exact_globally(true);
    let command: &&EbiCommand = &&crate::ebi_commands::ebi_command_analyse::EBI_ANALYSE_VARIETY;
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
fn analyse_non_stochastic_activities(py: Python<'_>, arg0: &Bound<'_, PyAny>) -> PyResult<Py<PyAny>> {
    ebi_objects::ebi_arithmetic::exact::set_exact_globally(true);
    let command: &&EbiCommand = &&crate::ebi_commands::ebi_command_analyse_non_stochastic::EBI_ANALYSE_NON_STOCHASTIC_ACTIVITIES;
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
fn analyse_non_stochastic_any_traces(py: Python<'_>, arg0: &Bound<'_, PyAny>) -> PyResult<Py<PyAny>> {
    ebi_objects::ebi_arithmetic::exact::set_exact_globally(true);
    let command: &&EbiCommand = &&crate::ebi_commands::ebi_command_analyse_non_stochastic::EBI_ANALYSE_NON_STOCHASTIC_ANY_TRACES;
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
fn analyse_non_stochastic_bounded(py: Python<'_>, arg0: &Bound<'_, PyAny>) -> PyResult<Py<PyAny>> {
    ebi_objects::ebi_arithmetic::exact::set_exact_globally(true);
    let command: &&EbiCommand = &&crate::ebi_commands::ebi_command_analyse_non_stochastic::EBI_ANALYSE_NON_STOCHASTIC_BOUNDED;
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
fn analyse_non_stochastic_cluster(py: Python<'_>, arg0: &Bound<'_, PyAny>, arg1: &Bound<'_, PyAny>) -> PyResult<Py<PyAny>> {
    ebi_objects::ebi_arithmetic::exact::set_exact_globally(true);
    let command: &&EbiCommand = &&crate::ebi_commands::ebi_command_analyse_non_stochastic::EBI_ANALYSE_NON_STOCHASTIC_CLUSTER;
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
fn analyse_non_stochastic_executions(py: Python<'_>, arg0: &Bound<'_, PyAny>, arg1: &Bound<'_, PyAny>) -> PyResult<Py<PyAny>> {
    ebi_objects::ebi_arithmetic::exact::set_exact_globally(true);
    let command: &&EbiCommand = &&crate::ebi_commands::ebi_command_analyse_non_stochastic::EBI_ANALYSE_NON_STOCHASTIC_EXECUTIONS;
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
fn analyse_non_stochastic_infinitely_many_traces(py: Python<'_>, arg0: &Bound<'_, PyAny>) -> PyResult<Py<PyAny>> {
    ebi_objects::ebi_arithmetic::exact::set_exact_globally(true);
    let command: &&EbiCommand = &&crate::ebi_commands::ebi_command_analyse_non_stochastic::EBI_ANALYSE_NON_STOCHASTIC_INFINITELY_MANY_TRACES;
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
fn analyse_non_stochastic_medoid(py: Python<'_>, arg0: &Bound<'_, PyAny>, arg1: &Bound<'_, PyAny>) -> PyResult<Py<PyAny>> {
    ebi_objects::ebi_arithmetic::exact::set_exact_globally(true);
    let command: &&EbiCommand = &&crate::ebi_commands::ebi_command_analyse_non_stochastic::EBI_ANALYSE_NON_STOCHASTIC_MEDOID;
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
fn analyse_non_stochastic_timestamps_ordered(py: Python<'_>, arg0: &Bound<'_, PyAny>) -> PyResult<Py<PyAny>> {
    ebi_objects::ebi_arithmetic::exact::set_exact_globally(true);
    let command: &&EbiCommand = &&crate::ebi_commands::ebi_command_analyse_non_stochastic::EBI_ANALYSE_NON_STOCHASTIC_TIMESTAMPS_ORDERED;
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
fn association_all_trace_attributes(py: Python<'_>, arg0: &Bound<'_, PyAny>, arg1: &Bound<'_, PyAny>) -> PyResult<Py<PyAny>> {
    ebi_objects::ebi_arithmetic::exact::set_exact_globally(true);
    let command: &&EbiCommand = &&crate::ebi_commands::ebi_command_association::EBI_ASSOCIATION_ATTRIBUTES;
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
fn association_trace_attribute(py: Python<'_>, arg0: &Bound<'_, PyAny>, arg1: &Bound<'_, PyAny>, arg2: &Bound<'_, PyAny>) -> PyResult<Py<PyAny>> {
    ebi_objects::ebi_arithmetic::exact::set_exact_globally(true);
    let command: &&EbiCommand = &&crate::ebi_commands::ebi_command_association::EBI_ASSOCIATION_ATTRIBUTE;
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
fn conformance_chi_squared(py: Python<'_>, arg0: &Bound<'_, PyAny>, arg1: &Bound<'_, PyAny>) -> PyResult<Py<PyAny>> {
    ebi_objects::ebi_arithmetic::exact::set_exact_globally(true);
    let command: &&EbiCommand = &&crate::ebi_commands::ebi_command_conformance::EBI_CONFORMANCE_CHI_SQUARED;
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
fn conformance_chi_squared_sample(py: Python<'_>, arg0: &Bound<'_, PyAny>, arg1: &Bound<'_, PyAny>, arg2: &Bound<'_, PyAny>) -> PyResult<Py<PyAny>> {
    ebi_objects::ebi_arithmetic::exact::set_exact_globally(true);
    let command: &&EbiCommand = &&crate::ebi_commands::ebi_command_conformance::EBI_CONFORMANCE_CHI_SQUARED_SAMPLE;
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
fn conformance_earth_movers(py: Python<'_>, arg0: &Bound<'_, PyAny>, arg1: &Bound<'_, PyAny>) -> PyResult<Py<PyAny>> {
    ebi_objects::ebi_arithmetic::exact::set_exact_globally(true);
    let command: &&EbiCommand = &&crate::ebi_commands::ebi_command_conformance::EBI_CONFORMANCE_EARTH_MOVERS;
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
fn conformance_earth_movers_sample(py: Python<'_>, arg0: &Bound<'_, PyAny>, arg1: &Bound<'_, PyAny>, arg2: &Bound<'_, PyAny>) -> PyResult<Py<PyAny>> {
    ebi_objects::ebi_arithmetic::exact::set_exact_globally(true);
    let command: &&EbiCommand = &&crate::ebi_commands::ebi_command_conformance::EBI_CONFORMANCE_EARTH_MOVERS_SAMPLE;
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
fn conformance_entropic_relevance(py: Python<'_>, arg0: &Bound<'_, PyAny>, arg1: &Bound<'_, PyAny>) -> PyResult<Py<PyAny>> {
    ebi_objects::ebi_arithmetic::exact::set_exact_globally(true);
    let command: &&EbiCommand = &&crate::ebi_commands::ebi_command_conformance::EBI_CONFORMANCE_ENTROPIC_RELEVANCE;
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
fn conformance_gain_precision(py: Python<'_>, arg0: &Bound<'_, PyAny>, arg1: &Bound<'_, PyAny>, arg2: &Bound<'_, PyAny>) -> PyResult<Py<PyAny>> {
    ebi_objects::ebi_arithmetic::exact::set_exact_globally(false);
    let command: &&EbiCommand = &&crate::ebi_commands::ebi_command_conformance::EBI_CONFORMANCE_GAIN_PRECISION;
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
fn conformance_gain_recall(py: Python<'_>, arg0: &Bound<'_, PyAny>, arg1: &Bound<'_, PyAny>, arg2: &Bound<'_, PyAny>) -> PyResult<Py<PyAny>> {
    ebi_objects::ebi_arithmetic::exact::set_exact_globally(false);
    let command: &&EbiCommand = &&crate::ebi_commands::ebi_command_conformance::EBI_CONFORMANCE_GAIN_RECALL;
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
fn conformance_hellinger(py: Python<'_>, arg0: &Bound<'_, PyAny>, arg1: &Bound<'_, PyAny>) -> PyResult<Py<PyAny>> {
    ebi_objects::ebi_arithmetic::exact::set_exact_globally(true);
    let command: &&EbiCommand = &&crate::ebi_commands::ebi_command_conformance::EBI_CONFORMANCE_HELLINGER;
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
fn conformance_hellinger_sample(py: Python<'_>, arg0: &Bound<'_, PyAny>, arg1: &Bound<'_, PyAny>, arg2: &Bound<'_, PyAny>) -> PyResult<Py<PyAny>> {
    ebi_objects::ebi_arithmetic::exact::set_exact_globally(true);
    let command: &&EbiCommand = &&crate::ebi_commands::ebi_command_conformance::EBI_CONFORMANCE_HELLINGER_SAMPLE;
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
fn conformance_jensen_shannon(py: Python<'_>, arg0: &Bound<'_, PyAny>, arg1: &Bound<'_, PyAny>) -> PyResult<Py<PyAny>> {
    ebi_objects::ebi_arithmetic::exact::set_exact_globally(false);
    let command: &&EbiCommand = &&crate::ebi_commands::ebi_command_conformance::EBI_CONFORMANCE_JSSC;
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
fn conformance_jensen_shannon_sample(py: Python<'_>, arg0: &Bound<'_, PyAny>, arg1: &Bound<'_, PyAny>, arg2: &Bound<'_, PyAny>) -> PyResult<Py<PyAny>> {
    ebi_objects::ebi_arithmetic::exact::set_exact_globally(false);
    let command: &&EbiCommand = &&crate::ebi_commands::ebi_command_conformance::EBI_CONFORMANCE_JSSC_SAMPLE;
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
fn conformance_markovian(py: Python<'_>, arg0: &Bound<'_, PyAny>, arg1: &Bound<'_, PyAny>, arg2: &Bound<'_, PyAny>, arg3: &Bound<'_, PyAny>) -> PyResult<Py<PyAny>> {
    ebi_objects::ebi_arithmetic::exact::set_exact_globally(true);
    let command: &&EbiCommand = &&crate::ebi_commands::ebi_command_conformance::EBI_CONFORMANCE_MARKOVIAN;
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
fn conformance_unit_earth_movers(py: Python<'_>, arg0: &Bound<'_, PyAny>, arg1: &Bound<'_, PyAny>) -> PyResult<Py<PyAny>> {
    ebi_objects::ebi_arithmetic::exact::set_exact_globally(true);
    let command: &&EbiCommand = &&crate::ebi_commands::ebi_command_conformance::EBI_CONFORMANCE_UEMSC;
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
fn conformance_unit_earth_movers_sample(py: Python<'_>, arg0: &Bound<'_, PyAny>, arg1: &Bound<'_, PyAny>, arg2: &Bound<'_, PyAny>) -> PyResult<Py<PyAny>> {
    ebi_objects::ebi_arithmetic::exact::set_exact_globally(false);
    let command: &&EbiCommand = &&crate::ebi_commands::ebi_command_conformance::EBI_CONFORMANCE_UEMSC_SAMPLE;
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
fn conformance_non_stochastic_alignments(py: Python<'_>, arg0: &Bound<'_, PyAny>, arg1: &Bound<'_, PyAny>) -> PyResult<Py<PyAny>> {
    ebi_objects::ebi_arithmetic::exact::set_exact_globally(true);
    let command: &&EbiCommand = &&crate::ebi_commands::ebi_command_conformance_non_stochastic::EBI_CONFORMANCE_NON_STOCHASTIC_ALIGNMENTS;
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
fn conformance_non_stochastic_escaping_edges_precision(py: Python<'_>, arg0: &Bound<'_, PyAny>, arg1: &Bound<'_, PyAny>) -> PyResult<Py<PyAny>> {
    ebi_objects::ebi_arithmetic::exact::set_exact_globally(true);
    let command: &&EbiCommand = &&crate::ebi_commands::ebi_command_conformance_non_stochastic::EBI_CONFORMANCE_NON_STOCHASTIC_ESCAPING_EDGES_PRECISION;
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
fn conformance_non_stochastic_set_alignments(py: Python<'_>, arg0: &Bound<'_, PyAny>, arg1: &Bound<'_, PyAny>) -> PyResult<Py<PyAny>> {
    ebi_objects::ebi_arithmetic::exact::set_exact_globally(true);
    let command: &&EbiCommand = &&crate::ebi_commands::ebi_command_conformance_non_stochastic::EBI_CONFORMANCE_NON_STOCHASTIC_SET_ALIGNMENTS;
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
fn conformance_non_stochastic_trace_fitness(py: Python<'_>, arg0: &Bound<'_, PyAny>) -> PyResult<Py<PyAny>> {
    ebi_objects::ebi_arithmetic::exact::set_exact_globally(true);
    let command: &&EbiCommand = &&crate::ebi_commands::ebi_command_conformance_non_stochastic::EBI_CONFORMANCE_NON_STOCHASTIC_TRACE_FITNESS;
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
fn convert_business_process_model_and_notation(py: Python<'_>, arg0: &Bound<'_, PyAny>) -> PyResult<Py<PyAny>> {
    ebi_objects::ebi_arithmetic::exact::set_exact_globally(true);
    let command: &&EbiCommand = &&crate::ebi_commands::ebi_command_convert::EBI_CONVERT_BPMN;
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
fn convert_finite_language(py: Python<'_>, arg0: &Bound<'_, PyAny>) -> PyResult<Py<PyAny>> {
    ebi_objects::ebi_arithmetic::exact::set_exact_globally(true);
    let command: &&EbiCommand = &&crate::ebi_commands::ebi_command_convert::EBI_CONVERT_LANG;
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
fn convert_finite_stochastic_language(py: Python<'_>, arg0: &Bound<'_, PyAny>) -> PyResult<Py<PyAny>> {
    ebi_objects::ebi_arithmetic::exact::set_exact_globally(true);
    let command: &&EbiCommand = &&crate::ebi_commands::ebi_command_convert::EBI_CONVERT_SLANG;
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
fn convert_labelled_petri_net(py: Python<'_>, arg0: &Bound<'_, PyAny>) -> PyResult<Py<PyAny>> {
    ebi_objects::ebi_arithmetic::exact::set_exact_globally(true);
    let command: &&EbiCommand = &&crate::ebi_commands::ebi_command_convert::EBI_CONVERT_LPN;
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
fn convert_log(py: Python<'_>, arg0: &Bound<'_, PyAny>) -> PyResult<Py<PyAny>> {
    ebi_objects::ebi_arithmetic::exact::set_exact_globally(true);
    let command: &&EbiCommand = &&crate::ebi_commands::ebi_command_convert::EBI_CONVERT_LOG;
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
fn convert_stochastic_deterministic_finite_automaton(py: Python<'_>, arg0: &Bound<'_, PyAny>) -> PyResult<Py<PyAny>> {
    ebi_objects::ebi_arithmetic::exact::set_exact_globally(true);
    let command: &&EbiCommand = &&crate::ebi_commands::ebi_command_convert::EBI_CONVERT_SDFA;
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
fn convert_stochastic_labelled_petri_net(py: Python<'_>, arg0: &Bound<'_, PyAny>) -> PyResult<Py<PyAny>> {
    ebi_objects::ebi_arithmetic::exact::set_exact_globally(true);
    let command: &&EbiCommand = &&crate::ebi_commands::ebi_command_convert::EBI_CONVERT_SLPN;
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
fn convert_stochastic_nondeterministic_finite_automaton(py: Python<'_>, arg0: &Bound<'_, PyAny>) -> PyResult<Py<PyAny>> {
    ebi_objects::ebi_arithmetic::exact::set_exact_globally(true);
    let command: &&EbiCommand = &&crate::ebi_commands::ebi_command_convert::EBI_CONVERT_SNFA;
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
fn discover_alignments_stochastic_business_process_model_and_notation(py: Python<'_>, arg0: &Bound<'_, PyAny>, arg1: &Bound<'_, PyAny>) -> PyResult<Py<PyAny>> {
    ebi_objects::ebi_arithmetic::exact::set_exact_globally(true);
    let command: &&EbiCommand = &&crate::ebi_commands::ebi_command_discover::EBI_DISCOVER_ALIGNMENTS_BPMN;
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
fn discover_alignments_stochastic_labelled_petri_nets(py: Python<'_>, arg0: &Bound<'_, PyAny>, arg1: &Bound<'_, PyAny>) -> PyResult<Py<PyAny>> {
    ebi_objects::ebi_arithmetic::exact::set_exact_globally(true);
    let command: &&EbiCommand = &&crate::ebi_commands::ebi_command_discover::EBI_DISCOVER_ALIGNMENTS_SLPN;
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
fn discover_directly_follows_graph(py: Python<'_>, arg0: &Bound<'_, PyAny>, arg1: &Bound<'_, PyAny>) -> PyResult<Py<PyAny>> {
    ebi_objects::ebi_arithmetic::exact::set_exact_globally(true);
    let command: &&EbiCommand = &&crate::ebi_commands::ebi_command_discover::EBI_DISCOVER_DIRECTLY_FOLLOWS;
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
fn discover_occurrence_stochastic_business_process_model_and_notation(py: Python<'_>, arg0: &Bound<'_, PyAny>, arg1: &Bound<'_, PyAny>) -> PyResult<Py<PyAny>> {
    ebi_objects::ebi_arithmetic::exact::set_exact_globally(true);
    let command: &&EbiCommand = &&crate::ebi_commands::ebi_command_discover::EBI_DISCOVER_OCCURRENCE_SBPMN;
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
fn discover_occurrence_stochastic_labelled_petri_net(py: Python<'_>, arg0: &Bound<'_, PyAny>, arg1: &Bound<'_, PyAny>) -> PyResult<Py<PyAny>> {
    ebi_objects::ebi_arithmetic::exact::set_exact_globally(true);
    let command: &&EbiCommand = &&crate::ebi_commands::ebi_command_discover::EBI_DISCOVER_OCCURRENCE_SLPN;
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
fn discover_occurrence_stochastic_process_tree(py: Python<'_>, arg0: &Bound<'_, PyAny>, arg1: &Bound<'_, PyAny>) -> PyResult<Py<PyAny>> {
    ebi_objects::ebi_arithmetic::exact::set_exact_globally(true);
    let command: &&EbiCommand = &&crate::ebi_commands::ebi_command_discover::EBI_DISCOVER_OCCURRENCE_SPTREE;
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
fn discover_random_stochastic_business_process_model_and_notation(py: Python<'_>, arg0: &Bound<'_, PyAny>) -> PyResult<Py<PyAny>> {
    ebi_objects::ebi_arithmetic::exact::set_exact_globally(true);
    let command: &&EbiCommand = &&crate::ebi_commands::ebi_command_discover::EBI_DISCOVER_RANDOM_SBPMN;
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
fn discover_random_stochastic_labelled_petri_net(py: Python<'_>, arg0: &Bound<'_, PyAny>) -> PyResult<Py<PyAny>> {
    ebi_objects::ebi_arithmetic::exact::set_exact_globally(true);
    let command: &&EbiCommand = &&crate::ebi_commands::ebi_command_discover::EBI_DISCOVER_RANDOM_SLPN;
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
fn discover_random_stochastic_process_tree(py: Python<'_>, arg0: &Bound<'_, PyAny>) -> PyResult<Py<PyAny>> {
    ebi_objects::ebi_arithmetic::exact::set_exact_globally(true);
    let command: &&EbiCommand = &&crate::ebi_commands::ebi_command_discover::EBI_DISCOVER_RANDOM_SPTREE;
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
fn discover_uniform_stochastic_business_process_model_and_notation(py: Python<'_>, arg0: &Bound<'_, PyAny>) -> PyResult<Py<PyAny>> {
    ebi_objects::ebi_arithmetic::exact::set_exact_globally(true);
    let command: &&EbiCommand = &&crate::ebi_commands::ebi_command_discover::EBI_DISCOVER_UNIFORM_SBPMN;
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
fn discover_uniform_stochastic_labelled_petri_net(py: Python<'_>, arg0: &Bound<'_, PyAny>) -> PyResult<Py<PyAny>> {
    ebi_objects::ebi_arithmetic::exact::set_exact_globally(true);
    let command: &&EbiCommand = &&crate::ebi_commands::ebi_command_discover::EBI_DISCOVER_UNIFORM_SLPN;
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
fn discover_uniform_stochastic_process_tree(py: Python<'_>, arg0: &Bound<'_, PyAny>) -> PyResult<Py<PyAny>> {
    ebi_objects::ebi_arithmetic::exact::set_exact_globally(true);
    let command: &&EbiCommand = &&crate::ebi_commands::ebi_command_discover::EBI_DISCOVER_UNIFORM_SPTREE;
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
fn discover_non_stochastic_flower_deterministic_finite_automaton(py: Python<'_>, arg0: &Bound<'_, PyAny>) -> PyResult<Py<PyAny>> {
    ebi_objects::ebi_arithmetic::exact::set_exact_globally(true);
    let command: &&EbiCommand = &&crate::ebi_commands::ebi_command_discover_non_stochastic::EBI_DISCOVER_NON_STOCHASTIC_FLOWER_DFA;
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
fn discover_non_stochastic_flower_process_tree(py: Python<'_>, arg0: &Bound<'_, PyAny>) -> PyResult<Py<PyAny>> {
    ebi_objects::ebi_arithmetic::exact::set_exact_globally(true);
    let command: &&EbiCommand = &&crate::ebi_commands::ebi_command_discover_non_stochastic::EBI_DISCOVER_NON_STOCHASTIC_FLOWER_TREE;
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
fn discover_non_stochastic_prefix_tree_deterministic_finite_automaton(py: Python<'_>, arg0: &Bound<'_, PyAny>) -> PyResult<Py<PyAny>> {
    ebi_objects::ebi_arithmetic::exact::set_exact_globally(true);
    let command: &&EbiCommand = &&crate::ebi_commands::ebi_command_discover_non_stochastic::EBI_DISCOVER_NON_STOCHASTIC_TREE_DFA;
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
fn discover_non_stochastic_prefix_tree_process_tree(py: Python<'_>, arg0: &Bound<'_, PyAny>) -> PyResult<Py<PyAny>> {
    ebi_objects::ebi_arithmetic::exact::set_exact_globally(true);
    let command: &&EbiCommand = &&crate::ebi_commands::ebi_command_discover_non_stochastic::EBI_DISCOVER_NON_STOCHASTIC_TREE_TREE;
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
fn discover_non_stochastic_trace_model(py: Python<'_>, arg0: &Bound<'_, PyAny>) -> PyResult<Py<PyAny>> {
    ebi_objects::ebi_arithmetic::exact::set_exact_globally(true);
    let command: &&EbiCommand = &&crate::ebi_commands::ebi_command_discover_non_stochastic::EBI_DISCOVER_NON_STOCHASTIC_TRACE_MODEL;
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
fn filter_traces_empty(py: Python<'_>, arg0: &Bound<'_, PyAny>) -> PyResult<Py<PyAny>> {
    ebi_objects::ebi_arithmetic::exact::set_exact_globally(true);
    let command: &&EbiCommand = &&crate::ebi_commands::ebi_command_filter::EBI_FILTER_TRACES_EMPTY;
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
fn filter_traces_event_activity(py: Python<'_>, arg0: &Bound<'_, PyAny>, arg1: &Bound<'_, PyAny>, arg2: &Bound<'_, PyAny>) -> PyResult<Py<PyAny>> {
    ebi_objects::ebi_arithmetic::exact::set_exact_globally(true);
    let command: &&EbiCommand = &&crate::ebi_commands::ebi_command_filter::EBI_FILTER_TRACES_EVENT_ACTIVITY;
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
fn filter_traces_length(py: Python<'_>, arg0: &Bound<'_, PyAny>, arg1: &Bound<'_, PyAny>, arg2: &Bound<'_, PyAny>) -> PyResult<Py<PyAny>> {
    ebi_objects::ebi_arithmetic::exact::set_exact_globally(true);
    let command: &&EbiCommand = &&crate::ebi_commands::ebi_command_filter::EBI_FILTER_TRACES_LENGTH;
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
fn information(py: Python<'_>, arg0: &Bound<'_, PyAny>) -> PyResult<Py<PyAny>> {
    ebi_objects::ebi_arithmetic::exact::set_exact_globally(true);
    let command: &&EbiCommand = &&crate::ebi_commands::ebi_command_info::EBI_INFO;
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
fn probability_log(py: Python<'_>, arg0: &Bound<'_, PyAny>, arg1: &Bound<'_, PyAny>) -> PyResult<Py<PyAny>> {
    ebi_objects::ebi_arithmetic::exact::set_exact_globally(true);
    let command: &&EbiCommand = &&crate::ebi_commands::ebi_command_probability::EBI_PROBABILITY_LOG;
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
fn sample_folds(py: Python<'_>, arg0: &Bound<'_, PyAny>, arg1: &Bound<'_, PyAny>, arg2: &Bound<'_, PyAny>, arg3: &Bound<'_, PyAny>) -> PyResult<Py<PyAny>> {
    ebi_objects::ebi_arithmetic::exact::set_exact_globally(true);
    let command: &&EbiCommand = &&crate::ebi_commands::ebi_command_sample::EBI_SAMPLE_FOLDS;
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
fn sample_partially_ordered_traces(py: Python<'_>, arg0: &Bound<'_, PyAny>, arg1: &Bound<'_, PyAny>) -> PyResult<Py<PyAny>> {
    ebi_objects::ebi_arithmetic::exact::set_exact_globally(true);
    let command: &&EbiCommand = &&crate::ebi_commands::ebi_command_sample::EBI_SAMPLE_PARTIALLY_ORDERED_TRACES;
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
fn sample_traces(py: Python<'_>, arg0: &Bound<'_, PyAny>, arg1: &Bound<'_, PyAny>) -> PyResult<Py<PyAny>> {
    ebi_objects::ebi_arithmetic::exact::set_exact_globally(true);
    let command: &&EbiCommand = &&crate::ebi_commands::ebi_command_sample::EBI_SAMPLE_TRACES;
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
fn test_bootstrap_test(py: Python<'_>, arg0: &Bound<'_, PyAny>, arg1: &Bound<'_, PyAny>, arg2: &Bound<'_, PyAny>, arg3: &Bound<'_, PyAny>) -> PyResult<Py<PyAny>> {
    ebi_objects::ebi_arithmetic::exact::set_exact_globally(true);
    let command: &&EbiCommand = &&crate::ebi_commands::ebi_command_test::EBI_TEST_BOOTSTRAP;
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
fn test_log_categorical_attribute(py: Python<'_>, arg0: &Bound<'_, PyAny>, arg1: &Bound<'_, PyAny>, arg2: &Bound<'_, PyAny>, arg3: &Bound<'_, PyAny>) -> PyResult<Py<PyAny>> {
    ebi_objects::ebi_arithmetic::exact::set_exact_globally(true);
    let command: &&EbiCommand = &&crate::ebi_commands::ebi_command_test::EBI_TEST_LOG_ATTRIBUTE;
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
fn visualise_graph(py: Python<'_>, arg0: &Bound<'_, PyAny>) -> PyResult<Py<PyAny>> {
    ebi_objects::ebi_arithmetic::exact::set_exact_globally(true);
    let command: &&EbiCommand = &&crate::ebi_commands::ebi_command_visualise::EBI_VISUALISE_GRAPH;
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
fn visualise_text(py: Python<'_>, arg0: &Bound<'_, PyAny>) -> PyResult<Py<PyAny>> {
    ebi_objects::ebi_arithmetic::exact::set_exact_globally(false);
    let command: &&EbiCommand = &&crate::ebi_commands::ebi_command_visualise::EBI_VISUALISE_TEXT;
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
pub fn ebi(_py: Python<'_>, m: &Bound<'_, PyModule>) -> PyResult<()> {    m.add_function(wrap_pyfunction!(analyse_all_traces, m)?)?;
    m.add_function(wrap_pyfunction!(analyse_completeness, m)?)?;
    m.add_function(wrap_pyfunction!(analyse_coverage, m)?)?;
    m.add_function(wrap_pyfunction!(analyse_directly_follows_edge_difference, m)?)?;
    m.add_function(wrap_pyfunction!(analyse_entropy, m)?)?;
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
    m.add_function(wrap_pyfunction!(analyse_non_stochastic_timestamps_ordered, m)?)?;
    m.add_function(wrap_pyfunction!(association_all_trace_attributes, m)?)?;
    m.add_function(wrap_pyfunction!(association_trace_attribute, m)?)?;
    m.add_function(wrap_pyfunction!(conformance_chi_squared, m)?)?;
    m.add_function(wrap_pyfunction!(conformance_chi_squared_sample, m)?)?;
    m.add_function(wrap_pyfunction!(conformance_earth_movers, m)?)?;
    m.add_function(wrap_pyfunction!(conformance_earth_movers_sample, m)?)?;
    m.add_function(wrap_pyfunction!(conformance_entropic_relevance, m)?)?;
    m.add_function(wrap_pyfunction!(conformance_gain_precision, m)?)?;
    m.add_function(wrap_pyfunction!(conformance_gain_recall, m)?)?;
    m.add_function(wrap_pyfunction!(conformance_hellinger, m)?)?;
    m.add_function(wrap_pyfunction!(conformance_hellinger_sample, m)?)?;
    m.add_function(wrap_pyfunction!(conformance_jensen_shannon, m)?)?;
    m.add_function(wrap_pyfunction!(conformance_jensen_shannon_sample, m)?)?;
    m.add_function(wrap_pyfunction!(conformance_markovian, m)?)?;
    m.add_function(wrap_pyfunction!(conformance_unit_earth_movers, m)?)?;
    m.add_function(wrap_pyfunction!(conformance_unit_earth_movers_sample, m)?)?;
    m.add_function(wrap_pyfunction!(conformance_non_stochastic_alignments, m)?)?;
    m.add_function(wrap_pyfunction!(conformance_non_stochastic_escaping_edges_precision, m)?)?;
    m.add_function(wrap_pyfunction!(conformance_non_stochastic_set_alignments, m)?)?;
    m.add_function(wrap_pyfunction!(conformance_non_stochastic_trace_fitness, m)?)?;
    m.add_function(wrap_pyfunction!(convert_business_process_model_and_notation, m)?)?;
    m.add_function(wrap_pyfunction!(convert_finite_language, m)?)?;
    m.add_function(wrap_pyfunction!(convert_finite_stochastic_language, m)?)?;
    m.add_function(wrap_pyfunction!(convert_labelled_petri_net, m)?)?;
    m.add_function(wrap_pyfunction!(convert_log, m)?)?;
    m.add_function(wrap_pyfunction!(convert_stochastic_deterministic_finite_automaton, m)?)?;
    m.add_function(wrap_pyfunction!(convert_stochastic_labelled_petri_net, m)?)?;
    m.add_function(wrap_pyfunction!(convert_stochastic_nondeterministic_finite_automaton, m)?)?;
    m.add_function(wrap_pyfunction!(discover_alignments_stochastic_business_process_model_and_notation, m)?)?;
    m.add_function(wrap_pyfunction!(discover_alignments_stochastic_labelled_petri_nets, m)?)?;
    m.add_function(wrap_pyfunction!(discover_directly_follows_graph, m)?)?;
    m.add_function(wrap_pyfunction!(discover_occurrence_stochastic_business_process_model_and_notation, m)?)?;
    m.add_function(wrap_pyfunction!(discover_occurrence_stochastic_labelled_petri_net, m)?)?;
    m.add_function(wrap_pyfunction!(discover_occurrence_stochastic_process_tree, m)?)?;
    m.add_function(wrap_pyfunction!(discover_random_stochastic_business_process_model_and_notation, m)?)?;
    m.add_function(wrap_pyfunction!(discover_random_stochastic_labelled_petri_net, m)?)?;
    m.add_function(wrap_pyfunction!(discover_random_stochastic_process_tree, m)?)?;
    m.add_function(wrap_pyfunction!(discover_uniform_stochastic_business_process_model_and_notation, m)?)?;
    m.add_function(wrap_pyfunction!(discover_uniform_stochastic_labelled_petri_net, m)?)?;
    m.add_function(wrap_pyfunction!(discover_uniform_stochastic_process_tree, m)?)?;
    m.add_function(wrap_pyfunction!(discover_non_stochastic_flower_deterministic_finite_automaton, m)?)?;
    m.add_function(wrap_pyfunction!(discover_non_stochastic_flower_process_tree, m)?)?;
    m.add_function(wrap_pyfunction!(discover_non_stochastic_prefix_tree_deterministic_finite_automaton, m)?)?;
    m.add_function(wrap_pyfunction!(discover_non_stochastic_prefix_tree_process_tree, m)?)?;
    m.add_function(wrap_pyfunction!(discover_non_stochastic_trace_model, m)?)?;
    m.add_function(wrap_pyfunction!(filter_traces_empty, m)?)?;
    m.add_function(wrap_pyfunction!(filter_traces_event_activity, m)?)?;
    m.add_function(wrap_pyfunction!(filter_traces_length, m)?)?;
    m.add_function(wrap_pyfunction!(information, m)?)?;
    m.add_function(wrap_pyfunction!(probability_log, m)?)?;
    m.add_function(wrap_pyfunction!(sample_folds, m)?)?;
    m.add_function(wrap_pyfunction!(sample_partially_ordered_traces, m)?)?;
    m.add_function(wrap_pyfunction!(sample_traces, m)?)?;
    m.add_function(wrap_pyfunction!(test_bootstrap_test, m)?)?;
    m.add_function(wrap_pyfunction!(test_log_categorical_attribute, m)?)?;
    m.add_function(wrap_pyfunction!(visualise_graph, m)?)?;
    m.add_function(wrap_pyfunction!(visualise_text, m)?)?;
    Ok(())
}

