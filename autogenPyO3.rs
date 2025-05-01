#[pyfunction]
fn analyse_all_traces(arg0: &PyAny) -> PyResult<String> {
    let command: &&EbiCommand = &&EBI_ANALYSE_ALL_TRACES;
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
fn analyse_completeness(arg0: &PyAny) -> PyResult<String> {
    let command: &&EbiCommand = &&EBI_ANALYSE_COMPLETENESS;
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
fn analyse_coverage(arg0: &PyAny, arg1: &PyAny) -> PyResult<String> {
    let command: &&EbiCommand = &&EBI_ANALYSE_COVERAGE;
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
    let input1 = [
        EventLog::import_from_pm4py,
        StochasticLabelledPetriNet::import_from_pm4py,
        LabelledPetriNet::import_from_pm4py,
    ]
    .iter()
    .find_map(|importer| importer(arg1, input_types[0]).ok())
    .ok_or_else(|| pyo3::exceptions::PyValueError::new_err("Could not import argument 0"))?;
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
    let input0 = [
        EventLog::import_from_pm4py,
        StochasticLabelledPetriNet::import_from_pm4py,
        LabelledPetriNet::import_from_pm4py,
    ]
    .iter()
    .find_map(|importer| importer(arg0, input_types[0]).ok())
    .ok_or_else(|| pyo3::exceptions::PyValueError::new_err("Could not import argument 0"))?;
    let input1 = [
        EventLog::import_from_pm4py,
        StochasticLabelledPetriNet::import_from_pm4py,
        LabelledPetriNet::import_from_pm4py,
    ]
    .iter()
    .find_map(|importer| importer(arg1, input_types[0]).ok())
    .ok_or_else(|| pyo3::exceptions::PyValueError::new_err("Could not import argument 0"))?;
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
    let command: &&EbiCommand = &&EBI_ANALYSE_MINIMUM_PROBABILITY_TRACES;
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
    let input1 = [
        EventLog::import_from_pm4py,
        StochasticLabelledPetriNet::import_from_pm4py,
        LabelledPetriNet::import_from_pm4py,
    ]
    .iter()
    .find_map(|importer| importer(arg1, input_types[0]).ok())
    .ok_or_else(|| pyo3::exceptions::PyValueError::new_err("Could not import argument 0"))?;
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
fn analyse_most_likely_traces(arg0: &PyAny, arg1: &PyAny) -> PyResult<String> {
    let command: &&EbiCommand = &&EBI_ANALYSE_MOST_LIKELY_TRACES;
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
    let input1 = [
        EventLog::import_from_pm4py,
        StochasticLabelledPetriNet::import_from_pm4py,
        LabelledPetriNet::import_from_pm4py,
    ]
    .iter()
    .find_map(|importer| importer(arg1, input_types[0]).ok())
    .ok_or_else(|| pyo3::exceptions::PyValueError::new_err("Could not import argument 0"))?;
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
fn analyse_non_stochastic_alignment(arg0: &PyAny, arg1: &PyAny) -> PyResult<String> {
    let command: &&EbiCommand = &&EBI_ANALYSE_NON_STOCHASTIC_ALIGNMENT;
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
    let input1 = [
        EventLog::import_from_pm4py,
        StochasticLabelledPetriNet::import_from_pm4py,
        LabelledPetriNet::import_from_pm4py,
    ]
    .iter()
    .find_map(|importer| importer(arg1, input_types[0]).ok())
    .ok_or_else(|| pyo3::exceptions::PyValueError::new_err("Could not import argument 0"))?;
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
fn analyse_non_stochastic_cluster(arg0: &PyAny, arg1: &PyAny) -> PyResult<String> {
    let command: &&EbiCommand = &&EBI_ANALYSE_NON_STOCHASTIC_CLUSTER;
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
    let input1 = [
        EventLog::import_from_pm4py,
        StochasticLabelledPetriNet::import_from_pm4py,
        LabelledPetriNet::import_from_pm4py,
    ]
    .iter()
    .find_map(|importer| importer(arg1, input_types[0]).ok())
    .ok_or_else(|| pyo3::exceptions::PyValueError::new_err("Could not import argument 0"))?;
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
    let input0 = [
        EventLog::import_from_pm4py,
        StochasticLabelledPetriNet::import_from_pm4py,
        LabelledPetriNet::import_from_pm4py,
    ]
    .iter()
    .find_map(|importer| importer(arg0, input_types[0]).ok())
    .ok_or_else(|| pyo3::exceptions::PyValueError::new_err("Could not import argument 0"))?;
    let input1 = [
        EventLog::import_from_pm4py,
        StochasticLabelledPetriNet::import_from_pm4py,
        LabelledPetriNet::import_from_pm4py,
    ]
    .iter()
    .find_map(|importer| importer(arg1, input_types[0]).ok())
    .ok_or_else(|| pyo3::exceptions::PyValueError::new_err("Could not import argument 0"))?;
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
fn analyse_non_stochastic_medoid(arg0: &PyAny, arg1: &PyAny) -> PyResult<String> {
    let command: &&EbiCommand = &&EBI_ANALYSE_NON_STOCHASTIC_MEDOID;
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
    let input1 = [
        EventLog::import_from_pm4py,
        StochasticLabelledPetriNet::import_from_pm4py,
        LabelledPetriNet::import_from_pm4py,
    ]
    .iter()
    .find_map(|importer| importer(arg1, input_types[0]).ok())
    .ok_or_else(|| pyo3::exceptions::PyValueError::new_err("Could not import argument 0"))?;
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
    let command: &&EbiCommand = &&EBI_ASSOCIATION_ALL_TRACE_ATTRIBUTES;
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
fn association_trace_attribute(arg0: &PyAny, arg1: &PyAny) -> PyResult<String> {
    let command: &&EbiCommand = &&EBI_ASSOCIATION_TRACE_ATTRIBUTE;
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
    let input1 = [
        EventLog::import_from_pm4py,
        StochasticLabelledPetriNet::import_from_pm4py,
        LabelledPetriNet::import_from_pm4py,
    ]
    .iter()
    .find_map(|importer| importer(arg1, input_types[0]).ok())
    .ok_or_else(|| pyo3::exceptions::PyValueError::new_err("Could not import argument 0"))?;
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
    let command: &&EbiCommand = &&EBI_CONFORMANCE_EARTH_MOVERS_STOCHASTIC_CONFORMANCE;
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
    let input1 = [
        EventLog::import_from_pm4py,
        StochasticLabelledPetriNet::import_from_pm4py,
        LabelledPetriNet::import_from_pm4py,
    ]
    .iter()
    .find_map(|importer| importer(arg1, input_types[0]).ok())
    .ok_or_else(|| pyo3::exceptions::PyValueError::new_err("Could not import argument 0"))?;
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
    let command: &&EbiCommand = &&EBI_CONFORMANCE_EARTH_MOVERS_STOCHASTIC_CONFORMANCE_SAMPLE;
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
    let input1 = [
        EventLog::import_from_pm4py,
        StochasticLabelledPetriNet::import_from_pm4py,
        LabelledPetriNet::import_from_pm4py,
    ]
    .iter()
    .find_map(|importer| importer(arg1, input_types[0]).ok())
    .ok_or_else(|| pyo3::exceptions::PyValueError::new_err("Could not import argument 0"))?;
    let input2 = [
        EventLog::import_from_pm4py,
        StochasticLabelledPetriNet::import_from_pm4py,
        LabelledPetriNet::import_from_pm4py,
    ]
    .iter()
    .find_map(|importer| importer(arg2, input_types[0]).ok())
    .ok_or_else(|| pyo3::exceptions::PyValueError::new_err("Could not import argument 0"))?;
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
    let command: &&EbiCommand = &&EBI_CONFORMANCE_ENTROPIC_RELEVANCE;
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
    let input1 = [
        EventLog::import_from_pm4py,
        StochasticLabelledPetriNet::import_from_pm4py,
        LabelledPetriNet::import_from_pm4py,
    ]
    .iter()
    .find_map(|importer| importer(arg1, input_types[0]).ok())
    .ok_or_else(|| pyo3::exceptions::PyValueError::new_err("Could not import argument 0"))?;
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
    let command: &&EbiCommand = &&EBI_CONFORMANCE_JENSEN_SHANNON;
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
    let input1 = [
        EventLog::import_from_pm4py,
        StochasticLabelledPetriNet::import_from_pm4py,
        LabelledPetriNet::import_from_pm4py,
    ]
    .iter()
    .find_map(|importer| importer(arg1, input_types[0]).ok())
    .ok_or_else(|| pyo3::exceptions::PyValueError::new_err("Could not import argument 0"))?;
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
    let command: &&EbiCommand = &&EBI_CONFORMANCE_JENSEN_SHANNON_SAMPLE;
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
    let input1 = [
        EventLog::import_from_pm4py,
        StochasticLabelledPetriNet::import_from_pm4py,
        LabelledPetriNet::import_from_pm4py,
    ]
    .iter()
    .find_map(|importer| importer(arg1, input_types[0]).ok())
    .ok_or_else(|| pyo3::exceptions::PyValueError::new_err("Could not import argument 0"))?;
    let input2 = [
        EventLog::import_from_pm4py,
        StochasticLabelledPetriNet::import_from_pm4py,
        LabelledPetriNet::import_from_pm4py,
    ]
    .iter()
    .find_map(|importer| importer(arg2, input_types[0]).ok())
    .ok_or_else(|| pyo3::exceptions::PyValueError::new_err("Could not import argument 0"))?;
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
    let command: &&EbiCommand = &&EBI_CONFORMANCE_UNIT_EARTH_MOVERS_STOCHASTIC_CONFORMANCE;
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
    let input1 = [
        EventLog::import_from_pm4py,
        StochasticLabelledPetriNet::import_from_pm4py,
        LabelledPetriNet::import_from_pm4py,
    ]
    .iter()
    .find_map(|importer| importer(arg1, input_types[0]).ok())
    .ok_or_else(|| pyo3::exceptions::PyValueError::new_err("Could not import argument 0"))?;
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
    let command: &&EbiCommand = &&EBI_CONVERT_FINITE_STOCHASTIC_LANGUAGE;
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
fn convert_labelled_Petri_net(arg0: &PyAny) -> PyResult<String> {
    let command: &&EbiCommand = &&EBI_CONVERT_LABELLED_PETRI_NET;
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
fn convert_stochastic_finite_deterministic_automaton(arg0: &PyAny) -> PyResult<String> {
    let command: &&EbiCommand = &&EBI_CONVERT_STOCHASTIC_FINITE_DETERMINISTIC_AUTOMATON;
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
fn discover_alignments(arg0: &PyAny, arg1: &PyAny) -> PyResult<String> {
    let command: &&EbiCommand = &&EBI_DISCOVER_ALIGNMENTS;
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
    let input1 = [
        EventLog::import_from_pm4py,
        StochasticLabelledPetriNet::import_from_pm4py,
        LabelledPetriNet::import_from_pm4py,
    ]
    .iter()
    .find_map(|importer| importer(arg1, input_types[0]).ok())
    .ok_or_else(|| pyo3::exceptions::PyValueError::new_err("Could not import argument 0"))?;
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
    let input0 = [
        EventLog::import_from_pm4py,
        StochasticLabelledPetriNet::import_from_pm4py,
        LabelledPetriNet::import_from_pm4py,
    ]
    .iter()
    .find_map(|importer| importer(arg0, input_types[0]).ok())
    .ok_or_else(|| pyo3::exceptions::PyValueError::new_err("Could not import argument 0"))?;
    let input1 = [
        EventLog::import_from_pm4py,
        StochasticLabelledPetriNet::import_from_pm4py,
        LabelledPetriNet::import_from_pm4py,
    ]
    .iter()
    .find_map(|importer| importer(arg1, input_types[0]).ok())
    .ok_or_else(|| pyo3::exceptions::PyValueError::new_err("Could not import argument 0"))?;
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
fn information(arg0: &PyAny) -> PyResult<String> {
    let command: &&EbiCommand = &&EBI_INFORMATION;
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
fn itself_pm4py() -> PyResult<String> {
    let command: &&EbiCommand = &&EBI_ITSELF_PM4PY;
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
    let input0 = [
        EventLog::import_from_pm4py,
        StochasticLabelledPetriNet::import_from_pm4py,
        LabelledPetriNet::import_from_pm4py,
    ]
    .iter()
    .find_map(|importer| importer(arg0, input_types[0]).ok())
    .ok_or_else(|| pyo3::exceptions::PyValueError::new_err("Could not import argument 0"))?;
    let input1 = [
        EventLog::import_from_pm4py,
        StochasticLabelledPetriNet::import_from_pm4py,
        LabelledPetriNet::import_from_pm4py,
    ]
    .iter()
    .find_map(|importer| importer(arg1, input_types[0]).ok())
    .ok_or_else(|| pyo3::exceptions::PyValueError::new_err("Could not import argument 0"))?;
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
    let input0 = [
        EventLog::import_from_pm4py,
        StochasticLabelledPetriNet::import_from_pm4py,
        LabelledPetriNet::import_from_pm4py,
    ]
    .iter()
    .find_map(|importer| importer(arg0, input_types[0]).ok())
    .ok_or_else(|| pyo3::exceptions::PyValueError::new_err("Could not import argument 0"))?;
    let input1 = [
        EventLog::import_from_pm4py,
        StochasticLabelledPetriNet::import_from_pm4py,
        LabelledPetriNet::import_from_pm4py,
    ]
    .iter()
    .find_map(|importer| importer(arg1, input_types[0]).ok())
    .ok_or_else(|| pyo3::exceptions::PyValueError::new_err("Could not import argument 0"))?;
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
fn sample(arg0: &PyAny, arg1: &PyAny) -> PyResult<String> {
    let command: &&EbiCommand = &&EBI_SAMPLE;
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
    let input1 = [
        EventLog::import_from_pm4py,
        StochasticLabelledPetriNet::import_from_pm4py,
        LabelledPetriNet::import_from_pm4py,
    ]
    .iter()
    .find_map(|importer| importer(arg1, input_types[0]).ok())
    .ok_or_else(|| pyo3::exceptions::PyValueError::new_err("Could not import argument 0"))?;
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
    let command: &&EbiCommand = &&EBI_TEST_LOG_CATEGORICAL_ATTRIBUTE;
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
    let input1 = [
        EventLog::import_from_pm4py,
        StochasticLabelledPetriNet::import_from_pm4py,
        LabelledPetriNet::import_from_pm4py,
    ]
    .iter()
    .find_map(|importer| importer(arg1, input_types[0]).ok())
    .ok_or_else(|| pyo3::exceptions::PyValueError::new_err("Could not import argument 0"))?;
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
fn visualise_pdf(arg0: &PyAny) -> PyResult<String> {
    let command: &&EbiCommand = &&EBI_VISUALISE_PDF;
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
fn visualise_svg(arg0: &PyAny) -> PyResult<String> {
    let command: &&EbiCommand = &&EBI_VISUALISE_SVG;
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


