use anyhow::{anyhow, Result};
use pyo3::prelude::*;
use pyo3::types::{PyAny, PyDict, PyList, PySet};
use pyo3::AsPyPointer;
use polars::prelude::*;
use pyo3::exceptions::PyValueError;
use std::io::Cursor;
use std::collections::HashMap;

use crate::math::fraction::Fraction;
use super::ebi_output::EbiOutput;
use crate::ebi_framework::ebi_object::EbiTraitObject;
use crate::ebi_commands::ebi_command_analyse::EBI_ANALYSE_COMPLETENESS;
use crate::ebi_commands::ebi_command_visualise::{EBI_VISUALISE_TEXT, EBI_VISUALISE_SVG};
use crate::ebi_framework::ebi_command::EbiCommand;
use crate::ebi_framework::{ebi_output, ebi_input::{EbiInput, EbiInputType}, ebi_object::{EbiObject, EbiObjectType}, ebi_trait::EbiTrait, activity_key::ActivityKey};
use crate::ebi_objects::event_log::{EventLog, EBI_EVENT_LOG};
use crate::ebi_objects::labelled_petri_net::{LabelledPetriNet, EBI_LABELLED_PETRI_NET};
use crate::ebi_framework::infoable::Infoable;
use crate::ebi_traits::ebi_trait_semantics::EbiTraitSemantics;
use crate::ebi_objects::{stochastic_labelled_petri_net::StochasticLabelledPetriNet, finite_stochastic_language::EBI_FINITE_STOCHASTIC_LANGUAGE};
use crate::marking::Marking;
use process_mining::event_log::{event_log_struct::{EventLogClassifier, to_attributes}, Attributes, AttributeValue};
use process_mining::event_log::{EventLog as ProcessMiningEventLog, Trace, Event};


pub trait ImportableFromPM4Py {
    /// Imports a PM4Py Object as its Rust equivalent.
    fn import_from_pm4py(event_log: &PyAny, input_types: &[&EbiInputType]) -> PyResult<EbiInput>;
}

impl ImportableFromPM4Py for EventLog {
    fn import_from_pm4py(event_log: &PyAny, input_types: &[&EbiInputType]) -> PyResult<EbiInput> {
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
        //Ok(EbiObject::EventLog(event_log_rust))
        //Ok(event_log_rust)


        for &itype in input_types {
            match itype {
                EbiInputType::Object(obj_type) if *obj_type == EbiObjectType::EventLog => {
                    let obj = EbiObject::EventLog(event_log_rust);
                    return Ok(EbiInput::Object(obj, &EBI_EVENT_LOG));
                },
                EbiInputType::Trait(etrait) => {
                    match etrait {
                        EbiTrait::EventLog => {
                            let tobj = EbiTraitObject::EventLog(Box::new(event_log_rust));
                            return Ok(EbiInput::Trait(tobj, &EBI_EVENT_LOG));
                        }
                        EbiTrait::FiniteStochasticLanguage => {
                            let fsl = event_log_rust.get_finite_stochastic_language();
                            let tobj = EbiTraitObject::FiniteStochasticLanguage(Box::new(fsl));                            
                            return Ok(EbiInput::Trait(tobj, &EBI_FINITE_STOCHASTIC_LANGUAGE));
                        }
                        // … add more traits here …
                        _ => { /* this trait isn’t supported by EventLog; skip */ }
                    }
                },
                _ => {
                    // skip other input types
                }
            }
        }

        Err(PyValueError::new_err(
            "EventLog could not be wrapped as any of the requested EbiInputType variants",
        ))


    }
}


impl ImportableFromPM4Py for LabelledPetriNet {
    fn import_from_pm4py(pn: &PyAny, input_types: &[&EbiInputType]) -> PyResult<EbiInput> {
        // Retrieve "places", "transitions", and "arcs" attributes as sets.
        let py_places = get_collection_as_set(pn, "places")
            .map_err(|e| pyo3::exceptions::PyValueError::new_err(format!("Failed to get places: {}", e)))?;
        let py_transitions = get_collection_as_set(pn, "transitions")
            .map_err(|e| pyo3::exceptions::PyValueError::new_err(format!("Failed to get transitions: {}", e)))?;
        let py_arcs = get_collection_as_set(pn, "arcs")
            .map_err(|e| pyo3::exceptions::PyValueError::new_err(format!("Failed to get arcs: {}", e)))?;

        // Convert sets to vectors.
        let places: Vec<&PyAny> = pyset_as_vec(py_places);
        let transitions: Vec<&PyAny> = pyset_as_vec(py_transitions);
        let arcs: Vec<&PyAny> = pyset_as_vec(py_arcs);

        let num_places = places.len();
        let num_transitions = transitions.len();

        // Create mappings using raw pointers as keys.
        let mut place_idx_map: HashMap<usize, usize> = HashMap::new();
        for (i, &place) in places.iter().enumerate() {
            place_idx_map.insert(place.as_ptr() as usize, i);
        }
        let mut trans_idx_map: HashMap<usize, usize> = HashMap::new();
        for (i, &trans) in transitions.iter().enumerate() {
            trans_idx_map.insert(trans.as_ptr() as usize, i);
        }

        // For this example, assume initial marking is all zeros.
        let initial_marking = Marking { place2token: vec![0u64; num_places] };

        // Build labels vector for transitions.
        let mut activity_key = ActivityKey::new();
        let mut labels = Vec::with_capacity(num_transitions);
        for trans in transitions.iter() {
            if let Ok(label_obj) = trans.getattr("label") {
                let label_str: String = label_obj.extract().unwrap_or_default();
                let act = activity_key.process_activity(&label_str);
                labels.push(Some(act));
            } else {
                labels.push(None);
            }
        }

        // Initialize mapping vectors.
        let mut transition2input_places = vec![Vec::new(); num_transitions];
        let mut transition2output_places = vec![Vec::new(); num_transitions];
        let mut transition2input_places_cardinality = vec![Vec::new(); num_transitions]; // default cardinality
        let mut transition2output_places_cardinality = vec![Vec::new(); num_transitions]; // default cardinality
        let mut place2output_transitions = vec![Vec::new(); num_places];

        // Process each arc.
        for arc in arcs {
            // Each arc should have "source" and "target" attributes.
            let source = arc.getattr("source")
                .map_err(|e| pyo3::exceptions::PyValueError::new_err(format!("Arc missing source attribute: {}", e)))?;
            let target = arc.getattr("target")
                .map_err(|e| pyo3::exceptions::PyValueError::new_err(format!("Arc missing target attribute: {}", e)))?;
            let weight = arc.getattr("weight")?.extract::<u64>()
                .map_err(|e| pyo3::exceptions::PyValueError::new_err(format!("Arc missing weight attribute: {}", e)))?;
            let source_key = source.as_ptr() as usize;
            let target_key = target.as_ptr() as usize;

            if place_idx_map.contains_key(&source_key) {
                let p_idx = *place_idx_map.get(&source_key).unwrap();
                let t_idx = *trans_idx_map.get(&target_key).ok_or_else(|| {
                    pyo3::exceptions::PyValueError::new_err("Arc target not found among transitions")
                })?;
                // Update place-to-transition mappings.
                place2output_transitions[p_idx].push(t_idx);
                transition2input_places[t_idx].push(p_idx);
                transition2input_places_cardinality[t_idx].push(weight);
            
            } else if trans_idx_map.contains_key(&source_key) {
                let t_idx = *trans_idx_map.get(&source_key).unwrap();
                let p_idx = *place_idx_map.get(&target_key).ok_or_else(|| {
                    pyo3::exceptions::PyValueError::new_err("Arc target not found among places")
                })?;
                // Update transition-to-place mappings.
                transition2output_places[t_idx].push(p_idx);
                place2output_transitions[p_idx].push(t_idx);
                transition2output_places_cardinality[t_idx].push(weight);
            } else {
                return Err(pyo3::exceptions::PyValueError::new_err(
                    "Arc source not found in either places or transitions"));
            }
        }

        let lpn = LabelledPetriNet {
            activity_key,
            initial_marking,
            labels,
            place2output_transitions,
            transition2input_places,
            transition2output_places,
            transition2input_places_cardinality,
            transition2output_places_cardinality,
        };

        //Ok(EbiObject::LabelledPetriNet(lpn))
        //Ok(lpn)

        for &itype in input_types {
            match itype {
                EbiInputType::Object(EbiObjectType::LabelledPetriNet) => {
                    let obj = EbiObject::LabelledPetriNet(lpn);
                    return Ok(EbiInput::Object(obj, &EBI_LABELLED_PETRI_NET));
                },
                EbiInputType::Trait(etrait) => {
                    match etrait {
                        EbiTrait::Semantics => {
                            let sem = EbiTraitSemantics::Marking(Box::new(lpn.clone()));
                            let trait_obj = EbiTraitObject::Semantics(sem);
                            return Ok(EbiInput::Trait(trait_obj, &EBI_LABELLED_PETRI_NET));
                        },
                        EbiTrait::Graphable => {
                            let graph = lpn.clone();
                            let trait_obj = EbiTraitObject::Graphable(Box::new(graph));
                            return Ok(EbiInput::Trait(trait_obj, &EBI_LABELLED_PETRI_NET));
                        },
                        _ => {}
                    
                    }
                },
                _ => {}
            }
        }


        Err(PyValueError::new_err(
            "Petri Net could not be wrapped as any of the requested EbiInputType variants",
        ))

    }
}

/// StochasticPetriNets are not fully integrated into PM4Py (yet)
/// this function will remain useless until PM4Py has a proper implementation of Stochastic Petri Nets
impl ImportableFromPM4Py for StochasticLabelledPetriNet {
    fn import_from_pm4py(pn: &PyAny, input_types: &[&EbiInputType]) -> PyResult<EbiInput> {
        // Retrieve "places", "transitions", and "arcs" attributes as sets.
        let py_places = get_collection_as_set(pn, "places")
            .map_err(|e| pyo3::exceptions::PyValueError::new_err(format!("Failed to get places: {}", e)))?;
        let py_transitions = get_collection_as_set(pn, "transitions")
            .map_err(|e| pyo3::exceptions::PyValueError::new_err(format!("Failed to get transitions: {}", e)))?;
        let py_arcs = get_collection_as_set(pn, "arcs")
            .map_err(|e| pyo3::exceptions::PyValueError::new_err(format!("Failed to get arcs: {}", e)))?;

        // Convert sets to vectors.
        let places: Vec<&PyAny> = pyset_as_vec(py_places);
        let transitions: Vec<&PyAny> = pyset_as_vec(py_transitions);
        let arcs: Vec<&PyAny> = pyset_as_vec(py_arcs);

        let num_places = places.len();
        let num_transitions = transitions.len();

        // Create mappings using raw pointers as keys.
        let mut place_idx_map: HashMap<usize, usize> = HashMap::new();
        for (i, &place) in places.iter().enumerate() {
            place_idx_map.insert(place.as_ptr() as usize, i);
        }
        let mut trans_idx_map: HashMap<usize, usize> = HashMap::new();
        for (i, &trans) in transitions.iter().enumerate() {
            trans_idx_map.insert(trans.as_ptr() as usize, i);
        }

        // For this example, assume initial marking is all zeros.
        let initial_marking = Marking { place2token: vec![0u64; num_places] };

        // Build labels vector for transitions.
        let mut activity_key = ActivityKey::new();
        let mut labels = Vec::with_capacity(num_transitions);
        let mut weights: Vec<Fraction> =  Vec::with_capacity(num_transitions);
        for trans in transitions.iter() {
            if let Ok(label_obj) = trans.getattr("label") {
                let label_str: String = label_obj.extract().unwrap_or_default();
                let act = activity_key.process_activity(&label_str);
                labels.push(Some(act));
            } else {
                labels.push(None);
            }

            let trans_weight = trans.getattr("weight")?.extract::<f64>()
                .map(|float_weight| crate::math::fraction::Fraction::Approx(float_weight))
                .map_err(|e| pyo3::exceptions::PyValueError::new_err(format!("Failed to retrieve transition weight: {}", e)))?;
            weights.push(trans_weight);
        }

        // Initialize mapping vectors.
        let mut transition2input_places = vec![Vec::new(); num_transitions];
        let mut transition2output_places = vec![Vec::new(); num_transitions];
        let mut transition2input_places_cardinality = vec![Vec::new(); num_transitions]; // default cardinality
        let mut transition2output_places_cardinality = vec![Vec::new(); num_transitions]; // default cardinality
        let mut place2output_transitions = vec![Vec::new(); num_places];

        // Process each arc.
        for arc in arcs {
            // Each arc should have "source" and "target" attributes.
            let source = arc.getattr("source")
                .map_err(|e| pyo3::exceptions::PyValueError::new_err(format!("Arc missing source attribute: {}", e)))?;
            let target = arc.getattr("target")
                .map_err(|e| pyo3::exceptions::PyValueError::new_err(format!("Arc missing target attribute: {}", e)))?;
            let weight = arc.getattr("weight")?.extract::<u64>()
                .map_err(|e| pyo3::exceptions::PyValueError::new_err(format!("Arc missing weight attribute: {}", e)))?;
            let source_key = source.as_ptr() as usize;
            let target_key = target.as_ptr() as usize;

            if place_idx_map.contains_key(&source_key) {
                let p_idx = *place_idx_map.get(&source_key).unwrap();
                let t_idx = *trans_idx_map.get(&target_key).ok_or_else(|| {
                    pyo3::exceptions::PyValueError::new_err("Arc target not found among transitions")
                })?;
                // Update place-to-transition mappings.
                place2output_transitions[p_idx].push(t_idx);
                transition2input_places[t_idx].push(p_idx);
                transition2input_places_cardinality[t_idx].push(weight);
            
            } else if trans_idx_map.contains_key(&source_key) {
                let t_idx = *trans_idx_map.get(&source_key).unwrap();
                let p_idx = *place_idx_map.get(&target_key).ok_or_else(|| {
                    pyo3::exceptions::PyValueError::new_err("Arc target not found among places")
                })?;
                // Update transition-to-place mappings.
                transition2output_places[t_idx].push(p_idx);
                place2output_transitions[p_idx].push(t_idx);
                transition2output_places_cardinality[t_idx].push(weight);
            } else {
                return Err(pyo3::exceptions::PyValueError::new_err(
                    "Arc source not found in either places or transitions"));
            }
        }

        let _slpn = StochasticLabelledPetriNet {
            activity_key,
            initial_marking,
            labels,
            place2output_transitions,
            transition2input_places,
            transition2output_places,
            transition2input_places_cardinality,
            transition2output_places_cardinality,
            weights,
        };

        //Ok(EbiObject::StochasticLabelledPetriNet(slpn))
        //Ok(slpn)

        for &itype in input_types {
            match itype {
                _ => {
                    // skip other input types
                }
            }
        }

        Err(PyValueError::new_err(
            "This is not implemented yet!! (and PM4Py does not support it)",
        ))
    }
}





// helper functions for event logs
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

// helper functions for petri nets

fn get_collection_as_set<'a>(obj: &'a PyAny, attr: &'a str) -> PyResult<&'a PySet> {
    Ok(obj.getattr(attr)?.downcast::<PySet>()?)
}

fn pyset_as_vec<'a>(set: &'a PySet) -> Vec<&'a PyAny> {
    // Note: sets in Python are unordered.
    set.iter().collect()
}

// ================ Experimentation with passing Logs as DataFrame =================
/// internal function that creates a Polars DataFrame from IPC bytes
fn extract_dataframe(ipc_bytes: &[u8]) -> Result<DataFrame, PolarsError> {
    let cursor = Cursor::new(ipc_bytes);
    // Use the IPC reader to deserialize the data into a DataFrame.
    polars::io::ipc::IpcReader::new(cursor).finish()
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
fn visualise_text(object: &PyAny) -> PyResult<String> {
    let command: &&EbiCommand = &&EBI_VISUALISE_TEXT;
    
    let input_types = match **command {
        EbiCommand::Command { input_types, .. } => input_types,
        _ => return Err(pyo3::exceptions::PyValueError::new_err("Expected a command.")),
    };

    let input = [
        EventLog::import_from_pm4py,
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

#[pyfunction]
pub fn get_log_length(event_log: &PyAny) -> PyResult<usize> {
    let list = event_log.getattr("_list")?.downcast::<PyList>()?;
    Ok(list.len())
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


/// A Python module implemented in Rust. The name of this function must match
/// the `lib.name` setting in the `Cargo.toml`, else Python will not be able to
/// import the module.
#[pymodule]
fn ebi(_py: Python<'_>, m: &PyModule) -> PyResult<()> {
    m.add_function(wrap_pyfunction!(analyse_completeness, m)?)?;
    m.add_function(wrap_pyfunction!(visualise_text, m)?)?;
    m.add_function(wrap_pyfunction!(visualise_svg, m)?)?;
    
    m.add_function(wrap_pyfunction!(get_log_length, m)?)?;
    m.add_function(wrap_pyfunction!(count_traces, m)?)?;
    m.add_function(wrap_pyfunction!(dataframe_head, m)?)?;
    m.add_function(wrap_pyfunction!(eventlog_head, m)?)?;
    Ok(())
}
