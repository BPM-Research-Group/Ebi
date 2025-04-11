use anyhow::{anyhow, Result};
use pyo3::prelude::*;
use pyo3::types::{PyAny, PyDict, PyList, PySet};
use pyo3::AsPyPointer;
use polars::prelude::*;
use pyo3::exceptions::PyValueError;
use std::io::Cursor;
use std::collections::HashMap;

use super::ebi_output::EbiOutput;
use crate::ebi_framework::ebi_object::EbiTraitObject;
use crate::ebi_commands::ebi_command_analyse::EBI_ANALYSE_COMPLETENESS;
use crate::ebi_commands::ebi_command_visualise::{EBI_VISUALISE_TEXT, EBI_VISUALISE_SVG};
use crate::ebi_framework::ebi_command::EbiCommand;
use crate::ebi_framework::{ebi_output, ebi_input::EbiInput, ebi_object::EbiObject, activity_key::ActivityKey};
use crate::ebi_objects::event_log::{EventLog, EBI_EVENT_LOG};
use crate::ebi_objects::labelled_petri_net::{LabelledPetriNet, EBI_LABELLED_PETRI_NET};
use crate::ebi_framework::infoable::Infoable;
use crate::marking::Marking;
use process_mining::event_log::{event_log_struct::{EventLogClassifier, to_attributes}, Attributes, AttributeValue};
use process_mining::event_log::{EventLog as ProcessMiningEventLog, Trace, Event};
use std::io::Write;



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

// impl ImportableFromPM4Py for LabelledPetriNet {
//     fn import_from_pm4py(pn: &PyAny) -> PyResult<EbiObject> {
//         // Get the Petri net name (or use an empty string).
//         let _name: String = pn.getattr("name").and_then(|s| s.extract()).unwrap_or_else(|_| "".into());

//         // Extract collections from the Python object.
//         let py_places = get_collection(pn, "places")
//             .map_err(|e| pyo3::exceptions::PyValueError::new_err(format!("Failed to get places: {}", e)))?;
//         let py_transitions = get_collection(pn, "transitions")
//             .map_err(|e| pyo3::exceptions::PyValueError::new_err(format!("Failed to get transitions: {}", e)))?;
//         let py_arcs = get_collection(pn, "arcs")
//             .map_err(|e| pyo3::exceptions::PyValueError::new_err(format!("Failed to get arcs: {}", e)))?;

//         let num_places = py_places.len();
//         let num_transitions = py_transitions.len();

//         // Convert PyLists into vectors.
//         let places: Vec<&PyAny> = pylist_as_vec(py_places);
//         let transitions: Vec<&PyAny> = pylist_as_vec(py_transitions);

//         // Instead of using &PyAny as keys directly, use their pointer addresses.
//         let mut place_idx_map: HashMap<usize, usize> = HashMap::new();
//         // Destructure so that each 'place' is of type &PyAny.
//         for (i, &place) in places.iter().enumerate() {
//             place_idx_map.insert(place.as_ptr() as usize, i);
//         }
//         let mut trans_idx_map: HashMap<usize, usize> = HashMap::new();
//         for (i, &trans) in transitions.iter().enumerate() {
//             trans_idx_map.insert(trans.as_ptr() as usize, i);
//         }

//         // (The rest of your conversion logic would follow here.)
//         // For example, create initial_marking, labels, etc.

//         // For now, create dummy values:
//         let initial_marking = Marking { place2token: vec![0u64; num_places] };
//         let activity_key = ActivityKey::new();
//         let labels = vec![None; num_transitions];
//         let mut place2output_transitions = vec![Vec::new(); num_places];
//         let mut transition2input_places = vec![Vec::new(); num_transitions];
//         let mut transition2output_places = vec![Vec::new(); num_transitions];
//         let transition2input_places_cardinality = vec![Vec::new(); num_transitions];
//         let transition2output_places_cardinality = vec![Vec::new(); num_transitions];

//         // Process each arc.
//         for arc in pylist_as_vec(py_arcs) {
//             // Get "source" and "target" attributes.
//             let source = arc.getattr("source")
//                 .map_err(|e| pyo3::exceptions::PyValueError::new_err(format!("Arc missing source attribute: {}", e)))?;
//             let target = arc.getattr("target")
//                 .map_err(|e| pyo3::exceptions::PyValueError::new_err(format!("Arc missing target attribute: {}", e)))?;
//             let source_key = source.as_ptr() as usize;
//             let target_key = target.as_ptr() as usize;
        
//             if place_idx_map.contains_key(&source_key) {
//                 let p_idx = *place_idx_map.get(&source_key).unwrap();
//                 let t_idx = *trans_idx_map.get(&target_key).ok_or_else(|| {
//                     pyo3::exceptions::PyValueError::new_err("Arc target not found among transitions")
//                 })?;
        
//                 // Update mappings for place-to-transition arcs.
//                 place2output_transitions[p_idx].push(t_idx);
//                 transition2input_places[t_idx].push(p_idx);
//             } else if trans_idx_map.contains_key(&source_key) {
//                 let t_idx = *trans_idx_map.get(&source_key).unwrap();
//                 let p_idx = *place_idx_map.get(&target_key).ok_or_else(|| {
//                     pyo3::exceptions::PyValueError::new_err("Arc target not found among places")
//                 })?;
        
//                 // Update mappings for transition-to-place arcs.
//                 transition2output_places[t_idx].push(p_idx);
//                 place2output_transitions[p_idx].push(t_idx);
//             } else {
//                 return Err(pyo3::exceptions::PyValueError::new_err(
//                     "Arc source not found in either places or transitions"));
//             }
//         }
        

//         let lpn = LabelledPetriNet {
//             activity_key,
//             initial_marking,
//             labels,
//             place2output_transitions,
//             transition2input_places,
//             transition2output_places,
//             transition2input_places_cardinality,
//             transition2output_places_cardinality,
//         };

//         Ok(EbiObject::LabelledPetriNet(lpn))
//     }
// }


impl ImportableFromPM4Py for LabelledPetriNet {
    fn import_from_pm4py(pn: &PyAny) -> PyResult<EbiObject> {
        // Get the Petri net's name (or use empty string if not provided).
        // let _name: String = pn.getattr("name")
        //     .and_then(|s| s.extract())
        //     .unwrap_or_else(|_| "".into());

        // Now get the "places", "transitions", and "arcs" attributes as sets.
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
        
        // Open a file to write the lengths and first entries.
        let mut file = std::fs::File::create("petri_net_info.txt")
            .map_err(|e| pyo3::exceptions::PyIOError::new_err(format!("Failed to create file: {}", e)))?;

        // Write lengths of places, transitions, and arcs.
        writeln!(file, "Number of places: {}", places.len())
            .map_err(|e| pyo3::exceptions::PyIOError::new_err(format!("Failed to write to file: {}", e)))?;
        writeln!(file, "Number of transitions: {}", transitions.len())
            .map_err(|e| pyo3::exceptions::PyIOError::new_err(format!("Failed to write to file: {}", e)))?;
        writeln!(file, "Number of arcs: {}", arcs.len())
            .map_err(|e| pyo3::exceptions::PyIOError::new_err(format!("Failed to write to file: {}", e)))?;

        // If not empty, write the first entries.
        if !places.is_empty() {
            let first_place = places[0].str()?.to_str()?;
            writeln!(file, "First place: {}", first_place)
            .map_err(|e| pyo3::exceptions::PyIOError::new_err(format!("Failed to write to file: {}", e)))?;
        }
        if !transitions.is_empty() {
            let first_transition = transitions[0].str()?.to_str()?;
            writeln!(file, "First transition: {}", first_transition)
            .map_err(|e| pyo3::exceptions::PyIOError::new_err(format!("Failed to write to file: {}", e)))?;
        }
        if !arcs.is_empty() {
            let first_arc = arcs[0].str()?.to_str()?;
            writeln!(file, "First arc: {}", first_arc)
            .map_err(|e| pyo3::exceptions::PyIOError::new_err(format!("Failed to write to file: {}", e)))?;
        }
        

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
        let mut transition2input_places_cardinality = vec![vec![1; num_places]; num_transitions]; // default cardinality
        let mut transition2output_places_cardinality = vec![vec![1; num_places]; num_transitions]; // default cardinality
        let mut place2output_transitions = vec![Vec::new(); num_places];

        // Process each arc.
        for arc in arcs {
            // Each arc should have "source" and "target" attributes.
            let source = arc.getattr("source")
                .map_err(|e| pyo3::exceptions::PyValueError::new_err(format!("Arc missing source attribute: {}", e)))?;
            let target = arc.getattr("target")
                .map_err(|e| pyo3::exceptions::PyValueError::new_err(format!("Arc missing target attribute: {}", e)))?;
            let weight = arc.getattr("weight")
                .map_err(|e| pyo3::exceptions::PyValueError::new_err(format!("Arc missing target attribute: {}", e)))?;
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
                // If you track cardinalities, update them here.
            } else if trans_idx_map.contains_key(&source_key) {
                let t_idx = *trans_idx_map.get(&source_key).unwrap();
                let p_idx = *place_idx_map.get(&target_key).ok_or_else(|| {
                    pyo3::exceptions::PyValueError::new_err("Arc target not found among places")
                })?;
                // Update transition-to-place mappings.
                transition2output_places[t_idx].push(p_idx);
                place2output_transitions[p_idx].push(t_idx);
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

        Ok(EbiObject::LabelledPetriNet(lpn))
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
    //let event_log_type = EbiInputType::Object(EbiObjectType::EventLog);
    let imported = EventLog::import_from_pm4py(event_log)?;

    let trait_object = match imported {
        EbiObject::EventLog(log) => EbiTraitObject::EventLog(Box::new(log)),
        _ => return Err(pyo3::exceptions::PyValueError::new_err("Expected an EventLog.")),
    };

    let input = EbiInput::Trait(trait_object, &EBI_EVENT_LOG);

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

    let imported = [
        EventLog::import_from_pm4py,
        LabelledPetriNet::import_from_pm4py,
    ]
        .iter()
        .find_map(|importer| importer(object).ok())
        .ok_or_else(|| {
            pyo3::exceptions::PyValueError::new_err("Object could not be imported by any handler")
        })?;

    let input = match &imported {
        EbiObject::EventLog(_log) => EbiInput::Object(imported, &EBI_EVENT_LOG),
        EbiObject::LabelledPetriNet(_pn) => EbiInput::Object(imported, &EBI_LABELLED_PETRI_NET),
        _ => return Err(pyo3::exceptions::PyValueError::new_err("Expected: EventLog or Labelled Petri Net.")),
    };
    

    let inputs = vec![input];

    let command: &&EbiCommand = &&EBI_VISUALISE_TEXT;

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

    // let imported = [
    //     LabelledPetriNet::import_from_pm4py,
    // ]
    //     .iter()
    //     .find_map(|importer| importer(object).ok())
    //     .ok_or_else(|| {
    //         pyo3::exceptions::PyValueError::new_err("Object could not be imported by any handler")
    //     })?;

    let imported = LabelledPetriNet::import_from_pm4py(object)?;


    let trait_object = match imported {
        EbiObject::LabelledPetriNet(pn) => EbiTraitObject::Graphable(Box::new(pn)),
        _ => return Err(pyo3::exceptions::PyValueError::new_err("Expected a Labelled Petri Net.")),
    };

    let input = match &trait_object {
        EbiTraitObject::Graphable(_pn) => EbiInput::Trait(trait_object, &EBI_LABELLED_PETRI_NET),
        _ => return Err(pyo3::exceptions::PyValueError::new_err("Expected: EventLog or Labelled Petri Net.")),
    };
    

    let inputs = vec![input];

    let command: &&EbiCommand = &&EBI_VISUALISE_SVG;

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

#[pyfunction]
pub fn import_petri_net(pn: &PyAny) -> PyResult<String> {
    let ebi_obj = LabelledPetriNet::import_from_pm4py(pn)?;
    if let EbiObject::LabelledPetriNet(pn) = ebi_obj {
        let mut output = Vec::new();
        pn.info(&mut output)
            .map_err(|e| pyo3::exceptions::PyValueError::new_err(format!("Failed to get info: {}", e)))?;
        let info_str = String::from_utf8(output)
            .map_err(|e| pyo3::exceptions::PyValueError::new_err(format!("UTF-8 error: {}", e)))?;
        Ok(info_str)
    } else {
        Err(pyo3::exceptions::PyValueError::new_err("Expected a Petri Net"))
    }
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
    m.add_function(wrap_pyfunction!(import_event_log, m)?)?;
    m.add_function(wrap_pyfunction!(import_petri_net, m)?)?;
    Ok(())
}
