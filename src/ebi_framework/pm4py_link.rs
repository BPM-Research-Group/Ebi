use anyhow::{anyhow, Result};
use pyo3::prelude::*;
use pyo3::types::{PyAny, PyDict, PyList, PySet};
use pyo3::AsPyPointer;
use polars::prelude::*;
use pyo3::exceptions::PyValueError;
use std::io::Cursor;
use std::collections::HashMap;

use crate::ebi_objects::process_tree::ProcessTree;
use crate::math::fraction::Fraction;
use super::ebi_output::EbiOutput;
use crate::ebi_framework::ebi_object::EbiTraitObject;
use crate::ebi_framework::ebi_command::EbiCommand;
use crate::ebi_framework::{ebi_input::{EbiInput, EbiInputType}, ebi_object::{EbiObject, EbiObjectType}, ebi_trait::EbiTrait, activity_key::ActivityKey};
use crate::ebi_objects::event_log::{EventLog, EBI_EVENT_LOG};
use crate::ebi_objects::labelled_petri_net::{LabelledPetriNet, EBI_LABELLED_PETRI_NET};
use crate::ebi_traits::ebi_trait_semantics::{EbiTraitSemantics, ToSemantics};
use crate::ebi_objects::{stochastic_labelled_petri_net::StochasticLabelledPetriNet, finite_stochastic_language::EBI_FINITE_STOCHASTIC_LANGUAGE, process_tree::{Node, Operator, EBI_PROCESS_TREE}};
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
                EbiInputType::AnyObject => {
                    let obj = EbiObject::EventLog(event_log_rust);
                    return Ok(EbiInput::Object(obj, &EBI_EVENT_LOG));
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
                EbiInputType::AnyObject => {
                    let obj = EbiObject::LabelledPetriNet(lpn);
                    return Ok(EbiInput::Object(obj, &EBI_LABELLED_PETRI_NET));
                }
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

impl ImportableFromPM4Py for ProcessTree {
    fn import_from_pm4py(
        ptree: &PyAny,
        input_types: &[&EbiInputType],
    ) -> PyResult<EbiInput> {
        // 1) Recursively flatten the Python ProcessTree into Vec<Node>
        let mut key = crate::ebi_framework::activity_key::ActivityKey::new();
        let mut flat: Vec<Node> = Vec::new();
        fn walk(
            py_node: &PyAny,
            flat: &mut Vec<Node>,
            key: &mut crate::ebi_framework::activity_key::ActivityKey,
        ) -> PyResult<()> {
            // children list
            let py_children = py_node.getattr("_children")?
                .downcast::<PyList>()
                .map_err(|e| PyValueError::new_err(format!("_children is not a list: {}", e)))?;
            let n_child = py_children.len();

            if n_child == 0 {
                // leaf: check label
                let py_label = py_node.getattr("_label")?;
                if !py_label.is_none() {
                    let label: String = py_label
                        .extract()
                        .map_err(|e| PyValueError::new_err(format!("Label not string: {}", e)))?;
                    let act = key.process_activity(&label);
                    flat.push(Node::Activity(act));
                } else {
                    flat.push(Node::Tau);
                }
            } else {
                // operator node
                // extract operator enum
                let op_str: String = py_node
                    .getattr("_operator")?
                    .extract()
                    .map_err(|e| PyValueError::new_err(format!("Operator not string: {}", e)))?;
                let op = op_str
                    .parse::<Operator>()
                    .map_err(|e| PyValueError::new_err(format!("Bad operator `{}`: {}", op_str, e)))?;
                flat.push(Node::Operator(op, n_child));
                // recurse children
                for child in py_children.iter() {
                    walk(child, flat, key)?;
                }
            }
            Ok(())
        }
        walk(ptree, &mut flat, &mut key)
            .map_err(|e| PyValueError::new_err(format!("Failed walking ProcessTree: {}", e)))?;

        // 2) Build the Rust ProcessTree
        let rust_tree = ProcessTree::new(key, flat);

        // 3) Dispatch on input_types
        for &itype in input_types {
            match itype {
                // raw object
                EbiInputType::Object(EbiObjectType::ProcessTree) => {
                    return Ok(EbiInput::Object(
                        EbiObject::ProcessTree(rust_tree.clone()),
                        &EBI_PROCESS_TREE,
                    ));
                }
                // Semantics trait
                EbiInputType::Trait(EbiTrait::Semantics) => {
                    let sem = rust_tree.to_semantics();
                    let sem_obj = EbiTraitObject::Semantics(sem);
                    return Ok(EbiInput::Trait(sem_obj, &EBI_PROCESS_TREE));
                }
                // Graphable trait
                EbiInputType::Trait(EbiTrait::Graphable) => {
                    let graph_obj = EbiTraitObject::Graphable(Box::new(rust_tree.clone()));
                    return Ok(EbiInput::Trait(graph_obj, &EBI_PROCESS_TREE));
                }
                // fallback any-object
                EbiInputType::AnyObject => {
                    return Ok(EbiInput::Object(
                        EbiObject::ProcessTree(rust_tree.clone()),
                        &EBI_PROCESS_TREE,
                    ));
                }
                _ => { /* skip unsupported */ }
            }
        }

        Err(PyValueError::new_err(
            "ProcessTree could not be wrapped as any of the requested EbiInputType variants",
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
fn _extract_dataframe(ipc_bytes: &[u8]) -> Result<DataFrame, PolarsError> {
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

