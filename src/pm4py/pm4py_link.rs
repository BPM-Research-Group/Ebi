use anyhow::{Result, anyhow};
use polars::prelude::*;
use pyo3::{
    AsPyPointer,
    exceptions::PyValueError,
    prelude::*,
    types::{IntoPyDict, PyAny, PyDict, PyList, PySet, PyString},
};
use std::{
    collections::HashMap,
    io::Cursor,
    iter::Peekable,
    str::{Chars, FromStr},
};

use ebi_arithmetic::{MaybeExact, fraction::fraction::Fraction, is_exact_globally};
use ebi_objects::{
    activity_key::activity_key::ActivityKey,
    constants::{ebi_object::EbiObject, ebi_object_type::EbiObjectType},
    ebi_objects::{
        event_log::EventLog, finite_language::FiniteLanguage,
        finite_stochastic_language::FiniteStochasticLanguage, labelled_petri_net::LabelledPetriNet,
        process_tree::ProcessTree,
    },
    marking::Marking,
};

use crate::ebi_file_handlers::{
    event_log::EBI_EVENT_LOG, finite_language::EBI_FINITE_LANGUAGE,
    finite_stochastic_language::EBI_FINITE_STOCHASTIC_LANGUAGE,
    labelled_petri_net::EBI_LABELLED_PETRI_NET, process_tree::EBI_PROCESS_TREE,
};

use crate::{
    ebi_framework::{
        ebi_command::EbiCommand,
        ebi_input::{EbiInput, EbiInputType},
        ebi_output::{self, EbiOutput},
        ebi_trait::EbiTrait,
        ebi_trait_object::EbiTraitObject,
        prom_link::attempt_parse,
    },
    ebi_traits::ebi_trait_semantics::{EbiTraitSemantics, ToSemantics},
};
use process_mining::event_log::{
    AttributeValue, Attributes, Event, EventLog as ProcessMiningEventLog, Trace,
    event_log_struct::{EventLogClassifier, to_attributes},
};

type Importer = fn(&PyAny, &[&'static EbiInputType]) -> PyResult<EbiInput>;
pub const IMPORTERS: &[Importer] = &[
    usize::import_from_pm4py,
    Fraction::import_from_pm4py,
    EventLog::import_from_pm4py,
    LabelledPetriNet::import_from_pm4py,
    ProcessTree::import_from_pm4py,
];

pub trait ImportableFromPM4Py {
    /// Imports a PM4Py Object as its Ebi equivalent.
    fn import_from_pm4py(
        object: &PyAny,
        input_types: &[&'static EbiInputType],
    ) -> PyResult<EbiInput>;
}
pub trait ExportableToPM4Py {
    /// Exports an Ebi Object as its PM4Py equivalent.
    fn export_to_pm4py(&self, py: Python<'_>) -> PyResult<Py<PyAny>>;
}

impl ExportableToPM4Py for EbiOutput {
    fn export_to_pm4py(&self, py: Python<'_>) -> PyResult<Py<PyAny>> {
        match self {
            EbiOutput::Fraction(frac) => frac.export_to_pm4py(py),
            EbiOutput::Bool(value) => value.export_to_pm4py(py),
            EbiOutput::Usize(value) => value.export_to_pm4py(py),
            EbiOutput::Object(EbiObject::EventLog(event_log)) => {
                // special case: export EventLog directly
                event_log.export_to_pm4py(py)
            }
            // default: no exporter available (not implemented yet or no equivalent in PM4Py) -> return string representation
            EbiOutput::Object(_)
            | EbiOutput::String(_)
            | EbiOutput::LogDiv(_)
            | EbiOutput::ContainsRoot(_)
            | EbiOutput::RootLogDiv(_) => {
                let exporter = EbiCommand::select_exporter(&self.get_type(), None);

                let output_string = if exporter.is_binary() {
                    let bytes =
                        ebi_output::export_to_bytes(self.clone(), exporter).map_err(|e| {
                            pyo3::exceptions::PyException::new_err(format!("Export error: {}", e))
                        })?;
                    String::from_utf8(bytes).map_err(|e| {
                        pyo3::exceptions::PyException::new_err(format!(
                            "UTF8 conversion error: {}",
                            e
                        ))
                    })?
                } else {
                    ebi_output::export_to_string(self.clone(), exporter).map_err(|e| {
                        pyo3::exceptions::PyException::new_err(format!("Export error: {}", e))
                    })?
                };
                let py_str: &PyString = PyString::new(py, &output_string);
                Ok(py_str.into())
            }
        }
    }
}

impl ExportableToPM4Py for bool {
    fn export_to_pm4py(&self, py: Python<'_>) -> PyResult<Py<PyAny>> {
        Ok(self.to_object(py))
    }
}

impl ImportableFromPM4Py for usize {
    fn import_from_pm4py(
        integer: &PyAny,
        input_types: &[&'static EbiInputType],
    ) -> PyResult<EbiInput> {
        let value: usize = integer.extract().map_err(|_| {
            PyValueError::new_err("Expected a Python integer to convert to Rust usize")
        })?;
        for &itype in input_types {
            match itype {
                EbiInputType::Usize(..) => {
                    return Ok(EbiInput::Usize(value, itype));
                }
                _ => {}
            }
        }
        Err(PyValueError::new_err(
            "Integer could not be wrapped as any of the requested EbiInputType variants",
        ))
    }
}

impl ExportableToPM4Py for usize {
    fn export_to_pm4py(&self, py: Python<'_>) -> PyResult<Py<PyAny>> {
        // Convert the Rust `usize` to a Python integer
        Ok(self.to_object(py))
    }
}

impl ImportableFromPM4Py for Fraction {
    fn import_from_pm4py(
        double: &PyAny,
        input_types: &[&'static EbiInputType],
    ) -> PyResult<EbiInput> {
        let value: f64 = double.extract().map_err(|_| {
            PyValueError::new_err("Expected a Python double to convert to Rust usize")
        })?;

        let fraction: Fraction = value.to_string().parse().map_err(|_| {
            PyValueError::new_err("Failed to convert f64 to appropriate Rational type")
        })?;

        for &itype in input_types {
            match itype {
                EbiInputType::Fraction(..) => {
                    if is_exact_globally() {
                        return Ok(EbiInput::Fraction(fraction, itype));
                    } else {
                    }
                }
                _ => { /* skip other input types */ }
            }
        }
        Err(PyValueError::new_err(
            "Integer could not be wrapped as any of the requested EbiInputType variants",
        ))
    }
}

impl ExportableToPM4Py for Fraction {
    fn export_to_pm4py(&self, py: Python<'_>) -> PyResult<Py<PyAny>> {
        if let Ok(rat) = self.exact_ref() {
            // malachite does not offer any direct way to convert Rational to f64, but log can be retrieved as f64
            // we apply exp to get back the original value
            let log_value = rat.approx_log();
            let float_value: f64 = log_value.exp();
            Ok(float_value.to_object(py))
        } else if let Ok(flo) = self.approx_ref() {
            Ok(flo.to_object(py))
        } else {
            Err(PyValueError::new_err(
                "Cannot export a Fraction that combines Exact and Approx",
            ))
        }
    }
}

impl ImportableFromPM4Py for EventLog {
    fn import_from_pm4py(
        event_log: &PyAny,
        input_types: &[&'static EbiInputType],
    ) -> PyResult<EbiInput> {
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
            vec![]
        };
        // Create a ProcessMiningEventLog instance.
        let pm_log = ProcessMiningEventLog {
            attributes,
            traces,
            extensions: None,
            classifiers: None,
            global_trace_attrs: None,
            global_event_attrs: None,
        };
        // For classifier, we use a default one.
        let classifier = EventLogClassifier::default();
        let event_log_rust = EventLog::new(pm_log, classifier);

        for &itype in input_types {
            match itype {
                EbiInputType::Object(obj_type) if *obj_type == EbiObjectType::EventLog => {
                    let obj = EbiObject::EventLog(event_log_rust);
                    return Ok(EbiInput::Object(obj, &EBI_EVENT_LOG));
                }
                EbiInputType::Trait(etrait) => {
                    match etrait {
                        EbiTrait::EventLog => {
                            let tobj = EbiTraitObject::EventLog(Box::new(event_log_rust));
                            return Ok(EbiInput::Trait(tobj, &EBI_EVENT_LOG));
                        }
                        EbiTrait::FiniteStochasticLanguage => {
                            let fsl = Box::new(Into::<FiniteStochasticLanguage>::into(Into::<
                                FiniteStochasticLanguage,
                            >::into(
                                event_log_rust,
                            )));
                            let tobj = EbiTraitObject::FiniteStochasticLanguage(fsl);
                            return Ok(EbiInput::Trait(tobj, &EBI_FINITE_STOCHASTIC_LANGUAGE));
                        }
                        EbiTrait::FiniteLanguage => {
                            let fl = Box::new(Into::<FiniteLanguage>::into(
                                Into::<FiniteLanguage>::into(event_log_rust),
                            ));
                            let tobj = EbiTraitObject::FiniteLanguage(fl);
                            return Ok(EbiInput::Trait(tobj, &EBI_FINITE_LANGUAGE));
                        }
                        // … add more traits here …
                        _ => { /* this trait isn’t supported by EventLog; skip */ }
                    }
                }
                EbiInputType::AnyObject => {
                    let obj = EbiObject::EventLog(event_log_rust);
                    return Ok(EbiInput::Object(obj, &EBI_EVENT_LOG));
                }
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

impl ExportableToPM4Py for EventLog {
    fn export_to_pm4py(&self, py: Python<'_>) -> PyResult<Py<PyAny>> {
        let pm4py_module = py.import("pm4py.objects.log.obj")?;
        let py_event_cls = pm4py_module.getattr("Event")?;
        let py_trace_cls = pm4py_module.getattr("Trace")?;
        let py_log_cls = pm4py_module.getattr("EventLog")?;

        // Collect traces
        let mut py_traces = Vec::with_capacity(self.rust4pm_log.traces.len());

        for (_trace_idx, rust_trace) in self.rust4pm_log.traces.iter().enumerate() {
            // Collect events
            let mut py_events = Vec::with_capacity(rust_trace.events.len());

            for rust_event in &rust_trace.events {
                let py_dict = PyDict::new(py);

                for attr in &rust_event.attributes {
                    let py_val = match &attr.value {
                        AttributeValue::String(s) => s.to_object(py),
                        AttributeValue::Int(i) => i.to_object(py),
                        AttributeValue::Float(f) => f.to_object(py),
                        AttributeValue::Boolean(b) => b.to_object(py),
                        AttributeValue::Date(dt) => dt.to_rfc3339().to_object(py),
                        AttributeValue::ID(id) => id.to_string().to_object(py),
                        AttributeValue::List(list) => {
                            let py_list = PyList::new(
                                py,
                                list.iter()
                                    .map(|a| a.value.export_to_pm4py(py))
                                    .collect::<Result<Vec<_>, _>>()?,
                            );
                            py_list.to_object(py)
                        }
                        AttributeValue::Container(container) => {
                            let py_container = PyDict::new(py);
                            for attr in container.iter() {
                                py_container
                                    .set_item(&attr.key, attr.value.export_to_pm4py(py)?)?;
                            }
                            py_container.to_object(py)
                        }
                        AttributeValue::None() => py.None(),
                    };
                    py_dict.set_item(&attr.key, py_val)?;
                }

                let py_event = py_event_cls.call1((py_dict,))?;
                py_events.push(py_event);
            }

            let py_trace_list = PyList::new(py, py_events);

            // Trace attributes
            let py_trace_attrs = PyDict::new(py);
            for attr in &rust_trace.attributes {
                let py_val = match &attr.value {
                    AttributeValue::String(s) => s.to_object(py),
                    AttributeValue::Int(i) => i.to_object(py),
                    AttributeValue::Float(f) => f.to_object(py),
                    AttributeValue::Boolean(b) => b.to_object(py),
                    AttributeValue::Date(dt) => dt.to_rfc3339().to_object(py),
                    AttributeValue::ID(id) => id.to_string().to_object(py),
                    AttributeValue::List(list) => {
                        let py_list = PyList::new(
                            py,
                            list.iter()
                                .map(|a| a.value.export_to_pm4py(py))
                                .collect::<Result<Vec<_>, _>>()?,
                        );
                        py_list.to_object(py)
                    }
                    AttributeValue::Container(container) => {
                        let py_container = PyDict::new(py);
                        for attr in container.iter() {
                            py_container.set_item(&attr.key, attr.value.export_to_pm4py(py)?)?;
                        }
                        py_container.to_object(py)
                    }
                    AttributeValue::None() => py.None(),
                };
                py_trace_attrs.set_item(&attr.key, py_val)?;
            }

            let py_trace = py_trace_cls.call(
                (py_trace_list,),
                Some([("attributes", py_trace_attrs)].into_py_dict(py)),
            )?;
            py_traces.push(py_trace);
        }

        let py_log_list = PyList::new(py, py_traces);

        // Log attributes
        let py_log_attrs = PyDict::new(py);
        for attr in &self.rust4pm_log.attributes {
            let py_val = match &attr.value {
                AttributeValue::String(s) => s.to_object(py),
                AttributeValue::Int(i) => i.to_object(py),
                AttributeValue::Float(f) => f.to_object(py),
                AttributeValue::Boolean(b) => b.to_object(py),
                AttributeValue::Date(dt) => dt.to_rfc3339().to_object(py),
                AttributeValue::ID(id) => id.to_string().to_object(py),
                AttributeValue::List(list) => {
                    let py_list = PyList::new(
                        py,
                        list.iter()
                            .map(|a| a.value.export_to_pm4py(py))
                            .collect::<Result<Vec<_>, _>>()?,
                    );
                    py_list.to_object(py)
                }
                AttributeValue::Container(container) => {
                    let py_container = PyDict::new(py);
                    for attr in container.iter() {
                        py_container.set_item(&attr.key, attr.value.export_to_pm4py(py)?)?;
                    }
                    py_container.to_object(py)
                }
                AttributeValue::None() => py.None(),
            };
            py_log_attrs.set_item(&attr.key, py_val)?;
        }

        let py_log = py_log_cls.call(
            (py_log_list,),
            Some([("attributes", py_log_attrs)].into_py_dict(py)),
        )?;

        Ok(py_log.into())
    }
}

impl ExportableToPM4Py for AttributeValue {
    fn export_to_pm4py(&self, py: Python<'_>) -> PyResult<Py<PyAny>> {
        match self {
            AttributeValue::String(s) => Ok(s.to_object(py)),
            AttributeValue::Date(dt) => Ok(dt.to_rfc3339().to_object(py)), // export as ISO string
            AttributeValue::Int(i) => Ok(i.to_object(py)),
            AttributeValue::Float(f) => Ok(f.to_object(py)),
            AttributeValue::Boolean(b) => Ok(b.to_object(py)),
            AttributeValue::ID(uuid) => Ok(uuid.to_string().to_object(py)), // export as string
            AttributeValue::List(attrs) => {
                let py_list = PyList::new(
                    py,
                    attrs
                        .iter()
                        .map(|a| a.value.export_to_pm4py(py))
                        .collect::<PyResult<Vec<_>>>()?,
                );
                Ok(py_list.into_py(py))
            }
            AttributeValue::Container(attrs) => {
                let py_dict = PyDict::new(py);
                for attr in attrs {
                    py_dict.set_item(&attr.key, attr.value.export_to_pm4py(py)?)?;
                }
                Ok(py_dict.into_py(py))
            }
            AttributeValue::None() => Ok(py.None()),
        }
    }
}

impl ImportableFromPM4Py for LabelledPetriNet {
    fn import_from_pm4py(pn: &PyAny, input_types: &[&'static EbiInputType]) -> PyResult<EbiInput> {
        // Retrieve "places", "transitions", and "arcs" attributes as sets.
        let py_places = get_collection_as_set(pn, "places").map_err(|e| {
            pyo3::exceptions::PyValueError::new_err(format!("Failed to get places: {}", e))
        })?;
        let py_transitions = get_collection_as_set(pn, "transitions").map_err(|e| {
            pyo3::exceptions::PyValueError::new_err(format!("Failed to get transitions: {}", e))
        })?;
        let py_arcs = get_collection_as_set(pn, "arcs").map_err(|e| {
            pyo3::exceptions::PyValueError::new_err(format!("Failed to get arcs: {}", e))
        })?;

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
        let initial_marking = Marking {
            place2token: vec![0u64; num_places],
        };

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
            let source = arc.getattr("source").map_err(|e| {
                pyo3::exceptions::PyValueError::new_err(format!(
                    "Arc missing source attribute: {}",
                    e
                ))
            })?;
            let target = arc.getattr("target").map_err(|e| {
                pyo3::exceptions::PyValueError::new_err(format!(
                    "Arc missing target attribute: {}",
                    e
                ))
            })?;
            let weight = arc.getattr("weight")?.extract::<u64>().map_err(|e| {
                pyo3::exceptions::PyValueError::new_err(format!(
                    "Arc missing weight attribute: {}",
                    e
                ))
            })?;
            let source_key = source.as_ptr() as usize;
            let target_key = target.as_ptr() as usize;

            if place_idx_map.contains_key(&source_key) {
                let p_idx = *place_idx_map.get(&source_key).unwrap();
                let t_idx = *trans_idx_map.get(&target_key).ok_or_else(|| {
                    pyo3::exceptions::PyValueError::new_err(
                        "Arc target not found among transitions",
                    )
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
                    "Arc source not found in either places or transitions",
                ));
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
                }
                EbiInputType::Trait(etrait) => match etrait {
                    EbiTrait::Semantics => {
                        let sem = EbiTraitSemantics::Marking(Box::new(lpn.clone()));
                        let trait_obj = EbiTraitObject::Semantics(sem);
                        return Ok(EbiInput::Trait(trait_obj, &EBI_LABELLED_PETRI_NET));
                    }
                    EbiTrait::Graphable => {
                        let graph = lpn.clone();
                        let trait_obj = EbiTraitObject::Graphable(Box::new(graph));
                        return Ok(EbiInput::Trait(trait_obj, &EBI_LABELLED_PETRI_NET));
                    }
                    _ => {}
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

// /// StochasticPetriNets are not fully integrated into PM4Py (yet)
// /// this function will remain useless until PM4Py has a proper implementation of Stochastic Petri Nets
// impl ImportableFromPM4Py for StochasticLabelledPetriNet {
//     fn import_from_pm4py(pn: &PyAny, input_types: &[&'static EbiInputType]) -> PyResult<EbiInput> {
//         // Retrieve "places", "transitions", and "arcs" attributes as sets.
//         let py_places = get_collection_as_set(pn, "places").map_err(|e| {
//             pyo3::exceptions::PyValueError::new_err(format!("Failed to get places: {}", e))
//         })?;
//         let py_transitions = get_collection_as_set(pn, "transitions").map_err(|e| {
//             pyo3::exceptions::PyValueError::new_err(format!("Failed to get transitions: {}", e))
//         })?;
//         let py_arcs = get_collection_as_set(pn, "arcs").map_err(|e| {
//             pyo3::exceptions::PyValueError::new_err(format!("Failed to get arcs: {}", e))
//         })?;

//         // Convert sets to vectors.
//         let places: Vec<&PyAny> = pyset_as_vec(py_places);
//         let transitions: Vec<&PyAny> = pyset_as_vec(py_transitions);
//         let arcs: Vec<&PyAny> = pyset_as_vec(py_arcs);

//         let num_places = places.len();
//         let num_transitions = transitions.len();

//         // Create mappings using raw pointers as keys.
//         let mut place_idx_map: HashMap<usize, usize> = HashMap::new();
//         for (i, &place) in places.iter().enumerate() {
//             place_idx_map.insert(place.as_ptr() as usize, i);
//         }
//         let mut trans_idx_map: HashMap<usize, usize> = HashMap::new();
//         for (i, &trans) in transitions.iter().enumerate() {
//             trans_idx_map.insert(trans.as_ptr() as usize, i);
//         }

//         // For this example, assume initial marking is all zeros.
//         let initial_marking = Marking {
//             place2token: vec![0u64; num_places],
//         };

//         // Build labels vector for transitions.
//         let mut activity_key = ActivityKey::new();
//         let mut labels = Vec::with_capacity(num_transitions);
//         let mut weights: Vec<Fraction> = Vec::with_capacity(num_transitions);
//         for trans in transitions.iter() {
//             if let Ok(label_obj) = trans.getattr("label") {
//                 let label_str: String = label_obj.extract().unwrap_or_default();
//                 let act = activity_key.process_activity(&label_str);
//                 labels.push(Some(act));
//             } else {
//                 labels.push(None);
//             }

//             let trans_weight = trans
//                 .getattr("weight")?
//                 .extract::<f64>()
//                 .map(|float_weight| Fraction::Approx(float_weight))
//                 .map_err(|e| {
//                     pyo3::exceptions::PyValueError::new_err(format!(
//                         "Failed to retrieve transition weight: {}",
//                         e
//                     ))
//                 })?;
//             weights.push(trans_weight);
//         }

//         // Initialize mapping vectors.
//         let mut transition2input_places = vec![Vec::new(); num_transitions];
//         let mut transition2output_places = vec![Vec::new(); num_transitions];
//         let mut transition2input_places_cardinality = vec![Vec::new(); num_transitions]; // default cardinality
//         let mut transition2output_places_cardinality = vec![Vec::new(); num_transitions]; // default cardinality
//         let mut place2output_transitions = vec![Vec::new(); num_places];

//         // Process each arc.
//         for arc in arcs {
//             // Each arc should have "source" and "target" attributes.
//             let source = arc.getattr("source").map_err(|e| {
//                 pyo3::exceptions::PyValueError::new_err(format!(
//                     "Arc missing source attribute: {}",
//                     e
//                 ))
//             })?;
//             let target = arc.getattr("target").map_err(|e| {
//                 pyo3::exceptions::PyValueError::new_err(format!(
//                     "Arc missing target attribute: {}",
//                     e
//                 ))
//             })?;
//             let weight = arc.getattr("weight")?.extract::<u64>().map_err(|e| {
//                 pyo3::exceptions::PyValueError::new_err(format!(
//                     "Arc missing weight attribute: {}",
//                     e
//                 ))
//             })?;
//             let source_key = source.as_ptr() as usize;
//             let target_key = target.as_ptr() as usize;

//             if place_idx_map.contains_key(&source_key) {
//                 let p_idx = *place_idx_map.get(&source_key).unwrap();
//                 let t_idx = *trans_idx_map.get(&target_key).ok_or_else(|| {
//                     pyo3::exceptions::PyValueError::new_err(
//                         "Arc target not found among transitions",
//                     )
//                 })?;
//                 // Update place-to-transition mappings.
//                 place2output_transitions[p_idx].push(t_idx);
//                 transition2input_places[t_idx].push(p_idx);
//                 transition2input_places_cardinality[t_idx].push(weight);
//             } else if trans_idx_map.contains_key(&source_key) {
//                 let t_idx = *trans_idx_map.get(&source_key).unwrap();
//                 let p_idx = *place_idx_map.get(&target_key).ok_or_else(|| {
//                     pyo3::exceptions::PyValueError::new_err("Arc target not found among places")
//                 })?;
//                 // Update transition-to-place mappings.
//                 transition2output_places[t_idx].push(p_idx);
//                 place2output_transitions[p_idx].push(t_idx);
//                 transition2output_places_cardinality[t_idx].push(weight);
//             } else {
//                 return Err(pyo3::exceptions::PyValueError::new_err(
//                     "Arc source not found in either places or transitions",
//                 ));
//             }
//         }

//         let _slpn = StochasticLabelledPetriNet {
//             activity_key,
//             initial_marking,
//             labels,
//             place2output_transitions,
//             transition2input_places,
//             transition2output_places,
//             transition2input_places_cardinality,
//             transition2output_places_cardinality,
//             weights,
//         };

//         //Ok(EbiObject::StochasticLabelledPetriNet(slpn))
//         //Ok(slpn)

//         for &itype in input_types {
//             match itype {
//                 _ => {
//                     // skip other input types
//                 }
//             }
//         }

//         Err(PyValueError::new_err(
//             "This is not implemented yet!! (and PM4Py does not support it)",
//         ))
//     }
// }

impl ImportableFromPM4Py for ProcessTree {
    fn import_from_pm4py(
        ptree: &PyAny,
        input_types: &[&'static EbiInputType],
    ) -> PyResult<EbiInput> {
        // 1) Get the `.ptree` text via Python's repr/to_string
        let repr: String = ptree.call_method0("to_string")?.extract().map_err(|e| {
            PyValueError::new_err(format!("Failed to serialize ProcessTree: {}", e))
        })?;

        // 2) Run our text‐based importer
        let text = translate_pm4py_process_tree(&repr)
            .map_err(|e| PyValueError::new_err(format!("Translation error: {}", e)))?;
        // 3) parse
        let rust_tree = ProcessTree::from_str(&text)
            .map_err(|e| PyValueError::new_err(format!("Import fail: {}", e)))?;

        // 3) Dispatch on requested input_types
        for &itype in input_types {
            match itype {
                // raw ProcessTree object
                EbiInputType::Object(obj_ty) if *obj_ty == EbiObjectType::ProcessTree => {
                    return Ok(EbiInput::Object(
                        EbiObject::ProcessTree(rust_tree.clone()),
                        &EBI_PROCESS_TREE,
                    ));
                }

                // as the Semantics trait
                EbiInputType::Trait(EbiTrait::Semantics) => {
                    let tobj = EbiTraitObject::Semantics(rust_tree.to_semantics());
                    return Ok(EbiInput::Trait(tobj, &EBI_PROCESS_TREE));
                }

                // as the Graphable trait
                EbiInputType::Trait(EbiTrait::Graphable) => {
                    let graph = rust_tree.clone();
                    let tobj = EbiTraitObject::Graphable(Box::new(graph));
                    return Ok(EbiInput::Trait(tobj, &EBI_PROCESS_TREE));
                }

                // fallback: treat as any‐object
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
        vec![]
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

// helper functions for process trees
fn translate_pm4py_process_tree(s: &str) -> Result<String, String> {
    // Minimal AST
    enum Node {
        Act(String),
        Tau,
        Op(&'static str, Vec<Node>),
    }
    // Parser with inline utilities
    struct P<'a> {
        it: Peekable<Chars<'a>>,
    }
    impl<'a> P<'a> {
        fn new(s: &'a str) -> Self {
            P {
                it: s.chars().peekable(),
            }
        }
        fn ws(&mut self) {
            while matches!(self.it.peek(), Some(c) if c.is_whitespace()) {
                self.it.next();
            }
        }
        fn eat(&mut self, c: char) -> bool {
            match self.it.peek() {
                Some(&x) if x == c => {
                    self.it.next();
                    true
                }
                _ => false,
            }
        }
        fn parse(&mut self) -> Result<Node, String> {
            self.ws();
            if self.eat('-') {
                self.eat('>');
                return self.op("sequence");
            }
            if self.eat('X') {
                return self.op("xor");
            }
            if self.eat('&') {
                return self.op("concurrent");
            }
            if self.eat('*') {
                return self.op("loop");
            }
            if self.eat('\'') {
                let mut lbl = String::new();
                while let Some(c) = self.it.next() {
                    if c == '\'' {
                        break;
                    } else {
                        lbl.push(c)
                    }
                }
                return Ok(Node::Act(lbl));
            }
            if self.it.clone().take(3).collect::<String>() == "tau" {
                for _ in 0..3 {
                    self.it.next();
                }
                return Ok(Node::Tau);
            }
            Err("Unexpected token".into())
        }
        fn op(&mut self, name: &'static str) -> Result<Node, String> {
            self.ws();
            if !self.eat('(') {
                return Err("Expected '('.".into());
            }
            let mut children = Vec::new();
            loop {
                self.ws();
                if self.eat(')') {
                    break;
                }
                children.push(self.parse()?);
                self.ws();
                let _ = self.eat(',');
            }
            if children.is_empty() {
                return Err(format!("Operator '{}' needs children", name));
            }
            Ok(Node::Op(name, children))
        }
    }
    // format
    fn fmt(n: &Node, indent: usize, out: &mut String) {
        let pad = "\t".repeat(indent);
        match n {
            Node::Act(l) => out.push_str(&format!("{pad}activity {l}\n")),
            Node::Tau => out.push_str(&format!("{pad}tau\n")),
            Node::Op(nm, ch) => {
                out.push_str(&format!("{pad}{nm}\n{pad}{}\n", ch.len()));
                for c in ch {
                    fmt(c, indent + 1, out);
                }
            }
        }
    }
    let mut parser = P::new(s);
    let root = parser.parse()?;
    parser.ws();
    if parser.it.peek().is_some() {
        return Err("Trailing chars".into());
    }
    let mut out = String::from("process tree\n");
    fmt(&root, 0, &mut out);
    Ok(out)
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
                    return Err(anyhow!(
                        "Output type {} does not match the declared output of {}.",
                        result.get_type(),
                        output_type
                    ));
                }
                Ok(result)
            }
            _ => Err(anyhow!("Not a command variant.")),
        }
    }
}

// helper: try import via PM4Py importer, else if it's a str, treat as file path
pub fn import_or_load(
    py_obj: &PyAny,
    input_types: &[&'static EbiInputType],
    index: usize,
) -> PyResult<EbiInput> {
    // 1) try all in‑memory importers
    let input = if let Some(inp) = IMPORTERS
        .iter()
        .find_map(|importer| importer(py_obj, input_types).ok())
    {
        inp
    }
    // 2) if that failed, see if we got a Python str → file path
    else if let Ok(path) = py_obj.extract::<&str>() {
        // read the file
        let content = std::fs::read_to_string(path)
            .map_err(|e| PyValueError::new_err(format!("Failed to read file `{}`: {}", path, e)))?;
        // parse via your CLI-style function
        attempt_parse(input_types, content).map_err(|e| {
            PyValueError::new_err(format!(
                "Failed to parse `{}` as input {}: {}",
                path, index, e
            ))
        })?
    }
    // 3) else, give up
    else {
        return Err(PyValueError::new_err(format!(
            "Could not import argument {}",
            index
        )));
    };

    Ok(input)
}
