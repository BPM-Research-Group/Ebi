use crate::{
    ebi_file_handlers::{
        event_log_python::EBI_EVENT_LOG_PYTHON, labelled_petri_net::EBI_LABELLED_PETRI_NET,
        process_tree::EBI_PROCESS_TREE,
    },
    ebi_framework::{
        ebi_file_handler::EbiFileHandler,
        ebi_input::{EbiInput, EbiInputType},
        ebi_trait::EbiTrait,
        ebi_trait_object::EbiTraitObject,
        object_importers::{
            ToDeterministicFiniteAutomatonObject, ToEventLogObject, ToEventLogXesObject,
            ToFiniteLanguageObject, ToFiniteStochasticLanguageObject, ToLabelledPetriNetObject,
            ToProcessTreeObject, ToStochasticDeterministicFiniteAutomatonObject,
        },
        trait_importers::{
            ToActivitiesTrait, ToEventLogTrait, ToFiniteLanguageTrait,
            ToFiniteStochasticLanguageTrait, ToGraphableTrait, ToIterableLanguageTrait,
            ToIterableStochasticLanguageTrait, ToQueriableStochasticLanguageTrait,
            ToSemanticsTrait, ToStochasticDeterministicSemanticsTrait, ToStochasticSemanticsTrait,
        },
    },
    ebi_traits::ebi_trait_semantics::EbiTraitSemantics,
    text::Joiner,
};
use anyhow::Result;
use ebi_objects::{
    ActivityKey, EbiObject, EbiObjectType, EventLogPython, LabelledPetriNet, ProcessTree,
    ebi_arithmetic::{Fraction, is_exact_globally},
    marking::Marking,
};
use process_mining::core::event_data::case_centric::{
    AttributeValue, Attributes, Event, EventLogClassifier, Trace, to_attributes,
};
use pyo3::{
    Bound, PyAny, PyResult,
    exceptions::PyValueError,
    types::{PyAnyMethods, PyDict, PyDictMethods, PyList, PyListMethods, PySet, PySetMethods},
};
use std::{
    collections::HashMap,
    iter::Peekable,
    str::{Chars, FromStr},
};

type PyImporter = fn(&Bound<'_, PyAny>, &[&'static EbiInputType]) -> PyResult<EbiInput>;

pub const PYTHON_IMPORTERS: &[PyImporter] = &[
    usize::py_import,
    Fraction::py_import,
    EventLogPython::py_import,
    LabelledPetriNet::py_import,
    ProcessTree::py_import,
];

pub trait ImportableFromPM4Py {
    /// Tranform a PM4Py Object into an Ebi struct
    fn py_extract_object(py_object: &Bound<'_, PyAny>) -> PyResult<Self>
    where
        Self: Sized;

    /// Convert an Ebi struct into an Ebi Input, depending on the requested input types
    fn py_object_to_ebi_input(self, input_types: &[&'static EbiInputType]) -> Option<EbiInput>;

    const PY_FILE_HANDLER: Option<&'static EbiFileHandler>;

    /// Imports a PM4Py Object as its Ebi equivalent.
    fn py_import(
        py_object: &Bound<'_, PyAny>,
        input_types: &[&'static EbiInputType],
    ) -> PyResult<EbiInput>
    where
        Self: Sized,
    {
        let ebi_object = Self::py_extract_object(py_object)?;
        match Self::py_object_to_ebi_input(ebi_object, input_types) {
            Some(ebi_input) => Ok(ebi_input),
            None => Err(PyValueError::new_err(format!(
                "Importing of input parameter failed. Expected {}",
                EbiInputType::get_possible_inputs(input_types).join_with(", ", " and ")
            ))),
        }
    }
}

impl ImportableFromPM4Py for usize {
    const PY_FILE_HANDLER: Option<&'static EbiFileHandler> = None;

    fn py_extract_object(py_object: &Bound<'_, PyAny>) -> PyResult<Self> {
        py_object.extract().map_err(|_| {
            PyValueError::new_err("Expected a Python integer to convert to Rust usize")
        })
    }

    fn py_object_to_ebi_input(self, input_types: &[&'static EbiInputType]) -> Option<EbiInput> {
        for &itype in input_types {
            match itype {
                EbiInputType::Usize(..) => {
                    return Some(EbiInput::Usize(self, itype));
                }
                _ => {}
            }
        }
        None
    }
}

impl ImportableFromPM4Py for Fraction {
    const PY_FILE_HANDLER: Option<&'static EbiFileHandler> = None;

    fn py_extract_object(py_object: &Bound<'_, PyAny>) -> PyResult<Self> {
        let value: f64 = py_object
            .extract()
            .map_err(|_| PyValueError::new_err("Expected a Python double"))?;

        value
            .to_string()
            .parse()
            .map_err(|_| PyValueError::new_err("Failed to convert f64 to fraction"))
    }

    fn py_object_to_ebi_input(self, input_types: &[&'static EbiInputType]) -> Option<EbiInput> {
        for &itype in input_types {
            match itype {
                EbiInputType::Fraction(..) => {
                    if is_exact_globally() {
                        return Some(EbiInput::Fraction(self, itype));
                    } else {
                    }
                }
                _ => { /* skip other input types */ }
            }
        }
        None
    }
}

impl ImportableFromPM4Py for EventLogPython {
    const PY_FILE_HANDLER: Option<&'static EbiFileHandler> = Some(&EBI_EVENT_LOG_PYTHON);

    fn py_extract_object(py_object: &Bound<'_, PyAny>) -> PyResult<Self> {
        // Extract the list of traces from the PM4Py event log.
        let binding = py_object.getattr("_list")?;
        let py_traces = binding.cast::<PyList>()?;
        let mut traces = Vec::new();
        for py_trace in py_traces.iter() {
            let trace = trace_from_py(&py_trace)?;
            traces.push(trace);
        }
        // Get log-level attributes, if available.
        let attributes = if let Ok(attr_obj) = py_object.getattr("_attributes") {
            convert_py_dict_to_attributes(&attr_obj)?
        } else {
            vec![]
        };
        // Create a ProcessMiningEventLog instance.
        let pm_log = process_mining::EventLog {
            attributes,
            traces,
            extensions: None,
            classifiers: None,
            global_trace_attrs: None,
            global_event_attrs: None,
        };
        Ok(EventLogPython::from((
            pm_log,
            EventLogClassifier::default(),
        )))
    }

    fn py_object_to_ebi_input(self, input_types: &[&EbiInputType]) -> Option<EbiInput> {
        for &itype in input_types {
            match itype {
                EbiInputType::Object(EbiObjectType::EventLog) => {
                    return Some(EbiInput::Object(
                        self.to_event_log_object(),
                        &Self::PY_FILE_HANDLER.unwrap(),
                    ));
                }
                EbiInputType::Object(EbiObjectType::EventLogXes) => {
                    return Some(EbiInput::Object(
                        self.to_event_log_xes_object(),
                        &Self::PY_FILE_HANDLER.unwrap(),
                    ));
                }
                EbiInputType::Object(EbiObjectType::FiniteLanguage) => {
                    return Some(EbiInput::Object(
                        self.to_finite_language_object(),
                        &Self::PY_FILE_HANDLER.unwrap(),
                    ));
                }
                EbiInputType::Object(EbiObjectType::FiniteStochasticLanguage) => {
                    return Some(EbiInput::Object(
                        self.to_finite_stochastic_language_object(),
                        &Self::PY_FILE_HANDLER.unwrap(),
                    ));
                }
                EbiInputType::Object(EbiObjectType::DeterministicFiniteAutomaton) => {
                    return Some(EbiInput::Object(
                        self.to_deterministic_finite_automaton_object(),
                        &Self::PY_FILE_HANDLER.unwrap(),
                    ));
                }
                EbiInputType::Object(EbiObjectType::StochasticDeterministicFiniteAutomaton) => {
                    return Some(EbiInput::Object(
                        self.to_stochastic_deterministic_finite_automaton_object(),
                        &Self::PY_FILE_HANDLER.unwrap(),
                    ));
                }
                EbiInputType::Trait(etrait) => {
                    match etrait {
                        EbiTrait::Activities => {
                            return Some(EbiInput::Trait(
                                self.to_activities_ebi_trait_object(),
                                &Self::PY_FILE_HANDLER.unwrap(),
                            ));
                        }
                        EbiTrait::EventLog => {
                            return Some(EbiInput::Trait(
                                self.to_event_log_ebi_trait_object(),
                                &Self::PY_FILE_HANDLER.unwrap(),
                            ));
                        }
                        EbiTrait::IterableLanguage => {
                            return Some(EbiInput::Trait(
                                self.to_iterable_language_ebi_trait_object(),
                                &Self::PY_FILE_HANDLER.unwrap(),
                            ));
                        }
                        EbiTrait::IterableStochasticLanguage => {
                            return Some(EbiInput::Trait(
                                self.to_iterable_stochastic_language_ebi_trait_object(),
                                &Self::PY_FILE_HANDLER.unwrap(),
                            ));
                        }
                        EbiTrait::FiniteStochasticLanguage => {
                            return Some(EbiInput::Trait(
                                self.to_finite_stochastic_language_ebi_trait_object(),
                                &Self::PY_FILE_HANDLER.unwrap(),
                            ));
                        }
                        EbiTrait::FiniteLanguage => {
                            return Some(EbiInput::Trait(
                                self.to_finite_language_ebi_trait_object(),
                                &Self::PY_FILE_HANDLER.unwrap(),
                            ));
                        }
                        EbiTrait::QueriableStochasticLanguage => {
                            return Some(EbiInput::Trait(
                                self.to_queriable_stochastic_language_ebi_trait_object(),
                                &Self::PY_FILE_HANDLER.unwrap(),
                            ));
                        }
                        EbiTrait::Semantics => {
                            return Some(EbiInput::Trait(
                                self.to_semantics_ebi_trait_object(),
                                &Self::PY_FILE_HANDLER.unwrap(),
                            ));
                        }
                        EbiTrait::StochasticSemantics => {
                            return Some(EbiInput::Trait(
                                self.to_stochastic_semantics_ebi_trait_object(),
                                &Self::PY_FILE_HANDLER.unwrap(),
                            ));
                        }
                        EbiTrait::StochasticDeterministicSemantics => {
                            return Some(EbiInput::Trait(
                                self.to_stochastic_deterministic_semantics_ebi_trait_object(),
                                &Self::PY_FILE_HANDLER.unwrap(),
                            ));
                        }
                        _ => { /* this trait isn’t supported by EventLog; skip */ }
                    }
                }
                EbiInputType::AnyObject => {
                    let obj = EbiObject::EventLog(self.into());
                    return Some(EbiInput::Object(obj, &Self::PY_FILE_HANDLER.unwrap()));
                }
                _ => {
                    // skip other input types
                }
            }
        }
        None
    }
}

// helper functions for event logs
// A helper to convert a Python dict into our Attributes.
fn convert_py_dict_to_attributes(py_obj: &Bound<'_, PyAny>) -> PyResult<Attributes> {
    let dict = py_obj.cast::<PyDict>()?;
    let mut map = HashMap::new();
    for (key, value) in dict.iter() {
        let key_str: String = key.extract()?;
        // For simplicity, we assume every attribute is a string.
        let value_str: String = value.str()?.to_string();
        map.insert(key_str, AttributeValue::String(value_str));
    }
    Ok(to_attributes(map))
}

// Build an Event from a PyAny that represents a PM4Py Event.
pub fn event_from_py(py_event: &Bound<'_, PyAny>) -> PyResult<Event> {
    // PM4Py Event stores its data in _dict
    let dict_obj = py_event.getattr("_dict")?;
    let attributes = convert_py_dict_to_attributes(&dict_obj)?;
    Ok(Event { attributes })
}

// Build a Trace from a PyAny that represents a PM4Py Trace.
pub fn trace_from_py(py_trace: &Bound<'_, PyAny>) -> PyResult<Trace> {
    let attributes = if let Ok(attr_obj) = py_trace.getattr("_attributes") {
        convert_py_dict_to_attributes(&attr_obj)?
    } else {
        vec![]
    };
    let binding = py_trace.getattr("_list")?;
    let py_events = binding.cast::<PyList>()?;
    let mut events = Vec::new();
    for py_event in py_events.iter() {
        let event = event_from_py(&py_event)?;
        events.push(event);
    }
    Ok(Trace { attributes, events })
}

impl ImportableFromPM4Py for LabelledPetriNet {
    const PY_FILE_HANDLER: Option<&'static EbiFileHandler> = Some(&EBI_LABELLED_PETRI_NET);

    fn py_extract_object(py_object: &Bound<'_, PyAny>) -> PyResult<Self> {
        // Retrieve "places", "transitions", and "arcs" attributes as sets.
        let binding = py_object.getattr("places")?;
        let py_places = binding.cast::<PySet>().map_err(|e| {
            pyo3::exceptions::PyValueError::new_err(format!("Failed to get places: {}", e))
        })?;
        let binding = py_object.getattr("transitions")?;
        let py_transitions = binding.cast::<PySet>().map_err(|e| {
            pyo3::exceptions::PyValueError::new_err(format!("Failed to get transitions: {}", e))
        })?;
        let binding = py_object.getattr("arcs")?;
        let py_arcs = binding.cast::<PySet>().map_err(|e| {
            pyo3::exceptions::PyValueError::new_err(format!("Failed to get arcs: {}", e))
        })?;

        // Convert sets to vectors.
        let places = py_places.iter().collect::<Vec<_>>();
        let transitions = py_transitions.iter().collect::<Vec<_>>();
        let arcs = py_arcs.iter().collect::<Vec<_>>();

        let num_places = places.len();
        let num_transitions = transitions.len();

        // Create mappings using raw pointers as keys.
        let mut place_idx_map: HashMap<usize, usize> = HashMap::new();
        for (i, place) in places.iter().enumerate() {
            place_idx_map.insert(place.as_ptr() as usize, i);
        }
        let mut trans_idx_map: HashMap<usize, usize> = HashMap::new();
        for (i, trans) in transitions.iter().enumerate() {
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

        Ok(LabelledPetriNet {
            activity_key,
            initial_marking,
            labels,
            place2output_transitions,
            transition2input_places,
            transition2output_places,
            transition2input_places_cardinality,
            transition2output_places_cardinality,
        })
    }

    fn py_object_to_ebi_input(self, input_types: &[&'static EbiInputType]) -> Option<EbiInput> {
        for &itype in input_types {
            match itype {
                EbiInputType::Object(EbiObjectType::LabelledPetriNet) => {
                    let obj = EbiObject::LabelledPetriNet(self);
                    return Some(EbiInput::Object(obj, &EBI_LABELLED_PETRI_NET));
                }
                EbiInputType::Trait(etrait) => match etrait {
                    EbiTrait::Activities => {
                        return Some(EbiInput::Trait(
                            self.to_activities_ebi_trait_object(),
                            &Self::PY_FILE_HANDLER.unwrap(),
                        ));
                    }
                    EbiTrait::Semantics => {
                        let sem = EbiTraitSemantics::Marking(Box::new(self));
                        let trait_obj = EbiTraitObject::Semantics(sem);
                        return Some(EbiInput::Trait(trait_obj, &EBI_LABELLED_PETRI_NET));
                    }
                    EbiTrait::Graphable => {
                        let trait_obj = EbiTraitObject::Graphable(Box::new(self));
                        return Some(EbiInput::Trait(trait_obj, &EBI_LABELLED_PETRI_NET));
                    }
                    _ => {}
                },
                EbiInputType::AnyObject => {
                    let obj = EbiObject::LabelledPetriNet(self);
                    return Some(EbiInput::Object(obj, &EBI_LABELLED_PETRI_NET));
                }
                _ => {}
            }
        }
        None
    }
}

impl ImportableFromPM4Py for ProcessTree {
    const PY_FILE_HANDLER: Option<&'static EbiFileHandler> = Some(&EBI_PROCESS_TREE);

    fn py_extract_object(py_object: &Bound<'_, PyAny>) -> PyResult<Self> {
        // 1) Get the `.ptree` text via Python's repr/to_string
        let repr: String = py_object
            .call_method0("to_string")?
            .extract()
            .map_err(|e| {
                PyValueError::new_err(format!("Failed to serialize ProcessTree: {}", e))
            })?;

        // 2) Run our text‐based importer
        let text = translate_pm4py_process_tree(&repr)
            .map_err(|e| PyValueError::new_err(format!("Translation error: {}", e)))?;
        // 3) parse
        let rust_tree = ProcessTree::from_str(&text)
            .map_err(|e| PyValueError::new_err(format!("Import fail: {}", e)))?;

        Ok(rust_tree)
    }

    fn py_object_to_ebi_input(self, input_types: &[&'static EbiInputType]) -> Option<EbiInput> {
        for &itype in input_types {
            match itype {
                // raw ProcessTree object
                EbiInputType::Object(EbiObjectType::ProcessTree) => {
                    return Some(EbiInput::Object(
                        self.to_process_tree_object(),
                        &Self::PY_FILE_HANDLER.unwrap(),
                    ));
                }
                EbiInputType::Object(EbiObjectType::LabelledPetriNet) => {
                    return Some(EbiInput::Object(
                        self.to_labelled_petri_net_object(),
                        &Self::PY_FILE_HANDLER.unwrap(),
                    ));
                }
                EbiInputType::Trait(EbiTrait::Activities) => {
                    return Some(EbiInput::Trait(
                        self.to_activities_ebi_trait_object(),
                        &Self::PY_FILE_HANDLER.unwrap(),
                    ));
                }
                // as the Semantics trait
                EbiInputType::Trait(EbiTrait::Semantics) => {
                    return Some(EbiInput::Trait(
                        self.to_semantics_ebi_trait_object(),
                        &Self::PY_FILE_HANDLER.unwrap(),
                    ));
                }

                // as the Graphable trait
                EbiInputType::Trait(EbiTrait::Graphable) => {
                    return Some(EbiInput::Trait(
                        self.to_graphable_ebi_trait_object(),
                        &Self::PY_FILE_HANDLER.unwrap(),
                    ));
                }

                // fallback: treat as any‐object
                EbiInputType::AnyObject => {
                    return Some(EbiInput::Object(
                        EbiObject::ProcessTree(self),
                        &Self::PY_FILE_HANDLER.unwrap(),
                    ));
                }

                _ => { /* skip unsupported */ }
            }
        }
        None
    }
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

#[cfg(test)]
mod tests {
    use crate::{
        ebi_framework::ebi_input::EbiInputType, python::python_import::ImportableFromPM4Py,
    };
    use ebi_objects::{EventLogPython, EventLogXes, LabelledPetriNet, ProcessTree};
    use std::fs;

    #[test]
    fn py_importers_event_log() {
        let fin = fs::read_to_string("testfiles/a-b.xes").unwrap();
        let log: EventLogPython = fin.parse::<EventLogXes>().unwrap().into();

        //every trait importer must be supported
        for trait_importer in EventLogPython::PY_FILE_HANDLER.unwrap().trait_importers {
            let input_type = Box::new(EbiInputType::Trait(trait_importer.get_trait()));
            let input_type: &'static EbiInputType = Box::leak(input_type);
            let input_types = [input_type];
            println!("trait_importer {}", trait_importer);
            let value = log.clone().py_object_to_ebi_input(&input_types).unwrap();

            //check that the value is of the requested trait
            assert_eq!(
                value.get_type(),
                EbiInputType::Trait(trait_importer.get_trait())
            );
        }

        //every object importer must be supported
        for object_importer in EventLogPython::PY_FILE_HANDLER.unwrap().object_importers {
            let input_type = Box::new(EbiInputType::Object(object_importer.get_type()));
            let input_type: &'static EbiInputType = Box::leak(input_type);
            let input_types = [input_type];
            println!("object_importer {}", object_importer);
            let value = log.clone().py_object_to_ebi_input(&input_types).unwrap();

            //check that the value is of the requested trait
            assert_eq!(
                value.get_type(),
                EbiInputType::Object(object_importer.get_type())
            );
        }
    }

    #[test]
    fn py_importers_event_lpn() {
        let fin = fs::read_to_string("testfiles/a-loop.lpn").unwrap();
        let log = fin.parse::<LabelledPetriNet>().unwrap();

        //every trait importer must be supported
        for trait_importer in LabelledPetriNet::PY_FILE_HANDLER.unwrap().trait_importers {
            let input_type = Box::new(EbiInputType::Trait(trait_importer.get_trait()));
            let input_type: &'static EbiInputType = Box::leak(input_type);
            let input_types = [input_type];
            println!("trait_importer {}", trait_importer);
            let value = log.clone().py_object_to_ebi_input(&input_types).unwrap();

            //check that the value is of the requested trait
            assert_eq!(
                value.get_type(),
                EbiInputType::Trait(trait_importer.get_trait())
            );
        }

        //every object importer must be supported
        for object_importer in LabelledPetriNet::PY_FILE_HANDLER.unwrap().object_importers {
            let input_type = Box::new(EbiInputType::Object(object_importer.get_type()));
            let input_type: &'static EbiInputType = Box::leak(input_type);
            let input_types = [input_type];
            println!("object_importer {}", object_importer);
            let value = log.clone().py_object_to_ebi_input(&input_types).unwrap();

            //check that the value is of the requested trait
            assert_eq!(
                value.get_type(),
                EbiInputType::Object(object_importer.get_type())
            );
        }
    }

    #[test]
    fn py_importers_event_ptree() {
        let fin = fs::read_to_string("testfiles/tree.ptree").unwrap();
        let log = fin.parse::<ProcessTree>().unwrap();

        //every trait importer must be supported
        for trait_importer in ProcessTree::PY_FILE_HANDLER.unwrap().trait_importers {
            let input_type = Box::new(EbiInputType::Trait(trait_importer.get_trait()));
            let input_type: &'static EbiInputType = Box::leak(input_type);
            let input_types = [input_type];
            println!("trait_importer {}", trait_importer);
            let value = log.clone().py_object_to_ebi_input(&input_types).unwrap();

            //check that the value is of the requested trait
            assert_eq!(
                value.get_type(),
                EbiInputType::Trait(trait_importer.get_trait())
            );
        }

        //every object importer must be supported
        for object_importer in ProcessTree::PY_FILE_HANDLER.unwrap().object_importers {
            let input_type = Box::new(EbiInputType::Object(object_importer.get_type()));
            let input_type: &'static EbiInputType = Box::leak(input_type);
            let input_types = [input_type];
            println!("object_importer {}", object_importer);
            let value = log.clone().py_object_to_ebi_input(&input_types).unwrap();

            //check that the value is of the requested trait
            assert_eq!(
                value.get_type(),
                EbiInputType::Object(object_importer.get_type())
            );
        }
    }
}
