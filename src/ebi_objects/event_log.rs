use core::fmt;
use std::{collections::{HashMap, HashSet}, fmt::Display, io::{self, BufRead, Write}, rc::Rc, str::FromStr, sync::Arc};
use anyhow::{anyhow, Result, Error};
use chrono::{DateTime, Utc};
use process_mining::{event_log::{event_log_struct::EventLogClassifier, AttributeValue}, XESImportOptions};

use crate::{ebi_framework::{activity_key::{Activity, ActivityKey}, ebi_file_handler::EbiFileHandler, ebi_input::{self, EbiObjectImporter, EbiTraitImporter}, ebi_object::EbiObject, ebi_output::{EbiObjectExporter, EbiOutput}, exportable::Exportable, importable::Importable, infoable::Infoable}, ebi_traits::{ebi_trait_event_log::{EbiTraitEventLog, IndexTrace}, ebi_trait_finite_language::EbiTraitFiniteLanguage, ebi_trait_finite_stochastic_language::EbiTraitFiniteStochasticLanguage, ebi_trait_iterable_language::EbiTraitIterableLanguage, ebi_trait_iterable_stochastic_language::EbiTraitIterableStochasticLanguage, ebi_trait_queriable_stochastic_language::EbiTraitQueriableStochasticLanguage, ebi_trait_semantics::Semantics, ebi_trait_stochastic_deterministic_semantics::EbiTraitStochasticDeterministicSemantics, ebi_trait_stochastic_semantics::{EbiTraitStochasticSemantics, StochasticSemantics, ToStochasticSemantics}}, math::fraction::Fraction};

use super::{finite_language::FiniteLanguage, finite_stochastic_language::FiniteStochasticLanguage, finite_stochastic_language_semantics::FiniteStochasticLanguageSemantics, stochastic_deterministic_finite_automaton::StochasticDeterministicFiniteAutomaton};

pub const EBI_EVENT_LOG: EbiFileHandler = EbiFileHandler {
    name: "event log",
    article: "an",
    file_extension: "xes",
    validator: ebi_input::validate::<EventLog>,
    trait_importers: &[
        EbiTraitImporter::FiniteLanguage(EventLog::read_as_finite_language),
        EbiTraitImporter::FiniteStochasticLanguage(EventLog::read_as_finite_stochastic_language),
        EbiTraitImporter::QueriableStochasticLanguage(EventLog::read_as_queriable_stochastic_language),
        EbiTraitImporter::IterableStochasticLanguage(EventLog::read_as_iterable_stochastic_language),
        EbiTraitImporter::EventLog(EventLog::read_as_event_log),
        EbiTraitImporter::StochasticSemantics(EventLog::import_as_stochastic_semantics),
        EbiTraitImporter::StochasticDeterministicSemantics(EventLog::read_as_stochastic_deterministic_semantics)
    ],
    object_importers: &[
        EbiObjectImporter::EventLog(EventLog::import_as_object),
    ],
    object_exporters: &[ 
        EbiObjectExporter::EventLog(EventLog::export_from_object)
    ]
};

pub struct EventLog {
    classifier: EventLogClassifier,
    log: process_mining::EventLog,
    activity_key: ActivityKey,
    traces: Vec<Vec<Activity>>
}

impl EventLog {
    pub fn new(log: process_mining::EventLog, classifier: EventLogClassifier) -> Self {
        let mut result = Self {
            classifier: classifier,
            log: log,
            activity_key: ActivityKey::new(),
            traces: vec![]
        };

        for trace_index in 0..result.log.traces.len() {
            result.traces.push(result.log.traces[trace_index].events.iter().map(|event| result.activity_key.process_activity(&result.classifier.get_class_identity(event))).collect::<Vec<Activity>>());
        }

        result
    }

    pub fn read_as_finite_language(reader: &mut dyn BufRead) -> Result<Box<dyn EbiTraitFiniteLanguage>> {
        let event_log = EventLog::import(reader)?;
        Ok(Box::new(event_log.get_finite_language()))
    }

    pub fn read_as_finite_stochastic_language(reader: &mut dyn BufRead) -> Result<Box<dyn EbiTraitFiniteStochasticLanguage>> {
        let event_log = EventLog::import(reader)?;
        Ok(Box::new(Into::<FiniteStochasticLanguage>::into(event_log.get_finite_stochastic_language())))
    }

    pub fn read_as_queriable_stochastic_language(reader: &mut dyn BufRead) -> Result<Box<dyn EbiTraitQueriableStochasticLanguage>> {
        let event_log = EventLog::import(reader)?;
        Ok(Box::new(event_log.get_finite_stochastic_language()))
    }

    pub fn read_as_iterable_stochastic_language(reader: &mut dyn BufRead) -> Result<Box<dyn EbiTraitIterableStochasticLanguage>> {
        let event_log = EventLog::import(reader)?;
        Ok(Box::new(Into::<FiniteStochasticLanguage>::into(event_log.get_finite_stochastic_language())))
    }

    pub fn read_as_event_log(reader: &mut dyn BufRead) -> Result<Box<dyn EbiTraitEventLog>> {
        let event_log = EventLog::import(reader)?;
        Ok(Box::new(event_log))
    }

    pub fn read_as_stochastic_deterministic_semantics(reader: &mut dyn BufRead) -> Result<EbiTraitStochasticDeterministicSemantics> {
        let event_log = EventLog::import(reader)?;
        let sdfa = event_log.to_stochastic_deterministic_finite_automaton();
        let semantics = StochasticDeterministicFiniteAutomaton::get_deterministic_semantics(Arc::new(sdfa))?;
        Ok(EbiTraitStochasticDeterministicSemantics::Usize(semantics))
    }

    pub fn get_finite_language(&self) -> FiniteLanguage {
        log::info!("create finite language");

        let mut map: HashSet<Vec<String>> = HashSet::new();
        for t in &self.log.traces {
            let trace = t.events.iter().map(|event| self.classifier.get_class_identity(event)).collect::<Vec<String>>();
            map.insert(trace);
        }

        FiniteLanguage::from(map)
    }

    pub fn get_finite_stochastic_language(&self) -> FiniteStochasticLanguage {
        log::info!("create stochastic language");
        let mut map = HashMap::new();
        for t in &self.log.traces {
            let trace = t.events.iter().map(|event| self.classifier.get_class_identity(event)).collect::<Vec<String>>();
            match map.entry(trace) {
                std::collections::hash_map::Entry::Occupied(mut e) => {*e.get_mut() += Fraction::one();()},
                std::collections::hash_map::Entry::Vacant(e) => {e.insert(Fraction::one());()},
            }
        }

        map.into()
    }

    pub fn to_stochastic_deterministic_finite_automaton(&self) -> StochasticDeterministicFiniteAutomaton {
        log::info!("convert event log to sdfa");

        let mut result = StochasticDeterministicFiniteAutomaton::new();
        let mut final_states = HashMap::new();

        //create automaton
        for trace_index in 0..self.log.traces.len() {
            let trace = self.read_trace_with_activity_key(result.get_activity_key_mut(), &trace_index);
            let mut state = result.get_initial_state();

            for activity in trace {
                state = result.take_or_add_transition(state, activity, Fraction::one());
            }

            match final_states.entry(state) {
                std::collections::hash_map::Entry::Occupied(mut e) => {*e.get_mut() += Fraction::one()},
                std::collections::hash_map::Entry::Vacant(e) => {e.insert(Fraction::one());},
            }
        }

        //count
        let mut sums = final_states;
        for (source, _, _, probability) in &result {
            match sums.entry(*source) {
                std::collections::hash_map::Entry::Occupied(mut e) => {*e.get_mut() += probability},
                std::collections::hash_map::Entry::Vacant(e) => {e.insert(probability.clone());},
            }
        }

        //normalise
        result.scale_outgoing_probabilities(sums);

        result
    }
}

impl ToStochasticSemantics for EventLog {
    type State = <FiniteStochasticLanguageSemantics as Semantics>::State;

    fn get_stochastic_semantics(&self) -> Box<dyn StochasticSemantics<State = Self::State>> {
        Box::new(FiniteStochasticLanguageSemantics::from_language((&self.get_finite_stochastic_language()).into()))
    }

    fn import_as_stochastic_semantics(reader: &mut dyn BufRead) -> Result<EbiTraitStochasticSemantics> {
        let log = Self::import(reader)?;
        let s = Rc::new(log);
        Ok(EbiTraitStochasticSemantics::Usize(s.get_stochastic_semantics()))
    }
}

impl Importable for EventLog {
    fn import_as_object(reader: &mut dyn BufRead) -> Result<EbiObject> {
        Ok(EbiObject::EventLog(Self::import(reader)?))
    }

    fn import(reader: &mut dyn BufRead) -> anyhow::Result<Self> where Self: Sized {
        let log = process_mining::event_log::import_xes::import_xes(reader, XESImportOptions::default());
        if log.is_err() {
            return Err(anyhow!("{}", log.err().unwrap()))
        }
        let log = log.unwrap();
        let classifier = EventLogClassifier{name: "concept:name".to_string(), keys: vec!["concept:name".to_string()]};
        if log.traces.is_empty() {
            return Err(anyhow!("event log has no traces"));
        }
        Ok(EventLog::new(log, classifier))
    }
}

impl FromStr for EventLog {
    type Err = Error;

    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        let mut reader = io::Cursor::new(s);
        Self::import(&mut reader)
    }
}

impl Exportable for EventLog {
    fn export_from_object(object: EbiOutput, f: &mut dyn Write) -> Result<()> {
        match object {
            EbiOutput::Object(EbiObject::EventLog(log)) => log.export(f),
            _ => unreachable!()
        }
    }

    fn export(&self, f: &mut dyn std::io::Write) -> Result<()> {
        process_mining::event_log::export_xes::export_xes_event_log(f, &self.log)?;
        Ok(())
    }
}

impl fmt::Display for EventLog {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "event log with {} traces", self.log.traces.len())
    }
}

impl Infoable for EventLog {
    fn info(&self, f: &mut impl std::io::Write) -> Result<()> {
        let lang = self.get_finite_language();
        writeln!(f, "Number of traces\t{}", self.log.traces.len())?;
        writeln!(f, "Number of events\t{}", self.log.traces.iter().map(|t| t.events.len()).sum::<usize>())?;
        writeln!(f, "Number of unique traces\t{}", lang.len())?;
        writeln!(f, "Number of activities\t{}", lang.get_activity_key().get_number_of_activities())?;

        let trace_atts = self.get_trace_attributes();
        let t: Vec<String> = trace_atts.iter().map(|(att, data_type)| format!("{}\t{}", att, data_type) ).collect();
        writeln!(f, "Trace attributes:")?;
        writeln!(f, "\t{}", t.join("\n\t"))?;

        Ok(write!(f, "")?)
    }
}

impl EbiTraitEventLog for EventLog {
    fn get_log(&self) -> &process_mining::EventLog {
        &self.log
    }

    fn get_trace_attributes(&self) -> HashMap<String, DataType> {
        let mut map: HashMap<String, DataType> = HashMap::new();
        for trace in &self.log.traces {
            for attribute in &trace.attributes {
                match map.entry(attribute.key.clone()) {
                    std::collections::hash_map::Entry::Occupied(mut e) => {e.get_mut().update(&attribute.value);()},
                    std::collections::hash_map::Entry::Vacant(e) => {e.insert(DataType::init(&attribute.value));()},
                }
            }
        }
        map
    }

    fn read_trace_with_activity_key(&self, activity_key: &mut ActivityKey, trace_index: &usize) -> Vec<Activity> {
        self.log.traces[*trace_index].events.iter().map(|event| activity_key.process_activity(&self.classifier.get_class_identity(event))).collect::<Vec<Activity>>()
    }
}

impl IndexTrace for EventLog {
    fn len(&self) -> usize {
        self.log.traces.len()
    }

    fn get_trace(&self, trace_index: usize) -> Option<&Vec<Activity>> {
        self.traces.get(trace_index)
    }
}

#[derive(Debug)]
pub enum DataType {
    Categorical,
    Numerical(Fraction, Fraction), //minimum, maximum
    Time(DateTime<Utc>, DateTime<Utc>), //minimum, maximum
    Undefined
}

impl DataType {
    fn init(value: &AttributeValue) -> Self {
        match value {
            AttributeValue::String(x) => 
                if let Ok(v) = x.parse::<Fraction>() {
                    Self::Numerical(v.clone(), v)
                } else if let Ok(v) = x.parse::<DateTime<Utc>>() {
                    Self::Time(v, v)
                } else {
                    Self::Categorical
                },
            AttributeValue::Date(x) => Self::Time(*x, *x),
            AttributeValue::Int(x) => Self::Numerical(Fraction::from(*x), Fraction::from(*x)),
            AttributeValue::Float(x) => Self::Numerical(x.to_string().parse::<Fraction>().unwrap(), x.to_string().parse::<Fraction>().unwrap()),
            AttributeValue::Boolean(_) => Self::Undefined,
            AttributeValue::ID(_) => Self::Undefined,
            AttributeValue::List(_) => Self::Undefined,
            AttributeValue::Container(_) => Self::Undefined,
            AttributeValue::None() => Self::Undefined,
        }
    }

    fn update(&mut self, value: &AttributeValue) {
        *self = match (&self, value) {
            (DataType::Categorical, AttributeValue::String(_)) => Self::Categorical,
            (DataType::Categorical, AttributeValue::Date(_)) => Self::Undefined,
            (DataType::Categorical, AttributeValue::Int(_)) => Self::Undefined,
            (DataType::Categorical, AttributeValue::Float(_)) => Self::Undefined,
            (DataType::Categorical, AttributeValue::Boolean(_)) => Self::Undefined,
            (DataType::Categorical, AttributeValue::ID(_)) => Self::Undefined,
            (DataType::Categorical, AttributeValue::List(_)) => Self::Undefined,
            (DataType::Categorical, AttributeValue::Container(_)) => Self::Undefined,
            (DataType::Categorical, AttributeValue::None()) => Self::Categorical,
            (DataType::Numerical(min, max), AttributeValue::String(s)) => {
                if let Ok(x1) = s.parse::<Fraction>() {Self::Numerical(x1.clone().min(min.clone()), x1.max(max.clone()))} else {Self::Undefined}
            },
            (DataType::Numerical(_, _), AttributeValue::Date(_)) => Self::Undefined,
            (DataType::Numerical(min, max), AttributeValue::Int(y)) => Self::Numerical(min.min(&mut Fraction::from(*y)).clone(), max.max(&mut Fraction::from(*y)).clone()),
            (DataType::Numerical(min, max), AttributeValue::Float(y)) => Self::Numerical(min.min(&mut y.to_string().parse::<Fraction>().unwrap()).clone(), max.max(&mut y.to_string().parse::<Fraction>().unwrap()).clone()),
            (DataType::Numerical(_, _), AttributeValue::Boolean(_)) => Self::Undefined,
            (DataType::Numerical(_, _), AttributeValue::ID(_)) => Self::Undefined,
            (DataType::Numerical(_, _), AttributeValue::List(_)) => Self::Undefined,
            (DataType::Numerical(_, _), AttributeValue::Container(_)) => Self::Undefined,
            (DataType::Numerical(min, max), AttributeValue::None()) => Self::Numerical(min.clone(), max.clone()),
            (DataType::Time(min, max), AttributeValue::String(s)) => 
                if let Ok(v) = s.parse::<DateTime<Utc>>() {
                    Self::Time(v.min(*min), v.max(*max))
                } else {
                    Self::Categorical
                },
            (DataType::Time(min, max), AttributeValue::Date(y)) => Self::Time(*min.min(y), *max.max(y)),
            (DataType::Time(_, _), AttributeValue::Int(_)) => Self::Undefined,
            (DataType::Time(_, _), AttributeValue::Float(_)) => Self::Undefined,
            (DataType::Time(_, _), AttributeValue::Boolean(_)) => Self::Undefined,
            (DataType::Time(_, _), AttributeValue::ID(_)) => Self::Undefined,
            (DataType::Time(_, _), AttributeValue::List(_)) => Self::Undefined,
            (DataType::Time(_, _), AttributeValue::Container(_)) => Self::Undefined,
            (DataType::Time(x, y), AttributeValue::None()) => Self::Time(*x, *y),
            (DataType::Undefined, AttributeValue::String(_)) => Self::Undefined,
            (DataType::Undefined, AttributeValue::Date(_)) => Self::Undefined,
            (DataType::Undefined, AttributeValue::Int(_)) => Self::Undefined,
            (DataType::Undefined, AttributeValue::Float(_)) => Self::Undefined,
            (DataType::Undefined, AttributeValue::Boolean(_)) => Self::Undefined,
            (DataType::Undefined, AttributeValue::ID(_)) => Self::Undefined,
            (DataType::Undefined, AttributeValue::List(_)) => Self::Undefined,
            (DataType::Undefined, AttributeValue::Container(_)) => Self::Undefined,
            (DataType::Undefined, AttributeValue::None()) => Self::Undefined,
        };
    }
}

impl Display for DataType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            DataType::Categorical => write!(f, "categorical"),
            DataType::Numerical(min, max) => write!(f, "numerical between {} and {}", min, max),
            DataType::Time(min, max) => write!(f, "time between {} and {}", min, max),
            DataType::Undefined => write!(f, "undefined"),
        }
    }
}