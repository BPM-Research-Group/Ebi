use anyhow::{Result, anyhow};
use chrono::{DateTime, FixedOffset};
use ebi_arithmetic::Fraction;
use ebi_objects::{Activity, CompressedEventLog, EventLog, HasActivityKey, Importable, IndexTrace};
use process_mining::event_log::{AttributeValue, Event, XESEditableAttribute};
use std::{
    borrow::Borrow,
    collections::HashMap,
    fmt::{Debug, Display},
    hash::Hash,
    io::BufRead,
};

use crate::{
    ebi_framework::{
        ebi_input::EbiInput, ebi_trait::FromEbiTraitObject, ebi_trait_object::EbiTraitObject,
    },
    math::data_type::DataType,
};

pub const ATTRIBUTE_TIME: &str = "time:timestamp";

pub trait EbiTraitEventLog: IndexTrace + HasActivityKey {
    /// Provides read-only access to the underlying Rust4pm data structure. Prefer to use other methods if possible.
    fn get_log(&self) -> &process_mining::EventLog;

    fn get_trace_attributes(&self) -> HashMap<String, DataType>;

    /// Remove traces for which the function returns false.
    ///
    /// === Does not change the underlying rust4pm data structure. ===
    /// That is, traces removed or altered here will still be exported in their original form.
    /// To remove/alter traces in the underlying rust4pm data structure, use the retain_traces_mut method from the EventLog struct.
    ///
    /// Note to callers: please put the closure definition inside the Box::new in the call of retain_traces.
    /// Otherwise, Rust may give weird compile errors.
    fn retain_traces<'a>(&'a mut self, f: Box<dyn Fn(&Vec<Activity>) -> bool + 'static>);
}

impl dyn EbiTraitEventLog {
    pub fn to_multiset(&self) -> HashMap<Vec<Activity>, usize> {
        let mut map = HashMap::new();
        let mut trace_index = 0;
        while let Some(trace) = self.get_trace(trace_index) {
            match map.entry(trace.clone()) {
                std::collections::hash_map::Entry::Occupied(mut e) => {
                    *e.get_mut() += 1;
                    ()
                }
                std::collections::hash_map::Entry::Vacant(e) => {
                    e.insert(1);
                    ()
                }
            }

            trace_index += 1;
        }
        map
    }

    /**
     * Returns a map from trace to with which attributes that trace appeared how often
     */
    pub fn get_traces_with_categorical_attributes(
        &self,
        attribute_key: &mut AttributeKey,
        trace_attribute: &String,
    ) -> HashMap<&Vec<Activity>, HashMap<Attribute, u64>> {
        let mut result = HashMap::new();

        let mut trace_index = 0;
        while let Some(trace) = self.get_trace(trace_index) {
            if let Some(attribute) =
                self.get_trace_attribute_categorical(trace_index, &trace_attribute)
            {
                //the trace has an attribute

                let attribute_id = attribute_key.process_attribute(&attribute);
                match result.entry(trace) {
                    std::collections::hash_map::Entry::Occupied(mut e) => {
                        //we have seen this trace before
                        let attributes: &mut HashMap<Attribute, u64> = e.get_mut();
                        match attributes.entry(attribute_id) {
                            std::collections::hash_map::Entry::Occupied(mut ae) => {
                                //attribute was seen before
                                *ae.get_mut() += 1;
                            }
                            std::collections::hash_map::Entry::Vacant(e) => {
                                //attribute was not seen before
                                e.insert(1);
                            }
                        }
                    }
                    std::collections::hash_map::Entry::Vacant(e) => {
                        //trace was not seen before
                        let mut map = HashMap::new();
                        map.insert(attribute_id, 1);
                        e.insert(map);
                    }
                }
            }

            trace_index += 1;
        }

        result
    }

    pub fn get_language_mapped(&self) -> (HashMap<Vec<Activity>, usize>, Vec<usize>) {
        let mut trace2language_index = HashMap::new();
        let mut language_index2trace = vec![];
        let mut log_index2language_index = vec![];
        let mut next_language_index = 0;
        let mut trace_index = 0;
        while let Some(trace) = self.get_trace(trace_index) {
            let language_index = match trace2language_index.entry(trace.clone()) {
                std::collections::hash_map::Entry::Occupied(e) => {
                    //we have seen this trace before
                    *e.get()
                }
                std::collections::hash_map::Entry::Vacant(e) => {
                    //we have not seen this trace before
                    let language_index = next_language_index;
                    next_language_index += 1;
                    e.insert(language_index);
                    language_index
                }
            };
            log_index2language_index.push(language_index);
            language_index2trace.push(trace);

            trace_index += 1;
        }

        for (trace, language_index) in &trace2language_index {
            log::info!("language index {} \ttrace {:?}", language_index, trace);
        }

        (trace2language_index, log_index2language_index)
    }

    pub fn get_trace_attribute_categorical<'a>(
        &'a self,
        trace_index: usize,
        trace_attribute: &String,
    ) -> Option<String> {
        if let Some(attribute) = self.get_log().traces[trace_index]
            .attributes
            .get_by_key(trace_attribute)
        {
            match &attribute.value {
                AttributeValue::String(x) => {
                    return Some(x.to_owned());
                }
                AttributeValue::Date(_) => (),
                AttributeValue::Int(x) => {
                    return Some(x.to_string());
                }
                AttributeValue::Float(x) => {
                    return Some(x.to_string());
                }
                AttributeValue::Boolean(x) => {
                    return Some(x.to_string());
                }
                AttributeValue::ID(_) => (),
                AttributeValue::List(_) => (),
                AttributeValue::Container(_) => (),
                AttributeValue::None() => (),
            };
        }
        None
    }

    pub fn get_trace_attribute_numeric(
        &self,
        trace_index: usize,
        case_attribute: &String,
    ) -> Option<Fraction> {
        if let Some(attribute) = self.get_log().traces[trace_index]
            .attributes
            .get_by_key(case_attribute)
        {
            match &attribute.value {
                AttributeValue::String(x) => {
                    return Some(x.parse::<Fraction>().expect("this should not fail"));
                }
                AttributeValue::Date(_) => (),
                AttributeValue::Int(x) => {
                    return Some(Fraction::from(*x));
                }
                AttributeValue::Float(x) => {
                    return Some(
                        x.to_string()
                            .parse::<Fraction>()
                            .expect("this should not fail"),
                    );
                }
                AttributeValue::Boolean(_) => (),
                AttributeValue::ID(_) => (),
                AttributeValue::List(_) => (),
                AttributeValue::Container(_) => (),
                AttributeValue::None() => (),
            };
        }
        None
    }

    pub fn get_trace_attribute_time(
        &self,
        trace_index: usize,
        case_attribute: &String,
    ) -> Option<(DateTime<FixedOffset>, Vec<Activity>)> {
        if let Some(attribute) = self.get_log().traces[trace_index]
            .attributes
            .get_by_key(case_attribute)
        {
            match &attribute.value {
                AttributeValue::String(x) => {
                    return Some((
                        x.parse::<DateTime<FixedOffset>>()
                            .expect("this should not fail"),
                        self.get_trace(trace_index)?.to_owned(),
                    ));
                }
                AttributeValue::Date(x) => {
                    return Some((*x, self.get_trace(trace_index)?.to_owned()));
                }
                AttributeValue::Int(_) => (),
                AttributeValue::Float(_) => (),
                AttributeValue::Boolean(_) => (),
                AttributeValue::ID(_) => (),
                AttributeValue::List(_) => (),
                AttributeValue::Container(_) => (),
                AttributeValue::None() => (),
            };
        }
        None
    }

    fn get_event(&self, trace_index: usize, event_index: usize) -> Option<&Event> {
        self.get_log()
            .traces
            .get(trace_index)?
            .events
            .get(event_index)
    }

    pub fn get_event_attribute_time(
        &self,
        trace_index: usize,
        event_index: usize,
        case_attribute: &String,
    ) -> Option<DateTime<FixedOffset>> {
        if let Some(attribute) = self
            .get_event(trace_index, event_index)?
            .attributes
            .get_by_key(case_attribute)
        {
            match &attribute.value {
                AttributeValue::String(x) => x.parse::<DateTime<FixedOffset>>().ok(),
                AttributeValue::Date(x) => Some(*x),
                _ => None,
            }
        } else {
            None
        }
    }
}

impl FromEbiTraitObject for dyn EbiTraitEventLog {
    fn from_trait_object(object: EbiInput) -> Result<Box<Self>> {
        match object {
            EbiInput::Trait(EbiTraitObject::EventLog(e), _) => Ok(e),
            _ => Err(anyhow!(
                "cannot read {} {} as an event log",
                object.get_type().get_article(),
                object.get_type()
            )),
        }
    }
}

#[derive(Clone, Copy)]
pub struct Attribute {
    id: usize,
}

impl PartialEq for Attribute {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id
    }
}

impl PartialEq<usize> for Attribute {
    fn eq(&self, other: &usize) -> bool {
        &self.id == other
    }
}

impl Eq for Attribute {}

impl Hash for Attribute {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.id.hash(state);
    }
}

impl Display for Attribute {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "at{}", self.id)
    }
}

impl Debug for Attribute {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "at{}", self.id)
    }
}

impl PartialOrd for Attribute {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        self.id.partial_cmp(&other.id)
    }
}

impl PartialOrd<usize> for Attribute {
    fn partial_cmp(&self, other: &usize) -> Option<std::cmp::Ordering> {
        self.id.partial_cmp(other)
    }
}

#[derive(Clone, Debug)]
pub struct AttributeKey {
    pub name2attribute: HashMap<String, Attribute>,
    pub attribute2name: Vec<String>,
    pub next_index: usize,
}

impl<'a> AttributeKey {
    pub fn new() -> Self {
        Self {
            name2attribute: HashMap::new(),
            attribute2name: vec![],
            next_index: 0,
        }
    }

    pub fn get_attribute_by_id(&self, attribute_id: usize) -> Attribute {
        Attribute { id: attribute_id }
    }

    pub fn get_id_from_attribute(&self, attribute: impl Borrow<Attribute>) -> usize {
        attribute.borrow().id
    }

    pub fn process_attribute(&mut self, attribute: &str) -> Attribute {
        match self.name2attribute.get(attribute) {
            Some(index) => return *index,
            None => {
                let result = Attribute {
                    id: self.next_index,
                };
                self.attribute2name.push(attribute.to_string());
                self.name2attribute.insert(attribute.to_string(), result);
                self.next_index += 1;
                return result;
            }
        }
    }
}

impl EbiTraitEventLog for EventLog {
    fn get_log(&self) -> &process_mining::EventLog {
        &self.rust4pm_log
    }

    fn get_trace_attributes(&self) -> HashMap<String, DataType> {
        let mut map: HashMap<String, DataType> = HashMap::new();
        for trace in &self.rust4pm_log.traces {
            for attribute in &trace.attributes {
                match map.entry(attribute.key.clone()) {
                    std::collections::hash_map::Entry::Occupied(mut e) => {
                        e.get_mut().update(&attribute.value);
                        ()
                    }
                    std::collections::hash_map::Entry::Vacant(e) => {
                        e.insert(DataType::init(&attribute.value));
                        ()
                    }
                }
            }
        }
        map
    }

    fn retain_traces<'a>(&'a mut self, f: Box<dyn Fn(&Vec<Activity>) -> bool + 'static>) {
        self.traces.retain(f);
    }
}

pub trait ToEventLog: Importable {
    fn to_event_log(self) -> Box<dyn EbiTraitEventLog>;

    fn import_as_event_log(reader: &mut dyn BufRead) -> Result<Box<dyn EbiTraitEventLog>>
    where
        Self: Sized,
    {
        Ok(Self::import(reader)?.to_event_log())
    }
}

impl<T> ToEventLog for T
where
    T: EbiTraitEventLog + Importable + 'static,
{
    fn to_event_log(self) -> Box<dyn EbiTraitEventLog> {
        Box::new(self)
    }
}

impl ToEventLog for CompressedEventLog {
    fn to_event_log(self) -> Box<dyn EbiTraitEventLog> {
        Box::new(self.log)
    }
}
