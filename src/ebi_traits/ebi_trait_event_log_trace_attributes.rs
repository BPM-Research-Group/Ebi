use crate::ebi_framework::{
    ebi_input::EbiInput, ebi_trait::FromEbiTraitObject, ebi_trait_object::EbiTraitObject,
};
use anyhow::{Result, anyhow};
use chrono::{DateTime, FixedOffset};
use ebi_objects::{
    Activity, Attribute, EventLogTraceAttributes, HasActivityKey, Importable,
    IntoAttributeIterator, IntoAttributeTraceIterator, IntoTraceIterator, NumberOfTraces,
    TraceAttributes, attribute_key::has_attribute_key::HasAttributeKey,
    ebi_objects::compressed_event_log_trace_attributes::CompressedEventLogTraceAttributes,
};
use process_mining::event_log::{AttributeValue, Event, XESEditableAttribute};
use std::{collections::HashMap, io::BufRead};

pub const ATTRIBUTE_TIME: &str = "time:timestamp";

pub trait EbiTraitEventLogTraceAttributes:
    HasActivityKey
    + HasAttributeKey
    + IntoAttributeIterator
    + IntoAttributeTraceIterator
    + IntoTraceIterator
    + TraceAttributes
    + NumberOfTraces
    + Sync
{
    fn get_log(&self) -> &process_mining::EventLog;

    /// Remove traces for which the function returns false.
    ///
    /// Note to callers: please put the closure definition inside the Box::new in the call of retain_traces.
    /// Otherwise, Rust may give weird compile errors.
    fn retain_traces<'a>(&'a mut self, f: Box<dyn Fn(&Vec<Activity>) -> bool + 'static>);
}

impl dyn EbiTraitEventLogTraceAttributes {
    /**
     * Returns a map from trace to with which attribute values that trace appeared how often
     */
    pub fn get_traces_with_categorical_attribute(
        &self,
        attribute: Attribute,
    ) -> HashMap<Vec<Activity>, HashMap<String, u64>> {
        let mut result = HashMap::new();

        for (trace_index, trace) in self.iter_traces().enumerate() {
            if let Some(attribute_value) =
                self.get_trace_attribute_categorical(trace_index, attribute)
            {
                //the trace has the attribute
                match result.entry(trace) {
                    std::collections::hash_map::Entry::Occupied(mut e) => {
                        //we have seen this trace before
                        let attributes: &mut HashMap<String, u64> = e.get_mut();
                        match attributes.entry(attribute_value) {
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
                        map.insert(attribute_value, 1);
                        e.insert(map);
                    }
                }
            }
        }

        result
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

impl FromEbiTraitObject for dyn EbiTraitEventLogTraceAttributes {
    fn from_trait_object(object: EbiInput) -> Result<Box<Self>> {
        match object {
            EbiInput::Trait(EbiTraitObject::EventLogTraceAttributes(e), _) => Ok(e),
            _ => Err(anyhow!(
                "cannot read {} {} as an event log",
                object.get_type().get_article(),
                object.get_type()
            )),
        }
    }
}

impl EbiTraitEventLogTraceAttributes for EventLogTraceAttributes {
    fn get_log(&self) -> &process_mining::EventLog {
        &self.rust4pm_log
    }

    fn retain_traces<'a>(&'a mut self, f: Box<dyn Fn(&Vec<Activity>) -> bool + 'static>) {
        let mut activity_key = self.activity_key().clone();
        let event_classifier = self.event_classifier().clone();
        self.rust4pm_log.traces.retain(|trace| {
            let mut result = Vec::with_capacity(trace.events.len());
            for event in trace.events.iter() {
                let activity =
                    activity_key.process_activity(&event_classifier.get_class_identity(event));
                result.push(activity);
            }

            f(&result)
        });
    }
}

pub trait ToEventLogTraceAttributes: Importable {
    fn to_event_log_trace_attributes(self) -> Box<dyn EbiTraitEventLogTraceAttributes>;

    fn import_as_event_log_trace_attributes(
        reader: &mut dyn BufRead,
    ) -> Result<Box<dyn EbiTraitEventLogTraceAttributes>>
    where
        Self: Sized,
    {
        Ok(Self::import(reader)?.to_event_log_trace_attributes())
    }
}

impl<T> ToEventLogTraceAttributes for T
where
    T: EbiTraitEventLogTraceAttributes + Importable + 'static,
{
    fn to_event_log_trace_attributes(self) -> Box<dyn EbiTraitEventLogTraceAttributes> {
        Box::new(self)
    }
}

impl ToEventLogTraceAttributes for CompressedEventLogTraceAttributes {
    fn to_event_log_trace_attributes(self) -> Box<dyn EbiTraitEventLogTraceAttributes> {
        Box::new(self.log)
    }
}
