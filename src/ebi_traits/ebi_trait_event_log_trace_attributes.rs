use crate::{
    ebi_framework::{
        ebi_input::EbiInput, ebi_trait::FromEbiTraitObject, ebi_trait_object::EbiTraitObject,
        trait_importers::ToEventLogTraceAttributesTrait,
    },
    ebi_traits::ebi_trait_event_log::EbiTraitEventLog,
};
use anyhow::{Result, anyhow};
use ebi_objects::{
    Activity, Attribute, EventLogTraceAttributes, Importable, IntoAttributeIterator,
    IntoAttributeTraceIterator, TraceAttributes, attribute_key::has_attribute_key::HasAttributeKey,
    ebi_objects::compressed_event_log_trace_attributes::CompressedEventLogTraceAttributes,
};
use intmap::IntMap;
use process_mining::core::event_data::case_centric::AttributeValue;
use std::collections::HashMap;

pub const ATTRIBUTE_TIME: &str = "time:timestamp";

pub trait EbiTraitEventLogTraceAttributes:
    EbiTraitEventLog
    + IntoAttributeIterator
    + IntoAttributeTraceIterator
    + TraceAttributes
    + HasAttributeKey
{
    /// Remove traces for which the function returns false.
    ///
    /// Note to callers: please put the closure definition inside the Box::new in the call of retain_traces.
    /// Otherwise, Rust may give weird compile errors.
    fn retain_traces_attributes<'a>(
        &'a mut self,
        f: Box<dyn Fn(&(Vec<Activity>, IntMap<Attribute, AttributeValue>)) -> bool + 'static>,
    );
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
                match result.entry(trace.clone()) {
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
    fn retain_traces_attributes<'a>(
        &'a mut self,
        mut f: Box<dyn Fn(&(Vec<Activity>, IntMap<Attribute, AttributeValue>)) -> bool + 'static>,
    ) {
        self.retain_traces_attributes_mut(&mut f);
    }
}

impl<T> ToEventLogTraceAttributesTrait for T
where
    T: EbiTraitEventLogTraceAttributes + Importable + 'static,
{
    fn to_event_log_trace_attributes_trait(self) -> Box<dyn EbiTraitEventLogTraceAttributes> {
        Box::new(self)
    }
}

impl ToEventLogTraceAttributesTrait for CompressedEventLogTraceAttributes {
    fn to_event_log_trace_attributes_trait(self) -> Box<dyn EbiTraitEventLogTraceAttributes> {
        Box::new(self.log)
    }
}
