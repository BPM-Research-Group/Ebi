use crate::{
    ebi_framework::{
        ebi_input::EbiInput, ebi_trait::FromEbiTraitObject, ebi_trait_object::EbiTraitObject,
        trait_importers::ToEventLogTraceAttributesTrait,
    },
    ebi_traits::ebi_trait_event_log::EbiTraitEventLog,
    trait_definition_logs,
};
use ebi_objects::{
    anyhow::{anyhow, Result},
    attribute_key::has_attribute_key::HasAttributeKey,
    ebi_objects::compressed_event_log_trace_attributes::CompressedEventLogTraceAttributes,
    Activity, Attribute, EventLogOcel, EventLogTraceAttributes, EventLogXes, Importable,
    IntoAttributeIterator, IntoAttributeTraceIterator, TraceAttributes,
};
use fnv::FnvBuildHasher;
use intmap::IntMap;
use process_mining::core::event_data::case_centric::AttributeValue;
use std::collections::HashMap;

/// Allows for iteration over traces of events with attributes attached to traces.
///
/// For documentation on Ebi traits and mathematical definitions, please refer to the [manual](https://github.com/BPM-Research-Group/Ebi/releases/latest/download/manual.pdf).
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

    fn trait_explanation() -> &'static str
    where
        Self: Sized,
    {
        concat!("The trait ``event log with trace attributes'' provides an iterator over traces, where each trace may have trace attributes attached.
                \\\\
                Definition: let $\\Sigma$ be an alphabet of activities.
                Let $A \\colon \\text{attribute} \\mapsto \\text{value}$ be an attribute-value mapping, and let $\\mathcal{A}$ be the set of all attribute-value mappings.
                A \\emph{trace with attributes} $\\sigma^{A} \\in \\Sigma^* \\times \\mathcal{A}$ is a finite sequence of activities, where the sequence is annotated with attributes.
                An \\emph{event log with trace attributes} $L^A \\in (\\Sigma^* \\times \\mathcal{A})^*$ is a sequence of traces with attributes.", trait_definition_logs!())
    }
}

impl dyn EbiTraitEventLogTraceAttributes {
    /**
     * Returns a map from trace to with which attribute values that trace appeared how often
     */
    pub fn get_traces_with_categorical_attribute(
        &self,
        attribute: Attribute,
    ) -> HashMap<Vec<Activity>, HashMap<String, u64, FnvBuildHasher>, FnvBuildHasher> {
        let mut result = HashMap::<_, _, FnvBuildHasher>::default();

        for (trace_index, trace) in self.iter_traces().enumerate() {
            if let Some(attribute_value) =
                self.get_trace_attribute_categorical(trace_index, attribute)
            {
                //the trace has the attribute
                match result.entry(trace.clone()) {
                    std::collections::hash_map::Entry::Occupied(mut e) => {
                        //we have seen this trace before
                        let attributes: &mut HashMap<String, u64, FnvBuildHasher> = e.get_mut();
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
                        let mut map = HashMap::<_, _, FnvBuildHasher>::default();
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
                "Cannot read {} {} as an event log.",
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

impl ToEventLogTraceAttributesTrait for EventLogOcel {
    fn to_event_log_trace_attributes_trait(self) -> Box<dyn EbiTraitEventLogTraceAttributes> {
        Box::new(EventLogTraceAttributes::from(self))
    }
}

impl ToEventLogTraceAttributesTrait for EventLogXes {
    fn to_event_log_trace_attributes_trait(self) -> Box<dyn EbiTraitEventLogTraceAttributes> {
        Box::new(EventLogTraceAttributes::from(self))
    }
}
