use crate::{
    ebi_framework::{
        ebi_input::EbiInput, ebi_trait::FromEbiTraitObject, ebi_trait_object::EbiTraitObject,
        trait_importers::ToEventLogEventAttributesTrait,
    },
    ebi_traits::ebi_trait_event_log::EbiTraitEventLog,
    trait_definition_logs,
};
use ebi_objects::{
    Activity, Attribute, EventLogCsv, EventLogOcel, EventLogPython, EventLogXes,
    anyhow::{Result, anyhow},
    attribute_key::has_attribute_key::HasAttributeKey,
    ebi_objects::event_log_event_attributes::EventLogEventAttributes,
    traits::{
        event_attribute_iterators::IntoEventAttributeIterator, event_attributes::EventAttributes,
    },
};
use intmap::IntMap;
use process_mining::core::event_data::case_centric::AttributeValue;

pub const TRAIT_DEFINITION_LATEX: &str = concat!("The trait ``event log with event attributes'' provides an iterator over traces, and within each trace, each event may have event attributes attached.
                \\\\
                Definition: let $\\Sigma$ be an alphabet of activities.
                Let $E \\colon \\text{attribute} \\mapsto \\text{value}$ be an attribute-value mapping, and let $\\mathcal{E}$ be the set of all attribute-value mappings.
                An \\emph{event with attributes} $\\sigma^{E} \\in \\Sigma \\times \\mathcal{E}$ is a combination of an activity and a attribute-value mapping.
                An \\emph{event log with event attributes} $L^E \\in (\\Sigma \\times \\mathcal{A})^*{}^*$ is a sequence of traces with event attributes.", trait_definition_logs!());

pub trait EbiTraitEventLogEventAttributes:
    EbiTraitEventLog + EventAttributes + HasAttributeKey + IntoEventAttributeIterator + EventAttributes
{
    /// Remove traces for which the function returns false.
    ///
    /// Note to callers: please put the closure definition inside the Box::new in the call of retain_traces.
    /// Otherwise, Rust may give weird compile errors.
    fn retain_traces_attributes<'a>(
        &'a mut self,
        f: Box<dyn Fn(&(Vec<Activity>, Vec<IntMap<Attribute, AttributeValue>>)) -> bool + 'static>,
    );
}

impl FromEbiTraitObject for dyn EbiTraitEventLogEventAttributes {
    fn from_trait_object(object: EbiInput) -> Result<Box<Self>> {
        match object {
            EbiInput::Trait(EbiTraitObject::EventLogEventAttributes(e), _) => Ok(e),
            _ => Err(anyhow!(
                "Cannot read {} {} as an event log with event attributes.",
                object.get_type().get_article(),
                object.get_type()
            )),
        }
    }
}

impl EbiTraitEventLogEventAttributes for EventLogEventAttributes {
    fn retain_traces_attributes<'a>(
        &'a mut self,
        mut f: Box<
            dyn Fn(&(Vec<Activity>, Vec<IntMap<Attribute, AttributeValue>>)) -> bool + 'static,
        >,
    ) {
        self.retain_traces_event_attributes_mut(&mut f);
    }
}

impl ToEventLogEventAttributesTrait for EventLogCsv {
    fn to_event_log_event_attributes_trait(self) -> Box<dyn EbiTraitEventLogEventAttributes> {
        Box::new(EventLogEventAttributes::from(self))
    }
}

impl ToEventLogEventAttributesTrait for EventLogOcel {
    fn to_event_log_event_attributes_trait(self) -> Box<dyn EbiTraitEventLogEventAttributes> {
        Box::new(EventLogEventAttributes::from(self))
    }
}

impl ToEventLogEventAttributesTrait for EventLogPython {
    fn to_event_log_event_attributes_trait(self) -> Box<dyn EbiTraitEventLogEventAttributes> {
        Box::new(EventLogEventAttributes::from(self))
    }
}

impl ToEventLogEventAttributesTrait for EventLogXes {
    fn to_event_log_event_attributes_trait(self) -> Box<dyn EbiTraitEventLogEventAttributes> {
        Box::new(EventLogEventAttributes::from(self))
    }
}
