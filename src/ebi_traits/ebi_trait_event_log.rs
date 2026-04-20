use crate::ebi_framework::{
    ebi_input::EbiInput, ebi_trait::FromEbiTraitObject, ebi_trait_object::EbiTraitObject,
    trait_importers::ToEventLogTrait,
};
use ebi_objects::{
    Activity, CompressedEventLog, EventLog, EventLogCsv, EventLogOcel, EventLogPython,
    EventLogTraceAttributes, EventLogXes, HasActivityKey, Importable, IntoRefTraceIterator,
    NumberOfTraces,
    anyhow::{Result, anyhow},
    ebi_objects::{
        compressed_event_log_trace_attributes::CompressedEventLogTraceAttributes,
        event_log_event_attributes::EventLogEventAttributes,
    },
};
use std::collections::HashMap;

#[macro_export]
macro_rules! trait_definition_logs {
    () => {
        " Comparison between log traits and objects: \\begin{tabularx}{\\linewidth}{XXccc}
            \\toprule
            \\rotatebox{90}{File type} & \\rotatebox{90}{use} & \\rotatebox{90}{activities} & \\rotatebox{90}{trace attributes} & \\rotatebox{90}{event attributes}\\\\
            \\midrule
            Traits:\\\\
            \\hyperref[trait:event log]{event log} & traces of activities & yes & no & no \\\\
            \\hyperref[trait:event log with event attributes]{event log with event attributes} & traces of events that have event attributes, time and resource have specific support of importer parameters & yes & no & yes\\\\
            \\hyperref[trait:event log with trace attributes]{event log with trace attributes} & traces that have attributes of events & yes & yes & no\\\\
            \\midrule
            Abstract objects:\\\\
            EventLog object & traces of activities; one-time conversion & yes & no & no \\\\
            EventLogEventAttributes object & traces of events that have event attributes, time and resource have specific support of importer parameters; one-time conversion & yes & yes & no \\\\
            EventLogTraceAttributes object & traces that have attributes of events; one-time conversion \\\\
            \\midrule
            Objects:\\\\
            EventLogXes object & full XES & yes & yes & yes \\\\
            EventLogCsv object & full CSV & yes & only trace ID & yes \\\\
            EventLogOcel object & OCEL & yes & yes & yes \\\\
            EventLogPython object & only accessible from PM4Py & yes & no & yes \\\\
            \\bottomrule
        \\end{tabularx}
        As a developer, please note that 
        \\begin{itemize}
            \\item An abstract object is a specialised data structure; thus memory consumption is lower than for an object. 
            However, there is an initial conversion performed (e.g., strings may be deduplicated).
            \\item An object requires no conversion as it extracts necessary information when requested.
            However, this extraction may be expensive (especially for Ocel).
            \\item A trait may either convert beforehand or extract on the fly.
        \\end{itemize}~"
    };
}

pub const TRAIT_DEFINITION_LATEX: &str = concat!(
    "The trait ``event log'' allows for iteration over an event log that contains traces of activities.
            \\\\
            Definition: let $\\Sigma$ be an alphabet of activities.
            Then, a \\emph{trace} $\\sigma \\in \\Sigma^*$ is a finite sequence of activities, and 
            an \\emph{event log} $L \\in (\\Sigma^*)^*$ is a sequence of traces.",
    trait_definition_logs!()
);

pub trait EbiTraitEventLog: HasActivityKey + IntoRefTraceIterator + NumberOfTraces {
    /// Remove traces for which the function returns false.
    ///
    /// Note to callers: please put the closure definition inside the Box::new in the call of retain_traces.
    /// Otherwise, Rust may give weird compile errors.
    fn retain_traces<'a>(&'a mut self, f: Box<dyn Fn(&Vec<Activity>) -> bool + 'static>);
}

impl dyn EbiTraitEventLog {
    pub fn to_multiset(&mut self) -> HashMap<Vec<Activity>, usize> {
        let mut map = HashMap::new();
        for trace in self.iter_traces() {
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
        }
        map
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

impl EbiTraitEventLog for EventLog {
    fn retain_traces<'a>(&'a mut self, f: Box<dyn Fn(&Vec<Activity>) -> bool + 'static>) {
        self.traces.retain(f);
    }
}

impl EbiTraitEventLog for EventLogTraceAttributes {
    fn retain_traces<'a>(&'a mut self, mut f: Box<dyn Fn(&Vec<Activity>) -> bool + 'static>) {
        self.retain_traces_mut(&mut f);
    }
}

impl EbiTraitEventLog for EventLogEventAttributes {
    fn retain_traces<'a>(&'a mut self, mut f: Box<dyn Fn(&Vec<Activity>) -> bool + 'static>) {
        self.retain_traces_mut(&mut f);
    }
}

impl<T> ToEventLogTrait for T
where
    T: EbiTraitEventLog + Importable + 'static,
{
    fn to_event_log_trait(self) -> Box<dyn EbiTraitEventLog> {
        Box::new(self)
    }
}

impl ToEventLogTrait for CompressedEventLog {
    fn to_event_log_trait(self) -> Box<dyn EbiTraitEventLog> {
        Box::new(self.log)
    }
}

impl ToEventLogTrait for CompressedEventLogTraceAttributes {
    fn to_event_log_trait(self) -> Box<dyn EbiTraitEventLog> {
        Box::new(self.log)
    }
}

macro_rules! via_log {
    ($t:ident) => {
        impl ToEventLogTrait for $t {
            fn to_event_log_trait(self) -> Box<dyn EbiTraitEventLog> {
                Box::new(EventLog::from(self))
            }
        }
    };
}

via_log!(EventLogCsv);
via_log!(EventLogOcel);
via_log!(EventLogPython);
via_log!(EventLogXes);
