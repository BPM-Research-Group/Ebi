use anyhow::{Result, anyhow};
use ebi_objects::{
    Activity, CompressedEventLog, EventLog, EventLogTraceAttributes, HasActivityKey, Importable,
    IntoRefTraceIterator, NumberOfTraces,
};
use std::{collections::HashMap, io::BufRead};

use crate::ebi_framework::{
    ebi_input::EbiInput, ebi_trait::FromEbiTraitObject, ebi_trait_object::EbiTraitObject,
};

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
