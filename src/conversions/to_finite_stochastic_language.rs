use std::collections::{hash_map::Entry, HashMap};

use crate::{ebi_objects::{event_log::EventLog, finite_stochastic_language::FiniteStochasticLanguage}, math::{fraction::Fraction, traits::One}};

impl From<EventLog> for FiniteStochasticLanguage {
    fn from(value: EventLog) -> Self {
        log::info!("create stochastic language");
        let mut map = HashMap::new();
        for t in &value.log.traces {
            let trace = t
                .events
                .iter()
                .map(|event| value.classifier.get_class_identity(event))
                .collect::<Vec<String>>();
            match map.entry(trace) {
                Entry::Occupied(mut e) => {
                    *e.get_mut() += Fraction::one();
                    ()
                }
                Entry::Vacant(e) => {
                    e.insert(Fraction::one());
                    ()
                }
            }
        }

        map.into()
    }
}