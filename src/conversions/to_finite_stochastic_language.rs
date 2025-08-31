use std::collections::{HashMap, hash_map::Entry};

use ebi_arithmetic::{Fraction, One};

use crate::ebi_objects::{
    event_log::EventLog, finite_stochastic_language::FiniteStochasticLanguage,
};

impl From<EventLog> for FiniteStochasticLanguage {
    fn from(value: EventLog) -> Self {
        log::info!("create stochastic language");
        let mut map = HashMap::new();

        let EventLog {
            activity_key,
            traces,
            ..
        } = value;

        for trace in traces {
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

        (activity_key, map).into()
    }
}
