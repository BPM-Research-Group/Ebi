use std::collections::HashSet;

use crate::ebi_objects::{
    event_log::EventLog, finite_language::FiniteLanguage,
    finite_stochastic_language::FiniteStochasticLanguage,
};

impl From<EventLog> for FiniteLanguage {
    fn from(value: EventLog) -> Self {
        log::info!("convert event log to finite language");

        let mut map: HashSet<Vec<String>> = HashSet::new();
        for t in &value.log.traces {
            let trace = t
                .events
                .iter()
                .map(|event| value.classifier.get_class_identity(event))
                .collect::<Vec<String>>();
            map.insert(trace);
        }

        FiniteLanguage::from(map)
    }
}

impl From<FiniteStochasticLanguage> for FiniteLanguage {
    fn from(value: FiniteStochasticLanguage) -> Self {
        log::info!("convert finite stochastic language into finite language");

        let FiniteStochasticLanguage {
            activity_key,
            traces,
        } = value;
        let mut map = FiniteLanguage::new_hashmap();
        for (trace, _) in traces {
            map.insert(trace);
        }

        FiniteLanguage::from((activity_key, map))
    }
}
