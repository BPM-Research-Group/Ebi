use std::collections::{HashMap, hash_map::Entry};

use crate::{
    ebi_framework::activity_key::HasActivityKey,
    ebi_objects::{
        event_log::EventLog, finite_stochastic_language::FiniteStochasticLanguage,
        stochastic_deterministic_finite_automaton::StochasticDeterministicFiniteAutomaton,
    },
    ebi_traits::ebi_trait_event_log::IndexTrace,
    math::{
        fraction::Fraction,
        traits::{One, Zero},
    },
};

impl From<FiniteStochasticLanguage> for StochasticDeterministicFiniteAutomaton {
    fn from(value: FiniteStochasticLanguage) -> Self {
        log::info!("convert finite stochastic language to SDFA");

        let mut result = StochasticDeterministicFiniteAutomaton::new();
        if value.len().is_zero() {
            result.set_initial_state(None);
        } else {
            let mut final_states = HashMap::new();
            result.set_activity_key(&value.activity_key);

            //create automaton
            for (trace, probability) in &value.traces {
                let mut state = result.get_initial_state().unwrap();
                for activity in trace {
                    state = result.take_or_add_transition(state, *activity, probability.clone());
                }

                match final_states.entry(state) {
                    Entry::Occupied(mut e) => *e.get_mut() += Fraction::one(),
                    Entry::Vacant(e) => {
                        e.insert(Fraction::one());
                    }
                }
            }

            //count
            let mut sums = final_states;
            for (source, _, _, probability) in &result {
                match sums.entry(*source) {
                    Entry::Occupied(mut e) => *e.get_mut() += probability,
                    Entry::Vacant(e) => {
                        e.insert(probability.clone());
                    }
                }
            }

            //normalise
            result.scale_outgoing_probabilities(sums);
        }
        result
    }
}

impl From<EventLog> for StochasticDeterministicFiniteAutomaton {
    fn from(value: EventLog) -> Self {
        log::info!("convert event log to SDFA");

        let mut result = StochasticDeterministicFiniteAutomaton::new();
        result.set_activity_key(value.get_activity_key());

        if value.len().is_zero() {
            result.set_initial_state(None);
        } else {
            let mut final_states = HashMap::new();

            //create automaton
            for trace_index in 0..value.log.traces.len() {
                let trace = value.get_trace(trace_index).unwrap();
                let mut state = result.get_initial_state().unwrap();

                for activity in trace {
                    state = result.take_or_add_transition(state, *activity, Fraction::one());
                }

                match final_states.entry(state) {
                    std::collections::hash_map::Entry::Occupied(mut e) => {
                        *e.get_mut() += Fraction::one()
                    }
                    std::collections::hash_map::Entry::Vacant(e) => {
                        e.insert(Fraction::one());
                    }
                }
            }

            //count
            let mut sums = final_states;
            for (source, _, _, probability) in &result {
                match sums.entry(*source) {
                    std::collections::hash_map::Entry::Occupied(mut e) => {
                        *e.get_mut() += probability
                    }
                    std::collections::hash_map::Entry::Vacant(e) => {
                        e.insert(probability.clone());
                    }
                }
            }

            //normalise
            result.scale_outgoing_probabilities(sums);
        }

        result
    }
}
