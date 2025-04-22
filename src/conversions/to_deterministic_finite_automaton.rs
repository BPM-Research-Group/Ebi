use crate::{ebi_framework::activity_key::HasActivityKey, ebi_objects::{
    deterministic_finite_automaton::DeterministicFiniteAutomaton, event_log::EventLog, finite_language::FiniteLanguage, finite_stochastic_language::FiniteStochasticLanguage
}, ebi_traits::{ebi_trait_event_log::IndexTrace, ebi_trait_iterable_language::EbiTraitIterableLanguage, ebi_trait_semantics::Semantics}, math::traits::Zero};

impl From<FiniteLanguage> for DeterministicFiniteAutomaton {
    fn from(value: FiniteLanguage) -> Self {
        log::info!("convert finite language into a DFA");
        let mut result = DeterministicFiniteAutomaton::new();
        result.set_activity_key(value.get_activity_key().clone());

        if value.len().is_zero() {
            result.set_initial_state(None);
        } else {
            for trace in value.iter() {
                let mut state = result.get_initial_state().unwrap();

                for activity in trace {
                    state = result.take_or_add_transition(state, *activity);
                }

                result.set_final_state(state, true);
            }
        }

        result
    }
}

impl From<FiniteStochasticLanguage> for DeterministicFiniteAutomaton {
    fn from(value: FiniteStochasticLanguage) -> Self {
        Into::<FiniteLanguage>::into(value).into()
    }
}

impl From<EventLog> for DeterministicFiniteAutomaton {
    fn from(value: EventLog) -> Self {
        Into::<FiniteLanguage>::into(value).into()
    }
}

