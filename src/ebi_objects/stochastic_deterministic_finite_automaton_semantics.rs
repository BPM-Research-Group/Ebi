use std::sync::Arc;
use anyhow::{anyhow, Result};

use crate::{ebi_framework::activity_key::{Activity, ActivityKey}, ebi_traits::{ebi_trait_queriable_stochastic_language::EbiTraitQueriableStochasticLanguage, ebi_trait_semantics::Semantics, ebi_trait_stochastic_deterministic_semantics::StochasticDeterministicSemantics, ebi_trait_stochastic_semantics::{StochasticSemantics, TransitionIndex}}, math::fraction::Fraction};

use super::stochastic_deterministic_finite_automaton::StochasticDeterministicFiniteAutomaton;

#[derive(Debug)]
pub struct StochasticDeterministicFiniteAutomatonSemantics {
    activity_key: ActivityKey, //need to clone for the borrow checker
    sdfa: Arc<StochasticDeterministicFiniteAutomaton>
}

impl StochasticDeterministicFiniteAutomatonSemantics {
    pub fn new(sdfa: Arc<StochasticDeterministicFiniteAutomaton>) -> Self {
        Self {
            activity_key: sdfa.get_activity_key().clone(),
            sdfa: sdfa
        }
    }
}

//In semantics, the final state must be a deadlock state. Therefore, for every state in which we can terminate, we add a virtual silent transition to a virtual deadlock state.
impl Semantics for StochasticDeterministicFiniteAutomatonSemantics {
    type State = usize;

    /**
     * max_state + 1 = final state
     * source.len() = silent transition to final state
     */

    fn get_activity_key(&self) -> &ActivityKey {
        &self.activity_key
    }

    fn get_activity_key_mut(&mut self) -> &mut ActivityKey {
        &mut self.activity_key
    }

    fn get_initial_state(&self) -> Self::State {
        self.sdfa.get_initial_state()
    }

    fn execute_transition(&self, state: &mut Self::State, transition: TransitionIndex) -> Result<()> {
        if *state > self.sdfa.get_max_state() {
            return Err(anyhow!("Cannot execute a transition in a final state."))
        }

        if transition == self.sdfa.get_number_of_transitions() {
            //request for termination
            if self.sdfa.get_termination_probability(*state).is_positive() {
                //termination possible
                *state = self.sdfa.get_max_state() + 1;
                return Ok(());
            } else {
                //termination not possible
                return Err(anyhow!("Cannot terminate in state {}.", state))
            }
        }

        *state = self.sdfa.get_targets()[transition];
        return Ok(())
    }

    fn is_final_state(&self, state: &Self::State) -> bool {
        state > &self.sdfa.get_max_state()
    }

    fn is_transition_silent(&self, transition: TransitionIndex) -> bool {
        transition >= self.sdfa.get_number_of_transitions()
    }

    fn get_transition_activity(&self, transition: TransitionIndex) -> Option<Activity> {
        if transition == self.sdfa.get_number_of_transitions() {
            None
        } else {
            Some(self.sdfa.get_activities()[transition])
        }
    }

    fn get_enabled_transitions(&self, state: &Self::State) -> Vec<TransitionIndex> {
        let mut result = vec![];

        let (_, mut i) = self.sdfa.binary_search(*state, 0);
        while i < self.sdfa.get_number_of_transitions() && self.sdfa.get_sources()[i] == *state {
            result.push(i);
            i += 1;
        }

        if !self.is_final_state(state) && self.get_termination_probability(state).is_positive() {
            result.push(self.sdfa.get_number_of_transitions())
        }

        return result;
    }

    fn get_number_of_transitions(&self) -> usize {
        self.sdfa.get_number_of_transitions()
    }
}

impl StochasticSemantics for StochasticDeterministicFiniteAutomatonSemantics {
    fn get_transition_weight(&self, state: &Self::State, transition: TransitionIndex) -> &Fraction {
        if transition == self.sdfa.get_number_of_transitions() {
            //terminating transition
            &self.sdfa.get_termination_probability(*state)
        } else {
            &self.sdfa.get_probabilities()[transition]
        }
    }

    fn get_total_weight_of_enabled_transitions(&self, _: &Self::State) -> Result<Fraction> {
        Ok(Fraction::one())
    }
}

impl StochasticDeterministicSemantics for StochasticDeterministicFiniteAutomatonSemantics {
    type DState = usize;

    fn get_activity_key(&self) -> &ActivityKey {
        self.sdfa.get_activity_key()
    }

    fn get_initial_state(&self) -> Result<Self::DState> {
        Ok(self.sdfa.get_initial_state())
    }

    fn execute_activity(&self, state: &Self::DState, activity: Activity) -> Result<Self::DState> {
        let (found, i) = self.sdfa.binary_search(*state, self.sdfa.get_activity_key().get_id_from_activity(activity));
        if found{
            Ok(self.sdfa.get_targets()[i])
        } else {
            Err(anyhow!("activity not enabled"))
        }
    }

    fn get_termination_probability(&self, state: &Self::DState) -> Fraction {
        self.sdfa.get_termination_probability(*state).clone()
    }

    fn get_activity_probability(&self, state: &Self::DState, activity: Activity) -> Fraction {
        let (found, i) = self.sdfa.binary_search(*state, self.sdfa.get_activity_key().get_id_from_activity(activity));
        match found {
            true => self.sdfa.get_probabilities()[i].clone(),
            false => Fraction::zero(),
        }
    }

    fn get_enabled_activities(&self, state: &Self::DState) -> Vec<Activity> {
        let mut result = vec![];

        let (_, mut i) = self.sdfa.binary_search(*state, 0);
        while i < self.sdfa.get_number_of_transitions() && self.sdfa.get_sources()[i] == *state {
            result.push(self.sdfa.get_activities()[i]);
            i += 1;
        }

        return result;
    }
}