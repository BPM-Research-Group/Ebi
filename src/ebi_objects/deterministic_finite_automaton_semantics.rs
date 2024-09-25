use std::sync::Arc;
use anyhow::{anyhow, Result};

use crate::{ebi_framework::activity_key::{Activity, ActivityKey}, ebi_traits::{ebi_trait_semantics::Semantics, ebi_trait_stochastic_semantics::TransitionIndex}};

use super::deterministic_finite_automaton::DeterministicFiniteAutomaton;

#[derive(Debug)]
pub struct DeterministicFiniteAutomatonSemantics {
    activity_key: ActivityKey, //need to clone for the borrow checker
    dfa: Arc<DeterministicFiniteAutomaton>
}

impl DeterministicFiniteAutomatonSemantics {
    pub fn new(dfa: Arc<DeterministicFiniteAutomaton>) -> Self {
        Self {
            activity_key: dfa.get_activity_key().clone(),
            dfa
        }
    }
}

//In semantics, the final state must be a deadlock state. Therefore, for every state in which we can terminate, we add a virtual silent transition to a virtual deadlock state.
impl Semantics for DeterministicFiniteAutomatonSemantics {
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
        self.dfa.get_initial_state()
    }

    fn execute_transition(&self, state: &mut Self::State, transition: TransitionIndex) -> Result<()> {
        if *state > self.dfa.get_max_state() {
            return Err(anyhow!("Cannot execute a transition in a final state."))
        }

        if transition == self.dfa.get_number_of_transitions() {
            //request for termination
            *state = self.dfa.get_max_state() + 1;
            return Ok(());
        }

        *state = self.dfa.get_targets()[transition];
        return Ok(())
    }

    fn is_final_state(&self, state: &Self::State) -> bool {
        state > &self.dfa.get_max_state()
    }

    fn is_transition_silent(&self, transition: TransitionIndex) -> bool {
        transition >= self.dfa.get_number_of_transitions()
    }

    fn get_transition_activity(&self, transition: TransitionIndex) -> Option<Activity> {
        if transition == self.dfa.get_number_of_transitions() {
            None
        } else {
            Some(self.dfa.get_activities()[transition])
        }
    }

    fn get_enabled_transitions(&self, state: &Self::State) -> Vec<TransitionIndex> {
        let mut result = vec![];

        let (_, mut i) = self.dfa.binary_search(*state, 0);
        while i < self.dfa.get_number_of_transitions() && self.dfa.get_sources()[i] == *state {
            result.push(i);
            i += 1;
        }

        if !self.is_final_state(state) {
            result.push(self.dfa.get_number_of_transitions())
        }

        return result;
    }
}