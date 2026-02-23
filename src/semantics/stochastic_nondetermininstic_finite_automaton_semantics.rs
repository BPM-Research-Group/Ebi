use crate::semantics::semantics::Semantics;
use anyhow::{Result, anyhow};
use ebi_objects::{
    Activity, StochasticNondeterministicFiniteAutomaton,
    ebi_objects::labelled_petri_net::TransitionIndex,
};

impl Semantics for StochasticNondeterministicFiniteAutomaton {
    type SemState = usize;

    /**
     * max_state + 1 = final state
     * source.len() = silent transition to final state
     */

    fn get_initial_state(&self) -> Option<usize> {
        self.initial_state
    }

    fn execute_transition(&self, state: &mut usize, transition: TransitionIndex) -> Result<()> {
        if *state > self.number_of_states() {
            return Err(anyhow!("Cannot execute a transition in a final state."));
        }

        if transition == self.sources.len() {
            //request for termination
            if self.can_terminate_in_state(*state) {
                //termination possible
                *state = self.number_of_states() + 1;
                return Ok(());
            } else {
                //termination not possible
                return Err(anyhow!("Cannot terminate in state {}.", state));
            }
        }

        *state = self.get_targets()[transition];
        return Ok(());
    }

    fn is_final_state(&self, state: &usize) -> bool {
        state > &self.number_of_states()
    }

    fn is_transition_silent(&self, transition: TransitionIndex) -> bool {
        transition >= self.number_of_transitions() || self.activities[transition].is_none()
    }

    fn get_transition_activity(&self, transition: TransitionIndex) -> Option<Activity> {
        println!("transition activity {}", transition);
        if transition == self.number_of_transitions() {
            None
        } else {
            self.get_activities()[transition]
        }
    }

    fn get_enabled_transitions(&self, state: &usize) -> Vec<TransitionIndex> {
        let mut result = vec![];

        //check the DFA for enabled transitions
        let (_, mut i) = self.binary_search(*state, 0, 0);
        while i < self.sources.len() && self.sources[i] == *state {
            if self.can_execute_transition(i) {
                result.push(i);
            }
            i += 1;
        }

        //if the DFA can terminate, then add a termination silent transition
        if state <= &self.number_of_states() && self.can_terminate_in_state(*state) {
            result.push(self.sources.len())
        }

        return result;
    }

    fn get_number_of_transitions(&self) -> usize {
        self.sources.len() + 1
    }
}

#[cfg(test)]
pub mod tests {
    use std::fs;

    use ebi_objects::StochasticNondeterministicFiniteAutomaton;

    use crate::semantics::semantics::Semantics;

    #[test]
    fn snfa_semantics() {
        let fin = fs::read_to_string("testfiles/aa-ab-ba.snfa").unwrap();
        let snfa = fin
            .parse::<StochasticNondeterministicFiniteAutomaton>()
            .unwrap();

        let state = Semantics::get_initial_state(&snfa).unwrap();

        assert_eq!(Semantics::is_final_state(&snfa, &state), false);
        assert_eq!(Semantics::get_enabled_transitions(&snfa, &state).len(), 3);
    }
}
