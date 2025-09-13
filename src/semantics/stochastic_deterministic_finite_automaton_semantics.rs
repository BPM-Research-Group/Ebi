use anyhow::{Result, anyhow};
use ebi_arithmetic::{Fraction, One, Zero};
use ebi_objects::{
    ebi_objects::labelled_petri_net::TransitionIndex, Activity, DeterministicFiniteAutomaton, HasActivityKey, StochasticDeterministicFiniteAutomaton
};

use crate::{
    ebi_traits::ebi_trait_stochastic_deterministic_semantics::StochasticDeterministicSemantics,
    semantics::semantics::Semantics, techniques::non_decreasing_livelock::NonDecreasingLivelock,
};

macro_rules! semantics_for_automaton {
    ($t:ident) => {
        //In semantics, the final state must be a deadlock state. Therefore, for every state in which we can terminate, we add a virtual silent transition to a virtual deadlock state.
        impl Semantics for $t {
            type SemState = usize;

            /**
             * max_state + 1 = final state
             * source.len() = silent transition to final state
             */

            fn get_initial_state(&self) -> Option<usize> {
                self.initial_state
            }

            fn execute_transition(
                &self,
                state: &mut usize,
                transition: TransitionIndex,
            ) -> Result<()> {
                if *state > self.max_state {
                    return Err(anyhow!("Cannot execute a transition in a final state."));
                }

                if transition == self.sources.len() {
                    //request for termination
                    if self.can_terminate_in_state(*state) {
                        //termination possible
                        *state = self.get_max_state() + 1;
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
                state > &self.max_state
            }

            fn is_transition_silent(&self, transition: TransitionIndex) -> bool {
                transition >= self.sources.len()
            }

            fn get_transition_activity(&self, transition: TransitionIndex) -> Option<Activity> {
                if transition == self.sources.len() {
                    None
                } else {
                    Some(self.get_activities()[transition])
                }
            }

            fn get_enabled_transitions(&self, state: &usize) -> Vec<TransitionIndex> {
                let mut result = vec![];

                //check the DFA for enabled transitions
                let (_, mut i) = self.binary_search(*state, 0);
                while i < self.sources.len() && self.sources[i] == *state {
                    if self.can_execute_transition(i) {
                        result.push(i);
                    }
                    i += 1;
                }

                //if the DFA can terminate, then add a termination silent transition
                if state <= &self.max_state && self.can_terminate_in_state(*state) {
                    result.push(self.sources.len())
                }

                return result;
            }

            fn get_number_of_transitions(&self) -> usize {
                self.sources.len() + 1
            }
        }
    };
}

semantics_for_automaton!(DeterministicFiniteAutomaton);
semantics_for_automaton!(StochasticDeterministicFiniteAutomaton);

impl StochasticDeterministicSemantics for StochasticDeterministicFiniteAutomaton {
    type DetState = usize;

    fn get_deterministic_initial_state(&self) -> Result<Option<usize>> {
        Ok(self.get_initial_state())
    }

    fn execute_deterministic_activity(&self, state: &usize, activity: Activity) -> Result<usize> {
        let (found, i) =
            self.binary_search(*state, self.activity_key().get_id_from_activity(activity));
        if found {
            Ok(self.targets[i])
        } else {
            Err(anyhow!("activity not enabled"))
        }
    }

    fn get_deterministic_termination_probability(&self, state: &usize) -> Fraction {
        self.get_termination_probability(*state).clone()
    }

    fn get_deterministic_activity_probability(
        &self,
        state: &usize,
        activity: Activity,
    ) -> Fraction {
        let (found, i) =
            self.binary_search(*state, self.activity_key().get_id_from_activity(activity));
        match found {
            true => self.get_probabilities()[i].clone(),
            false => Fraction::zero(),
        }
    }

    fn get_deterministic_enabled_activities(&self, state: &usize) -> Vec<Activity> {
        let mut result = vec![];

        let (_, mut i) = self.binary_search(*state, 0);
        while i < <StochasticDeterministicFiniteAutomaton>::get_number_of_transitions(self)
            && self.get_sources()[i] == *state
        {
            result.push(self.get_activities()[i]);
            i += 1;
        }

        return result;
    }

    fn get_deterministic_silent_livelock_probability(&self, _state: &Self::DetState) -> Fraction {
        Fraction::zero()
    }

    fn get_deterministic_non_decreasing_livelock_probability(
        &self,
        state: &mut Self::DetState,
    ) -> Result<Fraction> {
        if self.is_part_of_non_decreasing_livelock(state)? {
            return Ok(Fraction::one());
        } else {
            return Ok(Fraction::zero());
        }
    }
}

#[cfg(test)]
mod tests {
    use std::fs;

    use ebi_objects::StochasticDeterministicFiniteAutomaton;

    use crate::semantics::semantics::Semantics;

    #[test]
    fn sdfa_zero_edge() {
        let fin = fs::read_to_string("testfiles/a-livelock-zeroweight.sdfa").unwrap();
        let sdfa = fin
            .parse::<StochasticDeterministicFiniteAutomaton>()
            .unwrap();

        let state = sdfa.get_initial_state().unwrap();
        let enabled = sdfa.get_enabled_transitions(&state);
        assert_eq!(enabled.len(), 2); //termination and doing a
    }
}
