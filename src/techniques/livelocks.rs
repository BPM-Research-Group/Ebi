use anyhow::Result;

use crate::{ebi_framework::displayable::Displayable, ebi_objects::{deterministic_finite_automaton::DeterministicFiniteAutomaton, labelled_petri_net::{LPNMarking, LabelledPetriNet}, process_tree::ProcessTree, stochastic_deterministic_finite_automaton::StochasticDeterministicFiniteAutomaton, stochastic_labelled_petri_net::StochasticLabelledPetriNet}, ebi_traits::ebi_trait_semantics::Semantics, techniques::deterministic_semantics_for_stochastic_semantics::PMarking};

pub trait Livelock {
    type LivState: Displayable;

    fn is_non_decreasing_livelock(&self, state: &mut Self::LivState) -> Result<bool>;
}

macro_rules! is_non_decreasing_livelock_lpn {
    ($t:ident) => {
        impl Livelock for $t {
            type LivState = PMarking<LPNMarking>;

            /**
             * A q-state is in a livelock if all if its markings are in a livelock.
             */
            fn is_non_decreasing_livelock(&self, state: &mut PMarking<LPNMarking>) -> Result<bool> {
                for (marking, _) in &state.p_marking {
                    if !$t::is_non_decreasing_livelock(self, &mut marking.clone())? {
                        return Ok(false);
                    }
                }
                return Ok(true);
            }
        }

        impl $t {
            pub fn is_non_decreasing_livelock(&self, state: &mut LPNMarking) -> Result<bool> {

                let mut trace = vec![];
        
                while !self.is_final_state(state) {
                    let enabled = self.get_enabled_transitions(state);
                    if enabled.len() != 1 {
                        return Ok(false);
                    }
        
                    let transition = enabled.into_iter().next().unwrap();
        
                    if let Some(pos) = trace.iter().position(|t | t == &transition) {
                        //we are in a loop
        
                        //create incidence vector
                        let incidence = (pos..trace.len()).into_iter().fold(
                            vec![0; self.get_number_of_places()], 
                            |mut vec1, trace_index| {
                                let transition2 = trace[trace_index];
                                let vec2 = self.incidence_vector(transition2);
                                for (zref, aval) in vec1.iter_mut().zip(&vec2) {
                                    *zref += aval;
                                }
                                vec1
                            }
                        );
        
                        //if the incidence vector is negative somewhere, then the loop cannot be repeated forever
                        if incidence.iter().any(|x | x.is_negative()) {
                            return Ok(false);
                        }
        
                        //check that even if we would execute the loop an unbounded number of times, no other transition would become enabled.
                        {
                            //first, "execute" the loop a sufficient number of times
                            let omega = self.max_transition_input_arc_cardinality() + 1;
                            state.marking *= omega;
        
                            //technicality: after touching the state, we need to re-compute the enabled transitions
                            self.compute_enabled_transitions(state);
                            
                            //then, walk through the loop again
                            for transition in trace.iter().skip(pos) {
                                
                                if self.get_enabled_transitions(state).len() != 1 {
                                    return Ok(false);
                                }
                                self.execute_transition(state, *transition)?;
        
                            }
                        }
        
                        return Ok(true);
                    } else {
                        trace.push(transition);
                    }
        
                    self.execute_transition(state, transition)?;
                }
        
                Ok(false)
            }
        }
    };
}

macro_rules! is_non_decreasing_livelock_dfm {
    ($t:ident) => {
        impl Livelock for $t {
            type LivState = usize;

            fn is_non_decreasing_livelock(&self, state: &mut usize) -> Result<bool> {

                let mut trace = vec![*state];
                
                while !self.is_final_state(state) && self.get_enabled_transitions(state).len() == 1 {

                    let transition = self.get_enabled_transitions(state).into_iter().next().unwrap();
                    self.execute_transition(state, transition)?;

                    if trace.contains(state) {
                        //we are in a loop, and for DFM like models, that means it's a non-decreasing livelock
                        return Ok(true);
                    } else {
                        trace.push(*state);
                    }
                }

                Ok(false)
            }
        }
    }
}

is_non_decreasing_livelock_lpn!(LabelledPetriNet);
is_non_decreasing_livelock_lpn!(StochasticLabelledPetriNet);
is_non_decreasing_livelock_dfm!(DeterministicFiniteAutomaton);
is_non_decreasing_livelock_dfm!(StochasticDeterministicFiniteAutomaton);

impl Livelock for ProcessTree {
    type LivState = usize;

    fn is_non_decreasing_livelock(&self, _state: &mut Self::LivState) -> Result<bool> {
        Ok(false)
    }
}