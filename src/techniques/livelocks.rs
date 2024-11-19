use anyhow::Result;

use crate::{techniques::deterministic_semantics_for_stochastic_semantics::PMarking, ebi_objects::{deterministic_finite_automaton_semantics::DeterministicFiniteAutomatonSemantics, labelled_petri_net::{LPNMarking, LabelledPetriNet}, stochastic_deterministic_finite_automaton_semantics::StochasticDeterministicFiniteAutomatonSemantics, stochastic_labelled_petri_net::StochasticLabelledPetriNet}, ebi_traits::ebi_trait_semantics::Semantics};

pub trait Livelock {
    type LivelockMarking;

    fn is_non_decreasing_livelock(&self, state: &Self::LivelockMarking) -> Result<bool>;
}

macro_rules! is_non_decreasing_livelock_lpn {
    ($t:ident) => {
        impl $t {
            fn is_marking_non_decreasing_livelock(&self, state: &mut LPNMarking) -> Result<bool> {
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

        impl Livelock for $t {
            type LivelockMarking = PMarking<LPNMarking>;
        
            fn is_non_decreasing_livelock(&self, state: &Self::LivelockMarking) -> Result<bool> {
                for (marking, _) in state.p_marking {
                    if self.is_marking_non_decreasing_livelock(&mut marking.clone())? {
                        return Ok(true);
                    }
                }

                Ok(false)
            }
        
        }
    };
}

macro_rules! is_non_decreasing_livelock_dfm {
    ($t:ident) => {
        impl Livelock for $t {
            type LivelockMarking = usize;

            fn is_non_decreasing_livelock(&self, state: &mut Self::LivelockMarking) -> Result<bool> {
                let mut trace = vec![];
                
                while !self.is_final_state(state) {
                    let enabled = self.get_enabled_transitions(state);
                    if enabled.len() != 1 {
                        return Ok(false);
                    }

                    let transition = enabled.into_iter().next().unwrap();

                    if trace.contains(state) {
                        //we are in a loop

                        return Ok(true);
                    } else {
                        trace.push(transition);
                    }

                    self.execute_transition(state, transition)?;
                }

                Ok(false)
            }
        }
    }
}

is_non_decreasing_livelock_lpn!(LabelledPetriNet);
is_non_decreasing_livelock_lpn!(StochasticLabelledPetriNet);
is_non_decreasing_livelock_dfm!(DeterministicFiniteAutomatonSemantics);
is_non_decreasing_livelock_dfm!(StochasticDeterministicFiniteAutomatonSemantics);