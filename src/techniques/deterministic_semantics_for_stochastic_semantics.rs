use std::collections::hash_map::DefaultHasher;
use std::collections::HashMap;
use std::fmt::{self, Debug};
use std::hash::{Hash, Hasher};
use anyhow::Result;
use strum_macros::Display;
use std::collections::hash_map::Entry;

use crate::ebi_framework::activity_key::Activity;
use crate::ebi_framework::displayable::Displayable;
use crate::ebi_objects::labelled_petri_net::LPNMarking;
use crate::ebi_objects::stochastic_labelled_petri_net::StochasticLabelledPetriNet;
use crate::ebi_traits::ebi_trait_semantics::Semantics;
use crate::ebi_traits::ebi_trait_stochastic_deterministic_semantics::StochasticDeterministicSemantics;
use crate::ebi_traits::ebi_trait_stochastic_semantics::StochasticSemantics;
use crate::math::fraction::Fraction;
use crate::math::markov_model::MarkovModel;
use crate::math::matrix::Matrix;

// macro_rules! default_stochastic_deterministic_semantics {
    // ($t:ident, $s:ident) => {
        // impl StochasticDeterministicSemantics for $t {
            // type DetState = PMarking<$s>;
    impl StochasticDeterministicSemantics for StochasticLabelledPetriNet {
        type DetState = PMarking<LPNMarking>;

            fn get_deterministic_initial_state(&self) -> Result<Self::DetState> {
                let mut result = Self::DetState {
                    hash: 0,
                    p_marking: HashMap::new(),
                    termination_probability: Fraction::zero(),
                    activity_2_p_markings: HashMap::new(),
                    activity_2_probability: HashMap::new(),
                };
                let initial_state = <StochasticLabelledPetriNet as Semantics>::get_initial_state(self).clone();
                if <StochasticLabelledPetriNet as Semantics>::is_final_state(self, &initial_state) {
                    result.termination_probability = Fraction::one();
                }
                result.p_marking.insert(initial_state, Fraction::one());
                
                self.compute_next(&mut result)?;
                return Ok(result);
            }

            fn execute_deterministic_activity(&self, state: &Self::DetState, activity: Activity) -> Result<Self::DetState> {
                assert!(state.activity_2_p_markings.contains_key(&activity));
        
                let mut result = Self::DetState {
                    hash: 0,
                    p_marking: state.activity_2_p_markings.get(&activity).unwrap().clone(),
                    termination_probability: Fraction::zero(),
                    activity_2_p_markings: HashMap::new(),
                    activity_2_probability: HashMap::new(),
                };
                
                self.compute_next(&mut result)?;
                return Ok(result);
            }
        
            fn get_deterministic_termination_probability(&self, state: &Self::DetState) -> Fraction {
                state.termination_probability.clone()
            }
        
            fn get_deterministic_activity_probability(&self, state: &Self::DetState, activity: Activity) -> Fraction {
                state.activity_2_probability.get(&activity).unwrap().clone()
            }
        
            fn get_deterministic_enabled_activities(&self, state: &Self::DetState) -> Vec<Activity> {
                state.activity_2_probability.keys().cloned().collect()
            }
        }

        // impl $t {
        impl StochasticLabelledPetriNet {
            /**
             * Compute the next q-state.
             */
            fn compute_next(&self, q_state: &mut PMarking<LPNMarking>) -> Result<()> {
                log::debug!("compute next q-states for {:?}", q_state);

                //create the extended matrix
                let mut markov_model = self.create_markov_model(&q_state)?;

                log::debug!("T {:?}", markov_model);

                if markov_model.get_states().is_empty() {
                    //if there are no states at all, we are in a final state (final states were filtered out in the creation of the Markov model)
                    q_state.termination_probability = Fraction::one();
                    return Ok(());
                } else if !Self::contains_next_states(&markov_model) {
                    log::debug!("livelock detected for q-state {:?}", q_state);
                    q_state.termination_probability = Fraction::zero();
                    return Ok(());
                }

                //normalise the current state we are in
                markov_model.normalise_initial_vector()?;

                let new_state_vector = markov_model.pow_infty()?;
                log::debug!("new state vector{}", Matrix::into(new_state_vector.clone()));

                //create the next q-states
                for (probability, state) in new_state_vector.into_iter().zip(markov_model.get_states_owned()) {
                    if probability.is_positive() {
                        match state {
                            MarkovMarking::ReachableWithSilentTransitions(_) => {
                                /*
                                    This is a marking that can be reached by executing only silent transitions.
                                    By construction of the Markov chain, this also means that we cannot get to a final state or a labelled transition from here.
                                    Hence, it is a livelock state.
                                 */
                                todo!()
                            },
                            MarkovMarking::AfterExecutingAcrivity(marking, activity) => {
                                match q_state.activity_2_probability.entry(activity) {
                                    Entry::Occupied(mut x) => *x.get_mut() += &probability,
                                    Entry::Vacant(x) => {x.insert(probability.clone());()},
                                };
                                match q_state.activity_2_p_markings.entry(activity) {
                                    Entry::Occupied(mut x) => {
                                        x.get_mut().insert(marking, probability);
                                        ()
                                    },
                                    Entry::Vacant(x) => {
                                        let mut map = HashMap::new();
                                        map.insert(marking, probability);
                                        x.insert(map);
                                        ()
                                    },
                                }
                            },
                            MarkovMarking::Final(_) => {
                                q_state.termination_probability += probability;
                            },
                        };
                    }
                }
        
                // log::debug!("markov marking complete");
        
                //normalise the activities
                q_state.activity_2_p_markings.retain(|activity, distribution| {
                    let sum = distribution.values().fold(Fraction::zero(), |sum, probability| &sum + probability);
                    let mut s = format!("for activity {}, resulting q-marking [", activity);
                    distribution.retain(|marking, value| {*value /= &sum; s += format!("{}: {}, ", marking, value).as_str(); true});
                    // log::debug!("{}]", s.strip_suffix(", ").unwrap());
                    true
                });
        
                //update the hash
                q_state.compute_hash();
        
                Ok(())
            }

            fn contains_next_states<X: Displayable>(markov_model: &MarkovModel<MarkovMarking<X>>) -> bool {
                for state in markov_model.get_states() {
                    match state {
                        MarkovMarking::ReachableWithSilentTransitions(_) => {},
                        MarkovMarking::AfterExecutingAcrivity(_, _) => return true,
                        MarkovMarking::Final(_) => return true,
                    }
                }
                false
            }

            fn create_markov_model(&self, q_state: &PMarking<LPNMarking>) -> Result<MarkovModel<MarkovMarking<LPNMarking>>> {
                let mut markov: MarkovModel<MarkovMarking<LPNMarking>> = MarkovModel::new();

                let mut queue = vec![];
                {
                    for (marking, probability) in &q_state.p_marking {
                        if self.is_final_state(marking) { 
                            //Final states of the previous q-state are not part of the next q-state
                        } else {
                            let markov_marking = MarkovMarking::ReachableWithSilentTransitions(marking.clone());
                            let (markov_index, _) = markov.add_or_find_state(markov_marking, probability.clone());
                            queue.push((marking.clone(), markov_index));
                        }
                    }
                }

                //create the states and transitions
                while let Some((marking, markov_index)) = queue.pop() {
                    let total_weight = self.get_total_weight_of_enabled_transitions(&marking)?;

                    for transition in self.get_enabled_transitions(&marking) {
                        let probability = self.get_transition_weight(transition) / &total_weight;
        
                        let mut new_marking = marking.clone();
                        self.execute_transition(&mut new_marking, transition)?;
        
                        if self.is_transition_silent(transition) {
                            //we follow a silent transition   
        
                            if self.is_final_state(&new_marking) {
                                //we end up in a new marking that is final
                                let (new_markov_index, _) = markov.add_or_find_state(MarkovMarking::Final(new_marking), Fraction::zero());
                                markov.set_flow(markov_index, new_markov_index, &probability);

                            } else {
                                //we end up in a new marking that is not final
                                let new_markov_marking = MarkovMarking::ReachableWithSilentTransitions(new_marking.clone());
                                let (new_markov_index, added) = markov.add_or_find_state(new_markov_marking, Fraction::zero());
                                markov.set_flow(markov_index, new_markov_index, &probability);

                                if added {
                                    queue.push((new_marking, new_markov_index));
                                }
                            }
                        } else {
                            //we follow a labelled transition, and then we end up in an absorbing state
                            let activity = self.get_transition_activity(transition).unwrap();
                            let (new_markov_index, _) = markov.add_or_find_state(MarkovMarking::AfterExecutingAcrivity(new_marking, activity), Fraction::zero());
                            markov.set_flow(markov_index, new_markov_index, &probability);
                        }
                    }
                }

                Ok(markov)
            }
        }
    // }
// }

// default_stochastic_deterministic_semantics!(StochasticLabelledPetriNet, LPNMarking);

/**
 * Idea: as the computation of next p-states is expensive, it is performed once, and stored in this p-marking struct.
 * That is, this struct also contains the -next- p-markings.
 */
#[derive(Clone)]
pub struct PMarking<S> where S: Displayable {
    hash: u64,
    pub p_marking: HashMap<S, Fraction>,
    pub termination_probability: Fraction,
    pub activity_2_p_markings: HashMap<Activity, HashMap<S, Fraction>>,
    pub activity_2_probability: HashMap<Activity, Fraction>
}

impl<S: Displayable> PMarking<S> {
    pub fn compute_hash(&mut self) {
        let mut pairs: Vec<_> = self.p_marking.iter().collect();
        pairs.sort_by_key(|i| i.1);
        
        let mut h = DefaultHasher::new();
        for (state, probability) in pairs {
            state.hash(&mut h);
            probability.hash(&mut h);
        }
        self.hash = h.finish();
    }
}

impl <S: Displayable> Hash for PMarking<S> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.hash.hash(state);
    }
}

impl <S: Displayable> Eq for PMarking<S> {}

impl <S: Displayable> PartialEq for PMarking<S> {
    fn eq(&self, other: &Self) -> bool {
        self.hash == other.hash && self.p_marking == other.p_marking
    }
}

impl <S: Displayable> fmt::Display for PMarking<S> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "p-marking of size {}", self.p_marking.len())
    }
}

impl<S: Displayable> Debug for PMarking<S> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for (i, (marking, probability)) in self.p_marking.iter().enumerate() {

            //code for convergence test (not for production)
            // if !marking.debug().is_empty() {
            //     write!(f, "{:.8}", probability)?;
            // }

            write!(f, "{} p={}", marking, probability)?;
            if i < self.p_marking.len() - 1 {
                write!(f, ", ")?;
            }
        }
        write!(f, "")
    }
}



impl <S: Displayable> Displayable for PMarking<S> {}


#[derive(Clone,Hash,Eq,PartialEq,Debug,Display)]
enum MarkovMarking<S: Displayable> {
    ReachableWithSilentTransitions(S),
    AfterExecutingAcrivity(S, Activity),
    Final(S),
}

impl <S: Displayable> Displayable for MarkovMarking<S> {}