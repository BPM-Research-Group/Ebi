use anyhow::Result;
use std::collections::HashMap;
use std::collections::hash_map::DefaultHasher;
use std::collections::hash_map::Entry;
use std::fmt::{self, Debug};
use std::hash::{Hash, Hasher};
use strum_macros::Display;

use crate::ebi_framework::activity_key::Activity;
use crate::ebi_framework::displayable::Displayable;
use crate::ebi_objects::labelled_petri_net::LPNMarking;
use crate::ebi_objects::stochastic_labelled_petri_net::StochasticLabelledPetriNet;
use crate::ebi_objects::stochastic_process_tree::StochasticProcessTree;
use crate::ebi_objects::stochastic_process_tree_semantics::NodeStates;
use crate::ebi_traits::ebi_trait_semantics::Semantics;
use crate::ebi_traits::ebi_trait_stochastic_deterministic_semantics::StochasticDeterministicSemantics;
use crate::ebi_traits::ebi_trait_stochastic_semantics::StochasticSemantics;
use crate::math::fraction::Fraction;
use crate::math::markov_model::MarkovModel;
use crate::math::traits::One;
use crate::math::traits::Signed;
use crate::math::traits::Zero;

use super::non_decreasing_livelock::NonDecreasingLivelock;

macro_rules! default_stochastic_deterministic_semantics {
    ($t:ident, $s:ident) => {
        impl StochasticDeterministicSemantics for $t {
            type DetState = PMarking<$s>;
            // impl StochasticDeterministicSemantics for StochasticLabelledPetriNet {
            //     type DetState = PMarking<LPNMarking>;

            fn get_deterministic_initial_state(&self) -> Result<Option<Self::DetState>> {
                let mut result = Self::DetState {
                    hash: 0,
                    p_marking: HashMap::new(),
                    termination_probability: Fraction::zero(),
                    silent_livelock_probability: Fraction::zero(),
                    activity_2_p_markings: HashMap::new(),
                    activity_2_probability: HashMap::new(),
                };
                let initial_state =
                    <Self as Semantics>::get_initial_state(self)
                        .unwrap()
                        .clone();
                if <Self as Semantics>::is_final_state(self, &initial_state) {
                    result.termination_probability = Fraction::one();
                }
                result.p_marking.insert(initial_state, Fraction::one());

                self.compute_next(&mut result)?;
                return Ok(Some(result));
            }

            fn execute_deterministic_activity(
                &self,
                state: &Self::DetState,
                activity: Activity,
            ) -> Result<Self::DetState> {
                assert!(state.activity_2_p_markings.contains_key(&activity));

                let mut result = Self::DetState {
                    hash: 0,
                    p_marking: state.activity_2_p_markings.get(&activity).unwrap().clone(),
                    termination_probability: Fraction::zero(),
                    silent_livelock_probability: Fraction::zero(),
                    activity_2_p_markings: HashMap::new(),
                    activity_2_probability: HashMap::new(),
                };

                self.compute_next(&mut result)?;
                return Ok(result);
            }

            fn get_deterministic_termination_probability(
                &self,
                state: &Self::DetState,
            ) -> Fraction {
                state.termination_probability.clone()
            }

            fn get_deterministic_activity_probability(
                &self,
                state: &Self::DetState,
                activity: Activity,
            ) -> Fraction {
                state.activity_2_probability.get(&activity).unwrap().clone()
            }

            fn get_deterministic_enabled_activities(
                &self,
                state: &Self::DetState,
            ) -> Vec<Activity> {
                state.activity_2_probability.keys().cloned().collect()
            }

            fn get_deterministic_silent_livelock_probability(
                &self,
                state: &Self::DetState,
            ) -> Fraction {
                state.silent_livelock_probability.clone()
            }

            fn get_deterministic_non_decreasing_livelock_probability(
                &self,
                state: &mut Self::DetState,
            ) -> Result<Fraction> {
                let mut sum = Fraction::zero();
                for (sub_state, probability) in state.p_marking.iter_mut() {
                    if self.is_part_of_non_decreasing_livelock(&mut sub_state.clone())? {
                        sum += probability;
                    }
                }
                Ok(sum)
            }
        }

        impl $t {
            // impl StochasticLabelledPetriNet {
            /**
             * Compute the next q-state.
             */
            fn compute_next(&self, q_state: &mut PMarking<$s>) -> Result<()> {
                // log::debug!("\t\t\tcompute next q-states for {:?}", q_state);

                //create the extended matrix
                let mut markov_model = self.create_markov_model(&q_state)?;

                // log::debug!("\t\t\t\tT {}", markov_model);
                // log::debug!("T {:?}", markov_model);

                //replace livelock states by absorbing states
                let progress_states = Self::get_progress_states(&markov_model);
                // log::debug!("\t\t\t\tprogress states {:?}", progress_states);
                let silent_livelock_states =
                    markov_model.get_states_that_cannot_reach(progress_states);
                // log::debug!("\t\t\t\tstates that cannot reach a progress state {:?}", silent_livelock_states);
                markov_model.make_states_absorbing(&silent_livelock_states);
                markov_model.set_states(&silent_livelock_states, MarkovMarking::SilentLiveLock());

                // log::debug!("T made absorbing {}", markov_model);
                // log::debug!("T made absorbing {:?}", markov_model);

                //if there are no states at all, we are in a final state (final states were filtered out in the creation of the Markov model)
                if markov_model.get_states().is_empty() {
                    q_state.termination_probability = Fraction::one();
                    return Ok(());
                }

                let new_state_vector = markov_model.pow_infty()?;
                // log::debug!("new state vector {}", crate::math::matrix::Matrix::into(new_state_vector.clone()));

                //create the next q-states
                for (probability, state) in new_state_vector
                    .into_iter()
                    .zip(markov_model.get_states_owned())
                {
                    if probability.is_positive() {
                        match state {
                            MarkovMarking::ReachableWithSilentTransitions(_) => {
                                /*
                                 * Final state reachable after silent transitions.
                                 */
                                q_state.termination_probability += probability;
                            }
                            MarkovMarking::AfterExecutingAcrivity(marking, activity) => {
                                match q_state.activity_2_probability.entry(activity) {
                                    Entry::Occupied(mut x) => *x.get_mut() += &probability,
                                    Entry::Vacant(x) => {
                                        x.insert(probability.clone());
                                        ()
                                    }
                                };
                                match q_state.activity_2_p_markings.entry(activity) {
                                    Entry::Occupied(mut x) => {
                                        x.get_mut().insert(marking, probability);
                                        ()
                                    }
                                    Entry::Vacant(x) => {
                                        let mut map = HashMap::new();
                                        map.insert(marking, probability);
                                        x.insert(map);
                                        ()
                                    }
                                }
                            }
                            MarkovMarking::Final(_) => {
                                q_state.termination_probability += probability;
                            }
                            MarkovMarking::SilentLiveLock() => {
                                q_state.silent_livelock_probability += probability;
                            }
                        };
                    }
                }

                // log::debug!("markov marking complete");

                //normalise the activities
                q_state
                    .activity_2_p_markings
                    .retain(|activity, distribution| {
                        let sum = distribution
                            .values()
                            .fold(Fraction::zero(), |sum, probability| &sum + probability);
                        let mut s = format!("for activity {}, resulting q-marking [", activity);
                        distribution.retain(|marking, value| {
                            *value /= &sum;
                            s += format!("{}: {}, ", marking, value).as_str();
                            true
                        });
                        // log::debug!("{}]", s.strip_suffix(", ").unwrap());
                        true
                    });

                //update the hash
                q_state.compute_hash();

                Ok(())
            }

            fn get_progress_states<X: Displayable>(
                markov_model: &MarkovModel<MarkovMarking<X>>,
            ) -> Vec<usize> {
                markov_model
                    .get_states()
                    .iter()
                    .enumerate()
                    .filter_map(|(i, state)| match state {
                        MarkovMarking::ReachableWithSilentTransitions(_) => None,
                        MarkovMarking::AfterExecutingAcrivity(_, _) => Some(i),
                        MarkovMarking::Final(_) => Some(i),
                        MarkovMarking::SilentLiveLock() => None,
                    })
                    .collect()
            }

            fn create_markov_model(
                &self,
                q_state: &PMarking<$s>,
            ) -> Result<MarkovModel<MarkovMarking<$s>>> {
                let mut markov: MarkovModel<MarkovMarking<$s>> = MarkovModel::new();

                let mut queue = vec![];
                {
                    for (marking, probability) in &q_state.p_marking {
                        if self.is_final_state(marking) {
                            markov.add_or_find_state(
                                MarkovMarking::Final(marking.clone()),
                                probability.clone(),
                            );
                        } else {
                            let markov_marking =
                                MarkovMarking::ReachableWithSilentTransitions(marking.clone());
                            let (markov_index, _) =
                                markov.add_or_find_state(markov_marking, probability.clone());
                            queue.push((marking.clone(), markov_index));
                        }
                    }
                }

                //create the states and transitions
                while let Some((marking, markov_index)) = queue.pop() {
                    let total_weight = self.get_total_weight_of_enabled_transitions(&marking)?;

                    for transition in self.get_enabled_transitions(&marking) {
                        let probability =
                            self.get_transition_weight(&marking, transition) / &total_weight;

                        let mut new_marking = marking.clone();
                        self.execute_transition(&mut new_marking, transition)?;

                        if self.is_transition_silent(transition) {
                            //we follow a silent transition

                            if self.is_final_state(&new_marking) {
                                //we end up in a new marking that is final
                                let (new_markov_index, _) = markov.add_or_find_state(
                                    MarkovMarking::Final(new_marking),
                                    Fraction::zero(),
                                );
                                markov.set_flow(markov_index, new_markov_index, &probability);
                            } else {
                                //we end up in a new marking that is not final
                                let new_markov_marking =
                                    MarkovMarking::ReachableWithSilentTransitions(
                                        new_marking.clone(),
                                    );
                                let (new_markov_index, added) =
                                    markov.add_or_find_state(new_markov_marking, Fraction::zero());
                                markov.set_flow(markov_index, new_markov_index, &probability);

                                if added {
                                    queue.push((new_marking, new_markov_index));
                                }
                            }
                        } else {
                            //we follow a labelled transition, and then we end up in an absorbing state
                            let activity = self.get_transition_activity(transition).unwrap();
                            let (new_markov_index, _) = markov.add_or_find_state(
                                MarkovMarking::AfterExecutingAcrivity(new_marking, activity),
                                Fraction::zero(),
                            );
                            markov.set_flow(markov_index, new_markov_index, &probability);
                        }
                    }
                }

                Ok(markov)
            }
        }
    };
}

default_stochastic_deterministic_semantics!(StochasticLabelledPetriNet, LPNMarking);
default_stochastic_deterministic_semantics!(StochasticProcessTree, NodeStates);

/**
 * Idea: as the computation of next p-states is expensive, it is performed once, and stored in this p-marking struct.
 * That is, this struct also contains the -next- p-markings.
 */
#[derive(Clone)]
pub struct PMarking<S>
where
    S: Displayable,
{
    hash: u64,
    pub p_marking: HashMap<S, Fraction>,
    pub termination_probability: Fraction,
    pub silent_livelock_probability: Fraction,
    pub activity_2_p_markings: HashMap<Activity, HashMap<S, Fraction>>,
    pub activity_2_probability: HashMap<Activity, Fraction>,
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

impl<S: Displayable> Hash for PMarking<S> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.hash.hash(state);
    }
}

impl<S: Displayable> Eq for PMarking<S> {}

impl<S: Displayable> PartialEq for PMarking<S> {
    fn eq(&self, other: &Self) -> bool {
        self.hash == other.hash && self.p_marking == other.p_marking
    }
}

impl<S: Displayable> fmt::Display for PMarking<S> {
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

impl<S: Displayable> Displayable for PMarking<S> {}

#[derive(Clone, Hash, Eq, PartialEq, Debug, Display)]
enum MarkovMarking<S: Displayable> {
    ReachableWithSilentTransitions(S),
    AfterExecutingAcrivity(S, Activity),
    Final(S),
    SilentLiveLock(),
}

impl<S: Displayable> Displayable for MarkovMarking<S> {}
