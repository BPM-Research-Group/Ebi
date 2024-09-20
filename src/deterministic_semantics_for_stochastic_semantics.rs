use std::collections::hash_map::DefaultHasher;
use std::collections::HashMap;
use std::fmt::{self, Debug};
use std::hash::{Hash, Hasher};
use std::rc::Rc;
use anyhow::Result;
use std::collections::hash_map::Entry;

use crate::ebi_framework::activity_key::{Activity, ActivityKey};
use crate::ebi_framework::displayable::Displayable;
use crate::ebi_traits::ebi_trait_stochastic_deterministic_semantics::StochasticDeterministicSemantics;
use crate::ebi_traits::ebi_trait_stochastic_semantics::StochasticSemantics;
use crate::math::fraction::Fraction;
use crate::math::matrix::Matrix;



#[derive(Clone,Hash,Eq,PartialEq,Debug)]
enum MarkovMarking<S: Displayable> {
    Transient(S),
    AbsorbingActivity(S, Activity),
    AbsorbingFinal(S)
}

struct MarkovMarkings<S: Displayable> {
    markings: HashMap<MarkovMarking<S>, usize>, //marking to row number
    number_of_transient: usize,
    number_of_absorbing: usize
}

impl <S: Displayable> MarkovMarkings<S> {
    pub fn add(&mut self, markov_marking: MarkovMarking<S>) -> usize {
        let row = self.markings.entry(markov_marking).or_insert_with_key(|key| {
            match key {
                MarkovMarking::Transient(_) => {self.number_of_transient += 1; self.number_of_transient-1},
                MarkovMarking::AbsorbingActivity(_, _) => {self.number_of_absorbing += 1; self.number_of_absorbing-1},
                MarkovMarking::AbsorbingFinal(_) => {self.number_of_absorbing += 1; self.number_of_absorbing-1},
            }
        });
        *row
    }

    /**
     * Return a clone of the inserted marking if it was not there yet.
     */
    pub fn add_keep_copy(&mut self, markov_marking: MarkovMarking<S>) -> Option<MarkovMarking<S>> {
        let mut inserted = None;
        self.markings.entry(markov_marking).or_insert_with_key(|key| {
            inserted = Some(key.clone());
            match key {
                MarkovMarking::Transient(_) => {self.number_of_transient += 1; self.number_of_transient-1},
                MarkovMarking::AbsorbingActivity(_, _) => {self.number_of_absorbing += 1; self.number_of_absorbing-1},
                MarkovMarking::AbsorbingFinal(_) => {self.number_of_absorbing += 1; self.number_of_absorbing-1},
            }
        });
        inserted
    }

    fn get(&self, markov_marking: &MarkovMarking<S>) -> Option<&usize> {
        self.markings.get(markov_marking)
    }
}

impl <S: Displayable> Debug for MarkovMarkings<S> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", self.markings)
    }
}

pub struct DeterministicStochasticSemantics<S: Displayable> {
    net_semantics: Rc<dyn StochasticSemantics<State = S>>
}

impl <S: Displayable> DeterministicStochasticSemantics<S> {

    pub fn new<'a>(net_semantics: Rc<dyn StochasticSemantics<State = S>>) -> Self {
        Self{
            net_semantics: net_semantics
        }
    }

    /**
     * Compute the next p-state.
     */
    fn compute_next(&self, p_state: &mut PMarking<S>) -> Result<()> {
        // log::debug!("compute next p-states for {:?}", p_state);

        //first, gather the markings we can reach
        let (markov_markings, f) = self.markov_markings(p_state)?;

        // log::debug!("markov markings {:?}", markov_markings);
        // log::debug!("f {}", f);
        
        //construct the A and B matrices
        let (a, mut b) = self.markov_matrices(&markov_markings)?;

        // log::debug!("A is a {}", a);
        // log::debug!("B is a {}", b);

        //solve Markov chain
        let p;
        {
            b.identity_minus();

            // log::debug!("matrix I - B = {}", b);

            b.inverse()?;

            // log::debug!("matrix F = inv(I-B) = {}", b);

            let fundamental = b;

            p = fundamental * a;

            // log::debug!("matrix P = F * A = {:?}", p);
        }
        let x = f * p;

        // log::debug!("vector f' = f * P = {}", x);

        //construct the next states
        for (markov_marking, row) in &markov_markings.markings {
            match markov_marking {
                MarkovMarking::Transient(_) => (),
                MarkovMarking::AbsorbingActivity(marking, activity) =>  {
                    let probability = x[0][*row].clone();

                    // log::debug!("next activity {} with weight {:.5}", activity, probability);

                    p_state.termination_probability -= &probability;
                    match p_state.activity_2_probability.entry(*activity) {
                        Entry::Occupied(mut x) => *x.get_mut() += &probability,
                        Entry::Vacant(x) => {x.insert(probability.clone());()},
                    };
                    match p_state.activity_2_p_markings.entry(*activity) {
                        Entry::Occupied(mut x) => {
                            x.get_mut().insert(marking.clone(), probability);
                            ()
                        },
                        Entry::Vacant(x) => {
                            let mut map = HashMap::new();
                            map.insert(marking.clone(), probability);
                            x.insert(map);
                            ()
                        },
                    }
                },
                MarkovMarking::AbsorbingFinal(_) => (),
            };
        }

        // log::debug!("markov marking complete");

        //normalise the activities
        p_state.activity_2_p_markings.retain(|activity, distribution| {
            let sum = distribution.values().fold(Fraction::zero(), |sum, probability| &sum + probability);
            let mut s = format!("for activity {}, resulting p-marking [", activity);
            distribution.retain(|marking, value| {*value /= &sum; s += format!("{}: {}, ", marking, value).as_str(); true});
            // log::debug!("{}]", s.strip_suffix(", ").unwrap());
            true
        });

        // log::debug!("normalisation complete");

        //update the hash
        p_state.compute_hash();

        Ok(())
    }

    /**
     * Result: list of Markov states (markings), a matrix with the Markov model, and the probability to terminate
     */
    fn markov_markings(&self, p_state: &PMarking<S>) -> Result<(MarkovMarkings<S>, Matrix)> {
        let mut f = Matrix::new();
        let mut markov_markings = MarkovMarkings {
            markings: HashMap::new(),
            number_of_transient: 0,
            number_of_absorbing: 0,
        };
        
        let mut queue = vec![];
        {
            for (marking, probability) in &p_state.p_marking {
                if !self.net_semantics.is_final_state(marking) { //Final states cannot progress further, so we do not include them in the computation.
                    let markov_marking = MarkovMarking::Transient(marking.clone());
                    let row = markov_markings.add(markov_marking.clone());
                    queue.push(markov_marking);

                    //put the marking in F
                    f.ensure_capacity(1, &row + 1, &Fraction::zero());
                    f.element_add(&0, &row, probability);
                }
            }
        }

        //second, gather all other markings
        while let Some(MarkovMarking::Transient(transient_marking)) = queue.pop() {

            for transition in self.net_semantics.get_enabled_transitions(&transient_marking) {

                let mut new_marking = transient_marking.clone();
                self.net_semantics.execute_transition(&mut new_marking, transition)?;

                if self.net_semantics.is_transition_silent(transition) {
                    //we follow a silent transition   

                    if self.net_semantics.is_final_state(&new_marking) {
                        //final marking (absorbing)
                        markov_markings.add(MarkovMarking::AbsorbingFinal(new_marking));
                    } else {
                        //non-final marking (transient)
                        if let Some(markov_marking) = markov_markings.add_keep_copy(MarkovMarking::Transient(new_marking)) {
                            queue.push(markov_marking);

                            //f must provide a value (zero) for transient states that are not initial states
                            f.ensure_capacity(1, f.get_number_of_columns() + 1, &Fraction::zero());
                        }
                    }
                } else {
                    //we follow a labelled transition, and then we end up in an absorbing state
                    let activity = self.net_semantics.get_transition_activity(transition).unwrap();

                    markov_markings.add(MarkovMarking::AbsorbingActivity(new_marking, activity.clone()));
                }
            }
        }

        Ok((markov_markings, f))
    }

    fn markov_matrices(&self, markov_markings: &MarkovMarkings<S>) -> Result<(Matrix, Matrix)> {
        let mut a = Matrix::new_sized(markov_markings.number_of_transient, markov_markings.number_of_absorbing, Fraction::zero());
        let mut b = Matrix::new_squared(markov_markings.number_of_transient, Fraction::zero());

        for (markov_marking, row) in &markov_markings.markings {
            match markov_marking {
                MarkovMarking::Transient(marking) => { //a transient marking goes to B
                    let total_weight = self.net_semantics.get_total_weight_of_enabled_transitions(marking)?;
                    for transition in self.net_semantics.get_enabled_transitions(marking) {
                        let mut new_marking = marking.clone();
                        self.net_semantics.execute_transition(&mut new_marking, transition)?;

                        let probability = self.net_semantics.get_transition_weight(marking, transition) / &total_weight;

                        if self.net_semantics.is_transition_silent(transition) {
                            if self.net_semantics.is_final_state(&new_marking) {
                                // transient -> silent -> final (goes in A)
                                let new_markov_marking = MarkovMarking::AbsorbingFinal(new_marking);
                                let column = markov_markings.get(&new_markov_marking).expect("this should not happen");

                                a.element_add(row, column, &probability);
                            } else {
                                // transient -> silent -> transient (goes in B)
                                let new_markov_marking = MarkovMarking::Transient(new_marking);
                                let column = markov_markings.get(&new_markov_marking).expect("this should not happen");

                                b.element_add(row, column, &probability);
                            }
                        } else { //transient -> label -> absorbing (goes in A)
                            let activity = self.net_semantics.get_transition_activity(transition).unwrap();
                            let new_markov_marking = MarkovMarking::AbsorbingActivity(new_marking, activity);
                            let column = markov_markings.get(&new_markov_marking).expect("this should not happen");

                            a.element_add(row, column, &probability);
                        }
                     }        
                },
                MarkovMarking::AbsorbingActivity(_, _) => (),
                MarkovMarking::AbsorbingFinal(_) => (),
            }
        }
        Ok((a, b))
    }
}

impl <S: Displayable> StochasticDeterministicSemantics for DeterministicStochasticSemantics<S> {
    type DState = PMarking<S>;

    fn get_activity_key(&self) -> &ActivityKey {
        &self.net_semantics.get_activity_key()
    }

    fn get_initial_state(&self) -> Result<Self::DState> {
        let mut result = Self::DState {
            hash: 0,
            p_marking: HashMap::new(),
            termination_probability: Fraction::one(),
            activity_2_p_markings: HashMap::new(),
            activity_2_probability: HashMap::new(),
        };
        result.p_marking.insert(self.net_semantics.get_initial_state().clone(), Fraction::one());
        if self.net_semantics.is_final_state(&self.net_semantics.get_initial_state()) {
            result.termination_probability = Fraction::one();
        }
        self.compute_next(&mut result)?;
        return Ok(result);
    }

    fn execute_activity(&self, state: &Self::DState, activity: Activity) -> Result<Self::DState> {
        assert!(state.activity_2_p_markings.contains_key(&activity));

        let mut result = Self::DState {
            hash: 0,
            p_marking: state.activity_2_p_markings.get(&activity).unwrap().clone(),
            termination_probability: Fraction::one(),
            activity_2_p_markings: HashMap::new(),
            activity_2_probability: HashMap::new(),
        };
        self.compute_next(&mut result)?;
        return Ok(result);
    }

    fn get_termination_probability(&self, state: &Self::DState) -> Fraction {
        state.termination_probability.clone()
    }

    fn get_activity_probability(&self, state: &Self::DState, activity: Activity) -> Fraction {
        state.activity_2_probability.get(&activity).unwrap().clone()
    }

    fn get_enabled_activities(&self, state: &Self::DState) -> Vec<Activity> {
        state.activity_2_probability.keys().cloned().collect()
    }
    
}

/**
 * Idea: as the computation of next p-states is expensive, it is performed once, and stored in this p-marking struct.
 * That is, this struct also contains the -next- p-markings.
 */
#[derive(Clone)]
pub struct PMarking<S> where S: Displayable {
    hash: u64,
    pub p_marking: HashMap<S, Fraction>,
    termination_probability: Fraction,
    activity_2_p_markings: HashMap<Activity, HashMap<S, Fraction>>,
    activity_2_probability: HashMap<Activity, Fraction>
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