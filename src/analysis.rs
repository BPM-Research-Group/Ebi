use std::{collections::{HashMap, HashSet}, fmt::{Debug, Display}};
use core::hash::Hash;
use crate::{activity_key::Activity, ebi_objects::finite_stochastic_language::FiniteStochasticLanguage, ebi_traits::{ebi_trait_finite_stochastic_language::EbiTraitFiniteStochasticLanguage, ebi_trait_stochastic_deterministic_semantics::{EbiTraitStochasticDeterministicSemantics, StochasticDeterministicSemantics}}, math::fraction::Fraction, Trace};
use anyhow::{anyhow, Result};
use clap::error;
use fraction::{One, Zero};
use priority_queue::PriorityQueue;

pub trait FiniteStochasticLanguageAnalyser {
    fn analyse_most_likely_traces(&self, number_of_traces: &usize) -> Result<FiniteStochasticLanguage>;
}

impl FiniteStochasticLanguageAnalyser for dyn EbiTraitFiniteStochasticLanguage {
    fn analyse_most_likely_traces(&self, number_of_traces: &usize) -> Result<FiniteStochasticLanguage> {
        if number_of_traces.is_one() {
            let mut result = HashMap::new();

            let (mut max_trace, mut max_probability) = self.iter_trace_probability().next().ok_or_else(|| anyhow!("Finite stochastic language is empty."))?;

            for (trace, probability) in self.iter_trace_probability() {
                if probability > max_probability {
                    max_trace = trace;
                    max_probability = probability;
                }
            }

            result.insert(max_trace.clone(), max_probability.clone());

            Ok((result, self.get_activity_key().clone()).into())
        } else {
            let mut result = vec![];
            for (trace, probability) in self.iter_trace_probability() {
                match result.binary_search_by(|&(_, cmp_probability): &(_, &Fraction)| cmp_probability.cmp(probability)) {
                    Ok(index) => if index < number_of_traces - 1 { result.insert(index, (trace, probability)) },
                    Err(index) => if index < number_of_traces - 1 { result.insert(index, (trace, probability)) },
                }

            }

            let mut result2 = HashMap::new();
            for (trace, probability) in result {
                result2.insert(trace.clone(), probability.clone());
            }

            Ok((result2, self.get_activity_key().clone()).into())
        }
    }
}

impl <FS: Hash + Display + Debug + Clone + Eq> dyn StochasticDeterministicSemantics<DState = FS> {

    pub fn analyse_minimum_probability(&self, at_least: &Fraction) -> Result<FiniteStochasticLanguage> {
        let error_at_loop = at_least.is_zero(); //If at_least is zero, we are asked to return all traces. If there is a loop, then this is impossible.
        
        let mut seen = HashSet::new();
        if error_at_loop {
            seen.insert(self.get_initial_state()?);
        }

        log::info!("start minimum probability analysis");
        let mut result = HashMap::new();

        let mut queue = vec![];
        let initial_q_state = 
        queue.push(X{
            prefix: vec![],
            probability: Fraction::one(),
            p_state: self.get_initial_state()?,
        });

        while let Some(x) = queue.pop() {
            // log::debug!("queue length {}, process p-state {:?}", queue.len(), x.p_state);

            let mut probability_terminate_in_this_state = self.get_termination_probability(&x.p_state);
            // log::debug!("probability termination in this state {}", probability_terminate_in_this_state);
            probability_terminate_in_this_state *= &x.probability;

            if !probability_terminate_in_this_state.is_zero() && probability_terminate_in_this_state >= *at_least {
                result.insert(x.prefix.clone(), probability_terminate_in_this_state);
            }

            let mut iff = self.get_termination_probability(&x.p_state).one_minus();
            iff *= &x.probability;
            if iff >= *at_least {
                for activity in self.get_enabled_activities(&x.p_state) {
                    // log::info!("consider activity {}", activity);

                    let probability = self.get_activity_probability(&x.p_state, activity);

                    // log::info!("activity has probability {}", probability);

                    let new_p_state = self.execute_activity(&x.p_state, activity)?;

                    // log::info!("activity executed");

                    if error_at_loop {
                        if !seen.insert(new_p_state.clone()) {
                            return Err(anyhow!("As the language is not finite, we cannot return all traces that have a minimum probability of 0."));
                        }
                    }

                    let mut new_prefix = x.prefix.clone();
                    new_prefix.push(activity);
                    let new_x = X {
                        prefix: new_prefix,
                        probability: &x.probability * &probability,
                        p_state: new_p_state,
                    };

                    queue.push(new_x);
                }
            }
        }

        if result.is_empty() {
            return Err(anyhow!("Analsyis returned an empty language; there are no traces that have a probability of at least {}.", at_least));
        }

        Ok((result, self.get_activity_key().clone()).into())
    }

    pub fn analyse_most_likely_traces_i(&self, number_of_traces: &usize) -> Result<FiniteStochasticLanguage> {
        log::info!("start number of traces analysis");
        let mut result = HashMap::new();

        let mut queue = PriorityQueue::new();
        queue.push(Y::Prefix(vec![], self.get_initial_state()?), Fraction::one());

        while let Some((y, y_probability)) = queue.pop() {
            match y {
                Y::Prefix(prefix, p_state) => {

                    let termination_proability = self.get_termination_probability(&p_state);
                    if !termination_proability.is_zero() {
                        queue.push(Y::Trace(prefix.clone()), &y_probability * &termination_proability);
                    }

                    for activity in self.get_enabled_activities(&p_state) {
                        let activity_probability = self.get_activity_probability(&p_state, activity);
        
                        let new_p_state = self.execute_activity(&p_state, activity)?;
                        let mut new_prefix = prefix.clone();
                        new_prefix.push(activity);
                        let new_probability = &y_probability * &activity_probability;
        
                        queue.push(Y::Prefix(new_prefix, new_p_state), new_probability);
                    }
                },
                Y::Trace(trace) => {
                    // if trace.len() > 2 {
                        result.insert(trace, y_probability);
                        if result.len() >= *number_of_traces {
                            break;
                        }
                    // }
                },
            }
        }


        if result.is_empty() {
            return Err(anyhow!("Analysis returned an empty language; there are no traces in the model."));
        }

        Ok((result, self.get_activity_key().clone()).into())
    }
}

struct X<FS: Hash + Display + Debug + Clone + Eq> {
    prefix: Vec<Activity>,
    probability: Fraction,
    p_state: FS
}

enum Y<FS: Hash + Display + Debug + Clone + Eq> {
    Prefix(Vec<Activity>, FS),
    Trace(Vec<Activity>)
}

impl <FS: Hash + Display + Debug + Clone + Eq> Eq for Y<FS> {}

impl <FS: Hash + Display + Debug + Clone + Eq> PartialEq for Y<FS> {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Prefix(l0, _), Self::Prefix(r0, _)) => l0 == r0,
            (Self::Trace(l0), Self::Trace(r0)) => l0 == r0,
            _ => false,
        }
    }
}

impl <FS: Hash + Display + Debug + Clone + Eq> Hash for Y<FS> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        match self {
            Y::Prefix(t, _) => t.hash(state),
            Y::Trace(t) => t.hash(state),
        }
    }
}