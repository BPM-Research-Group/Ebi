use std::collections::{hash_map::Entry, HashMap};

use anyhow::{anyhow, Result};
use rand::Rng;

use crate::{
    ebi_framework::displayable::Displayable,
    ebi_objects::finite_stochastic_language::FiniteStochasticLanguage,
    ebi_traits::{
        ebi_trait_finite_stochastic_language::EbiTraitFiniteStochasticLanguage,
        ebi_trait_stochastic_semantics::{EbiTraitStochasticSemantics, StochasticSemantics},
    },
    math::{fraction::{ChooseRandomly, Fraction}, traits::One},
};

pub trait Sampler {
    fn sample(&self, number_of_traces: usize) -> Result<FiniteStochasticLanguage>;
}

impl Sampler for EbiTraitStochasticSemantics {
    fn sample(&self, number_of_traces: usize) -> Result<FiniteStochasticLanguage> {
        match self {
            EbiTraitStochasticSemantics::Marking(s) => s.sample(number_of_traces),
            EbiTraitStochasticSemantics::Usize(s) => s.sample(number_of_traces),
        }
    }
}

impl Sampler for dyn EbiTraitFiniteStochasticLanguage {
    fn sample(&self, number_of_traces: usize) -> Result<FiniteStochasticLanguage> {
        let mut result = HashMap::new();

        for _ in 0..number_of_traces {
            let trace_index = rand::thread_rng().gen_range(0..self.len());

            match result.entry(self.get_trace(trace_index).unwrap().clone()) {
                Entry::Occupied(mut e) => *e.get_mut() += 1,
                Entry::Vacant(e) => {
                    e.insert(Fraction::one());
                }
            };
        }

        Ok((result, self.get_activity_key().clone()).into())
    }
}

impl<T, State> Sampler for T
where
    T: StochasticSemantics<StoSemState = State> + ?Sized,
    State: Displayable,
{
    fn sample(&self, number_of_traces: usize) -> Result<FiniteStochasticLanguage> {
        let mut result = HashMap::new();

        for _ in 0..number_of_traces {
            let mut current_state = self.get_initial_state();
            let mut trace = vec![];

            let mut outgoing_probabilities = vec![];

            while !self.is_final_state(&current_state) {
                let total_weight = self
                    .get_total_weight_of_enabled_transitions(&current_state)
                    .unwrap();
                let enabled_transitions = self.get_enabled_transitions(&current_state);

                outgoing_probabilities.clear();
                for transition in &enabled_transitions {
                    outgoing_probabilities.push(
                        self.get_transition_weight(&current_state, *transition) / &total_weight,
                    );
                }
                // get firing transition
                let i = Fraction::choose_randomly(&outgoing_probabilities)?;
                let chosen_transition = enabled_transitions[i];

                // execute transition
                self.execute_transition(&mut current_state, chosen_transition)?;

                match self.get_transition_activity(chosen_transition) {
                    Some(activity) => trace.push(activity),
                    None => {}
                }
            }

            match result.entry(trace) {
                Entry::Occupied(mut e) => *e.get_mut() += 1,
                Entry::Vacant(e) => {
                    e.insert(Fraction::one());
                }
            };
        }

        if result.is_empty() {
            return Err(anyhow!(
                "Analysis resulted in an empty language; there are no traces in the model."
            ));
        }

        // log::debug!("Sampled {:?} traces", result);

        Ok((result, self.get_activity_key().clone()).into())
    }
}

/**
 * Fills the given vector with random numbers in the range 0..number_of_indices
 */
pub fn sample_indices(number_of_indices: usize, result: &mut Vec<usize>) {
    for i in 0..result.len() {
        let trace_index = rand::thread_rng().gen_range(0..number_of_indices);
        result[i] = trace_index;
    }
}
