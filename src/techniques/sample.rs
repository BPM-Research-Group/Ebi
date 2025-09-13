use std::collections::{HashMap, hash_map::Entry};

use anyhow::{Result, anyhow};
use ebi_arithmetic::{ChooseRandomly, Fraction, FractionRandomCache, One, Zero};
use ebi_objects::FiniteStochasticLanguage;
use rand::Rng;

use crate::{
    ebi_framework::displayable::Displayable,
    ebi_traits::{
        ebi_trait_finite_stochastic_language::EbiTraitFiniteStochasticLanguage,
        ebi_trait_stochastic_semantics::EbiTraitStochasticSemantics,
    },
    stochastic_semantics::stochastic_semantics::StochasticSemantics,
};

pub trait Sampler {
    fn sample(&self, number_of_traces: usize) -> Result<FiniteStochasticLanguage>;
}

pub trait Resampler {
    fn resample_cache_init(&self) -> Result<FractionRandomCache>;
    fn resample(&self, cache: &FractionRandomCache, number_of_traces: usize) -> Vec<Fraction>;
}

impl Sampler for EbiTraitStochasticSemantics {
    fn sample(&self, number_of_traces: usize) -> Result<FiniteStochasticLanguage> {
        match self {
            EbiTraitStochasticSemantics::Marking(s) => s.sample(number_of_traces),
            EbiTraitStochasticSemantics::Usize(s) => s.sample(number_of_traces),
            EbiTraitStochasticSemantics::NodeStates(s) => s.sample(number_of_traces),
        }
    }
}

impl Sampler for dyn EbiTraitFiniteStochasticLanguage {
    fn sample(&self, number_of_traces: usize) -> Result<FiniteStochasticLanguage> {
        let mut result = HashMap::new();

        let cache = self.resample_cache_init()?;

        if self.number_of_traces().is_zero() {
            return Err(anyhow!("Cannot sample from empty language."));
        }

        for _ in 0..number_of_traces {
            let trace_index = Fraction::choose_randomly_cached(&cache);

            match result.entry(self.get_trace(trace_index).unwrap().clone()) {
                Entry::Occupied(mut e) => *e.get_mut() += 1,
                Entry::Vacant(e) => {
                    e.insert(Fraction::one());
                }
            };
        }

        Ok((self.activity_key().clone(), result).into())
    }
}

impl<T, State> Sampler for T
where
    T: StochasticSemantics<StoSemState = State> + ?Sized,
    State: Displayable,
{
    fn sample(&self, number_of_traces: usize) -> Result<FiniteStochasticLanguage> {
        if let Some(initial_state) = self.get_initial_state() {
            let mut result = HashMap::new();

            for _ in 0..number_of_traces {
                let mut current_state = initial_state.clone();
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

            Ok((self.activity_key().clone(), result).into())
        } else {
            return Err(anyhow!("Language contains no traces, so cannot sample."));
        }
    }
}

impl Resampler for dyn EbiTraitFiniteStochasticLanguage {
    fn resample_cache_init(&self) -> Result<FractionRandomCache> {
        if self.number_of_traces().is_zero() {
            return Err(anyhow!("Cannot sample from empty language."));
        }

        let probabilities = self.iter_trace_probability().map(|(_, p)| p);
        let cache = Fraction::choose_randomly_create_cache(probabilities)?;

        Ok(cache)
    }

    fn resample(&self, cache: &FractionRandomCache, number_of_traces: usize) -> Vec<Fraction> {
        let mut result = vec![0usize; self.number_of_traces()];
        for _ in 0..number_of_traces {
            let trace_index = Fraction::choose_randomly_cached(&cache);
            result[trace_index] += 1;
        }

        //normalise vector
        result
            .into_iter()
            .map(|c| Fraction::from((c, number_of_traces)))
            .collect()
    }
}

/**
 * Fills the given vector with uniformly random numbers in the range 0..number_of_indices
 */
pub fn sample_indices_uniform(number_of_indices: usize, result: &mut Vec<usize>) {
    let mut rng = rand::rng();
    for i in 0..result.len() {
        let trace_index = rng.random_range(0..number_of_indices);
        result[i] = trace_index;
    }
}

#[cfg(test)]
mod tests {
    use std::fs;

    use ebi_objects::FiniteStochasticLanguage;

    use crate::ebi_traits::{
        ebi_trait_finite_stochastic_language::EbiTraitFiniteStochasticLanguage,
        ebi_trait_stochastic_semantics::{EbiTraitStochasticSemantics, ToStochasticSemantics},
    };

    use super::Sampler;

    #[test]
    fn sample_finite_stochastic_language() {
        let fin = fs::read_to_string("testfiles/aa.slang").unwrap();
        let slpn = fin.parse::<FiniteStochasticLanguage>().unwrap();

        let slpn: Box<dyn EbiTraitFiniteStochasticLanguage> = Box::new(slpn);

        let sample = slpn.sample(10).unwrap();

        let slpn2 = fin.parse::<FiniteStochasticLanguage>().unwrap();

        assert_eq!(sample, slpn2);
    }

    #[test]
    fn sample_stochastic_semantics() {
        let fin = fs::read_to_string("testfiles/aa.slang").unwrap();
        let slpn = fin.parse::<FiniteStochasticLanguage>().unwrap();

        let slpn: EbiTraitStochasticSemantics = slpn.to_stochastic_semantics();

        let sample = slpn.sample(10).unwrap();

        let slpn2 = fin.parse::<FiniteStochasticLanguage>().unwrap();

        assert_eq!(sample, slpn2);
    }

    #[test]
    fn sample_finite_stochastic_language_empty() {
        let fin = fs::read_to_string("testfiles/empty.slang").unwrap();
        let slpn = fin.parse::<FiniteStochasticLanguage>().unwrap();

        let slpn: Box<dyn EbiTraitFiniteStochasticLanguage> = Box::new(slpn);

        assert!(slpn.sample(10).is_err());
    }

    #[test]
    fn sample_stochastic_semantics_empty() {
        let fin = fs::read_to_string("testfiles/empty.slang").unwrap();
        let slpn = fin.parse::<FiniteStochasticLanguage>().unwrap();

        let slpn: EbiTraitStochasticSemantics = slpn.to_stochastic_semantics();

        assert!(slpn.sample(10).is_err());
    }
}
