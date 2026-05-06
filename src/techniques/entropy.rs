use std::ops::Neg;

use crate::{
    ebi_traits::ebi_trait_finite_stochastic_language::EbiTraitFiniteStochasticLanguage,
    math::log_div::LogDiv,
};
use ebi_objects::{
    StochasticDeterministicFiniteAutomaton,
    anyhow::{Result, anyhow},
    ebi_arithmetic::{Fraction, One, Signed, Zero, f, is_exact_globally},
};

pub trait Entropy {
    /// Returns the entropy of the struct.
    /// Given current restrictions, this function is only available in approximate mode. It may return an Err when called in exact mode.
    fn entropy_approximate(&self) -> Result<LogDiv>;
}

impl<T: ?Sized> Entropy for T
where
    T: EbiTraitFiniteStochasticLanguage,
{
    fn entropy_approximate(&self) -> Result<LogDiv> {
        if is_exact_globally() {
            return Err(anyhow!("Entropy is only available in approximate mode."));
        }

        let mut entropy = LogDiv::zero();

        for (_, probability) in self.iter_traces_probabilities() {
            if probability.is_positive() {
                entropy -= LogDiv::n_log_n(probability)?
            }
        }

        Ok(entropy)
    }
}

impl Entropy for StochasticDeterministicFiniteAutomaton {
    fn entropy_approximate(&self) -> Result<LogDiv> {
        if is_exact_globally() {
            return Err(anyhow!("Entropy is only available in approximate mode."));
        }

        let state_count = self.number_of_states();
        let state_visits = number_of_average_visits_per_state(self)?;

        let mut termination_entropy = LogDiv::zero();
        for state in 0..state_count {
            let termination_probability = self.get_termination_probability(state);
            if termination_probability.is_positive() {
                let mut entropy = LogDiv::n_log_n(termination_probability)?;
                entropy *= &state_visits[state];
                termination_entropy += entropy;
            }
        }

        let mut transition_entropy = LogDiv::zero();
        for (index, &source) in self.get_sources().iter().enumerate() {
            let probability = &self.get_probabilities()[index];
            if probability.is_positive() {
                let mut entropy = LogDiv::n_log_n(probability)?;
                entropy *= &state_visits[source];
                transition_entropy += entropy;
            }
        }

        return Ok(transition_entropy + termination_entropy);
    }
}

/// Compute the number of times each state is visited, on average.
/// This is an approximating algorithm. Do not call in exact mode (will return an Err).
pub fn number_of_average_visits_per_state(
    sdfa: &StochasticDeterministicFiniteAutomaton,
) -> Result<Vec<Fraction>> {
    if is_exact_globally() {
        return Err(anyhow!("Number of average visits per state is only available in approximate mode."));
    }
    let epsilon = f!(1, 1_000_000_000_000u64);
    let max_iterations = 20_000;
    c_s_iterative(sdfa, &epsilon, max_iterations)
}

/// Compute c_s by fixed-point iteration.
/// c[s] is the expected number of visits to state s.
fn c_s_iterative(
    sdfa: &StochasticDeterministicFiniteAutomaton,
    epsilon: &Fraction,
    max_iterations: usize,
) -> Result<Vec<Fraction>> {
    let state_count = sdfa.number_of_states();
    let Some(initial_state) = sdfa.get_initial_state() else {
        return Ok(vec![Fraction::zero(); state_count]);
    };

    let mut state_visits = vec![Fraction::zero(); state_count];
    let mut next_state_visits = vec![Fraction::zero(); state_count];

    for _ in 0..max_iterations {
        for value in &mut next_state_visits {
            *value = Fraction::zero();
        }

        for (index, &source) in sdfa.get_sources().iter().enumerate() {
            let target = sdfa.get_targets()[index];
            let probability = &sdfa.get_probabilities()[index];

            if !probability.is_zero() {
                next_state_visits[target] += &state_visits[source] * probability;
            }
        }

        next_state_visits[initial_state] += Fraction::one();

        let mut max_diff = Fraction::zero();
        for state in 0..state_count {
            let mut diff = &next_state_visits[state] - &state_visits[state];

            if diff.is_negative() {
                diff = diff.neg();
            }

            if diff > max_diff {
                max_diff = diff;
            }
        }

        std::mem::swap(&mut state_visits, &mut next_state_visits);

        if &max_diff < epsilon {
            break;
        }
    }

    Ok(state_visits)
}
