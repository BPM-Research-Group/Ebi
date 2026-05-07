use crate::{
    ebi_traits::ebi_trait_finite_stochastic_language::EbiTraitFiniteStochasticLanguage,
    math::log_div::LogDiv, techniques::livelock::IsPartOfLivelock,
};
use ebi_objects::{
    StochasticDeterministicFiniteAutomaton,
    anyhow::{Context, Result, anyhow},
    ebi_arithmetic::{
        EbiMatrix, Fraction, FractionMatrix, IdentityMinus, Inversion, One, Signed, Zero, f,
        is_exact_globally,
    },
};
use std::ops::Neg;

pub trait Entropy {
    /// Returns the entropy of the struct.
    /// Given current restrictions, this function is only available in approximate mode. It may return an Err when called in exact mode.
    /// This is, for now, for two reasons: n_log_n is prohibitively expensive to compute, and the average number of visits per state is computed using an approximation algorithm.
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
        let state_visits = number_of_average_visits_per_state_approximate(self)?;

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
pub fn number_of_average_visits_per_state_approximate(
    sdfa: &StochasticDeterministicFiniteAutomaton,
) -> Result<Vec<Fraction>> {
    if is_exact_globally() {
        return Err(anyhow!(
            "Number of average visits per state is only available in approximate mode."
        ));
    }
    let epsilon = f!(1, 1_000_000_000_000u64);
    let max_iterations = 20_000;
    c_s_iterative(sdfa, &epsilon, max_iterations)
}

pub fn number_of_average_visits_per_state_exact(
    sdfa: &StochasticDeterministicFiniteAutomaton,
) -> Result<Vec<Option<Fraction>>> {
    //verify initial marking
    let initial_state = if let Some(initial_state) = sdfa.get_initial_state() {
        initial_state
    } else {
        //This has no initial state, thus its states will never be visited.
        return Ok(vec![Some(Fraction::zero()); sdfa.number_of_states()]);
    };

    //obtain transient states
    let mut state_2_transient_state = vec![None; sdfa.number_of_states()];
    let mut transient_state_2_state = vec![];
    {
        let mut livelock_cache = sdfa.get_livelock_cache();
        for state in 0..sdfa.number_of_states() {
            if livelock_cache.is_state_part_of_livelock(&state)? {
                let transient_state = transient_state_2_state.len();
                transient_state_2_state.push(state);
                state_2_transient_state[state] = Some(transient_state);
            }
        }
    }

    //verify that the initial state is transient
    if state_2_transient_state[initial_state].is_none() {
        // the initial state is recurring, so we will visit every reachable state infinitely often.
        return Ok(vec![None; sdfa.number_of_states()]);
    }

    //create transient state matrix (PT in https://www.columbia.edu/~ks20/4106-18-Fall/Notes-Transient.pdf)
    let mut matrix =
        FractionMatrix::new(transient_state_2_state.len(), transient_state_2_state.len());
    for (source, (target, probability)) in sdfa
        .sources
        .iter()
        .zip(sdfa.targets.iter().zip(sdfa.probabilities))
    {
        if let (Some(source_transient), Some(target_transient)) = (
            state_2_transient_state[*source],
            state_2_transient_state[*target],
        ) {
            //add transition
            matrix.set(source_transient, target_transient, probability);
        }
    }

    matrix.identity_minus();
    matrix = matrix
        .invert()
        .with_context(|| anyhow!("Cannot compute (I - PT)^-1."))?;

    matrix[initial_state, ]

    Ok(())
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
