use crate::{
    ebi_traits::ebi_trait_finite_stochastic_language::EbiTraitFiniteStochasticLanguage,
    techniques::livelock::IsPartOfLivelock,
};
use ebi_objects::{
    AutomatonSemantics, StochasticAutomatonSemantics, StochasticDeterministicFiniteAutomaton,
    anyhow::{Context, Result, anyhow},
    ebi_arithmetic::{
        EbiMatrix, Fraction, FractionMatrix, IdentityMinus, Inversion, Log, One, Signed, Zero, f,
    },
};
use ebi_optimisation::ebi_arithmetic::log_polynomial::log_polynomial::LogPolynomial;
use std::ops::Neg;

pub trait Entropy {
    /// Returns the entropy of the struct.
    fn entropy(&self) -> Result<LogPolynomial>;
}

impl<T: ?Sized> Entropy for T
where
    T: EbiTraitFiniteStochasticLanguage,
{
    fn entropy(&self) -> Result<LogPolynomial> {
        let mut entropy = LogPolynomial::zero();

        for (_, probability) in self.iter_traces_probabilities() {
            if probability.is_positive() {
                entropy -= probability.n_log_n()?
            }
        }

        Ok(entropy)
    }
}

impl Entropy for StochasticDeterministicFiniteAutomaton {
    fn entropy(&self) -> Result<LogPolynomial> {
        let state_visits = number_of_average_visits_per_state(self)?;

        let mut sum = LogPolynomial::zero();

        //transitions
        for (transition, source, _, _) in self.transitions() {
            if let Some(average_visits) = &state_visits[source] {
                let probability = self
                    .transition_2_weight(source, transition)
                    .ok_or_else(|| anyhow!("Transition weight not found."))?;
                if probability.is_positive() {
                    let mut entropy = probability.n_log_n()?;
                    entropy *= average_visits;
                    sum += entropy;
                }
            }
        }

        return Ok(-sum);
    }
}

/// Returns how often each state is visited on average on an arbitrary run.
/// If a state is part of a livelock, returns None for that state (even if it is unreachable).
pub fn number_of_average_visits_per_state(
    sdfa: &StochasticDeterministicFiniteAutomaton,
) -> Result<Vec<Option<Fraction>>> {
    //verify initial marking
    let initial_state = if let Some(initial_state) = sdfa.initial_state() {
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
        for state in sdfa.states() {
            if !livelock_cache.is_state_part_of_livelock(&state)? {
                let transient_state = transient_state_2_state.len();
                transient_state_2_state.push(state);
                state_2_transient_state[state] = Some(transient_state);
            }
        }
    }

    //verify that the initial state is transient
    if state_2_transient_state[initial_state].is_none() {
        // The initial state is recurring, so we will visit every reachable state infinitely often.
        // It is up to the caller to figure out whether a state is reachable, should that be of interest.
        return Ok(vec![None; sdfa.number_of_states()]);
    }

    //create transient state matrix (PT in https://www.columbia.edu/~ks20/4106-18-Fall/Notes-Transient.pdf)
    let mut matrix =
        FractionMatrix::new(transient_state_2_state.len(), transient_state_2_state.len());
    for (source, (target, probability)) in sdfa
        .sources
        .iter()
        .zip(sdfa.targets.iter().zip(sdfa.probabilities.iter()))
    {
        if let (Some(source_transient), Some(target_transient)) = (
            state_2_transient_state[*source],
            state_2_transient_state[*target],
        ) {
            //add transition
            matrix.set(source_transient, target_transient, probability.clone());
        }
    }

    //compute inverse(I - matrix)
    matrix.identity_minus();
    matrix = matrix
        .invert()
        .with_context(|| anyhow!("Cannot compute inversion of (I - PT)."))?;

    //read result
    // It is up to the caller to figure out whether a recurrent state is reachable, should that be of interest.
    let mut result = vec![None; sdfa.number_of_states()];
    for (transient_state, state) in transient_state_2_state.iter().enumerate() {
        result[*state] = matrix.get(initial_state.0, transient_state);
    }

    Ok(result)
}

/// Compute the number of times each state is visited, on average.
/// This is an approximating algorithm. A livelock state will get an arbitrary high value.
pub(crate) fn number_of_average_visits_per_state_approximate(
    sdfa: &StochasticDeterministicFiniteAutomaton,
) -> Result<Vec<Fraction>> {
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
    let Some(initial_state) = sdfa.initial_state() else {
        return Ok(vec![Fraction::zero(); state_count]);
    };

    let mut state_visits = vec![Fraction::zero(); state_count];
    let mut next_state_visits = vec![Fraction::zero(); state_count];

    for _ in 0..max_iterations {
        for value in &mut next_state_visits {
            *value = Fraction::zero();
        }

        for (index, &source) in sdfa.sources.iter().enumerate() {
            let target = sdfa.targets[index];
            let probability = &sdfa.probabilities[index];

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

#[cfg(test)]
mod tests {
    use crate::techniques::entropy::{Entropy, c_s_iterative, number_of_average_visits_per_state};
    use ebi_objects::{
        StochasticDeterministicFiniteAutomaton,
        anyhow::Result,
        ebi_arithmetic::{Fraction, f, fraction::approximate::Approximate},
    };
    use std::fs;

    fn compute_approx(
        sdfa: &StochasticDeterministicFiniteAutomaton,
    ) -> Result<Vec<Option<Fraction>>> {
        let epsilon = f!(1, 1_000_000_000_000u64);
        let max_iterations = 20_000;
        Ok(c_s_iterative(sdfa, &epsilon, max_iterations)?
            .into_iter()
            .map(|x| Some(x))
            .collect())
    }

    #[test]
    fn number_of_average_visits_per_state_test() {
        let fin = fs::read_to_string("testfiles/aa-ab-ba.sdfa").unwrap();
        let sdfa = fin
            .parse::<StochasticDeterministicFiniteAutomaton>()
            .unwrap();

        let approx = compute_approx(&sdfa).unwrap();
        let exact = number_of_average_visits_per_state(&sdfa).unwrap();
        assert_eq!(approx, exact);
        println!("approx {:?}", approx);
        println!("exact  {:?}", exact);
    }

    #[test]
    fn number_of_average_visits_per_state_livelock() {
        let fin = fs::read_to_string("testfiles/a-livelock.sdfa").unwrap();
        let sdfa = fin
            .parse::<StochasticDeterministicFiniteAutomaton>()
            .unwrap();

        let approx = compute_approx(&sdfa).unwrap();
        let exact = number_of_average_visits_per_state(&sdfa).unwrap();
        assert_eq!(approx[0], exact[0]);
        println!("approx {:?}", approx);
        println!("exact  {:?}", exact);
    }

    #[test]
    fn entropy_livelock() {
        let fin = fs::read_to_string("testfiles/a-b-livelock.sdfa").unwrap();
        let sdfa = fin
            .parse::<StochasticDeterministicFiniteAutomaton>()
            .unwrap();

        let entropy = sdfa.entropy().unwrap();

        assert_eq!(entropy.approximate().unwrap(), 1.5);
    }
}
