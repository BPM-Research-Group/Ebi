use std::ops::Neg;

use ebi_objects::{
    anyhow::{anyhow, Result},
    ebi_arithmetic::{f, Fraction, One, Signed, Zero},
    traits::trace_iterators::IntoRefTraceProbabilityIterator,
    FiniteStochasticLanguage,
    StochasticDeterministicFiniteAutomaton,  
};
 use crate::ebi_traits::{
        ebi_trait_queriable_stochastic_language::EbiTraitQueriableStochasticLanguage,
    };
use crate::follower_semantics::FollowerSemantics;
use crate::math::log_div::LogDiv;


/// Default lambda = 1e-6.
pub fn default_lambda() -> Fraction {
    f!(1i64, 1_000_000i64)
}

/// A simple list of lambda values for tests.
pub fn lambda_sweep_defaults() -> Vec<Fraction> {
    vec![
        Fraction::zero(),       // lambda = 0 (original)
        f!(1i64, 1_000_000i64), // 1e-6 (default)
        f!(1i64, 10_000i64),    // 1e-4
        f!(1i64, 100i64),       // 1e-2
    ]
}

/// Check whether lambda is in [0, 1].
fn check_lambda(lambda: &Fraction) -> Result<()> {
    if lambda.is_negative() || *lambda > Fraction::one() {
        return Err(anyhow!("lambda must be in [0, 1], got {}", lambda));
    }
    Ok(())
}

/// Resolve the smoothing parameter.
/// If `lambda` is `None`, the default value is used.
fn resolve_lambda_checked(lambda: Option<&Fraction>) -> Result<Fraction> {
    let resolved = lambda.cloned().unwrap_or_else(default_lambda);
    check_lambda(&resolved)?;
    Ok(resolved)
}

/// Check whether the SDFA is a valid stochastic model.
///
/// Rules:
/// 1. It can have no initial state.
/// 2. For each state, outgoing probabilities plus termination probability sum to 1.
/// 3. Each transition probability is strictly positive.
/// 4. At least one state has a non-zero termination probability.
pub fn is_valid_sdfa(sdfa: &StochasticDeterministicFiniteAutomaton) -> Result<()> {
    if sdfa.get_initial_state().is_none() {
        return Ok(());
    }

    let state_count = sdfa.number_of_states();

    for state in 0..state_count {
        let mut total_probability = Fraction::zero();

        for (index, &source) in sdfa.get_sources().iter().enumerate() {
            if source == state {
                total_probability += sdfa.get_probabilities()[index].clone();
            }
        }

        total_probability += sdfa.get_termination_probability(state).clone();

        if !total_probability.is_one() {
            return Err(anyhow!(
                "invalid probability sum at state {}: total = {} (expected 1)",
                state,
                total_probability
            ));
        }
    }

    for (index, probability) in sdfa.get_probabilities().iter().enumerate() {
        if probability.is_zero() || probability.is_negative() {
            return Err(anyhow!(
                "invalid transition probability at index {}: {} (must be > 0)",
                index,
                probability
            ));
        }
    }

    let has_terminating_state =
        (0..state_count).any(|state| !sdfa.get_termination_probability(state).is_zero());

    if !has_terminating_state {
        return Err(anyhow!("no terminating state found (model may livelock)"));
    }

    Ok(())
}

/// Entropy helper: h(p) = -p * log(p).
/// If p = 0, return 0.
fn entropy_term(p: &Fraction) -> LogDiv {
    if p.is_zero() {
        LogDiv::zero()
    } else {
        let term = LogDiv::n_log_n(p).unwrap();
        - term
    }
}

/// Entropy of the event log with lambda.
/// If lambda = 0, this becomes the normal entropy.
pub fn entropy_eventlog_with_lambda(fsl: FiniteStochasticLanguage, lambda: &Fraction) -> Result<LogDiv> {
    check_lambda(lambda)?;

    if lambda.is_zero() {
        let mut entropy = LogDiv::zero();

        for (_, probability) in fsl.iter_traces_probabilities() {
            entropy += entropy_term(probability);
        }

        return Ok(entropy);
    }
    let one_minus_lambda = &Fraction::one() - lambda;
    let mut entropy = LogDiv::zero();

    for (_, probability) in fsl.iter_traces_probabilities() {
        entropy += entropy_term(&(probability * &one_minus_lambda));
        entropy += entropy_term(&(probability * lambda));
    }

    Ok(entropy)
}

/// A wrapper for normal entropy (lambda = 0).
pub fn entropy_eventlog(fsl: FiniteStochasticLanguage) -> Result<LogDiv> {
    entropy_eventlog_with_lambda(fsl, &Fraction::zero())
        
}

fn log_div_min(left: &LogDiv, right: &LogDiv) -> LogDiv {
    debug_assert!(left.partial_cmp(right).is_some(), "incomparable LogDiv values");

    if left <= right {
        left.clone()
    } else {
        right.clone()
    }
}

/// Gain numerator with lambda.
/// Only traces that exist in both the event log and the model are counted.
/// For each trace, use min(-pL log pL, -pM log pM).
pub fn gain_numerator_lambda(
    fsl: FiniteStochasticLanguage,
    sdfa: StochasticDeterministicFiniteAutomaton,
    lambda: &Fraction,
) -> Result<LogDiv> {
    check_lambda(lambda)?;
    let one_minus_lambda = &Fraction::one() - lambda;
    let mut numerator = LogDiv::zero();

    for (trace, log_probability) in fsl.iter_traces_probabilities() {
        let model_probability = sdfa.get_probability(&FollowerSemantics::Trace(trace))?;

        if model_probability.is_zero() {
            continue;
        }

        if lambda.is_zero() {
            let log_entropy = entropy_term(log_probability);
            let model_entropy = entropy_term(&model_probability);
            numerator += log_div_min(&log_entropy, &model_entropy);
        } else {
            let log_main_entropy =
                entropy_term(&(log_probability * &one_minus_lambda));
            let model_main_entropy =
                entropy_term(&(&model_probability * &one_minus_lambda));
            numerator += log_div_min(&log_main_entropy, &model_main_entropy);

            let log_tail_entropy = entropy_term(&(log_probability * lambda));
            let model_tail_entropy = entropy_term(&(&model_probability * lambda));

            numerator += log_div_min(&log_tail_entropy, &model_tail_entropy);
        }
    }

    Ok(numerator)
}

/// Wrapper for the normal numerator (lambda = 0).
pub fn gain_numerator(
    fsl: FiniteStochasticLanguage,
    sdfa: StochasticDeterministicFiniteAutomaton,
) -> Result<LogDiv> {
    gain_numerator_lambda(fsl, sdfa, &Fraction::zero())
}

/// Compute c_s by fixed-point iteration.
/// c[s] is the expected number of visits to state s.
pub fn c_s_iterative(
    sdfa: &StochasticDeterministicFiniteAutomaton,
    epsilon: Fraction,
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

        if max_diff < epsilon {
            break;
        }
    }

    Ok(state_visits)
}

pub fn c_s(sdfa: &StochasticDeterministicFiniteAutomaton) -> Result<Vec<Fraction>> {
    let epsilon = f!(1i64, 1_000_000_000_000i64);
    let max_iterations = 20_000;
    c_s_iterative(sdfa, epsilon, max_iterations)
}

/// Entropy of the SDFA with lambda.
/// Note: this code applies lambda on termination only.
pub fn entropy_sdfa_with_lambda(
    sdfa: &StochasticDeterministicFiniteAutomaton,
    lambda: &Fraction,
) -> Result<LogDiv> {
    check_lambda(lambda)?;
    is_valid_sdfa(sdfa)?;

    if lambda.is_zero() {
        let state_count = sdfa.number_of_states();
        let state_visits = c_s(sdfa)?;

        let mut termination_entropy = LogDiv::zero();
        for state in 0..state_count {
            let termination_probability = sdfa.get_termination_probability(state);
            if termination_probability.is_zero() {
                continue;
            }

            let mut entropy = entropy_term(termination_probability);
            entropy *= &state_visits[state];
            termination_entropy += entropy;
        }

        let mut transition_entropy = LogDiv::zero();
        for (index, &source) in sdfa.get_sources().iter().enumerate() {
            let probability = &sdfa.get_probabilities()[index];
            if probability.is_zero() {
                continue;
            }

            let mut entropy = entropy_term(probability);
            entropy *= &state_visits[source];
            transition_entropy += entropy;
        }

        return Ok(transition_entropy + termination_entropy);
    }

    let state_count = sdfa.number_of_states();
    let state_visits = c_s(sdfa)?;
    let one_minus_lambda = Fraction::one() - lambda.clone();

    let mut transition_entropy = LogDiv::zero();
    for (index, &source) in sdfa.get_sources().iter().enumerate() {
        let probability = &sdfa.get_probabilities()[index];
        if probability.is_zero() {
            continue;
        }

        let mut entropy = entropy_term(probability);
        entropy *= &state_visits[source];
        transition_entropy += entropy;
    }

    let mut termination_entropy = LogDiv::zero();
    let mut lambda_step_entropy = LogDiv::zero();

    for state in 0..state_count {
        let termination_probability = sdfa.get_termination_probability(state);
        if termination_probability.is_zero() {
            continue;
        }

        let main_probability = termination_probability * &one_minus_lambda;
        if !main_probability.is_zero() {
            let mut entropy = entropy_term(&main_probability);
            entropy *= &state_visits[state];
            termination_entropy += entropy;
        }

        let tail_probability = termination_probability.clone() * lambda.clone();
        if !tail_probability.is_zero() {
            let mut entropy = entropy_term(&tail_probability);
            entropy *= &state_visits[state];
            lambda_step_entropy += entropy;
        }
    }

    Ok(transition_entropy + termination_entropy + lambda_step_entropy)
}

/// Wrapper for normal model entropy (lambda = 0).
pub fn entropy_sdfa(sdfa: &StochasticDeterministicFiniteAutomaton) -> Result<LogDiv> {
    entropy_sdfa_with_lambda(sdfa, &Fraction::zero())
}

/// ratio = numerator / denominator
fn ratio(num: LogDiv, denom: LogDiv) -> Result<Fraction> {
    let numerator = num.approximate();
    let denominator = denom.approximate();

    if denominator.is_zero() {
        return Err(anyhow!("denominator is zero"));
    }

    Ok(numerator / denominator)
}

/// Precision(L, M, λ) = Numerator(L, M, λ) / H(M, λ)
pub fn potential_gain_precision(
    fsl: FiniteStochasticLanguage,
    sdfa: StochasticDeterministicFiniteAutomaton,
    lambda: Option<&Fraction>,
) -> Result<Fraction> {
    let resolved_lambda = resolve_lambda_checked(lambda)?;
    is_valid_sdfa(&sdfa)?;

    let denominator = entropy_sdfa_with_lambda(&sdfa, &resolved_lambda)?;
    let numerator = gain_numerator_lambda(fsl, sdfa, &resolved_lambda)?;

    ratio(numerator, denominator)
}

/// Recall(L, M, λ) = Numerator(L, M, λ) / H(L, λ)
pub fn potential_gain_recall(
    fsl: FiniteStochasticLanguage,
    sdfa: StochasticDeterministicFiniteAutomaton,
    lambda: Option<&Fraction>,
) -> Result<Fraction> {
    let resolved_lambda = resolve_lambda_checked(lambda)?;
    is_valid_sdfa(&sdfa)?;

    let numerator = gain_numerator_lambda(fsl.clone(), sdfa, &resolved_lambda)?;
    let denominator = entropy_eventlog_with_lambda(fsl, &resolved_lambda)?;

    ratio(numerator, denominator)
}

#[cfg(test)]
mod tests {
    use super::*;
    use ebi_objects::anyhow::Result;
    use ebi_objects::EventLog;
    mod sdfa_fig_c {
        use super::*;
        use ebi_objects::anyhow::Result;
        use ebi_objects::ebi_arithmetic::f;
        use ebi_objects::StochasticDeterministicFiniteAutomaton;

        /// Figure (c): 2-state SDFA with a self-loop.
        pub fn build_fig_c_loop() -> Result<StochasticDeterministicFiniteAutomaton> {
            let mut sdfa = StochasticDeterministicFiniteAutomaton::new();
            sdfa.set_initial_state(Some(0));

            let a = sdfa.activity_key.process_activity("a");
            sdfa.add_transition(0, a, 1, f!(4, 5))?;

            let a = sdfa.activity_key.process_activity("a");
            sdfa.add_transition(1, a, 1, f!(1, 2))?;

            Ok(sdfa)
        }
    }

    #[test]
    fn test_entropy_eventlog() -> Result<()> {
        ebi_objects::ebi_arithmetic::exact::set_exact_globally(false);

        let xes_fig_a = include_str!("../../testfiles/fig_a.xes");
        let log: EventLog = xes_fig_a.parse()?;
        let fsl = FiniteStochasticLanguage::from(log);
        let entropy = entropy_eventlog(fsl)?;
        println!("entropy ≈ {:.12}", entropy.approximate());

        Ok(())
    }

    #[test]
    fn test_entropy_sdfa() -> Result<()> {
        ebi_objects::ebi_arithmetic::exact::set_exact_globally(false);

        let sdfa = sdfa_fig_c::build_fig_c_loop()?;
        let state_visits = c_s(&sdfa)?;

        for (state, value) in state_visits.iter().enumerate() {
            println!("state {}: {}", state, value);
        }

        let lambda = f!(1i64, 1_000_000i64);
        let entropy = entropy_sdfa_with_lambda(&sdfa, &lambda)?;

        println!("SDFA entropy = {:.12}", entropy.approximate());

        Ok(())
    }

    #[test]
    fn test_gain_numerator() -> Result<()> {
        ebi_objects::ebi_arithmetic::exact::set_exact_globally(false);

        let xes_fig_a = include_str!("../../testfiles/fig_a.xes");
        let log: EventLog = xes_fig_a.parse()?;
        let fsl = FiniteStochasticLanguage::from(log);
        let sdfa = sdfa_fig_c::build_fig_c_loop()?;
        let lambda = Fraction::zero();

        let numerator = gain_numerator_lambda(fsl, sdfa, &lambda)?;
        println!("Gain Numerator ≈ {:.12}", numerator.approximate());

        Ok(())
    }

    #[test]
    fn test_gain() -> Result<()> {
        ebi_objects::ebi_arithmetic::exact::set_exact_globally(false);
        let xes_fig_a = include_str!("../../testfiles/fig_a.xes");
        let log: EventLog = xes_fig_a.parse()?;
        let fsl = FiniteStochasticLanguage::from(log);
        let sdfa = sdfa_fig_c::build_fig_c_loop()?;

        let lambda: Fraction = f!(1i64, 1_000_000i64);

        let precision = potential_gain_precision(fsl.clone(), sdfa.clone(), Some(&lambda))?;
        let recall = potential_gain_recall(fsl.clone(), sdfa.clone() , Some(&lambda))?;

        println!("precision(L_e, S_e) ≈ {}", precision);
        println!("recall   (L_e, S_e) ≈ {}", recall);

        Ok(())
    }

        #[test]
    fn test_entropy_sdfa_without_initial_state() -> Result<()> {
        use ebi_objects::ebi_arithmetic::Zero;
        ebi_objects::ebi_arithmetic::exact::set_exact_globally(false);

        let mut sdfa = sdfa_fig_c::build_fig_c_loop()?;
        sdfa.set_initial_state(None);

        assert!(sdfa.get_initial_state().is_none());

        let state_visits = c_s(&sdfa)?;
        for value in &state_visits {
            assert!(value.is_zero());
        }

        let entropy = entropy_sdfa(&sdfa)?;
        assert!(entropy.is_zero());

        Ok(())
    }

}

