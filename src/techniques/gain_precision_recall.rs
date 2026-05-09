use crate::{
    ebi_traits::{
        ebi_trait_finite_stochastic_language::EbiTraitFiniteStochasticLanguage,
        ebi_trait_queriable_stochastic_language::EbiTraitQueriableStochasticLanguage,
    },
    follower_semantics::FollowerSemantics,
    techniques::entropy::{Entropy, number_of_average_visits_per_state_approximate},
};
use ebi_objects::{
    StochasticDeterministicFiniteAutomaton,
    anyhow::{Result, anyhow},
    ebi_arithmetic::{
        ConstFraction, Fraction, Log, MaybeExact, One, OneMinus, Signed, Zero,
        fraction::approximate::Approximate,
        is_exact_globally,
        log_polynomial::{log_polynomial::LogPolynomial, log_polynomial_f64::LogPolynomialF64},
    },
};

pub const DEFAULT_LAMBDA_HIGHER_THAN_ZERO: ConstFraction = ConstFraction::of(1, 1_000_000);

pub trait PotentialGainRecallPrecision {
    /// Recall(L, M, λ) = Numerator(L, M, λ) / H(L, λ)
    fn potential_gain_recall(
        &self,
        slang: &Box<dyn EbiTraitFiniteStochasticLanguage>,
        lambda: &Fraction,
    ) -> Result<LogPolynomial>;

    /// Precision(L, M, λ) = Numerator(L, M, λ) / H(M, λ)
    fn potential_gain_precision(
        &self,
        slang: &Box<dyn EbiTraitFiniteStochasticLanguage>,
        lambda: &Fraction,
    ) -> Result<LogPolynomial>;
}

impl PotentialGainRecallPrecision for StochasticDeterministicFiniteAutomaton {
    fn potential_gain_recall(
        &self,
        slang: &Box<dyn EbiTraitFiniteStochasticLanguage>,
        lambda: &Fraction,
    ) -> Result<LogPolynomial> {
        if is_exact_globally() {
            return Err(anyhow!(
                "This method is only available in approximate mode."
            ));
        }
        is_valid_sdfa(&self)?;

        let numerator = gain_numerator_lambda(slang, self, lambda)?;
        let denominator = entropy_eventlog_with_lambda(slang, lambda)?;

        ratio(numerator, denominator)
    }

    fn potential_gain_precision(
        &self,
        slang: &Box<dyn EbiTraitFiniteStochasticLanguage>,
        lambda: &Fraction,
    ) -> Result<LogPolynomial> {
        if is_exact_globally() {
            return Err(anyhow!(
                "This method is only available in approximate mode."
            ));
        }
        is_valid_sdfa(&self)?;

        let numerator = gain_numerator_lambda(&slang, self, lambda)?;
        let denominator = entropy_sdfa_with_lambda(&self, lambda)?;

        ratio(numerator, denominator)
    }
}

/// Check whether lambda is in [0, 1].
fn check_lambda(lambda: &Fraction) -> Result<()> {
    if lambda.is_negative() || *lambda > Fraction::one() {
        return Err(anyhow!("Lambda must be in [0, 1], but is {}.", lambda));
    }
    Ok(())
}

/// Check whether the SDFA is a valid stochastic model.
///
/// Rules:
/// 1. For each state, outgoing probabilities plus termination probability sum to 1.
/// 2. Each transition probability is strictly positive.
/// 3. At least one state has a non-zero termination probability.
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
                "Invalid probability sum at state {state}: total = {total_probability} (expected 1)."
            ));
        }
    }

    for (index, probability) in sdfa.get_probabilities().iter().enumerate() {
        if probability.is_zero() || probability.is_negative() {
            return Err(anyhow!(
                "Invalid transition probability at index {index}: {probability} (must be > 0)."
            ));
        }
    }

    if !(0..state_count).any(|state| !sdfa.get_termination_probability(state).is_zero()) {
        return Err(anyhow!(
            "No terminating state found (model can only livelock)."
        ));
    }

    Ok(())
}

/// Entropy helper: h(p) = -p * log(p).
/// If p = 0, return 0.
fn entropy_term(p: &Fraction) -> Result<LogPolynomial> {
    if p.is_zero() {
        Ok(LogPolynomial::zero())
    } else {
        let term = p.n_log_n()?;
        Ok(-term)
    }
}

/// Entropy of the event log with lambda.
/// If lambda = 0, this becomes the normal entropy.
fn entropy_eventlog_with_lambda(
    fsl: &Box<dyn EbiTraitFiniteStochasticLanguage>,
    lambda: &Fraction,
) -> Result<LogPolynomial> {
    if lambda.is_zero() {
        return fsl.entropy();
    }

    let one_minus_lambda = &Fraction::one() - lambda;
    let mut entropy = LogPolynomial::zero();

    for (_, probability) in fsl.iter_traces_probabilities() {
        entropy += entropy_term(&(probability * &one_minus_lambda))?;
        entropy += entropy_term(&(probability * lambda))?;
    }

    Ok(entropy)
}

fn log_div_min(left: LogPolynomial, right: LogPolynomial) -> Result<LogPolynomial> {
    LogPolynomial::try_to_approx(LogPolynomialF64::from(
        left.approximate()?.min(right.approximate()?),
    ))
}

/// Gain numerator with lambda.
/// Only traces that exist in both the event log and the model are counted.
/// For each trace, use min(-pL log pL, -pM log pM).
fn gain_numerator_lambda(
    fsl: &Box<dyn EbiTraitFiniteStochasticLanguage>,
    sdfa: &StochasticDeterministicFiniteAutomaton,
    lambda: &Fraction,
) -> Result<LogPolynomial> {
    check_lambda(lambda)?;
    let one_minus_lambda = &Fraction::one() - lambda;
    let mut numerator = LogPolynomial::zero();

    for (trace, log_probability) in fsl.iter_traces_probabilities() {
        let model_probability = sdfa.get_probability(&FollowerSemantics::Trace(trace))?;

        if model_probability.is_zero() {
            continue;
        }

        if lambda.is_zero() {
            let log_entropy = entropy_term(log_probability)?;
            let model_entropy = entropy_term(&model_probability)?;
            numerator += log_div_min(log_entropy, model_entropy)?;
        } else {
            let log_main_entropy = entropy_term(&(log_probability * &one_minus_lambda))?;
            let model_main_entropy = entropy_term(&(&model_probability * &one_minus_lambda))?;
            numerator += log_div_min(log_main_entropy, model_main_entropy)?;

            let log_tail_entropy = entropy_term(&(log_probability * lambda))?;
            let model_tail_entropy = entropy_term(&(&model_probability * lambda))?;

            numerator += log_div_min(log_tail_entropy, model_tail_entropy)?;
        }
    }

    Ok(numerator)
}

/// Entropy of the SDFA with lambda.
/// Note: this code applies lambda on termination only.
fn entropy_sdfa_with_lambda(
    sdfa: &StochasticDeterministicFiniteAutomaton,
    lambda: &Fraction,
) -> Result<LogPolynomial> {
    check_lambda(lambda)?;
    is_valid_sdfa(sdfa)?;

    if lambda.is_zero() {
        return sdfa.entropy();
    }

    let state_count = sdfa.number_of_states();
    let state_visits = number_of_average_visits_per_state_approximate(sdfa)?;
    let one_minus_lambda = lambda.clone().one_minus();

    let mut transition_entropy = LogPolynomial::zero();
    for (index, &source) in sdfa.get_sources().iter().enumerate() {
        let probability = &sdfa.get_probabilities()[index];
        if probability.is_zero() {
            continue;
        }

        let mut entropy = entropy_term(probability)?;
        entropy *= &state_visits[source];
        transition_entropy += entropy;
    }

    let mut termination_entropy = LogPolynomial::zero();
    let mut lambda_step_entropy = LogPolynomial::zero();

    for state in 0..state_count {
        let termination_probability = sdfa.get_termination_probability(state);
        if termination_probability.is_zero() {
            continue;
        }

        let main_probability = termination_probability * &one_minus_lambda;
        if !main_probability.is_zero() {
            let mut entropy = entropy_term(&main_probability)?;
            entropy *= &state_visits[state];
            termination_entropy += entropy;
        }

        let tail_probability = termination_probability.clone() * lambda.clone();
        if !tail_probability.is_zero() {
            let mut entropy = entropy_term(&tail_probability)?;
            entropy *= &state_visits[state];
            lambda_step_entropy += entropy;
        }
    }

    let mut result = transition_entropy;
    result += termination_entropy;
    result += lambda_step_entropy;
    Ok(result)
}

/// ratio = numerator / denominator
fn ratio(num: LogPolynomial, denom: LogPolynomial) -> Result<LogPolynomial> {
    let numerator = num.approximate()?;
    let denominator = denom.approximate()?;

    if denominator.is_zero() {
        return Err(anyhow!("denominator is zero"));
    }

    LogPolynomial::try_to_approx(LogPolynomialF64::from(numerator / denominator))
}

#[cfg(all(
    not(feature = "exactarithmetic"),
    feature = "approximatearithmetic",
    test
))]
mod tests {
    use super::*;
    use ebi_objects::anyhow::Result;
    use ebi_objects::ebi_arithmetic::{f, f0};
    use ebi_objects::{EventLog, FiniteStochasticLanguage};
    use std::fs;

    mod sdfa_fig_c {
        use super::*;
        use ebi_objects::StochasticDeterministicFiniteAutomaton;
        use ebi_objects::anyhow::Result;
        use ebi_objects::ebi_arithmetic::f;

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
        let fin = fs::read_to_string("testfiles/fig_a.xes").unwrap();
        let log = fin.parse::<EventLog>().unwrap();
        let slang: Box<dyn EbiTraitFiniteStochasticLanguage> =
            Box::new(FiniteStochasticLanguage::from(log));
        let entropy = entropy_eventlog_with_lambda(&slang, &f0!())?;
        println!("entropy ≈ {:.12}", entropy.approximate().unwrap());

        Ok(())
    }

    #[test]
    fn test_entropy_sdfa() -> Result<()> {
        let sdfa = sdfa_fig_c::build_fig_c_loop()?;
        let state_visits = number_of_average_visits_per_state_approximate(&sdfa)?;

        for (state, value) in state_visits.iter().enumerate() {
            println!("state {}: {}", state, value);
        }

        let lambda = f!(1i64, 1_000_000i64);
        let entropy = entropy_sdfa_with_lambda(&sdfa, &lambda)?;

        println!("SDFA entropy = {:.12}", entropy.approximate().unwrap());

        Ok(())
    }

    #[test]
    fn test_gain_numerator() -> Result<()> {
        let fin = fs::read_to_string("testfiles/fig_a.xes").unwrap();
        let log = fin.parse::<EventLog>().unwrap();
        let fsl: Box<dyn EbiTraitFiniteStochasticLanguage> =
            Box::new(FiniteStochasticLanguage::from(log));
        let sdfa = sdfa_fig_c::build_fig_c_loop()?;
        let lambda = Fraction::zero();

        let numerator = gain_numerator_lambda(&fsl, &sdfa, &lambda)?;
        println!("Gain Numerator ≈ {:.12}", numerator.approximate().unwrap());

        Ok(())
    }

    #[test]
    fn test_gain() -> Result<()> {
        let fin = fs::read_to_string("testfiles/fig_a.xes").unwrap();
        let log = fin.parse::<EventLog>().unwrap();
        let fsl: Box<dyn EbiTraitFiniteStochasticLanguage> =
            Box::new(FiniteStochasticLanguage::from(log));
        let sdfa = sdfa_fig_c::build_fig_c_loop()?;

        let lambda: Fraction = f!(1i64, 1_000_000i64);

        let precision = sdfa.potential_gain_precision(&fsl, &lambda)?;
        let recall = sdfa.potential_gain_recall(&fsl, &lambda)?;

        println!("precision(L_e, S_e) ≈ {}", precision);
        println!("recall   (L_e, S_e) ≈ {}", recall);

        Ok(())
    }

    #[test]
    fn test_entropy_sdfa_without_initial_state() -> Result<()> {
        use ebi_objects::ebi_arithmetic::Zero;

        let mut sdfa = sdfa_fig_c::build_fig_c_loop()?;
        sdfa.set_initial_state(None);

        assert!(sdfa.get_initial_state().is_none());

        let state_visits = number_of_average_visits_per_state_approximate(&sdfa)?;
        for value in &state_visits {
            assert!(value.is_zero());
        }

        let entropy = entropy_sdfa_with_lambda(&sdfa, &f0!())?;
        assert!(entropy.is_zero());

        Ok(())
    }
}
