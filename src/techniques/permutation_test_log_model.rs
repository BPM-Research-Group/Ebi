use crate::{
    ebi_framework::ebi_command::EbiCommand,
    ebi_traits::{
        ebi_trait_event_log::EbiTraitEventLog,
        ebi_trait_finite_stochastic_language::EbiTraitFiniteStochasticLanguage,
        ebi_trait_stochastic_semantics::EbiTraitStochasticSemantics,
    },
    techniques::{
        earth_movers_stochastic_conformance::EarthMoversStochasticConformance, sample::Sampler,
    },
};
use ebi_objects::{
    anyhow::{anyhow, Context, Result},
    ebi_arithmetic::{Fraction, Zero},
    EventLog, FiniteStochasticLanguage, HasActivityKey, IntoRefTraceProbabilityIterator,
    TranslateActivityKey,
};
use rand::seq::index;
use std::collections::HashSet;

pub trait PermutationTestLogModel {
    /// Perform a permutation test on the hypothesis that this log and the
    /// stochastic model are derived from identical processes, and return the
    /// p-value and whether the hypothesis was sustained.
    ///
    /// The model is sampled with the same number of traces as the log for each
    /// permutation. Samples that cannot be represented as trace cardinalities
    /// are discarded.
    fn permutation_test_log_model(
        &mut self,
        model: &mut EbiTraitStochasticSemantics,
        number_of_samples: usize,
        alpha: &Fraction,
    ) -> Result<(Fraction, bool)>;
}

impl PermutationTestLogModel for dyn EbiTraitEventLog {
    fn permutation_test_log_model(
        &mut self,
        model: &mut EbiTraitStochasticSemantics,
        number_of_samples: usize,
        alpha: &Fraction,
    ) -> Result<(Fraction, bool)> {
        if number_of_samples == 0 {
            return Err(anyhow!(
                "Cannot perform a log-model permutation test without samples."
            ));
        }

        let number_of_traces = self.number_of_traces();
        if number_of_traces == 0 {
            return Err(anyhow!(
                "Cannot perform a log-model permutation test on an empty log."
            ));
        }

        let mut log_language: FiniteStochasticLanguage = EventLog {
            activity_key: self.activity_key().clone(),
            traces: self.iter_traces().cloned().collect(),
        }
        .into();

        model.translate_using_activity_key(log_language.activity_key_mut());

        let mut errors = 0;

        log::info!("Perform the log-model permutation test");
        let progress_bar = EbiCommand::get_progress_bar_ticks(number_of_samples);

        let mut e = 0;
        for _ in 0..number_of_samples {
            // Keep this loop serial: downstream conformance computation can parallelise internally.
            let result = iteration(&mut log_language, model, number_of_traces)
                .with_context(|| "Compute one log-model permutation test iteration.");

            progress_bar.inc(1);

            match result {
                Ok(true) => e += 1,
                Ok(false) => {}
                Err(_) => {
                    errors += 1;
                }
            }
        }

        if errors == number_of_samples {
            return Err(anyhow!("All samples were discarded."));
        }

        let mut p_value = Fraction::from(e);
        p_value /= number_of_samples - errors;

        let reject = &p_value < alpha;

        progress_bar.finish_and_clear();

        Ok((p_value, !reject))
    }
}

fn iteration(
    log_language: &mut FiniteStochasticLanguage,
    model: &EbiTraitStochasticSemantics,
    number_of_traces: usize,
) -> Result<bool> {
    let mut sample_language = model
        .sample(number_of_traces)
        .context("Sample traces from stochastic semantics.")?;

    let sample_language_trait: &mut dyn EbiTraitFiniteStochasticLanguage = &mut sample_language;
    let log_language_trait: &mut dyn EbiTraitFiniteStochasticLanguage = log_language;
    let base_conformance = sample_language_trait
        .earth_movers_stochastic_conformance(log_language_trait)
        .context("Compute base log-model conformance.")?;

    let (mut permuted_a, mut permuted_b) =
        permuted_split(log_language, &sample_language, number_of_traces)?;

    let permuted_a_trait: &mut dyn EbiTraitFiniteStochasticLanguage = &mut permuted_a;
    let permuted_b_trait: &mut dyn EbiTraitFiniteStochasticLanguage = &mut permuted_b;
    let permuted_conformance = permuted_a_trait
        .earth_movers_stochastic_conformance(permuted_b_trait)
        .context("Compute permuted conformance.")?;

    Ok(permuted_conformance <= base_conformance)
}

fn permuted_split(
    log_language: &FiniteStochasticLanguage,
    sample_language: &FiniteStochasticLanguage,
    half_size: usize,
) -> Result<(FiniteStochasticLanguage, FiniteStochasticLanguage)> {
    let mut rng = rand::rng();
    let selected = index::sample(&mut rng, half_size * 2, half_size)
        .into_vec()
        .into_iter()
        .collect::<HashSet<_>>();

    let mut traces_a = FiniteStochasticLanguage::new_hashmap();
    let mut traces_b = FiniteStochasticLanguage::new_hashmap();
    let trace_weight = Fraction::from((1, half_size));

    let mut pool_index = 0;
    for language in [log_language, sample_language] {
        for (trace, probability) in language.iter_traces_probabilities() {
            let cardinality = sample_probability_to_cardinality(probability, half_size)?;
            for _ in 0..cardinality {
                if selected.contains(&pool_index) {
                    *traces_a.entry(trace.clone()).or_default() += &trace_weight;
                } else {
                    *traces_b.entry(trace.clone()).or_default() += &trace_weight;
                }
                pool_index += 1;
            }
        }
    }

    if pool_index != half_size * 2 {
        return Err(anyhow!(
            "Permutation pool contained {} traces instead of {}.",
            pool_index,
            half_size * 2
        ));
    }

    Ok((
        FiniteStochasticLanguage::from((log_language.activity_key().clone(), traces_a)),
        FiniteStochasticLanguage::from((log_language.activity_key().clone(), traces_b)),
    ))
}

fn sample_probability_to_cardinality(
    probability: &Fraction,
    number_of_traces: usize,
) -> Result<usize> {
    if probability.is_zero() {
        return Ok(0);
    }

    let mut low = 0;
    let mut high = number_of_traces;

    while low <= high {
        let cardinality = low + (high - low) / 2;
        let candidate = Fraction::from((cardinality, number_of_traces));

        if probability == &candidate {
            return Ok(cardinality);
        }

        if candidate < *probability {
            low = cardinality + 1;
        } else if cardinality == 0 {
            break;
        } else {
            high = cardinality - 1;
        }
    }

    Err(anyhow!(
        "Sample probability {} is not a multiple of 1/{}.",
        probability,
        number_of_traces
    ))
}
