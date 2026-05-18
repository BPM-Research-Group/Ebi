use crate::{
    ebi_framework::ebi_command::EbiCommand,
    ebi_traits::{
        ebi_trait_event_log::EbiTraitEventLog,
        ebi_trait_finite_stochastic_language::EbiTraitFiniteStochasticLanguage,
        ebi_trait_stochastic_semantics::EbiTraitStochasticSemantics,
    },
    math::{distances::WeightedDistances, distances_matrix::WeightedDistanceMatrix},
    techniques::sample::Sampler,
};
use ebi_objects::{
    anyhow::{anyhow, Context, Result},
    ebi_arithmetic::{Fraction, OneMinus},
    Activity, FiniteStochasticLanguage, HasActivityKey, IntoRefTraceProbabilityIterator,
    TranslateActivityKey,
};
use rand::seq::index;
use std::collections::{HashMap, HashSet};

pub trait PermutationTestMl {
    fn permutation_test_ml(
        &mut self,
        model: &mut EbiTraitStochasticSemantics,
        number_of_samples: usize,
    ) -> Result<Fraction>;
}

impl PermutationTestMl for dyn EbiTraitEventLog {
    fn permutation_test_ml(
        &mut self,
        model: &mut EbiTraitStochasticSemantics,
        number_of_samples: usize,
    ) -> Result<Fraction> {
        if number_of_samples == 0 {
            return Err(anyhow!(
                "Cannot perform a model-log permutation test without samples."
            ));
        }

        let number_of_traces = self.number_of_traces();
        if number_of_traces == 0 {
            return Err(anyhow!(
                "Cannot perform a model-log permutation test on an empty log."
            ));
        }

        let log_traces = self.iter_traces().cloned().collect::<Vec<_>>();
        let mut log_language = language_from_traces(self.activity_key().clone(), &log_traces);

        model.translate_using_activity_key(log_language.activity_key_mut());

        let mut errors = 0;

        log::info!("Perform the model-log permutation test");
        let progress_bar = EbiCommand::get_progress_bar_ticks(number_of_samples);

        let mut e = 0;
        for _ in 0..number_of_samples {
            let result = iteration(&mut log_language, &log_traces, model, number_of_traces)
                .with_context(|| "Compute one model-log permutation test iteration.");

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

        progress_bar.finish_and_clear();

        Ok(p_value)
    }
}

fn iteration(
    log_language: &mut FiniteStochasticLanguage,
    log_traces: &[Vec<Activity>],
    model: &EbiTraitStochasticSemantics,
    number_of_traces: usize,
) -> Result<bool> {
    let mut sample_language = model
        .sample(number_of_traces)
        .context("Sample traces from stochastic semantics.")?;

    let base_distance = earth_movers_distance(&mut sample_language, log_language)
        .context("Compute base model-log distance.")?;

    let (mut permuted_a, mut permuted_b) = split_traces(
        log_language.activity_key(),
        log_traces,
        &sample_language,
        number_of_traces,
    )?;

    let permuted_distance = earth_movers_distance(&mut permuted_a, &mut permuted_b)
        .context("Compute permuted distance.")?;

    Ok(permuted_distance >= base_distance)
}

fn earth_movers_distance(
    lang_a: &mut dyn EbiTraitFiniteStochasticLanguage,
    lang_b: &mut dyn EbiTraitFiniteStochasticLanguage,
) -> Result<Fraction> {
    let distances: Box<dyn WeightedDistances> =
        Box::new(WeightedDistanceMatrix::new(lang_a, lang_b));
    Ok(distances.earth_movers_stochastic_conformance()?.one_minus())
}

fn split_traces(
    activity_key: &ebi_objects::ActivityKey,
    log_traces: &[Vec<Activity>],
    sample_language: &FiniteStochasticLanguage,
    half_size: usize,
) -> Result<(FiniteStochasticLanguage, FiniteStochasticLanguage)> {
    let mut rng = rand::rng();
    let selected = index::sample(&mut rng, half_size * 2, half_size)
        .into_vec()
        .into_iter()
        .collect::<HashSet<_>>();

    let mut traces_a = HashMap::new();
    let mut traces_b = HashMap::new();
    let trace_weight = Fraction::from((1, half_size));

    let mut pool_index = 0;
    for trace in log_traces {
        if selected.contains(&pool_index) {
            add_trace(&mut traces_a, trace, &trace_weight);
        } else {
            add_trace(&mut traces_b, trace, &trace_weight);
        }
        pool_index += 1;
    }

    for (trace, probability) in sample_language.iter_traces_probabilities() {
        let cardinality = probability_to_cardinality(probability, half_size)?;
        for _ in 0..cardinality {
            if selected.contains(&pool_index) {
                add_trace(&mut traces_a, trace, &trace_weight);
            } else {
                add_trace(&mut traces_b, trace, &trace_weight);
            }
            pool_index += 1;
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
        FiniteStochasticLanguage::new_raw(traces_a, activity_key.clone()),
        FiniteStochasticLanguage::new_raw(traces_b, activity_key.clone()),
    ))
}

fn add_trace(
    traces: &mut HashMap<Vec<Activity>, Fraction>,
    trace: &Vec<Activity>,
    trace_weight: &Fraction,
) {
    traces
        .entry(trace.clone())
        .and_modify(|probability| *probability += trace_weight)
        .or_insert_with(|| trace_weight.clone());
}

fn language_from_traces(
    activity_key: ebi_objects::ActivityKey,
    traces: &[Vec<Activity>],
) -> FiniteStochasticLanguage {
    let mut result = HashMap::new();
    let trace_weight = Fraction::from((1, traces.len()));

    for trace in traces {
        result
            .entry(trace.clone())
            .and_modify(|probability| *probability += &trace_weight)
            .or_insert_with(|| trace_weight.clone());
    }

    FiniteStochasticLanguage::new_raw(result, activity_key)
}

fn probability_to_cardinality(probability: &Fraction, number_of_traces: usize) -> Result<usize> {
    for cardinality in 0..=number_of_traces {
        if probability == &Fraction::from((cardinality, number_of_traces)) {
            return Ok(cardinality);
        }
    }

    Err(anyhow!(
        "Sample probability {} is not a multiple of 1/{}.",
        probability,
        number_of_traces
    ))
}
