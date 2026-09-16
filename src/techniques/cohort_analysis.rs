use crate::{
    ebi_framework::ebi_command::EbiCommand,
    ebi_traits::{
        ebi_trait_event_log::EbiTraitEventLog,
        ebi_trait_event_log_trace_attributes::EbiTraitEventLogTraceAttributes,
    },
    math::{distances::WeightedDistances, distances_triangular::WeightedTriangularDistanceMatrix},
};
use ebi_objects::{
    Attribute, AttributeKey, DataType,
    anyhow::{Result, anyhow},
    ebi_arithmetic::{Fraction, Recip, ToNative, Zero, fraction::approximate::Approximate},
};
use fnv::FnvBuildHasher;
use rayon::iter::{IntoParallelIterator, ParallelIterator};
use std::{collections::HashMap, fmt::Display, sync::Arc};

pub trait CohortAnalysis {
    fn cohort_analysis(
        &self,
        number_of_random_splits: usize,
        minimum_cohort_size_fraction: &Fraction,
    ) -> Result<RankedCohorts>;
}

pub struct RankedCohorts {
    attribute_key: AttributeKey,
    features: Vec<Feature>,
    emsc: Vec<Fraction>,
    emsc_corrected: Option<Vec<Fraction>>,
}

impl Display for RankedCohorts {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.features.len() == 0 {
            return write!(f, "No cohorts.");
        }

        let attribute_name_max_length = self
            .features
            .iter()
            .map(|feature| {
                self.attribute_key
                    .attribute_to_label(feature.attribute())
                    .unwrap()
                    .len()
            })
            .max()
            .unwrap()
            .max(9);

        let attribute_value_max_length = self
            .features
            .iter()
            .map(|feature| feature.value_to_string().len())
            .max()
            .unwrap()
            .max(5);

        if let Some(emsc_corrected) = &self.emsc_corrected {
            let exact_value_max_length = emsc_corrected
                .iter()
                .map(|emsc| format!("{}", emsc.clone().approximate().unwrap()).len())
                .max()
                .unwrap()
                .max(26);

            writeln!(
                f,
                "{:<attribute_name_max_length$}  {:<attribute_value_max_length$}  {:<exact_value_max_length$}  {:>6}",
                "Attribute", "value", "approximate corrected EMSC", "corrected EMSC"
            )?;
            writeln!(f, "")?;

            for (feature, emsc_corrected) in self.features.iter().zip(emsc_corrected) {
                writeln!(
                    f,
                    "{:<attribute_name_max_length$}  {:<attribute_value_max_length$}  {:<exact_value_max_length$}  {:>6}",
                    self.attribute_key
                        .attribute_to_label(feature.attribute())
                        .unwrap(),
                    feature.value_to_string(),
                    emsc_corrected.clone().approximate().unwrap(),
                    emsc_corrected
                )?;
            }
        } else {
            let exact_value_max_length = self
                .emsc
                .iter()
                .map(|emsc| format!("{}", emsc.clone().approximate().unwrap()).len())
                .max()
                .unwrap()
                .max(16);

            writeln!(
                f,
                "{:<attribute_name_max_length$}  {:<attribute_value_max_length$}  {:<exact_value_max_length$}  {:>6}",
                "Attribute", "value", "approximate EMSC", "EMSC"
            )?;
            writeln!(f, "")?;

            for (feature, emsc) in self.features.iter().zip(self.emsc.iter()) {
                writeln!(
                    f,
                    "{:<attribute_name_max_length$}  {:<attribute_value_max_length$}  {:<exact_value_max_length$}  {:>6}",
                    self.attribute_key
                        .attribute_to_label(feature.attribute())
                        .unwrap(),
                    feature.value_to_string(),
                    emsc.clone().approximate().unwrap(),
                    emsc,
                )?;
            }
        }
        write!(f, "")
    }
}

#[derive(Debug)]
pub enum Feature {
    Categorical {
        attribute: Attribute,
        value: Option<String>,
    },
    NumericLess {
        attribute: Attribute,
        threshold: Option<Fraction>,
    },
}

impl Feature {
    pub fn attribute(&self) -> Attribute {
        match self {
            Feature::Categorical { attribute, .. } | Feature::NumericLess { attribute, .. } => {
                *attribute
            }
        }
    }

    pub fn value_to_string(&self) -> String {
        match self {
            Feature::Categorical { value, .. } => {
                if let Some(s) = value {
                    format!("{s}")
                } else {
                    format!("-missing-")
                }
            }
            Feature::NumericLess { threshold, .. } => {
                if let Some(s) = threshold {
                    format!("< {s}")
                } else {
                    format!("-missing-")
                }
            }
        }
    }
}

impl CohortAnalysis for dyn EbiTraitEventLogTraceAttributes {
    fn cohort_analysis(
        &self,
        number_of_random_splits: usize,
        minimum_cohort_size_fraction: &Fraction,
    ) -> Result<RankedCohorts> {
        // gather features and remove too-small or too-large cohorts
        let features = elicit_features(self, minimum_cohort_size_fraction);

        if features.len() == 0 {
            return Err(anyhow!(
                "Event log contains no suitable features. A feature must yield more traces than the minimum cohort size."
            ));
        }

        //create variant map
        let variant_2_cardinality = <dyn EbiTraitEventLog>::to_multiset(self);
        let variant_2_variant_index = variant_2_cardinality
            .iter()
            .map(|(trace, _)| trace)
            .enumerate()
            .map(|(x, y)| (y, x))
            .collect::<HashMap<_, _, FnvBuildHasher>>();
        let trace_index_2_variant_index = self
            .iter_traces()
            .map(|trace| *variant_2_variant_index.get(trace).unwrap())
            .collect::<Vec<_>>();

        //prepare distances
        let distances = Arc::new(WeightedTriangularDistanceMatrix::new_from_iterator(
            variant_2_cardinality.iter(),
        ));

        let progress_bar = EbiCommand::get_progress_bar_ticks(features.len());

        let mut results = features
            .into_iter()
            .map(|feature| {
                //create cohorts
                let (cohorts_distances, cohort_a_size) = split_log_on_feature(
                    self,
                    &trace_index_2_variant_index,
                    &feature,
                    Arc::clone(&distances),
                );

                let raw_emsc = cohorts_distances.earth_movers_stochastic_conformance()?;

                if number_of_random_splits == 0 {
                    progress_bar.inc(1);
                    Ok((feature, (raw_emsc, None)))
                } else {
                    let random_splits = perform_random_splits(
                        self,
                        &trace_index_2_variant_index,
                        number_of_random_splits,
                        cohort_a_size,
                        Arc::clone(&distances),
                    )?;
                    let random_avg =
                        (&random_splits.iter().sum::<Fraction>()) / random_splits.len();
                    progress_bar.inc(1);
                    if !random_avg.is_zero() {
                        Ok((feature, (raw_emsc.clone(), Some(raw_emsc / random_avg))))
                    } else {
                        Err(anyhow!("A random split did not yield any variance."))
                    }
                }
            })
            .collect::<Result<Vec<_>>>()?;

        progress_bar.finish_and_clear();

        results.sort_by(|a, b| {
            if number_of_random_splits == 0 {
                a.1.0
                    .partial_cmp(&b.1.0)
                    .unwrap_or(std::cmp::Ordering::Equal)
            } else {
                a.1.1
                    .partial_cmp(&b.1.1)
                    .unwrap_or(std::cmp::Ordering::Equal)
            }
        });

        let (features, arr): (Vec<_>, Vec<_>) = results.into_iter().unzip();
        let (emsc, emsc_corrected): (Vec<_>, Vec<_>) = arr.into_iter().unzip();
        let emsc_corrected = emsc_corrected.into_iter().collect::<Option<Vec<_>>>();
        Ok(RankedCohorts {
            attribute_key: self.attribute_key().clone(),
            features,
            emsc,
            emsc_corrected,
        })
    }
}

fn elicit_features(
    log: &dyn EbiTraitEventLogTraceAttributes,
    minimum_cohort_size_fraction: &Fraction,
) -> Vec<Feature> {
    let min_traces = (minimum_cohort_size_fraction * log.number_of_traces()).to_usize();
    let max_traces = log.number_of_traces() - min_traces;

    let mut features = vec![];
    for attribute in log.attribute_key().attributes() {
        match log.attribute_key().attribute_to_data_type(attribute) {
            Some(DataType::Categorical) => {
                let mut value_counts = HashMap::new();
                for opt_val in log.iter_categorical(attribute) {
                    *value_counts.entry(opt_val).or_insert(0) += 1;
                }
                for (value, count) in value_counts {
                    if min_traces <= count && count <= max_traces {
                        features.push(Feature::Categorical { attribute, value });
                    }
                }
            }
            Some(DataType::Numerical(_, _)) => {
                let mut values = log.iter_numeric(attribute).flatten().collect::<Vec<_>>();
                let values_len = values.len();

                let empty_traces = log.number_of_traces() - values.len();
                if min_traces <= empty_traces && empty_traces <= max_traces {
                    features.push(Feature::NumericLess {
                        attribute,
                        threshold: None,
                    });
                }

                if min_traces <= values.len() / 2 && values.len() / 2 <= max_traces {
                    let (smaller, median, _) = values.select_nth_unstable(values_len / 2);

                    //verify that there are enough smaller values
                    let count = smaller.iter().filter(|x| *x < median).count();
                    if min_traces <= count && count / 2 <= max_traces {
                        features.push(Feature::NumericLess {
                            attribute,
                            threshold: Some(median.clone()),
                        });
                    }
                }
            }
            _ => {}
        }
    }
    features
}

fn split_log_on_feature(
    log: &dyn EbiTraitEventLogTraceAttributes,
    trace_index_2_variant_index: &Vec<usize>,
    feature: &Feature,
    distances: Arc<WeightedTriangularDistanceMatrix>,
) -> (Box<dyn WeightedDistances>, usize) {
    let mut cohort_has = Vec::with_capacity(log.number_of_traces() / 2);
    let mut cohort_has_not = Vec::with_capacity(log.number_of_traces() / 2);
    match feature {
        Feature::Categorical {
            attribute,
            value: feature_value,
        } => {
            for (trace_index, (_, value)) in log.iter_categorical_and_traces(*attribute).enumerate()
            {
                if value == *feature_value {
                    cohort_has.push(trace_index_2_variant_index[trace_index]);
                } else {
                    cohort_has_not.push(trace_index_2_variant_index[trace_index]);
                }
            }
        }
        Feature::NumericLess {
            attribute,
            threshold,
        } => {
            if let Some(threshold) = threshold {
                for (trace_index, (_, value)) in log.iter_numeric_and_traces(*attribute).enumerate()
                {
                    if let Some(value) = value
                        && value < *threshold
                    {
                        cohort_has.push(trace_index_2_variant_index[trace_index]);
                    } else {
                        cohort_has_not.push(trace_index_2_variant_index[trace_index]);
                    }
                }
            } else {
                //absent
                for (trace_index, (_, value)) in log.iter_numeric_and_traces(*attribute).enumerate()
                {
                    if value.is_none() {
                        cohort_has.push(trace_index_2_variant_index[trace_index]);
                    } else {
                        cohort_has_not.push(trace_index_2_variant_index[trace_index]);
                    }
                }
            }
        }
    }

    //set weights
    let mut distances = WeightedDistances::clone_weights_zero(distances.as_ref());
    {
        let has_weight = Fraction::from(cohort_has.len()).recip();
        for variant_index in &cohort_has {
            *distances.weight_a_mut(*variant_index) += &has_weight;
        }
    }
    {
        let has_not_weight = Fraction::from(cohort_has_not.len()).recip();
        for variant_index in cohort_has_not {
            *distances.weight_b_mut(variant_index) += &has_not_weight;
        }
    }

    (distances, cohort_has.len())
}

fn perform_random_splits(
    log: &dyn EbiTraitEventLogTraceAttributes,
    trace_index_2_variant_index: &Vec<usize>,
    sample_size: usize,
    number_of_random_splits: usize,
    distances: Arc<WeightedTriangularDistanceMatrix>,
) -> Result<Vec<Fraction>> {
    (0..number_of_random_splits)
        .into_par_iter()
        .map(|_| {
            //create sample
            let mut cohort_has = Vec::with_capacity(sample_size);
            let mut cohort_has_not = Vec::with_capacity(log.number_of_traces() - sample_size);
            for trace_index in 0..log.number_of_traces() {
                if rand::random_range(0..log.number_of_traces()) <= sample_size {
                    cohort_has.push(trace_index_2_variant_index[trace_index]);
                } else {
                    cohort_has_not.push(trace_index_2_variant_index[trace_index]);
                }
            }

            //set weights
            let mut distances = WeightedDistances::clone_weights_zero(distances.as_ref());
            {
                let has_weight = Fraction::from(cohort_has.len()).recip();
                for trace_index in cohort_has {
                    *distances.weight_a_mut(trace_index) += &has_weight;
                }
            }
            {
                let has_not_weight = Fraction::from(cohort_has_not.len()).recip();
                for trace_index in cohort_has_not {
                    *distances.weight_b_mut(trace_index) += &has_not_weight;
                }
            }

            //compute emsc
            distances.earth_movers_stochastic_conformance()
        })
        .collect::<Result<Vec<_>>>()
}
