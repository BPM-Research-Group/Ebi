use anyhow::{Context, Result, anyhow};
use ebi_arithmetic::{Fraction, OneMinus};
use rayon::iter::{IntoParallelIterator, ParallelIterator};
use std::sync::{
    Arc,
    atomic::{AtomicUsize, Ordering},
};

use crate::{
    ebi_framework::ebi_command::EbiCommand,
    ebi_traits::{
        ebi_trait_event_log::{AttributeKey, EbiTraitEventLog},
        ebi_trait_finite_stochastic_language::EbiTraitFiniteStochasticLanguage,
    },
    math::{
        average::Average,
        distances::{TriangularDistanceMatrix, WeightedDistances},
        distances_matrix::WeightedDistanceMatrix,
        distances_triangular::WeightedTriangularDistanceMatrix,
    },
    techniques::sample::{self, Resampler},
};

pub trait BootstrapTest {
    /**
     * Performs the bootstrap test. It is the responsibility of the caller to ensure that the acitvity keys match.
     */
    fn bootstrap_test(
        &mut self,
        other: &mut dyn EbiTraitFiniteStochasticLanguage,
        number_of_samples: usize,
        alpha: &Fraction,
    ) -> Result<(Fraction, bool)>;
}

pub trait StatisticalTestsLogCategoricalAttribute {
    /**
     * Perform a test on the hypothesis that the sub-logs defined by the categorical attribute are derived from identical processes and return the p-value of the test, and whether the hypothesis was sustained.
     *
     * See Statistical Tests and Association Measures for Business Processes. Sander J.J. Leemans, James M. McGree, Artem Polyvyanyy, Arthur H.M. ter Hofstede. IEEE Transactions on Knowledge and Data Engineering 2023.
     */
    fn log_categorical_attribute(
        &self,
        number_of_samples: usize,
        trace_attribute: &String,
        alpha: &Fraction,
    ) -> Result<(Fraction, bool)>;
}

impl StatisticalTestsLogCategoricalAttribute for dyn EbiTraitEventLog {
    fn log_categorical_attribute(
        &self,
        number_of_samples: usize,
        trace_attribute: &String,
        alpha: &Fraction,
    ) -> Result<(Fraction, bool)> {
        let mut attribute_key = AttributeKey::new();
        let traces_with_attributes = self
            .get_traces_with_categorical_attributes(&mut attribute_key, trace_attribute)
            .into_iter()
            .collect::<Vec<_>>();

        if traces_with_attributes.is_empty() {
            return Err(anyhow!(
                "The log does not contain traces with attribute `{}`.",
                trace_attribute
            ));
        }

        //create sampling arrays
        let mut trace_indices = Vec::with_capacity(self.number_of_traces());
        let mut attribute_indices = Vec::with_capacity(self.number_of_traces());
        for (ti, (_, attributes)) in traces_with_attributes.iter().enumerate() {
            for (attribute, cardinality) in attributes.iter() {
                for _ in 0..*cardinality {
                    trace_indices.push(ti);
                    attribute_indices.push(attribute);
                }
            }
        }
        let trace_indices = Arc::new(trace_indices);
        let attribute_indices = Arc::new(attribute_indices);

        //compute the distances
        let distances = TriangularDistanceMatrix::new(&traces_with_attributes);
        let average_base = Arc::new(Average::new(distances)?);

        let err = AtomicUsize::new(0);

        log::info!("Perform the test");
        let progress_bar = EbiCommand::get_progress_bar_ticks(number_of_samples);

        let e: usize = (0..number_of_samples)
            .into_iter()
            .map(|_| {
                //get multi-threaded access to what we need
                let mut average_a = Arc::clone(&average_base).as_ref().clone();
                let mut average_r = Arc::clone(&average_base).as_ref().clone();

                let trace_indices = Arc::clone(&trace_indices);
                let attribute_indices = Arc::clone(&attribute_indices);

                let mut sample = vec![0; self.number_of_traces()];
                sample::sample_indices_uniform(trace_indices.len(), &mut sample);

                // log::debug!("sample {:?}", sample);

                for x in 0..sample.len() {
                    let i = sample[x];
                    let trace_index_i = trace_indices[i];
                    let attribute_i = attribute_indices[i];

                    // log::info!("trace {} of {}", x, event_log.len());

                    for y in x + 1..sample.len() {
                        let j = sample[y];
                        let trace_index_j = trace_indices[j];
                        let attribute_j = attribute_indices[j];

                        if x != y {
                            average_r.add(trace_index_i, trace_index_j);

                            if attribute_i == attribute_j {
                                average_a.add(trace_index_i, trace_index_j);
                            }
                        }
                    }

                    // log::debug!("average_r {:?}", average_r);
                    // log::debug!("average_a {:?}", average_a);
                }

                progress_bar.inc(1);

                // log::debug!("sample done\n   average distance with knowledge of attribute {} \naverage distance without knowledge of attribute {}", &sum_a / count_a, &sum_r / count_r);
                if let Ok(a) = average_a.average() {
                    let r = average_r.average().unwrap();
                    if &a < &r {
                        // log::debug!("a wins -- average a {}, average r {} -- sample {:?}", a, r, sample);
                        1
                    } else {
                        // log::debug!("r wins -- average a {}, average r {} -- sample {:?}", a, r, sample);
                        0
                    }
                } else {
                    err.fetch_add(1, Ordering::Relaxed);
                    0
                }
            })
            .sum();

        if err.load(Ordering::Relaxed) == number_of_samples {
            return Err(anyhow!("All samples were discarded."));
        }

        let mut p_value = Fraction::from(e);
        p_value /= number_of_samples - err.load(Ordering::Relaxed);
        p_value = p_value.one_minus();

        let reject = &p_value < alpha;

        progress_bar.finish_and_clear();

        Ok((p_value, !reject))
    }
}

impl BootstrapTest for dyn EbiTraitFiniteStochasticLanguage {
    fn bootstrap_test(
        &mut self,
        other: &mut dyn EbiTraitFiniteStochasticLanguage,
        number_of_samples: usize,
        alpha: &Fraction,
    ) -> Result<(Fraction, bool)> {
        //compute the distance between self and other
        log::info!("Compute the base log-log distance");
        let self_other_distances: Box<dyn WeightedDistances> =
            Box::new(WeightedDistanceMatrix::new(self, other));
        let base_conformance = Arc::new(
            self_other_distances
                .earth_movers_stochastic_conformance()
                .with_context(|| format!("computing base earth movers' stochastic conformance"))?,
        );

        log::info!("Compute the trace distances");
        let self_self_distances = Arc::new(WeightedTriangularDistanceMatrix::new(self));

        //create resampling cache
        let resample_cache = Arc::new(self.resample_cache_init()?);

        let err = AtomicUsize::new(0);

        log::info!("Compute the self log-log distances");
        let progress_bar = EbiCommand::get_progress_bar_ticks(number_of_samples);

        let e: usize = (0..number_of_samples)
            .into_par_iter()
            .map(|_| {
                //get multi-threaded access to what we need
                let base_conformance = base_conformance.as_ref();
                let mut self_self_distances =
                    WeightedDistances::clone(self_self_distances.as_ref());
                let resample_cache = resample_cache.as_ref();

                //create the sample
                let sample = self.resample(resample_cache, self.number_of_traces());
                sample
                    .into_iter()
                    .enumerate()
                    .for_each(|(index_b, weight)| {
                        *self_self_distances.weight_b_mut(index_b) = weight
                    });

                //compute emsc
                let sample_conformance = self_self_distances.earth_movers_stochastic_conformance();

                progress_bar.inc(1);

                if let Ok(d) = sample_conformance {
                    //Here, the paper says "<=", however that does not match intuition.
                    if &d < &base_conformance { 1 } else { 0 }
                } else {
                    err.fetch_add(1, Ordering::Relaxed);
                    0
                }
            })
            .sum();

        if err.load(Ordering::Relaxed) == number_of_samples {
            return Err(anyhow!("All samples were discarded."));
        }

        let mut p_value = Fraction::from(e);
        p_value /= number_of_samples - err.load(Ordering::Relaxed);

        let reject = p_value >= alpha.clone().one_minus();

        progress_bar.finish_and_clear();

        Ok((p_value, !reject))
    }
}

#[cfg(test)]
mod tests {
    use std::fs;

    use ebi_arithmetic::Fraction;
    use ebi_objects::{EventLog, FiniteStochasticLanguage};

    use crate::{
        ebi_traits::{
            ebi_trait_event_log::EbiTraitEventLog,
            ebi_trait_finite_stochastic_language::EbiTraitFiniteStochasticLanguage,
        },
        techniques::bootstrap_test::{BootstrapTest, StatisticalTestsLogCategoricalAttribute},
    };

    #[test]
    fn cla_test() {
        let fin = fs::read_to_string("testfiles/a-b.xes").unwrap();
        let event_log: Box<dyn EbiTraitEventLog> = Box::new(fin.parse::<EventLog>().unwrap());

        let (_, sustain) = event_log
            .log_categorical_attribute(500, &"attribute".to_string(), &Fraction::from((1, 20)))
            .unwrap();
        assert!(sustain) //The hypothesis should be rejected if we consider the meaning of things, however, as we have only two traces, it will be sustained.
    }

    #[test]
    fn llup_test_single() {
        let fin = fs::read_to_string("testfiles/aa.slang").unwrap();
        let slpn = fin.parse::<FiniteStochasticLanguage>().unwrap();
        let mut slpn2 = slpn.clone();
        let mut slpn: Box<dyn EbiTraitFiniteStochasticLanguage> = Box::new(slpn);
        let (_, sustain) = slpn
            .bootstrap_test(&mut slpn2, 1, &Fraction::from((1, 20)))
            .unwrap();
        assert!(sustain);
    }

    #[test]
    fn llup_test() {
        let fin = fs::read_to_string("testfiles/a-b.xes").unwrap();
        let slpn: FiniteStochasticLanguage = fin.parse::<EventLog>().unwrap().into();
        let mut slpn2 = slpn.clone();
        let mut slpn: Box<dyn EbiTraitFiniteStochasticLanguage> = Box::new(slpn);
        let (_, sustain) = slpn
            .bootstrap_test(&mut slpn2, 1, &Fraction::from((1, 20)))
            .unwrap();
        assert!(sustain);
    }
}
