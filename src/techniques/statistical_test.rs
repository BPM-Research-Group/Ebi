use std::sync::{atomic::{AtomicUsize, Ordering}, Arc};
use anyhow::{anyhow, Result};

use crate::{distances::TriangularDistanceMatrix, ebi_framework::ebi_command::EbiCommand, ebi_traits::ebi_trait_event_log::{AttributeKey, EbiTraitEventLog}, math::{average::Average, fraction::Fraction}, techniques::sample};

pub trait StatisticalTestsLogCategoricalAttribute {
    /**
     * Perform a test on the hypothesis that the sub-logs defined by the categorical attribute are derived from identical processes and return the p-value of the test, and whether the hypothesis was sustained.
     * 
     * See Statistical Tests and Association Measures for Business Processes. Sander J.J. Leemans, James M. McGree, Artem Polyvyanyy, Arthur H.M. ter Hofstede. IEEE Transactions on Knowledge and Data Engineering 2023. 
     */
    fn log_categorical_attribute(&self, number_of_samples: usize, trace_attribute: &String, alpha: &Fraction) -> Result<(Fraction, bool)>;
}


impl StatisticalTestsLogCategoricalAttribute for dyn EbiTraitEventLog {
    fn log_categorical_attribute(&self, number_of_samples: usize, trace_attribute: &String, alpha: &Fraction) -> Result<(Fraction, bool)> {

        let mut attribute_key = AttributeKey::new();
        let traces_with_attributes = self.get_traces_with_categorical_attributes(&mut attribute_key, trace_attribute).into_iter().collect::<Vec<_>>();

        if traces_with_attributes.is_empty() {
            return Err(anyhow!("The log does not contain traces with attribute `{}`.", trace_attribute));
        }

        //create sampling arrays
        let mut trace_indices = Vec::with_capacity(self.len());
        let mut attribute_indices = Vec::with_capacity(self.len());
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

        let e: usize = (0..number_of_samples).into_iter().map(|_| {

            //get multi-threaded access to what we need
            let mut average_a = Arc::clone(&average_base).as_ref().clone();
            let mut average_r = Arc::clone(&average_base).as_ref().clone();

            let trace_indices = Arc::clone(&trace_indices);
            let attribute_indices = Arc::clone(&attribute_indices);

            let mut sample = vec![0; self.len()];
            sample::sample_indices(trace_indices.len(), &mut sample);

            // log::debug!("sample {:?}", sample);

            for x in 0..sample.len() {
                let i = sample[x];
                let trace_index_i = trace_indices[i];
                let attribute_i = attribute_indices[i];

                // log::info!("trace {} of {}", x, event_log.len());

                for y in x+1..sample.len() {
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

        }).sum();

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

#[cfg(test)]
mod tests {
    use std::fs;

    use crate::{ebi_objects::event_log::EventLog, ebi_traits::ebi_trait_event_log::EbiTraitEventLog, math::fraction::Fraction, techniques::statistical_test::StatisticalTestsLogCategoricalAttribute};

    #[test]
    fn cla_test() {
        let fin = fs::read_to_string("testfiles/a-b.xes").unwrap();
        let event_log: Box<dyn EbiTraitEventLog> = Box::new(fin.parse::<EventLog>().unwrap());

        let (_, sustain) = event_log
            .log_categorical_attribute(500, &"attribute".to_string(), &Fraction::from((1, 20)))
            .unwrap();
        assert!(sustain) //The hypothesis should be rejected if we consider the meaning of things, however, as we have only two traces, it will be sustained.
    }
}