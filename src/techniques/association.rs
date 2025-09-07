use crate::{
    ebi_traits::ebi_trait_event_log::EbiTraitEventLog,
    math::{correlation::correlation, data_type::DataType, levenshtein, root::ContainsRoot},
};
use anyhow::{Result, anyhow};
use ebi_arithmetic::{Fraction, Signed, Zero};
use rand::Rng;
use rayon::prelude::*;

pub trait Associations {
    fn association(
        self: &Box<Self>,
        number_of_samples: usize,
        attribute: &String,
    ) -> Result<ContainsRoot>;

    fn associations(
        self: &Box<Self>,
        number_of_samples: usize,
    ) -> Vec<(String, Result<ContainsRoot>)>;
}

impl Associations for dyn EbiTraitEventLog {
    fn association(
        self: &Box<Self>,
        number_of_samples: usize,
        attribute: &String,
    ) -> Result<ContainsRoot> {
        let attributes = self.get_trace_attributes();
        let data_type = attributes.get(attribute);
        log::info!("number of samples {}", number_of_samples);
        match data_type {
            Some(d_type) => self.association_type(number_of_samples, attribute, d_type),
            None => Err(anyhow!(
                "attribute is missing, attribute type is not consistent, or attribute type is not supported"
            )),
        }
    }

    fn associations(
        self: &Box<Self>,
        number_of_samples: usize,
    ) -> Vec<(String, Result<ContainsRoot>)> {
        let attributes = self.get_trace_attributes();
        log::info!("found attributes {:?}", attributes);
        log::info!("number of samples {}", number_of_samples);
        let mut result = vec![];
        for (attribute, data_type) in attributes {
            result.push((
                attribute.clone(),
                self.association_type(number_of_samples, &attribute, &data_type),
            ));
        }
        result
    }
}

impl dyn EbiTraitEventLog {
    fn association_type(
        self: &Box<Self>,
        number_of_samples: usize,
        attribute: &String,
        data_type: &DataType,
    ) -> Result<ContainsRoot> {
        let result = match data_type {
            DataType::Categorical => self.association_categorical(&attribute, number_of_samples),
            DataType::Numerical(_, _) => self.association_numerical(&attribute, number_of_samples),
            DataType::Time(_, _) => self.association_time(&attribute, number_of_samples),
            DataType::Undefined => Err(anyhow!(
                "attribute is missing, attribute type is not consistent, or attribute type is not supported"
            )),
        };
        log::info!("association {:?}", result);
        result
    }

    fn association_time(
        self: &Box<Self>,
        case_attribute: &String,
        number_of_samples: usize,
    ) -> Result<ContainsRoot> {
        //gather pairs
        let mut pairs = vec![];
        for trace_index in 0..self.number_of_traces() {
            if let Some(t) = self.get_trace_attribute_time(trace_index, case_attribute) {
                pairs.push(t);
            }
        }

        if pairs.is_empty() {
            return Err(anyhow!("no values of the attribute to consider"));
        }
        if pairs.len() == 1 {
            return Err(anyhow!("only a single value of the attribute to consider"));
        }

        log::info!(
            "found {} trace-attribute pairs for time attribute {}",
            pairs.len(),
            case_attribute
        );

        //transform to numerical pairs and sample
        let mut pairs_numeric = vec![];
        let sample_space = SamplePairsSpace::new(number_of_samples, pairs.len());
        for (i, j) in &sample_space {
            let (value1, trace1) = &pairs[i];
            let (value2, trace2) = &pairs[j];
            let lev_dist = levenshtein::normalised(trace1, trace2);

            let dif = *value1 - value2;
            let adif = dif.num_milliseconds().abs();
            // pairs_numeric.push((&Fraction::from(adif) / &max_diff, lev_dist));
            pairs_numeric.push((Fraction::from(adif), lev_dist));
        }

        Ok(ContainsRoot::of(correlation(&pairs_numeric)?))
    }

    pub fn association_categorical(
        self: &Box<Self>,
        case_attribute: &String,
        number_of_samples: usize,
    ) -> Result<ContainsRoot> {
        let sample_size = self.number_of_traces();

        //gather pairs
        let mut pairs = vec![];
        for trace_index in 0..self.number_of_traces() {
            if let Some(t) = self.get_trace_attribute_categorical(trace_index, case_attribute) {
                pairs.push((t, trace_index));
            }
        }

        if pairs.is_empty() {
            return Err(anyhow!("no values of the attribute to consider"));
        }
        if pairs.len() == 1 {
            return Err(anyhow!("only a single value of the attribute to consider"));
        }

        log::info!(
            "found {} trace-attribute pairs for categorical attribute {}",
            pairs.len(),
            case_attribute
        );

        let pairs_categorical: Vec<(Fraction, Fraction)> = (0..number_of_samples)
            .into_par_iter()
            .filter_map(|_| {
                //parallel execution

                let mut sample = vec![];
                //create sample
                for _ in 0..sample_size {
                    let i = rand::rng().random_range(0..pairs.len());
                    sample.push(i);
                }

                //measure
                let mut sum_same = Fraction::zero();
                let mut count_same = 0u64;
                let mut sum_different = Fraction::zero();
                for i in &sample {
                    for j in &sample {
                        let trace_dist = levenshtein::normalised(
                            self.get_trace(pairs[*i].1).unwrap(),
                            self.get_trace(pairs[*j].1).unwrap(),
                        );

                        if pairs[*i].0 != pairs[*j].0 {
                            count_same += 1;
                            sum_same += &trace_dist;
                        }

                        sum_different += trace_dist;
                    }
                }

                if count_same.is_zero() {
                    return None;
                }

                let count_different = sample.len() as u64 * sample.len() as u64;

                sum_same /= count_same;
                sum_different /= count_different;
                let p = (sum_same, sum_different);

                return Some(p);
            })
            .collect(); // end parallel execution

        Ok(ContainsRoot::one_minus(correlation(&pairs_categorical)?))
    }

    pub fn association_numerical(
        self: &Box<Self>,
        case_attribute: &String,
        number_of_samples: usize,
    ) -> Result<ContainsRoot> {
        //gather pairs
        let mut pairs = vec![];
        for trace_index in 0..self.number_of_traces() {
            if let Some(t) = self.get_trace_attribute_numeric(trace_index, case_attribute) {
                pairs.push((t, trace_index));
            }
        }

        if pairs.is_empty() {
            return Err(anyhow!("no values of the attribute to consider"));
        }
        if pairs.len() == 1 {
            return Err(anyhow!("only a single value of the attribute to consider"));
        }

        log::info!(
            "found {} trace-attribute pairs for numerical attribute {}",
            pairs.len(),
            case_attribute
        );

        //transform to numerical pairs and sample
        let mut pairs_numeric = vec![];
        let sample_space = SamplePairsSpace::new(number_of_samples, pairs.len());
        for (i, j) in &sample_space {
            let (value1, trace1) = &pairs[i];
            let (value2, trace2) = &pairs[j];
            let lev_dist = levenshtein::normalised(
                self.get_trace(*trace1).unwrap(),
                self.get_trace(*trace2).unwrap(),
            );

            // pairs_numeric.push((&Fraction::from(value1 - value2).abs() / &maxx, lev_dist));
            pairs_numeric.push((Fraction::from(value1 - value2).abs(), lev_dist));
        }

        Ok(ContainsRoot::of(correlation(&pairs_numeric)?))
    }
}

pub struct SamplePairsSpace {
    number_of_samples: usize,
    len: usize,
}

impl SamplePairsSpace {
    pub fn new(number_of_samples: usize, len: usize) -> Self {
        Self {
            number_of_samples: number_of_samples,
            len: len,
        }
    }
}

impl<'a> IntoIterator for &'a SamplePairsSpace {
    type Item = (usize, usize);

    type IntoIter = SamplePairsSpaceIterator<'a>;

    fn into_iter(self) -> Self::IntoIter {
        Self::IntoIter {
            done: 0,
            sample_space: &self,
        }
    }
}

pub struct SamplePairsSpaceIterator<'a> {
    done: usize,
    sample_space: &'a SamplePairsSpace,
}

impl<'a> Iterator for SamplePairsSpaceIterator<'a> {
    type Item = (usize, usize);

    fn next(&mut self) -> Option<Self::Item> {
        if self.sample_space.len * self.sample_space.len > self.sample_space.number_of_samples {
            //sample
            if self.done >= self.sample_space.number_of_samples {
                None
            } else {
                let i = rand::rng().random_range(0..self.sample_space.len);
                let j = rand::rng().random_range(0..self.sample_space.len);
                self.done += 1;
                Some((i, j))
            }
        } else {
            //exhaustive
            if self.done >= self.sample_space.len * self.sample_space.len {
                None
            } else {
                let i = self.done % self.sample_space.len;
                let j = self.done / self.sample_space.len;
                self.done += 1;
                Some((i, j))
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use std::fs;

    use ebi_objects::EventLog;

    use crate::{
        ebi_traits::ebi_trait_event_log::EbiTraitEventLog, techniques::association::Associations,
    };

    #[test]
    fn zero() {
        let fin = fs::read_to_string("testfiles/a-b-double.xes").unwrap();
        let log = fin.parse::<EventLog>().unwrap();
        let log: Box<dyn EbiTraitEventLog> = Box::new(log);
        log.associations(500);
    }
}
