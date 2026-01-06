use crate::{
    ebi_traits::ebi_trait_event_log_trace_attributes::EbiTraitEventLogTraceAttributes,
    math::{correlation::correlation, levenshtein, root::ContainsRoot},
};
use anyhow::{Result, anyhow};
use ebi_objects::{
    Attribute, DataType,
    ebi_arithmetic::{Fraction, Signed, Zero},
};
use rand::Rng;
use rayon::prelude::*;

pub trait Associations {
    fn association(
        self: &Box<Self>,
        number_of_samples: usize,
        attribute: Attribute,
    ) -> Result<ContainsRoot>;

    fn associations(
        self: &Box<Self>,
        number_of_samples: usize,
    ) -> Vec<(String, Result<ContainsRoot>)>;
}

impl Associations for dyn EbiTraitEventLogTraceAttributes {
    fn association(
        self: &Box<Self>,
        number_of_samples: usize,
        attribute: Attribute,
    ) -> Result<ContainsRoot> {
        match self.attribute_key().attribute_to_data_type(attribute) {
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
        log::info!("found attributes {:?}", self.attribute_key());
        log::info!("number of samples {}", number_of_samples);
        self.attribute_key()
            .par_iter()
            .map(|(attribute, data_type)| {
                (
                    self.attribute_key()
                        .attribute_to_label(attribute)
                        .unwrap()
                        .to_owned(),
                    self.association_type(number_of_samples, attribute, &data_type),
                )
            })
            .collect()
    }
}

impl dyn EbiTraitEventLogTraceAttributes {
    fn association_type(
        self: &Box<Self>,
        number_of_samples: usize,
        attribute: Attribute,
        data_type: &DataType,
    ) -> Result<ContainsRoot> {
        let result = match data_type {
            DataType::Categorical => self.association_categorical(attribute, number_of_samples),
            DataType::Numerical(_, _) => self.association_numerical(attribute, number_of_samples),
            DataType::Time(_, _) => self.association_time(attribute, number_of_samples),
            DataType::Undefined => Err(anyhow!(
                "attribute is missing, attribute type is not consistent, or attribute type is not supported"
            )),
        };
        log::info!("association {:?}", result);
        result
    }

    fn association_time(
        self: &Box<Self>,
        case_attribute: Attribute,
        number_of_samples: usize,
    ) -> Result<ContainsRoot> {
        //gather pairs
        let pairs = self
            .iter_time_and_traces(case_attribute)
            .filter_map(|(trace, value)| {
                if let Some(value) = value {
                    Some((value, trace))
                } else {
                    None
                }
            })
            .collect::<Vec<_>>();

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
        case_attribute: Attribute,
        number_of_samples: usize,
    ) -> Result<ContainsRoot> {
        let sample_size = self.number_of_traces();

        //gather pairs
        let pairs = self
            .iter_categorical_and_traces(case_attribute)
            .filter_map(|(trace, value)| {
                if let Some(value) = value {
                    Some((value, trace))
                } else {
                    None
                }
            })
            .collect::<Vec<_>>();

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
                        let trace_dist = levenshtein::normalised(&pairs[*i].1, &pairs[*j].1);

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
        case_attribute: Attribute,
        number_of_samples: usize,
    ) -> Result<ContainsRoot> {
        //gather pairs
        let pairs = self
            .iter_numeric_and_traces(case_attribute)
            .filter_map(|(trace, value)| {
                if let Some(value) = value {
                    Some((value, trace))
                } else {
                    None
                }
            })
            .collect::<Vec<_>>();

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
            let lev_dist = levenshtein::normalised(trace1, trace2);

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

    use ebi_objects::EventLogTraceAttributes;

    use crate::{
        ebi_traits::ebi_trait_event_log_trace_attributes::EbiTraitEventLogTraceAttributes,
        techniques::association::Associations,
    };

    #[test]
    fn zero() {
        let fin = fs::read_to_string("testfiles/a-b-double.xes").unwrap();
        let log = fin.parse::<EventLogTraceAttributes>().unwrap();
        let log: Box<dyn EbiTraitEventLogTraceAttributes> = Box::new(log);
        log.associations(500);
    }
}
