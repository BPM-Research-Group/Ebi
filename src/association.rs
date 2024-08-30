use core::num;

use anyhow::{anyhow, Result};
use chrono::{DateTime, Utc};
use fraction::Zero;
use num_bigint::ToBigUint;
use process_mining::event_log::{AttributeValue, XESEditableAttribute};
use rand::Rng;
use rayon::prelude::*;
use crate::{activity_key::Activity, ebi_objects::event_log::DataType, ebi_traits::ebi_trait_event_log::EbiTraitEventLog, levenshtein, math::{fraction::Fraction, root::{ContainsRoot, Root}}};

pub fn association(event_log: &mut Box<dyn EbiTraitEventLog>, number_of_samples: usize, attribute: &String) -> Result<ContainsRoot> {
    let attributes = event_log.get_trace_attributes();
    let data_type = attributes.get(attribute);
    log::info!("number of samples {}", number_of_samples);
    match data_type {
        Some(d_type) => association_type(event_log, number_of_samples, attribute, d_type),
        None => Err(anyhow!("attribute type is not consistent or not supported")),
    }
}

pub fn associations(event_log: &mut Box<dyn EbiTraitEventLog>, number_of_samples: usize) -> Vec<(String, Result<ContainsRoot>)> {
    let attributes = event_log.get_trace_attributes();
    log::info!("found attributes {:?}", attributes);
    log::info!("number of samples {}", number_of_samples);
    let mut result = vec![];
    for (attribute, data_type) in attributes {
        result.push((attribute.clone(), association_type(event_log, number_of_samples, &attribute, &data_type)));
    }
    result
}

pub fn association_type(event_log: &Box<dyn EbiTraitEventLog>, number_of_samples: usize, attribute: &String, data_type: &DataType) -> Result<ContainsRoot> {
    let result = match data_type {
         DataType::Categorical => association_categorical(event_log, &attribute, number_of_samples),
         DataType::Numerical(_, _) => association_numerical(event_log, &attribute, number_of_samples),
         DataType::Time(_, _) => association_time(event_log, &attribute, number_of_samples),
         DataType::Undefined => Err(anyhow!("attribute type is not consistent or not supported"))
     };
     log::info!("association {:?}", result);
     result
 } 

pub fn association_time(event_log: &Box<dyn EbiTraitEventLog>, case_attribute: &String, number_of_samples: usize) -> Result<ContainsRoot> {
    //gather pairs
    let mut pairs = vec![];
    for trace_index in 0..event_log.len() {
        if let Some(t) = event_log.get_trace_attribute_time(trace_index, case_attribute) {
            pairs.push(t);
        }
    }

    if pairs.is_empty() {
        return Err(anyhow!("no values of the attribute to consider"));
    }
    if pairs.len() == 1 {
        return Err(anyhow!("only a single value of the attribute to consider"));
    }

    log::info!("found {} trace-attribute pairs for time attribute {}", pairs.len(), case_attribute);

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

pub fn association_categorical(event_log: &Box<dyn EbiTraitEventLog>, case_attribute: &String, number_of_samples: usize) -> Result<ContainsRoot> {
    let sample_size = event_log.len();

    //gather pairs
    let mut pairs = vec![];
    for trace_index in 0..event_log.len() {
        if let Some(t) = event_log.get_trace_attribute_categorical(trace_index, case_attribute) {
            pairs.push((t, trace_index));
        }
    }

    if pairs.is_empty() {
        return Err(anyhow!("no values of the attribute to consider"));
    }
    if pairs.len() == 1 {
        return Err(anyhow!("only a single value of the attribute to consider"));
    }

    log::info!("found {} trace-attribute pairs for categorical attribute {}", pairs.len(), case_attribute);

    let pairs_categorical: Vec<(Fraction, Fraction)> = (0..number_of_samples).into_par_iter().filter_map(|samplenr| { //parallel execution

        let mut sample = vec![];
        //create sample
        for _ in 0..sample_size {
            let i = rand::thread_rng().gen_range(0..pairs.len());
            sample.push(i);
        }

        //measure
        let mut sum_same = Fraction::zero();
        let mut count_same = 0u64;
        let mut sum_different = Fraction::zero();
        for i in &sample {
            for j in &sample {
                let trace_dist = levenshtein::normalised(event_log.get_trace(pairs[*i].1).unwrap(), event_log.get_trace(pairs[*j].1).unwrap());

                if pairs[*i].0 != pairs[*j].0 {
                    count_same += 1;
                    sum_same += &trace_dist;
                }

                sum_different += trace_dist;
            }
        }

        if count_same.is_zero() {
            return None
        }

        let count_different = sample.len() as u64 * sample.len() as u64;

        sum_same /= count_same;
        sum_different /= count_different;
        let p = (sum_same, sum_different);

        return Some(p);

    }).collect();// end parallel execution

    Ok(ContainsRoot::one_minus(correlation(&pairs_categorical)?))
}

pub fn association_numerical(event_log: &Box<dyn EbiTraitEventLog>, case_attribute: &String, number_of_samples: usize) -> Result<ContainsRoot> {
    //gather pairs
    let mut pairs = vec![];
    for trace_index in 0..event_log.len() {
        if let Some(t) = event_log.get_trace_attribute_numeric(trace_index, case_attribute) {
            pairs.push((t, trace_index));
        }
    }

    if pairs.is_empty() {
        return Err(anyhow!("no values of the attribute to consider"));
    }
    if pairs.len() == 1 {
        return Err(anyhow!("only a single value of the attribute to consider"));
    }

    log::info!("found {} trace-attribute pairs for numerical attribute {}", pairs.len(), case_attribute);

    //transform to numerical pairs and sample
    let mut pairs_numeric = vec![];
    let sample_space = SamplePairsSpace::new(number_of_samples, pairs.len());
    for (i, j) in &sample_space {

        let (value1, trace1) = &pairs[i]; 
        let (value2, trace2) = &pairs[j];
        let lev_dist = levenshtein::normalised(event_log.get_trace(*trace1).unwrap(), event_log.get_trace(*trace2).unwrap());

        // pairs_numeric.push((&Fraction::from(value1 - value2).abs() / &maxx, lev_dist));
        pairs_numeric.push((Fraction::from(value1 - value2).abs(), lev_dist));
    }
    
    Ok(ContainsRoot::of(correlation(&pairs_numeric)?))
}

pub fn correlation(pairs: &[(Fraction, Fraction)]) -> Result<Root> {
    //method 1

    // let mut sum_x = zero();
    // let mut sum_y = zero();

    // for (x, y) in pairs {
    //     sum_x += x;
    //     sum_y += y;
    // }

    // let mean_x = sum_x / pairs.len();
    // let mean_y = sum_y / pairs.len();

    // let mut sum_denom = zero();
    // let mut sum_nom_x = zero();
    // let mut sum_nom_y = zero();

    // for (x, y) in pairs {
    //     sum_denom += (x - &mean_x) * (y - &mean_y);
    //     sum_nom_x += (x - &mean_x) * (x - &mean_x);
    //     sum_nom_y += (y - &mean_y) * (y - &mean_y);
    // }

    // let result1 = Root::from(sum_denom) / (Root::of(sum_nom_x) * Root::of(sum_nom_y));
    
    //method 2

    let mut sum_xy = Fraction::zero();
    let mut sum_x = Fraction::zero();
    let mut sum_y = Fraction::zero();
    let mut sum_x_squared = Fraction::zero();
    let mut sum_y_squared = Fraction::zero();

    let n = Fraction::from(pairs.len());
    for (x, y) in pairs {
        sum_xy += x * y;
        sum_x += x;
        sum_y += y;
        sum_x_squared += x * x;
        sum_y_squared += y * y;
    }
    
    let num = Root::from(&(&n * &sum_xy) - &(&sum_x * &sum_y));
    let den_x = Root::of(&(&n * &sum_x_squared) - &(&sum_x * &sum_x));
    let den_y = Root::of(&(&n * &sum_y_squared) - &(&sum_y * &sum_y));

    if den_x.is_zero() || den_y.is_zero() {
        return Err(anyhow!("the standard deviation is zero"));
    }

    let result = num / (den_x * den_y);

    Ok(result)
}

pub struct SamplePairsSpace {
    number_of_samples: usize,
    len: usize
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
            sample_space: &self
        }
    }
}

pub struct SamplePairsSpaceIterator<'a> {
    done: usize,
    sample_space: &'a SamplePairsSpace
}

impl<'a> Iterator for SamplePairsSpaceIterator<'a> {
    type Item = (usize, usize);

    fn next(&mut self) -> Option<Self::Item> {
        if (self.sample_space.len * self.sample_space.len > self.sample_space.number_of_samples) {
            //sample
            if self.done >= self.sample_space.number_of_samples {
                None
            } else {
                let i = rand::thread_rng().gen_range(0..self.sample_space.len);
                let j = rand::thread_rng().gen_range(0..self.sample_space.len);
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