use anyhow::{Result, anyhow};
use ebi_arithmetic::{Fraction, One, Zero, f};
use ebi_objects::FiniteLanguage;

use crate::{
    ebi_traits::ebi_trait_finite_stochastic_language::EbiTraitFiniteStochasticLanguage,
    math::distances::TriangularDistanceMatrix,
};

pub fn medoid<T>(log: &T, number_of_traces: &usize) -> Result<FiniteLanguage>
where
    T: EbiTraitFiniteStochasticLanguage + ?Sized,
{
    let activity_key = log.activity_key().clone();
    let mut result = FiniteLanguage::new_hashmap();

    log::info!("Computing {} medoid traces", number_of_traces);

    let distances = TriangularDistanceMatrix::new(log);

    if number_of_traces.is_one() {
        let trace_number = medoid_single(log, &distances);
        if trace_number.is_none() {
            return Err(anyhow!(
                "1 trace was requested, but the stochastic language contains none."
            ));
        }
        result.insert(log.get_trace(trace_number.unwrap()).unwrap().to_owned());
        return Ok((activity_key, result).into());
    }

    if log.number_of_traces() < *number_of_traces {
        return Err(anyhow!(
            "{} traces were requested, but the stochastic language contains only {} traces.",
            number_of_traces,
            log.number_of_traces()
        ));
    }

    let mut sum_distance = sum_distances(log, &distances);

    let mut list = Vec::new();
    while list.len() < *number_of_traces {
        //find the position of the minimum value
        let mut min_pos = 0;
        for i in 1..sum_distance.len() {
            if sum_distance[i] < sum_distance[min_pos] {
                min_pos = i;
            }
        }

        //report the minimum value
        list.push(min_pos);
        sum_distance[min_pos] = f!(2);
    }
    list.sort();

    //put in the output format
    let mut list_i = 0;
    for (i1, trace1) in log.iter().enumerate() {
        if list_i < list.len() && i1 == list[list_i] {
            result.insert(trace1.to_vec());
            list_i += 1;
        }
    }

    Ok((activity_key, result).into())
}

/**
 * Returns the index of the weighted medoid, if there is one.
 */
pub fn medoid_single<T>(log: &T, distances: &TriangularDistanceMatrix) -> Option<usize>
where
    T: EbiTraitFiniteStochasticLanguage + ?Sized,
{
    let sum_distance = sum_distances(log, distances);

    //report the minimum value
    let mut min_pos = 0;
    let mut min_value = &Fraction::one();
    for (pos, value) in sum_distance.iter().enumerate() {
        if value < &min_value {
            min_pos = pos;
            min_value = value;
        }
    }

    return Some(min_pos);
}

pub fn sum_distances<T>(log: &T, distances: &TriangularDistanceMatrix) -> Vec<Fraction>
where
    T: EbiTraitFiniteStochasticLanguage + ?Sized,
{
    let mut sum_distance = vec![Fraction::zero(); log.number_of_traces()];

    for (i, j, _, distance) in distances {
        let mut distance_i = distance.as_ref().clone();
        distance_i *= log.get_trace_probability(j).unwrap();
        sum_distance[i] += &distance_i;

        let mut distance_j = distance.as_ref().clone();
        distance_j *= log.get_trace_probability(i).unwrap();
        sum_distance[j] += distance_j;
    }

    sum_distance
}

#[cfg(test)]
mod tests {
    use std::fs;

    use ebi_objects::FiniteStochasticLanguage;

    use crate::techniques::medoid;

    #[test]
    fn medoid() {
        let fin = fs::read_to_string("testfiles/aa-ab-ba.slang").unwrap();
        let slang = fin.parse::<FiniteStochasticLanguage>().unwrap();
        let fout = fs::read_to_string("testfiles/ba.lang").unwrap();
        let medoid = medoid::medoid(&slang, &1).unwrap();
        assert_eq!(fout, medoid.to_string())
    }
}
