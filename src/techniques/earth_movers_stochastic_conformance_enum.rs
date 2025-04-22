use crate::distances::DistanceMatrix;
use crate::math::fraction::MaybeExact;
use crate::math::traits::One;
use crate::optimisation_algorithms::network_simplex::NetworkSimplex;
use crate::{
    ebi_traits::ebi_trait_finite_stochastic_language::EbiTraitFiniteStochasticLanguage,
    math::fraction_enum::FractionEnum,

};
use anyhow::{Context, Result};
use fraction::{BigInt, ToPrimitive};
use num_bigint::ToBigInt;
use rayon::iter::{IntoParallelIterator, IntoParallelRefIterator, ParallelIterator};
use rayon::prelude::*;
use std::sync::Arc;

use super::earth_movers_stochastic_conformance::EarthMoversStochasticConformance;

/// Authored by Leonhard Mühlmeyer (2024)
/// Implementation of the Earth Movers Stochastic Conformance Cheching (EMSC) described in
/// Leemans et al. *Earth movers’ stochastic conformance checking.* BPM Forum 2019.
/// Leemans et al. *Stochastic process mining: Earth movers’ stochastic conformance.* Information Systems 102 2021.
///
/// The command *ebi earth-mover FILE1 FILE2* can be used to compare two event logs or stochastic languages.
/// In case an event log and a stochastic model are compared, a second event log is to be sampled from the model.
/// This can be done using the *ebi earth-mover-sample FILE1 FILE2 NUMBER_OF_TRACES* command.
impl EarthMoversStochasticConformance for dyn EbiTraitFiniteStochasticLanguage {
    /// Calculate the Earth Movers Stochastic Conformance (EMSC) between two finite stochastic languages.
    /// Return its value as a Fraction within the range [0, 1].
    ///
    /// # Description
    /// Function is called on the first language, the second language is passed as an argument.
    /// Note that the second language is mutable, as the it is necessary to translate it to match the first language.
    ///
    /// # Example
    /// ```ignore
    /// lang_a.earth_movers_stochastic_conformance(lang_b.as_mut())
    /// ```
    ///
    /// # Algorithm
    /// 1. **Compute all pairwise distances** between the traces of the two languages (parallelized, see `DistanceMatrix`).
    ///
    /// 2. **Is exact arithmetic required?**<br>
    ///     a. Calculate the Least Common Multiple (LCM) of all denominators of distances (i.e., the elements in the `DistanceMatrix`).<br>
    ///     b. Calculate the Least Common Multiple (LCM) of all denominators of trace probabilities from both stochastic languages.<br>
    ///     c. If the LCMs are within the range of `i64`, use `i64` for the `NetworkSimplex` computation (remains exact but faster).
    ///        If the LCMs are within the range of `i128`, use `i128` for the `NetworkSimplex` computation.
    ///        Otherwise, use `BigInt`.<br>
    ///     d. Scale the distances and probabilities by the respective identified LCM to retrieve integer values.<br>
    ///     e. Create a network graph with the scaled distances and probabilities:<br>
    ///         i. For each trace in the first language, create a supply node with the corresponding trace probability as supply.<br>
    ///        ii. For each trace in the second language, create a demand node with the corresponding trace probability as demand (i.e., negative supply).<br>
    ///       iii. Create an edge between each pair of traces with the respective scaled distance as cost.<br>
    ///     f. Run the `NetworkSimplex` algorithm to find the optimal flow between the supply and demand nodes.<br>
    ///     g. Calculate the EMSC value as `1 - (result / (LCM of distances * LCM of probabilities))` (i.e., undo the scaling trick).<br>
    ///
    /// 3. **If exact arithmetic is not required**, use `f64` for the `NetworkSimplex` computation.<br>
    ///     a. Create a network graph with the scaled distances and probabilities:<br>
    ///         i. For each trace in the first language, create a supply node with the corresponding trace probability as supply.<br>
    ///        ii. For each trace in the second language, create a demand node with the corresponding trace probability as demand (i.e., negative supply).<br>
    ///       iii. Create an edge between each pair of traces with the respective distance as cost.<br>
    ///     b. Run the `NetworkSimplex` algorithm to find the optimal flow between the supply and demand nodes.<br>
    ///     c. Calculate the EMSC value as `1 - result`.
    fn earth_movers_stochastic_conformance(
        &mut self,
        lang_b: &mut dyn EbiTraitFiniteStochasticLanguage,
    ) -> Result<FractionEnum> {
        // 1. Compute all pairwise distances between the traces of the two languages (parallized, see DistanceMatrix).

        let distances = DistanceMatrix::new(self, lang_b);
        let distances: Vec<Vec<FractionEnum>> = distances
            .distances
            .par_iter()
            .map(|row| {
                row.par_iter()
                    .map(|arc_frac| {
                        Arc::try_unwrap(arc_frac.clone()).unwrap_or_else(|arc| (*arc).clone())
                    })
                    .collect()
            })
            .collect();

        // 2. Is exact arithmetic required?
        if crate::math::fraction::is_exaxt_globally() {
            log::info!("Calculating exact EMSC value");
            // 2a. Calculate the Least Common Multiple (LCM) of all denominators of distances (i.e. the elements in the DistanceMatrix).
            let denominators: Vec<BigInt> = distances
                .par_iter()
                .flat_map(|row| {
                    row.par_iter().map(|value| {
                        value
                            .extract_exact()
                            .unwrap()
                            .denom()
                            .unwrap()
                            .to_bigint()
                            .unwrap()
                    })
                })
                .collect();

            let lcm_distances = denominators
                .par_iter()
                .cloned()
                .reduce(|| BigInt::from(1), |a, b| num::integer::lcm(a, b));

            // 2b. Calculate the Least Common Multiple (LCM) of all denominators of trace probabilities from both stochastic languages.
            let n = distances.len();
            let m = distances[0].len();

            let self_probs: Vec<FractionEnum> = (0..n)
                .into_par_iter()
                .map(|i| self.get_trace_probability(i).unwrap().clone())
                .collect();

            let lang_b_probs: Vec<FractionEnum> = (0..m)
                .into_par_iter()
                .map(|i| lang_b.get_trace_probability(i).unwrap().clone())
                .collect();

            let self_denominators: Vec<BigInt> = self_probs
                .par_iter()
                .map(|frac| {
                    frac.extract_exact()
                        .unwrap()
                        .denom()
                        .unwrap()
                        .to_bigint()
                        .unwrap()
                })
                .collect();

            let lang_b_denominators: Vec<BigInt> = lang_b_probs
                .par_iter()
                .map(|frac| {
                    frac.extract_exact()
                        .unwrap()
                        .denom()
                        .unwrap()
                        .to_bigint()
                        .unwrap()
                })
                .collect();

            // Combine and calculate LCM
            let lcm_probabilities = self_denominators
                .into_par_iter()
                .chain(lang_b_denominators)
                .reduce(|| BigInt::from(1), |a, b| num::integer::lcm(a, b));

            let lcm_distance_fraction = FractionEnum::try_from(lcm_distances.clone())?;
            let lcm_probability_fraction = FractionEnum::try_from(lcm_probabilities.clone())?;

            log::debug!(
                "LCM of distances: {:?} \n LCM of probabilities {:?}",
                lcm_distances,
                lcm_probabilities
            );

            // 2c. If the LCMs are within the range of i64, use i64 for the NetworkSimplex computation (remains exact but faster). Otherwise use BigInt.
            if lcm_probabilities <= BigInt::from(i64::MAX)
                && (lcm_distances.clone() + BigInt::from(1)) * BigInt::from(n+m) <= BigInt::from(i64::MAX)
            {
                log::info!("Using i64 for NetworkSimplex computation.");

                // (i64) 2d. Scale the distances and probabilities by the respective identified LCM to retrieve integer values.
                let scaled_distances: Vec<Vec<i64>> = distances
                    .par_iter()
                    .map(|row| {
                        row.par_iter()
                            .map(|frac| {
                                let product = frac * &lcm_distance_fraction;
                                product
                                    .extract_exact()
                                    .unwrap()
                                    .numer()
                                    .unwrap()
                                    .to_i64()
                                    .unwrap()
                            })
                            .collect()
                    })
                    .collect();

                // (i64) 2e. Create a network graph with the scaled distances and probabilities:
                // (i64) 2e(i). For each trace in the first language, create a supply node with the corresponding trace probability as supply.
                // (i64) 2e(ii). For each trace in the second language, create a demand node with the corresponding trace probability as demand (i.e. negative supply).
                let mut supply = vec![0i64; n + m];
                supply
                    .par_chunks_mut(1024)
                    .enumerate()
                    .for_each(|(chunk_idx, chunk)| {
                        chunk.iter_mut().enumerate().for_each(|(i, s)| {
                            let idx = chunk_idx * 1024 + i;
                            *s = if idx < n {
                                (&self_probs[idx] * &lcm_probability_fraction)
                                    .extract_exact()
                                    .unwrap()
                                    .numer()
                                    .unwrap()
                                    .to_i64()
                                    .unwrap()
                            } else if idx < n + m {
                                -(&lang_b_probs[idx - n] * &lcm_probability_fraction)
                                    .extract_exact()
                                    .unwrap()
                                    .numer()
                                    .unwrap()
                                    .to_i64()
                                    .unwrap()
                            } else {
                                0
                            };
                        });
                    });

                // (i64) 2e(iii). Create an edge between each pair of traces with the respective scaled distance as cost.
                let mut graph_and_costs = vec![vec![None; n + m]; n + m];
                for i in 0..n {
                    for j in 0..m {
                        graph_and_costs[i][j + n] = Some(scaled_distances[i][j].clone());
                    }
                }

                // (i64) 2f. Run the NetworkSimplex algorithm to find the optimal flow between the supply and demand nodes.
                log::info!("Starting Network Simplex.");
                let mut ns = NetworkSimplex::new(&graph_and_costs, &supply, false, false);

                ns.run(false);

                let ns_result = ns
                    .get_bigint_result()
                    .context("NetworkSimplex did not return a result, cannot calculate EMSC")?;

                log::debug!("NetworkSimplex result: {:?}", ns_result);

                // (i64) 2g. Calculate the EMSC value as 1 - (result / (LCM of distances * LCM of probabilities)) (i.e. undo the scaling trick).
                let mut result = FractionEnum::from(1);
                let mut distance = FractionEnum::try_from(ns_result)?;
                distance /= FractionEnum::try_from(lcm_distances)?;
                distance /= FractionEnum::try_from(lcm_probabilities)?;
                result -= distance;

                return Ok(result);
            } else if lcm_probabilities <= BigInt::from(i128::MAX)
                && (lcm_distances.clone() + BigInt::from(1)) * BigInt::from(n+m) <= BigInt::from(i128::MAX)
            {
                log::info!("Using i128 for NetworkSimplex computation.");

                // (i128) 2d. Scale the distances and probabilities by the respective identified LCM to retrieve integer values.
                let scaled_distances: Vec<Vec<i128>> = distances
                    .par_iter()
                    .map(|row| {
                        row.par_iter()
                            .map(|frac| {
                                let product = frac * &lcm_distance_fraction;
                                product
                                    .extract_exact()
                                    .unwrap()
                                    .numer()
                                    .unwrap()
                                    .to_i128()
                                    .unwrap()
                            })
                            .collect()
                    })
                    .collect();

                // (i128) 2e. Create a network graph with the scaled distances and probabilities:
                // (i128) 2e(i). For each trace in the first language, create a supply node with the corresponding trace probability as supply.
                // (i128) 2e(ii). For each trace in the second language, create a demand node with the corresponding trace probability as demand (i.e. negative supply).
                let mut supply = vec![0i128; n + m];
                supply
                    .par_chunks_mut(1024)
                    .enumerate()
                    .for_each(|(chunk_idx, chunk)| {
                        chunk.iter_mut().enumerate().for_each(|(i, s)| {
                            let idx = chunk_idx * 1024 + i;
                            *s = if idx < n {
                                (&self_probs[idx] * &lcm_probability_fraction)
                                    .extract_exact()
                                    .unwrap()
                                    .numer()
                                    .unwrap()
                                    .to_i128()
                                    .unwrap()
                            } else if idx < n + m {
                                -(&lang_b_probs[idx - n] * &lcm_probability_fraction)
                                    .extract_exact()
                                    .unwrap()
                                    .numer()
                                    .unwrap()
                                    .to_i128()
                                    .unwrap()
                            } else {
                                0
                            };
                        });
                    });

                // (i128) 2e(iii). Create an edge between each pair of traces with the respective scaled distance as cost.
                let mut graph_and_costs = vec![vec![None; n + m]; n + m];
                for i in 0..n {
                    for j in 0..m {
                        graph_and_costs[i][j + n] = Some(scaled_distances[i][j].clone());
                    }
                }

                // 2f. Run the NetworkSimplex algorithm to find the optimal flow between the supply and demand nodes.
                log::info!("Starting Network Simplex.");
                let mut ns = NetworkSimplex::new(&graph_and_costs, &supply, false, false);

                ns.run(false);

                let ns_result = ns
                    .get_bigint_result()
                    .context("NetworkSimplex did not return a result, cannot calculate EMSC")?;
                log::debug!("NetworkSimplex result: {:?}", ns_result);

                // (i128) 2g. Calculate the EMSC value as 1 - (result / (LCM of distances * LCM of probabilities)) (i.e. undo the scaling trick).
                let mut result = FractionEnum::from(1);
                let mut distance = FractionEnum::try_from(ns_result)?;
                distance /= FractionEnum::try_from(lcm_distances)?;
                distance /= FractionEnum::try_from(lcm_probabilities)?;
                result -= distance;

                return Ok(result);
            } else {
                log::info!("Using BigInt for NetworkSimplex computation.");

                // 2d(BigInt). Scale the distances and probabilities by the respective identified LCM to retrieve integer values.
                let scaled_distances: Vec<Vec<BigInt>> = distances
                    .par_iter()
                    .map(|row| {
                        row.par_iter()
                            .map(|frac| {
                                let product = frac * &lcm_distance_fraction;
                                product
                                .extract_exact()
                                .unwrap()
                                .numer()
                                .unwrap()
                                .to_bigint().unwrap()
                            })
                            .collect()
                    })
                    .collect();

                // (BigInt) 2e. Create a network graph with the scaled distances and probabilities:
                // (BigInt) 2e(i). For each trace in the first language, create a supply node with the corresponding trace probability as supply.
                // (BigInt) 2e(ii). For each trace in the second language, create a demand node with the corresponding trace probability as demand (i.e. negative supply).
                let mut supply = vec![BigInt::from(0); n + m];
                supply
                    .par_chunks_mut(1024)
                    .enumerate()
                    .for_each(|(chunk_idx, chunk)| {
                        chunk.iter_mut().enumerate().for_each(|(i, s)| {
                            let idx = chunk_idx * 1024 + i;
                            *s = if idx < n {
                                (&self_probs[idx] * &lcm_probability_fraction)
                                    .extract_exact()
                                    .unwrap()
                                    .numer()
                                    .unwrap()
                                    .to_bigint()
                                    .unwrap()
                            } else if idx < n + m {
                                -(&lang_b_probs[idx - n] * &lcm_probability_fraction)
                                    .extract_exact()
                                    .unwrap()
                                    .numer()
                                    .unwrap()
                                    .to_bigint()
                                    .unwrap()
                            } else {
                                BigInt::from(0)
                            };
                        });
                    });

                // (BigInt) 2e(iii). Create an edge between each pair of traces with the respective scaled distance as cost.
                let mut graph_and_costs = vec![vec![None; n + m]; n + m];
                for i in 0..n {
                    for j in 0..m {
                        graph_and_costs[i][j + n] = Some(scaled_distances[i][j].clone());
                    }
                }

                // 2f. Run the NetworkSimplex algorithm to find the optimal flow between the supply and demand nodes.
                log::info!("Starting Network Simplex.");
                let mut ns = NetworkSimplex::new(&graph_and_costs, &supply, false, false);

                ns.run(false);

                let ns_result = ns
                    .get_result()
                    .context("NetworkSimplex did not return a result, cannot calculate EMSC")?;
                log::debug!("NetworkSimplex result: {:?}", ns_result);

                // 2g. Calculate the EMSC value as 1 - (result / (LCM of distances * LCM of probabilities)) (i.e. undo the scaling trick).
                let mut result = FractionEnum::one();
                let mut distance = FractionEnum::try_from(ns_result)?;
                distance /= FractionEnum::try_from(lcm_distances)?;
                distance /= FractionEnum::try_from(lcm_probabilities)?;
                result -= distance;

                return Ok(result);
            }
        } else {
            // 3. Exact arithmetic is not required, use f64 for the NetworkSimplex computation.
            log::info!(
                "Calculating approximate EMSC value. Using f64 for NetworkSimplex computation."
            );

            let float_distances: Vec<Vec<f64>> = distances
                .into_iter()
                .map(|row| {
                    row.into_iter()
                        .map(|frac| frac.extract_approx().unwrap())
                        .collect()
                })
                .collect();

            // 3a. Create a network graph with the scaled distances and probabilities:
            let n = float_distances.len();
            let m = float_distances[0].len();

            // 3a(i). For each trace in the first language, create a supply node with the corresponding trace probability as supply.
            let mut supply = vec![0.0; n + m];
            supply
                .par_iter_mut()
                .enumerate()
                .take(n)
                .for_each(|(i, supply)| {
                    *supply = self
                        .get_trace_probability(i)
                        .unwrap()
                        .extract_approx()
                        .unwrap();
                });
            // 3a(ii). For each trace in the second language, create a demand node with the corresponding trace probability as demand (i.e. negative supply).
            supply
                .par_iter_mut()
                .enumerate()
                .skip(n)
                .take(m)
                .for_each(|(i, supply)| {
                    *supply = -lang_b
                        .get_trace_probability(i - n)
                        .unwrap()
                        .extract_approx()
                        .unwrap();
                });

            // 3a(iii). Create an edge between each pair of traces with the respective distance as cost.
            let mut graph_and_costs = vec![vec![None; n + m]; n + m];
            // Populate the top-right n x m part of graph_and_costs with scaled_distances
            for i in 0..n {
                for j in 0..m {
                    graph_and_costs[i][j + n] = Some(float_distances[i][j]);
                }
            }

            // 3b. Run the NetworkSimplex algorithm to find the optimal flow between the supply and demand nodes.
            let mut ns = NetworkSimplex::new(&graph_and_costs, &supply, false, true);
            log::info!("Starting Network Simplex.");

            ns.run(true);

            let ns_result = match ns.get_result() {
                Some(result) => result,
                None => {
                    log::info!("NetworkSimplex did not return a result, retrying with adjusted parameters.");
                    let mut retry_ns = NetworkSimplex::new(&graph_and_costs, &supply, false, false);

                    retry_ns.run(true);

                    retry_ns
                        .get_result()
                        .context("NetworkSimplex did not return a result, cannot calculate EMSC")?
                }
            };

            log::debug!("NetworkSimplex result: {:?}", ns_result);
            // 3c. Calculate the EMSC value as 1 - result.
            let result = FractionEnum::Approx(1.0 - ns_result);

            Ok(result)
        }
    }
}

#[cfg(test)]
mod tests {
    use std::fs;
    use crate::{ebi_objects::finite_stochastic_language::FiniteStochasticLanguage, ebi_traits::ebi_trait_finite_stochastic_language::EbiTraitFiniteStochasticLanguage, math::{fraction::Fraction, traits::{One, Zero}}, techniques::earth_movers_stochastic_conformance::EarthMoversStochasticConformance};

    #[test]
    fn emsc_one() {
        let fin1 = fs::read_to_string("testfiles/aa.slang").unwrap();
        let mut slang1: Box<dyn EbiTraitFiniteStochasticLanguage> = Box::new(fin1.parse::<FiniteStochasticLanguage>().unwrap());

        let fin2 = fs::read_to_string("testfiles/aa.slang").unwrap();
        let mut slang2 = fin2.parse::<FiniteStochasticLanguage>().unwrap();

        let emsc = slang1.earth_movers_stochastic_conformance(&mut slang2).unwrap();

        assert_eq!(emsc, Fraction::one());
    }

    #[test]
    fn emsc_zero() {
        let fin1 = fs::read_to_string("testfiles/aa.slang").unwrap();
        let mut slang1: Box<dyn EbiTraitFiniteStochasticLanguage> = Box::new(fin1.parse::<FiniteStochasticLanguage>().unwrap());

        let fin2 = fs::read_to_string("testfiles/bb.slang").unwrap();
        let mut slang2 = fin2.parse::<FiniteStochasticLanguage>().unwrap();

        let emsc = slang1.earth_movers_stochastic_conformance(&mut slang2).unwrap();

        assert_eq!(emsc, Fraction::zero());
    }
}