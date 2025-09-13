use crate::math::distances::WeightedDistances;
use anyhow::{Context, Result};
use ebi_arithmetic::{
    One,
    exact::{MaybeExact, is_exact_globally},
    fraction::{fraction_enum::FractionEnum, signed::Numerator},
};
use ebi_optimisation::network_simplex::NetworkSimplex;
use malachite::{
    Integer,
    base::num::basic::traits::{One as MOne, Zero as MZero},
    rational::Rational,
};
use rayon::prelude::*;

/// Authored by Leonhard Mühlmeyer (2024)
/// Implementation of the Earth Movers Stochastic Conformance Cheching (EMSC) described in
/// Leemans et al. *Earth movers’ stochastic conformance checking.* BPM Forum 2019.
/// Leemans et al. *Stochastic process mining: Earth movers’ stochastic conformance.* Information Systems 102 2021.
impl dyn WeightedDistances {
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
    pub fn earth_movers_stochastic_conformance(&self) -> Result<FractionEnum> {
        if is_exact_globally() {
            //exact mode
            // 2. Is exact arithmetic required?
            log::info!("Calculating exact EMSC value");
            // 2a. Calculate the Least Common Multiple (LCM) of all denominators of distances (i.e. the elements in the DistanceMatrix).
            let lcm_distances = self.lowest_common_multiple_denominators_distances()?;
            let lcm_probabilities = self.lowest_common_multiple_denominators_weights()?;

            // 2b. Calculate the Least Common Multiple (LCM) of all denominators of trace probabilities from both stochastic languages.
            let n = self.len_a();
            let m = self.len_b();

            let lcm_distance_fraction = FractionEnum::Exact(Rational::from(lcm_distances.clone()));
            let lcm_probability_fraction =
                FractionEnum::Exact(Rational::from(lcm_probabilities.clone()));

            log::debug!(
                "LCM of distances: {:?} \n LCM of probabilities {:?}",
                lcm_distances,
                lcm_probabilities
            );

            // 2c. If the LCMs are within the range of i64, use i64 for the NetworkSimplex computation (remains exact but faster). Otherwise use BigInt.
            if lcm_probabilities <= Integer::from(i64::MAX)
                && (Into::<Integer>::into(lcm_distances.clone()) + Integer::ONE)
                    * Integer::from(n + m)
                    <= Integer::from(i64::MAX)
            {
                log::info!("Using i64 for NetworkSimplex computation.");

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
                                (self.weight_a(idx) * &lcm_probability_fraction)
                                    .exact_ref()
                                    .unwrap()
                                    .numerator_ref()
                                    .try_into()
                                    .unwrap()
                            } else if idx < n + m {
                                -TryInto::<i64>::try_into(
                                    (self.weight_b(idx - n) * &lcm_probability_fraction)
                                        .exact_ref()
                                        .unwrap()
                                        .numerator_ref(),
                                )
                                .unwrap()
                            } else {
                                0
                            };
                        });
                    });

                // (i64) 2d. Scale the distances and probabilities by the respective identified LCM to retrieve integer values.
                // (i64) 2e(iii). Create an edge between each pair of traces with the respective scaled distance as cost.
                let mut graph_and_costs = vec![vec![None; n + m]; n + m];
                for i in 0..n {
                    for j in 0..m {
                        let product = self.distance(i, j) * &lcm_distance_fraction;
                        let i64 = product
                            .exact_ref()
                            .unwrap()
                            .numerator_ref()
                            .try_into()
                            .unwrap();
                        graph_and_costs[i][j + n] = Some(i64);
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
                let mut result = FractionEnum::one();
                let mut distance = FractionEnum::Exact(Rational::from(ns_result));
                distance /= FractionEnum::Exact(Rational::from(lcm_distances));
                distance /= FractionEnum::Exact(Rational::from(lcm_probabilities));
                result -= distance;

                return Ok(result);
            } else if lcm_probabilities <= Integer::from(i128::MAX)
                && (Into::<Integer>::into(lcm_distances.clone()) + Integer::ONE)
                    * Integer::from(n + m)
                    <= Integer::from(i128::MAX)
            {
                log::info!("Using i128 for NetworkSimplex computation.");

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
                                (self.weight_a(idx) * &lcm_probability_fraction)
                                    .exact_ref()
                                    .unwrap()
                                    .numerator_ref()
                                    .try_into()
                                    .unwrap()
                            } else if idx < n + m {
                                -TryInto::<i128>::try_into(
                                    (self.weight_b(idx - n) * &lcm_probability_fraction)
                                        .exact_ref()
                                        .unwrap()
                                        .numerator_ref(),
                                )
                                .unwrap()
                            } else {
                                0
                            };
                        });
                    });

                // (i128) 2d. Scale the distances and probabilities by the respective identified LCM to retrieve integer values.
                // (i128) 2e(iii). Create an edge between each pair of traces with the respective scaled distance as cost.
                let mut graph_and_costs = vec![vec![None; n + m]; n + m];
                for i in 0..n {
                    for j in 0..m {
                        let product = self.distance(i, j) * &lcm_distance_fraction;
                        let i128 = product
                            .exact_ref()
                            .unwrap()
                            .numerator_ref()
                            .try_into()
                            .unwrap();
                        graph_and_costs[i][j + n] = Some(i128);
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
                let mut result = FractionEnum::one();
                let mut distance = FractionEnum::Exact(Rational::from(ns_result));
                distance /= FractionEnum::Exact(Rational::from(lcm_distances));
                distance /= FractionEnum::Exact(Rational::from(lcm_probabilities));
                result -= distance;

                return Ok(result);
            } else {
                log::info!("Using BigInt for NetworkSimplex computation.");

                // (BigInt) 2e. Create a network graph with the scaled distances and probabilities:
                // (BigInt) 2e(i). For each trace in the first language, create a supply node with the corresponding trace probability as supply.
                // (BigInt) 2e(ii). For each trace in the second language, create a demand node with the corresponding trace probability as demand (i.e. negative supply).
                let mut supply = vec![Integer::ZERO; n + m];
                supply
                    .par_chunks_mut(1024)
                    .enumerate()
                    .for_each(|(chunk_idx, chunk)| {
                        chunk.iter_mut().enumerate().for_each(|(i, s)| {
                            let idx = chunk_idx * 1024 + i;
                            *s = if idx < n {
                                (self.weight_a(idx) * &lcm_probability_fraction)
                                    .exact_ref()
                                    .unwrap()
                                    .to_numerator()
                                    .into()
                            } else if idx < n + m {
                                -(self.weight_b(idx - n) * &lcm_probability_fraction)
                                    .exact_ref()
                                    .unwrap()
                                    .to_numerator()
                            } else {
                                Integer::ZERO
                            };
                        });
                    });

                // 2d(BigInt). Scale the distances and probabilities by the respective identified LCM to retrieve integer values.
                // (BigInt) 2e(iii). Create an edge between each pair of traces with the respective scaled distance as cost.
                let mut graph_and_costs = vec![vec![None; n + m]; n + m];
                for index_a in 0..n {
                    for index_b in 0..m {
                        let product = self.distance(index_a, index_b) * &lcm_distance_fraction;
                        let bigint = product.exact_ref().unwrap().signed_numerator();
                        graph_and_costs[index_a][index_b + n] = Some(bigint);
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
                let mut distance = FractionEnum::Exact(Rational::from(ns_result));
                distance /= FractionEnum::Exact(Rational::from(lcm_distances));
                distance /= FractionEnum::Exact(Rational::from(lcm_probabilities));
                result -= distance;

                return Ok(result);
            }
        } else {
            //approximate mode
            // 2. Is exact arithmetic required?
            //not applicable in this compilation mode

            // 3. Exact arithmetic is not required, use f64 for the NetworkSimplex computation.
            log::info!(
                "Calculating approximate EMSC value. Using f64 for NetworkSimplex computation."
            );

            // 3a. Create a network graph with the scaled distances and probabilities:
            let n = self.len_a();
            let m = self.len_b();

            // 3a(i). For each trace in the first language, create a supply node with the corresponding trace probability as supply.
            let mut supply = vec![0.0; n + m];
            supply
                .par_iter_mut()
                .enumerate()
                .take(n)
                .for_each(|(i, supply)| {
                    *supply = *self.weight_a(i).approx_ref().unwrap();
                });
            // 3a(ii). For each trace in the second language, create a demand node with the corresponding trace probability as demand (i.e. negative supply).
            supply
                .par_iter_mut()
                .enumerate()
                .skip(n)
                .take(m)
                .for_each(|(i, supply)| {
                    *supply = -self.weight_b(i - n).approx_ref().unwrap();
                });

            // 3a(iii). Create an edge between each pair of traces with the respective distance as cost.
            let mut graph_and_costs = vec![vec![None; n + m]; n + m];
            // Populate the top-right n x m part of graph_and_costs with scaled_distances
            self.iter().for_each(|(i, j, f)| {
                graph_and_costs[i][j + n] = Some(*f.approx_ref().unwrap())
            });

            // 3b. Run the NetworkSimplex algorithm to find the optimal flow between the supply and demand nodes.
            let mut ns = NetworkSimplex::new(&graph_and_costs, &supply, false, true);
            log::info!("Starting Network Simplex.");

            ns.run(true);

            let ns_result = match ns.get_result() {
                Some(result) => result,
                None => {
                    log::info!(
                        "NetworkSimplex did not return a result, retrying with adjusted parameters."
                    );
                    let mut retry_ns = NetworkSimplex::new(&graph_and_costs, &supply, false, false);

                    retry_ns.run(true);

                    retry_ns
                        .get_result()
                        .context("NetworkSimplex did not return a result, cannot calculate EMSC")?
                }
            };

            log::debug!("NetworkSimplex result: {:?}", ns_result);
            // 3c. Calculate the EMSC value as 1 - result.
            let result = 1.0 - ns_result;

            Ok(FractionEnum::Approx(result))
        }
    }
}

#[cfg(test)]
mod tests {
    use ebi_arithmetic::{Fraction, One, Zero};
    use ebi_objects::FiniteStochasticLanguage;

    use crate::{
        ebi_traits::ebi_trait_finite_stochastic_language::EbiTraitFiniteStochasticLanguage,
        techniques::earth_movers_stochastic_conformance::EarthMoversStochasticConformance,
    };
    use std::fs;

    #[test]
    fn emsc_one() {
        let fin1 = fs::read_to_string("testfiles/aa.slang").unwrap();
        let mut slang1: Box<dyn EbiTraitFiniteStochasticLanguage> =
            Box::new(fin1.parse::<FiniteStochasticLanguage>().unwrap());

        let fin2 = fs::read_to_string("testfiles/aa.slang").unwrap();
        let mut slang2 = fin2.parse::<FiniteStochasticLanguage>().unwrap();

        let emsc = slang1
            .earth_movers_stochastic_conformance(&mut slang2)
            .unwrap();

        assert_eq!(emsc, Fraction::one());
    }

    #[test]
    fn emsc_zero() {
        let fin1 = fs::read_to_string("testfiles/aa.slang").unwrap();
        let mut slang1: Box<dyn EbiTraitFiniteStochasticLanguage> =
            Box::new(fin1.parse::<FiniteStochasticLanguage>().unwrap());

        let fin2 = fs::read_to_string("testfiles/bb.slang").unwrap();
        let mut slang2 = fin2.parse::<FiniteStochasticLanguage>().unwrap();

        let emsc = slang1
            .earth_movers_stochastic_conformance(&mut slang2)
            .unwrap();

        assert_eq!(emsc, Fraction::zero());
    }
}
