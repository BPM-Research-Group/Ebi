use crate::distances::DistanceMatrix;
use crate::math::traits::Zero;
use crate::optimisation_algorithms::network_simplex::NetworkSimplex;
use crate::{
    ebi_traits::ebi_trait_finite_stochastic_language::EbiTraitFiniteStochasticLanguage,
    math::fraction::Fraction,
};
use anyhow::{Context, Result};
use rayon::iter::{IntoParallelRefIterator, ParallelIterator};
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
    ) -> Result<Fraction> {
        // 1. Compute all pairwise distances between the traces of the two languages (parallized, see DistanceMatrix).

        let distances = DistanceMatrix::new(self, lang_b);
        let distances: Vec<Vec<Fraction>> = distances
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
        //not applicable in this compilation mode

        // 3. Exact arithmetic is not required, use f64 for the NetworkSimplex computation.
        log::info!("Calculating approximate EMSC value. Using f64 for NetworkSimplex computation.");

        let float_distances = distances
            .into_iter()
            .map(|row| row.into_iter().map(|frac| frac).collect::<Vec<_>>())
            .collect::<Vec<_>>();

        // 3a. Create a network graph with the scaled distances and probabilities:
        let n = float_distances.len();
        let m = float_distances[0].len();

        // 3a(i). For each trace in the first language, create a supply node with the corresponding trace probability as supply.
        let mut supply = vec![Fraction::zero(); n + m];
        supply
            .par_iter_mut()
            .enumerate()
            .take(n)
            .for_each(|(i, supply)| {
                *supply = *self.get_trace_probability(i).unwrap();
            });
        // 3a(ii). For each trace in the second language, create a demand node with the corresponding trace probability as demand (i.e. negative supply).
        supply
            .par_iter_mut()
            .enumerate()
            .skip(n)
            .take(m)
            .for_each(|(i, supply)| {
                *supply = -lang_b.get_trace_probability(i - n).unwrap();
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
        let result = ns_result.one_minus();

        Ok(result)
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