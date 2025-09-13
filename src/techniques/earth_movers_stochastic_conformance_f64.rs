use crate::math::distances::WeightedDistances;
use anyhow::{Context, Result};
use ebi_arithmetic::OneMinus;
use ebi_arithmetic::{Zero, fraction::fraction_f64::FractionF64};
use ebi_optimisation::network_simplex::NetworkSimplex;
use rayon::iter::ParallelIterator;
use rayon::prelude::*;

/// Authored by Leonhard Mühlmeyer (2024)
/// Implementation of the Earth Movers Stochastic Conformance Cheching (EMSC) described in
/// Leemans et al. *Earth movers’ stochastic conformance checking.* BPM Forum 2019.
/// Leemans et al. *Stochastic process mining: Earth movers’ stochastic conformance.* Information Systems 102 2021.
impl dyn WeightedDistances {
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
    pub fn earth_movers_stochastic_conformance(&self) -> Result<FractionF64> {
        // 2. Is exact arithmetic required?
        //not applicable in this compilation mode

        // 3. Exact arithmetic is not required, use f64 for the NetworkSimplex computation.
        log::info!("Calculating approximate EMSC value. Using f64 for NetworkSimplex computation.");

        // 3a. Create a network graph with the scaled distances and probabilities:
        let n = self.len_a();
        let m = self.len_b();

        // 3a(i). For each trace in the first language, create a supply node with the corresponding trace probability as supply.
        let mut supply = vec![FractionF64::zero(); n + m];
        supply
            .par_iter_mut()
            .enumerate()
            .take(n)
            .for_each(|(i, supply)| {
                *supply = *self.weight_a(i);
            });
        // 3a(ii). For each trace in the second language, create a demand node with the corresponding trace probability as demand (i.e. negative supply).
        supply
            .par_iter_mut()
            .enumerate()
            .skip(n)
            .take(m)
            .for_each(|(i, supply)| {
                *supply = -self.weight_b(i - n);
            });

        // 3a(iii). Create an edge between each pair of traces with the respective distance as cost.
        let mut graph_and_costs = vec![vec![None; n + m]; n + m];
        // Populate the top-right n x m part of graph_and_costs with scaled_distances
        self.iter()
            .for_each(|(i, j, f)| graph_and_costs[i][j + n] = Some(*f));

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
