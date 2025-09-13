use std::sync::{
    Arc,
    atomic::{AtomicUsize, Ordering},
};

use crate::{
    ebi_framework::ebi_command::EbiCommand,
    ebi_traits::ebi_trait_finite_stochastic_language::EbiTraitFiniteStochasticLanguage,
    math::{
        distances::WeightedDistances, distances_matrix::WeightedDistanceMatrix,
        distances_triangular::WeightedTriangularDistanceMatrix,
    },
    techniques::sample::Resampler,
};
use anyhow::{Context, Result, anyhow};
use ebi_arithmetic::{Fraction, OneMinus};
use rayon::iter::{IntoParallelIterator, ParallelIterator};

pub trait PermutationTest {
    fn permutation_test(
        &mut self,
        other: &mut dyn EbiTraitFiniteStochasticLanguage,
        number_of_samples: usize,
        alpha: &Fraction,
    ) -> Result<(Fraction, bool)>;
}

impl PermutationTest for dyn EbiTraitFiniteStochasticLanguage {
    fn permutation_test(
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
