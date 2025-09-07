use std::sync::Arc;

#[cfg(any(
    all(
        not(feature = "eexactarithmetic"),
        not(feature = "eapproximatearithmetic")
    ),
    all(feature = "eexactarithmetic", feature = "eapproximatearithmetic"),
    all(feature = "eexactarithmetic", not(feature = "eapproximatearithmetic")),
))]
use anyhow::Result;
use ebi_arithmetic::{Fraction, Zero};
#[cfg(any(
    all(
        not(feature = "eexactarithmetic"),
        not(feature = "eapproximatearithmetic")
    ),
    all(feature = "eexactarithmetic", feature = "eapproximatearithmetic"),
    all(feature = "eexactarithmetic", not(feature = "eapproximatearithmetic")),
))]
use malachite::Natural;
use rayon::iter::{IntoParallelIterator, ParallelIterator};

#[cfg(any(
    all(
        not(feature = "eexactarithmetic"),
        not(feature = "eapproximatearithmetic")
    ),
    all(feature = "eexactarithmetic", feature = "eapproximatearithmetic"),
    all(feature = "eexactarithmetic", not(feature = "eapproximatearithmetic")),
))]
use ebi_arithmetic::exact::MaybeExact;

use crate::{
    ebi_framework::ebi_command::EbiCommand,
    ebi_traits::ebi_trait_finite_stochastic_language::EbiTraitFiniteStochasticLanguage,
    math::{distances::WeightedDistances, levenshtein},
};

/**
 * A weighted distance matrix for a language with itself.
 * Computes each distance once, and supports changing the weights of the language in the comparison with itself.
 * Cloning will not clone the distances, but will clone the weights, which can be changed.
 */
#[derive(Clone)]
pub struct WeightedTriangularDistanceMatrix {
    weights_a: Vec<Fraction>,
    weights_b: Vec<Fraction>,
    distances: Vec<Vec<Arc<Fraction>>>,
    zero: Arc<Fraction>,
}

impl WeightedTriangularDistanceMatrix {
    pub fn new<L>(lang: &mut L) -> Self
    where
        L: EbiTraitFiniteStochasticLanguage + ?Sized,
    {
        log::info!("Compute triangular distances");

        // Pre-allocate the entire matrix
        let mut distances = Vec::with_capacity(lang.number_of_traces() - 1);

        // Create thread pool with custom configuration
        let pool = rayon::ThreadPoolBuilder::new().build().unwrap();

        // Create weights vectors
        let weights_a = (0..lang.number_of_traces())
            .filter_map(|trace_index| lang.get_trace_probability(trace_index))
            .cloned()
            .collect::<Vec<_>>();
        let weights_b = weights_a.clone();

        // log::debug!("weights_a {:?}", weights_a);
        // log::debug!("weights_b {:?}", weights_b);

        let progress_bar = EbiCommand::get_progress_bar_ticks(
            (lang.number_of_traces() * lang.number_of_traces())
                .try_into()
                .unwrap(),
        );

        // Compute in chunks for better cache utilisation
        pool.install(|| {
            distances = (0..lang.number_of_traces() - 1)
                .into_par_iter()
                .map(|i| {
                    let trace_a = lang.get_trace(i).unwrap();
                    let row: Vec<Arc<Fraction>> = (0..lang.number_of_traces() - 1)
                        .into_par_iter()
                        .map(|j| {
                            let trace_b = lang.get_trace(j).unwrap();
                            let result = levenshtein::normalised(trace_a, trace_b);
                            progress_bar.inc(1);
                            Arc::new(result)
                        })
                        .collect();
                    row
                })
                .collect();
        });

        // log::debug!("distances {:?}", distances);

        progress_bar.finish_and_clear();

        Self {
            weights_a,
            weights_b,
            distances,
            zero: Arc::new(Fraction::zero()),
        }
    }
}

impl WeightedDistances for WeightedTriangularDistanceMatrix {
    fn len_a(&self) -> usize {
        self.distances.len() + 1
    }

    fn len_b(&self) -> usize {
        self.distances.len() + 1
    }

    fn weight_a(&self, index_a: usize) -> &Fraction {
        &self.weights_a[index_a]
    }

    fn weight_a_mut(&mut self, index_a: usize) -> &mut Fraction {
        &mut self.weights_a[index_a]
    }

    fn weight_b(&self, index_b: usize) -> &Fraction {
        &self.weights_b[index_b]
    }

    fn weight_b_mut(&mut self, index_b: usize) -> &mut Fraction {
        &mut self.weights_b[index_b]
    }

    fn distance(&self, index_a: usize, index_b: usize) -> &Fraction {
        // log::debug!("distance {}, {}", index_a, index_b);
        if index_a == index_b {
            &self.zero
        } else if index_a < index_b {
            &self.distances[index_a][index_b - 1]
        } else {
            &self.distances[index_b][index_a - 1]
        }
    }

    fn iter(&self) -> Box<dyn Iterator<Item = (usize, usize, &Fraction)> + '_> {
        Box::new(self.into_iter())
    }

    fn clone(&self) -> Box<dyn WeightedDistances> {
        Box::new(Clone::clone(self))
    }

    #[cfg(any(
        all(
            not(feature = "eexactarithmetic"),
            not(feature = "eapproximatearithmetic")
        ),
        all(feature = "eexactarithmetic", feature = "eapproximatearithmetic"),
        all(feature = "eexactarithmetic", not(feature = "eapproximatearithmetic")),
    ))]
    fn lowest_common_multiple_denominators_distances(&self) -> Result<Natural> {
        use malachite::base::num::arithmetic::traits::Lcm;
        use malachite::base::num::basic::traits::One;
        // 2a. Calculate the Least Common Multiple (LCM) of all denominators of distances (i.e. the elements in the DistanceMatrix).
        use rayon::iter::{IntoParallelRefIterator, ParallelIterator};
        let denominators: Vec<Natural> = self
            .distances
            .par_iter()
            .flat_map(|row| {
                row.par_iter()
                    .map(|value| value.exact_ref().unwrap().to_denominator())
            })
            .collect();

        let lcm = denominators
            .par_iter()
            .cloned()
            .reduce(|| Natural::ONE, |a, b| a.lcm(b));

        Ok(lcm)
    }

    #[cfg(any(
        all(
            not(feature = "eexactarithmetic"),
            not(feature = "eapproximatearithmetic")
        ),
        all(feature = "eexactarithmetic", feature = "eapproximatearithmetic"),
        all(feature = "eexactarithmetic", not(feature = "eapproximatearithmetic")),
    ))]
    fn lowest_common_multiple_denominators_weights(&self) -> Result<Natural> {
        use malachite::base::num::arithmetic::traits::Lcm;
        use malachite::base::num::basic::traits::One;
        use rayon::iter::{IntoParallelIterator, IntoParallelRefIterator, ParallelIterator};

        let self_denominators: Vec<Natural> = self
            .weights_a
            .par_iter()
            .map(|frac| frac.exact_ref().unwrap().to_denominator())
            .collect();

        let lang_b_denominators: Vec<Natural> = self
            .weights_b
            .par_iter()
            .map(|frac| frac.exact_ref().unwrap().to_denominator())
            .collect();

        // Combine and calculate LCM
        let lcm_probabilities = self_denominators
            .into_par_iter()
            .chain(lang_b_denominators)
            .reduce(|| Natural::ONE, |a, b| a.lcm(b));

        Ok(lcm_probabilities)
    }
}

impl<'a> IntoIterator for &'a WeightedTriangularDistanceMatrix {
    type Item = (usize, usize, &'a Fraction);

    type IntoIter = WeightedTriangularDistanceMatrixIter<'a>;

    fn into_iter(self) -> Self::IntoIter {
        WeightedTriangularDistanceMatrixIter::new(self)
    }
}

pub struct WeightedTriangularDistanceMatrixIter<'a> {
    i: usize,
    j: usize,
    index: usize,
    distances: &'a WeightedTriangularDistanceMatrix,
}

impl<'a> WeightedTriangularDistanceMatrixIter<'a> {
    pub fn new(distances: &'a WeightedTriangularDistanceMatrix) -> Self {
        Self {
            i: 0,
            j: 0,
            index: 0,
            distances,
        }
    }
}

impl<'a> Iterator for WeightedTriangularDistanceMatrixIter<'a> {
    type Item = (usize, usize, &'a Fraction);

    fn next(&mut self) -> Option<Self::Item> {
        if self.i >= self.distances.len_a() {
            return None;
        }

        let result = Some((self.i, self.j, self.distances.distance(self.i, self.j)));

        self.j += 1;
        if self.j >= self.distances.len_b() {
            self.i += 1;
            self.j = 0;
        }

        result
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        let remaining = (self.distances.len_a() * self.distances.len_b()) - self.index;
        (remaining, Some(remaining))
    }
}

impl<'a> ExactSizeIterator for WeightedTriangularDistanceMatrixIter<'a> {}
impl<'a> std::iter::FusedIterator for WeightedTriangularDistanceMatrixIter<'a> {}
