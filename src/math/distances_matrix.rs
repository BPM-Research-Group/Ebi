#[cfg(any(
    all(
        not(feature = "eexactarithmetic"),
        not(feature = "eapproximatearithmetic")
    ),
    all(feature = "eexactarithmetic", feature = "eapproximatearithmetic"),
    all(feature = "eexactarithmetic", not(feature = "eapproximatearithmetic")),
))]
use anyhow::Result;
use ebi_arithmetic::Fraction;
#[cfg(any(
    all(
        not(feature = "eexactarithmetic"),
        not(feature = "eapproximatearithmetic")
    ),
    all(feature = "eexactarithmetic", feature = "eapproximatearithmetic"),
    all(feature = "eexactarithmetic", not(feature = "eapproximatearithmetic")),
))]
use malachite::Natural;
#[cfg(any(
    all(
        not(feature = "eexactarithmetic"),
        not(feature = "eapproximatearithmetic")
    ),
    all(feature = "eexactarithmetic", feature = "eapproximatearithmetic"),
    all(feature = "eexactarithmetic", not(feature = "eapproximatearithmetic")),
))]
use rayon::iter::IntoParallelRefIterator;
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
 * A standard weighted distance matrix.
 */
#[derive(Clone)]
pub struct WeightedDistanceMatrix {
    pub(crate) weights_a: Vec<Fraction>,
    pub(crate) weights_b: Vec<Fraction>,
    distances: Vec<Vec<Fraction>>,
}

impl WeightedDistanceMatrix {
    /**
     * It is the responsibility of the caller to ensure that the two input languages use the same activity key, for instance using `translate_using_activity_key`.
     */
    pub fn new<L, K>(lang_a: &mut L, lang_b: &mut K) -> Self
    where
        L: EbiTraitFiniteStochasticLanguage + ?Sized,
        K: EbiTraitFiniteStochasticLanguage + ?Sized,
    {
        log::info!("Compute distances");

        // Pre-allocate the entire matrix
        let mut distances = Vec::with_capacity(lang_a.number_of_traces());

        // Create thread pool with custom configuration
        let pool = rayon::ThreadPoolBuilder::new().build().unwrap();

        // Pre-fetch all traces to avoid repeated get_trace calls
        let traces_a: Vec<_> = (0..lang_a.number_of_traces())
            .map(|i| lang_a.get_trace(i).unwrap())
            .collect();
        let traces_b: Vec<_> = (0..lang_b.number_of_traces())
            .map(|j| lang_b.get_trace(j).unwrap())
            .collect();

        // Create weights vectors
        let weights_a = (0..lang_a.number_of_traces())
            .filter_map(|trace_index| lang_a.get_trace_probability(trace_index))
            .cloned()
            .collect();
        let weights_b = (0..lang_b.number_of_traces())
            .filter_map(|trace_index| lang_b.get_trace_probability(trace_index))
            .cloned()
            .collect();

        let progress_bar = EbiCommand::get_progress_bar_ticks(
            (lang_a.number_of_traces() * lang_b.number_of_traces())
                .try_into()
                .unwrap(),
        );

        // Compute in chunks for better cache utilisation
        pool.install(|| {
            distances = (0..lang_a.number_of_traces())
                .into_par_iter()
                .map(|i| {
                    let trace_a = &traces_a[i];
                    let row: Vec<Fraction> = (0..lang_b.number_of_traces())
                        .into_par_iter()
                        .map(|j| {
                            let trace_b = &traces_b[j];
                            let result = levenshtein::normalised(trace_a, trace_b);
                            progress_bar.inc(1);
                            result
                        })
                        .collect();
                    row
                })
                .collect();
        });

        progress_bar.finish_and_clear();

        Self {
            weights_a,
            weights_b,
            distances,
        }
    }
}

impl WeightedDistances for WeightedDistanceMatrix {
    fn len_a(&self) -> usize {
        self.weights_a.len()
    }

    fn len_b(&self) -> usize {
        self.weights_b.len()
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
        &self.distances[index_a][index_b]
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
        // 2a. Calculate the Least Common Multiple (LCM) of all denominators of distances (i.e. the elements in the DistanceMatrix).

        use malachite::{
            Natural,
            base::num::{arithmetic::traits::Lcm, basic::traits::One},
        };

        let denominators: Vec<Natural> = self
            .distances
            .par_iter()
            .flat_map(|row| {
                row.par_iter().map(|value| {
                    use ebi_arithmetic::exact::MaybeExact;

                    value.exact_ref().unwrap().to_denominator()
                })
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
        use malachite::Natural;
        use malachite::base::num::arithmetic::traits::Lcm;
        use malachite::base::num::basic::traits::One;

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

impl<'a> IntoIterator for &'a WeightedDistanceMatrix {
    type Item = (usize, usize, &'a Fraction);

    type IntoIter = WeightedDistanceMatrixIter<'a>;

    fn into_iter(self) -> Self::IntoIter {
        WeightedDistanceMatrixIter::new(self)
    }
}

pub struct WeightedDistanceMatrixIter<'a> {
    i: usize,
    j: usize,
    index: usize,
    distances: &'a WeightedDistanceMatrix,
}

impl<'a> WeightedDistanceMatrixIter<'a> {
    pub fn new(distances: &'a WeightedDistanceMatrix) -> Self {
        Self {
            i: 0,
            j: 0,
            index: 0,
            distances,
        }
    }
}

impl<'a> Iterator for WeightedDistanceMatrixIter<'a> {
    type Item = (usize, usize, &'a Fraction);

    fn next(&mut self) -> Option<Self::Item> {
        if self.i >= self.distances.len_a() {
            return None;
        }

        let result = Some((self.i, self.j, &self.distances.distances[self.i][self.j]));

        self.j += 1;
        self.index += 1;
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

impl<'a> ExactSizeIterator for WeightedDistanceMatrixIter<'a> {}
impl<'a> std::iter::FusedIterator for WeightedDistanceMatrixIter<'a> {}

impl IntoIterator for WeightedDistanceMatrix {
    type Item = (usize, usize, Fraction);
    type IntoIter = WeightedDistanceMatrixIterator;

    fn into_iter(self) -> Self::IntoIter {
        WeightedDistanceMatrixIterator::new(self)
    }
}

pub struct WeightedDistanceMatrixIterator {
    i: usize,
    j: usize,
    len: usize,
    it_a: std::vec::IntoIter<Vec<Fraction>>,
    it_b: std::vec::IntoIter<Fraction>,
}

impl<'a> WeightedDistanceMatrixIterator {
    pub fn new(distances: WeightedDistanceMatrix) -> Self {
        let len = distances.len_a() * distances.len_b();
        let mut it_a = distances.distances.into_iter();
        let it_b = match it_a.next() {
            Some(x) => x.into_iter(),
            None => vec![].into_iter(),
        };
        Self {
            i: 0,
            j: 0,
            len,
            it_a,
            it_b,
        }
    }
}

impl Iterator for WeightedDistanceMatrixIterator {
    type Item = (usize, usize, Fraction);

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            match self.it_b.next() {
                Some(f) => {
                    self.j += 1;
                    self.len -= 1;
                    return Some((self.i, self.j - 1, f));
                }
                None => match self.it_a.next() {
                    Some(arr) => {
                        self.i += 1;
                        self.j = 0;
                        self.it_b = arr.into_iter();
                    }
                    None => return None,
                },
            }
        }
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        (self.len, Some(self.len))
    }
}

impl ExactSizeIterator for WeightedDistanceMatrixIterator {}
impl std::iter::FusedIterator for WeightedDistanceMatrixIterator {}
