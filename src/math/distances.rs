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
use ebi_arithmetic::Zero;
use ebi_objects::IndexTrace;
#[cfg(any(
    all(
        not(feature = "eexactarithmetic"),
        not(feature = "eapproximatearithmetic")
    ),
    all(feature = "eexactarithmetic", feature = "eapproximatearithmetic"),
    all(feature = "eexactarithmetic", not(feature = "eapproximatearithmetic")),
))]
use malachite::Natural;
use std::fmt;
use std::fmt::Debug;
use std::{iter::FusedIterator, sync::Arc};

use rayon::iter::{IntoParallelIterator, ParallelIterator};

use crate::ebi_framework::ebi_command::EbiCommand;
use crate::ebi_traits::ebi_trait_finite_stochastic_language::EbiTraitFiniteStochasticLanguage;
use crate::math::levenshtein;

pub trait WeightedDistances: Send + Sync {
    fn len_a(&self) -> usize;

    fn len_b(&self) -> usize;

    fn weight_a(&self, index_a: usize) -> &Fraction;

    fn weight_a_mut(&mut self, index_a: usize) -> &mut Fraction;

    fn weight_b(&self, index_b: usize) -> &Fraction;

    fn weight_b_mut(&mut self, index_b: usize) -> &mut Fraction;

    fn distance(&self, index_a: usize, index_b: usize) -> &Fraction;

    fn iter(&self) -> Box<dyn Iterator<Item = (usize, usize, &Fraction)> + '_>;

    fn clone(&self) -> Box<dyn WeightedDistances>;

    #[cfg(any(
        all(
            not(feature = "eexactarithmetic"),
            not(feature = "eapproximatearithmetic")
        ),
        all(feature = "eexactarithmetic", feature = "eapproximatearithmetic"),
        all(feature = "eexactarithmetic", not(feature = "eapproximatearithmetic")),
    ))]
    /**
     * Only call in exact mode.
     */
    fn lowest_common_multiple_denominators_distances(&self) -> Result<Natural>;

    #[cfg(any(
        all(
            not(feature = "eexactarithmetic"),
            not(feature = "eapproximatearithmetic")
        ),
        all(feature = "eexactarithmetic", feature = "eapproximatearithmetic"),
        all(feature = "eexactarithmetic", not(feature = "eapproximatearithmetic")),
    ))]
    /**
     * Only call in exact mode.
     */
    fn lowest_common_multiple_denominators_weights(&self) -> Result<Natural>;
}

pub struct TriangularDistanceMatrix {
    len: usize, //length of one side of the matrix
    pub(crate) distances: Vec<Arc<Fraction>>,
    zero: Arc<Fraction>,
}

impl TriangularDistanceMatrix {
    pub fn new<T>(log: &T) -> Self
    where
        T: IndexTrace + ?Sized,
    {
        log::info!("Compute distances");
        let progress_bar = EbiCommand::get_progress_bar_ticks(Self::get_number_of_distances(
            log.number_of_traces(),
        ));

        let len = log.number_of_traces();
        let log = Arc::new(log);

        let distances = (0..Self::get_number_of_distances(len))
            .into_par_iter()
            .map(|index| {
                let log = Arc::clone(&log);
                let i = Self::get_i(index);
                let j = Self::get_j(index, i);

                let trace1 = log.get_trace(i).unwrap();
                let trace2 = log.get_trace(j).unwrap();

                let result = levenshtein::normalised(trace1, trace2);
                progress_bar.inc(1);
                result
            })
            .collect::<Vec<_>>();
        let distances = distances.into_iter().map(|d| Arc::new(d)).collect();

        progress_bar.finish_and_clear();

        Self {
            len,
            distances: distances,
            zero: Arc::new(Fraction::zero()),
        }
    }

    /**
     * Return the lenghth of one side of the matrix
     */
    pub fn len(&self) -> usize {
        self.len
    }

    pub fn get_zero(&self) -> Arc<Fraction> {
        self.zero.clone()
    }

    /**
     * Expensive function; avoid
     */
    fn get_i(index: usize) -> usize {
        (1.0 + ((-1.0 + (1.0f64 + index as f64 * 8.0f64).sqrt()) / 2.0)).floor() as usize
    }

    fn get_j(index: usize, i: usize) -> usize {
        index - Self::get_index(i, 0)
    }

    fn get_index(i: usize, j: usize) -> usize {
        (i * (i - 1)) / 2 + j
    }

    fn get_number_of_distances(len: usize) -> usize {
        (len * (len - 1)) / 2
    }

    pub fn get(&self, i: usize, j: usize) -> &Arc<Fraction> {
        if i == j {
            &self.zero
        } else if i > j {
            &self.distances[Self::get_index(i, j)]
        } else {
            &self.distances[Self::get_index(j, i)]
        }
    }
}

impl<'a> IntoIterator for &'a TriangularDistanceMatrix {
    type Item = (usize, usize, usize, Arc<Fraction>);

    type IntoIter = TriangularIterator<'a>;

    fn into_iter(self) -> Self::IntoIter {
        TriangularIterator::new(self)
    }
}

impl Debug for TriangularDistanceMatrix {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Distances")
            .field("single_len", &self.len)
            .field("distances", &self.distances)
            .field("zero", &self.zero)
            .finish()
    }
}

pub struct TriangularIterator<'a> {
    i: usize,
    j: usize,
    index: usize,
    distances: &'a TriangularDistanceMatrix,
}

impl<'a> TriangularIterator<'a> {
    pub fn new(distances: &'a TriangularDistanceMatrix) -> Self {
        Self {
            i: 1,
            j: 0,
            index: 0,
            distances: &distances,
        }
    }
}

impl<'a> Iterator for TriangularIterator<'a> {
    type Item = (usize, usize, usize, Arc<Fraction>);

    /**
     * (i, j, index)
     */
    fn next(&mut self) -> Option<Self::Item> {
        if self.i >= self.distances.len {
            return None;
        }
        let result = Some((
            self.i,
            self.j,
            self.index,
            self.distances.distances[self.index].clone(),
        ));

        self.index += 1;
        self.j += 1;
        if self.j >= self.i {
            self.i += 1;
            self.j = 0;
        }

        result
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        (
            self.distances.distances.len() - self.index - 1,
            Some(self.distances.distances.len() - self.index - 1),
        )
    }
}

impl<'a> FusedIterator for TriangularIterator<'a> {}

impl<'a> ExactSizeIterator for TriangularIterator<'a> {}

pub struct DistanceMatrix {
    len_a: usize, // number of traces in the first language
    len_b: usize, // number of traces in the second language
    pub(crate) distances: Vec<Vec<Arc<Fraction>>>,
    zero: Arc<Fraction>,
}

impl DistanceMatrix {
    pub fn new<L, K>(lang_a: &mut L, lang_b: &mut K) -> Self
    where
        L: EbiTraitFiniteStochasticLanguage + ?Sized,
        K: EbiTraitFiniteStochasticLanguage + ?Sized,
    {
        log::info!("Translate second language to first");
        lang_b.translate_using_activity_key(lang_a.activity_key_mut());

        log::info!("Compute distances");
        let len_a = lang_a.number_of_traces();
        let len_b = lang_b.number_of_traces();

        // Pre-allocate the entire matrix
        let mut distances = Vec::with_capacity(len_a);

        // Create thread pool with custom configuration
        let pool = rayon::ThreadPoolBuilder::new().build().unwrap();

        // Pre-fetch all traces to avoid repeated get_trace calls
        let traces_a: Vec<_> = (0..len_a).map(|i| lang_a.get_trace(i).unwrap()).collect();
        let traces_b: Vec<_> = (0..len_b).map(|j| lang_b.get_trace(j).unwrap()).collect();

        let progress_bar = EbiCommand::get_progress_bar_ticks((len_a * len_b).try_into().unwrap());

        // Compute in chunks for better cache utilization
        pool.install(|| {
            distances = (0..len_a)
                .into_par_iter()
                .map(|i| {
                    let trace_a = &traces_a[i];
                    let row: Vec<Arc<Fraction>> = (0..len_b)
                        .into_par_iter()
                        .map(|j| {
                            let trace_b = &traces_b[j];
                            let result = levenshtein::normalised(trace_a, trace_b);
                            progress_bar.inc(1);
                            Arc::new(result)
                        })
                        .collect();
                    row
                })
                .collect();
        });

        progress_bar.finish_and_clear();

        Self {
            len_a,
            len_b,
            distances,
            zero: Arc::new(Fraction::zero()),
        }
    }

    pub fn get(&self, i: usize, j: usize) -> &Arc<Fraction> {
        &self.distances[i][j]
    }

    pub fn len_a(&self) -> usize {
        self.len_a
    }

    pub fn len_b(&self) -> usize {
        self.len_b
    }

    pub fn get_zero(&self) -> Arc<Fraction> {
        self.zero.clone()
    }
}

impl<'a> IntoIterator for &'a DistanceMatrix {
    type Item = (usize, usize, Arc<Fraction>);
    type IntoIter = DistanceMatrixIterator<'a>;

    fn into_iter(self) -> Self::IntoIter {
        DistanceMatrixIterator::new(self)
    }
}

impl Debug for DistanceMatrix {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("DistanceMatrix")
            .field("len_a", &self.len_a)
            .field("len_b", &self.len_b)
            .field("distances", &self.distances)
            .field("zero", &self.zero)
            .finish()
    }
}

pub struct DistanceMatrixIterator<'a> {
    i: usize,
    j: usize,
    index: usize,
    distances: &'a DistanceMatrix,
}

impl<'a> DistanceMatrixIterator<'a> {
    pub fn new(distances: &'a DistanceMatrix) -> Self {
        Self {
            i: 0,
            j: 0,
            index: 0,
            distances,
        }
    }
}

impl<'a> Iterator for DistanceMatrixIterator<'a> {
    type Item = (usize, usize, Arc<Fraction>);

    fn next(&mut self) -> Option<Self::Item> {
        if self.i >= self.distances.len_a {
            return None;
        }

        let result = Some((self.i, self.j, self.distances.get(self.i, self.j).clone()));

        self.j += 1;
        if self.j >= self.distances.len_b {
            self.i += 1;
            self.j = 0;
        }

        result
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        let remaining = (self.distances.len_a * self.distances.len_b) - self.index;
        (remaining, Some(remaining))
    }
}

impl<'a> ExactSizeIterator for DistanceMatrixIterator<'a> {}
impl<'a> std::iter::FusedIterator for DistanceMatrixIterator<'a> {}
