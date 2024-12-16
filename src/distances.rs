use std::{iter::FusedIterator, sync::Arc};
use std::fmt::Debug;

use rayon::iter::{IntoParallelIterator, ParallelIterator};

use crate::ebi_framework::ebi_command::EbiCommand;
use crate::ebi_traits::ebi_trait_event_log::IndexTrace;
use crate::math::levenshtein;
use crate::math::fraction::Fraction;

pub struct TriangularDistanceMatrix {
    len: usize, //lenght of one side of the matrix
    pub(crate) distances: Vec<Arc<Fraction>>,
    zero: Arc<Fraction>
}

impl TriangularDistanceMatrix {
    pub fn new<T>(log: &T) -> Self where T: IndexTrace + ?Sized {
        log::info!("Compute distances");
        let progress_bar = EbiCommand::get_progress_bar_ticks(Self::get_number_of_distances(log.len()));

        let len = log.len();
        let log = Arc::new(log);

        let distances = (0..Self::get_number_of_distances(len)).into_par_iter().map(|index| {
            let log = Arc::clone(&log);
            let i = Self::get_i(index);
            let j = Self::get_j(index, i);
            
            let trace1 = log.get_trace(i).unwrap();
            let trace2 = log.get_trace(j).unwrap();
            
            let result = levenshtein::normalised(trace1, trace2);
            progress_bar.inc(1);
            result
        }).collect::<Vec<_>>();
        let distances = distances.into_iter().map(|d| Arc::new(d)).collect();

        progress_bar.finish_and_clear();

        Self {
            len,
            distances: distances,
            zero: Arc::new(Fraction::zero())
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
        (i * (i-1)) / 2 + j
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

impl <'a> IntoIterator for &'a TriangularDistanceMatrix {
    type Item = (usize, usize, usize, Arc<Fraction>);

    type IntoIter = TriangularIterator<'a>;

    fn into_iter(self) -> Self::IntoIter {
        TriangularIterator::new(self)
    }
}

impl Debug for TriangularDistanceMatrix {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Distances").field("single_len", &self.len).field("distances", &self.distances).field("zero", &self.zero).finish()
    }
}

pub struct TriangularIterator<'a> {
    i: usize,
    j: usize,
    index: usize,
    distances: &'a TriangularDistanceMatrix
}

impl <'a> TriangularIterator<'a> {
    pub fn new(distances: &'a TriangularDistanceMatrix) -> Self {
        Self {
            i: 1,
            j: 0,
            index: 0,
            distances: &distances
        }
    }
}

impl <'a> Iterator for TriangularIterator<'a> {
    type Item = (usize, usize, usize, Arc<Fraction>);

    /**
     * (i, j, index)
     */
    fn next(&mut self) -> Option<Self::Item> {

        if self.i >= self.distances.len {
            return None;
        }
        let result = Some((self.i, self.j, self.index, self.distances.distances[self.index].clone()));
        
        self.index += 1;
        self.j += 1;
        if self.j >= self.i {
            self.i += 1;
            self.j = 0;
        }
        
        result
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        (self.distances.distances.len() - self.index - 1, Some(self.distances.distances.len() - self.index - 1))
    }
}

impl <'a> FusedIterator for TriangularIterator<'a> {}

impl <'a> ExactSizeIterator for TriangularIterator<'a> {}