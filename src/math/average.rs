use anyhow::{Result, anyhow};
use ebi_arithmetic::{Fraction, Zero};
use std::{fmt::Debug, sync::Arc};

use crate::math::distances::TriangularDistanceMatrix;

use super::fixed_denominator_fraction::FixedDenominatorFraction;

/**
 * Class to compute the weighted average distance over distances, where these weights are integers and all distances are not negative.
 *
 * Internally, makes all the denominators equal, such that addition can be done without gcd computations.
 */
#[derive(Clone)]
pub struct Average {
    distances: Arc<MatchedDistances>,
    cardinality: Vec<Vec<u64>>,
}

impl Average {
    pub fn new(matrix: TriangularDistanceMatrix) -> Result<Self> {
        let len = matrix.len();
        Ok(Self {
            distances: Arc::new(MatchedDistances::new(matrix)?),
            cardinality: vec![vec![0; len]; len],
        })
    }

    pub fn from_matched_distances(distances: &Arc<MatchedDistances>) -> Self {
        Self {
            distances: Arc::clone(distances),
            cardinality: vec![vec![0; distances.len()]; distances.len()],
        }
    }

    pub fn add(&mut self, index_i: usize, index_j: usize) {
        self.cardinality[index_i][index_j] += 1;
    }

    pub fn average(self) -> Result<Fraction> {
        let mut count = 0;
        let mut sum = FixedDenominatorFraction::zero();

        for i in 0..self.distances.len() {
            for j in 0..self.distances.len() {
                if self.cardinality[i][j] > 0 {
                    count += self.cardinality[i][j];
                    sum += self.distances.get(i, j).as_ref().clone() * self.cardinality[i][j];
                }
            }
        }

        if count == 0 {
            Err(anyhow!("Cannot take the average over no values."))
        } else {
            let result = &sum.to_fraction() / count;
            // log::debug!("average {}", result);
            Ok(result)
        }
    }
}

impl Debug for Average {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Average")
            .field("cardinality", &self.cardinality)
            .finish()
    }
}

pub struct MatchedDistances {
    distances: Vec<Arc<FixedDenominatorFraction>>,
    zero: Arc<FixedDenominatorFraction>,
    len: usize,
}

impl MatchedDistances {
    pub fn new(matrix: TriangularDistanceMatrix) -> Result<Self> {
        Ok(Self {
            distances: FixedDenominatorFraction::create(&matrix.distances)?,
            zero: Arc::new(FixedDenominatorFraction::zero()),
            len: matrix.len(),
        })
    }

    /**
     * Return the lenghth of one side of the matrix
     */
    pub fn len(&self) -> usize {
        self.len
    }

    fn get_index(i: usize, j: usize) -> usize {
        (i * (i - 1)) / 2 + j
    }

    pub fn get(&self, i: usize, j: usize) -> &Arc<FixedDenominatorFraction> {
        if i == j {
            &self.zero
        } else {
            let index = if i > j {
                Self::get_index(i, j)
            } else {
                Self::get_index(j, i)
            };
            &self.distances[index]
        }
    }
}
