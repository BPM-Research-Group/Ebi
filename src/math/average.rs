use std::{borrow::Borrow, sync::Arc, fmt::Debug};
use anyhow::{anyhow, Result};
use fraction::{BigUint, GenericFraction, Ratio, Sign};

use crate::distances::TriangularDistanceMatrix;

use super::{fraction::Fraction, fraction_matched::FractionMatched};

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
            cardinality: vec![vec![0; len]; len]
        })
    }

    pub fn from_matched_distances(distances: &Arc<MatchedDistances>) -> Self {
        Self {
            distances: Arc::clone(distances),
            cardinality: vec![vec![0; distances.len()]; distances.len()]
        }
    }

    pub fn add(&mut self, index_i: usize, index_j: usize) {
        self.cardinality[index_i][index_j] += 1;
    }

    pub fn average(self) -> Result<Fraction> {
        let mut count = 0;
        let mut sum = FractionMatched::zero();

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
            let result = &self.distances.to_fraction(sum) / count;
            // log::debug!("average {}", result);
            Ok(result)
        }
    }
}

impl Debug for Average {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Average").field("cardinality", &self.cardinality).finish()
    }
}

pub enum MatchedDistances {
    Exact{distances: Vec<Arc<FractionMatched>>, zero: Arc<FractionMatched>, denominator: BigUint, len: usize},
    Approximate{distances: Vec<Arc<FractionMatched>>, zero: Arc<FractionMatched>, len: usize},
}

impl MatchedDistances {

    pub fn new(matrix: TriangularDistanceMatrix) -> Result<Self> {
        if Fraction::is_exaxt_globally() {
            let (matched_fractions, lcm) = FractionMatched::create(&matrix.distances)?;
            Ok(Self::Exact {
                distances: matched_fractions,
                zero: Arc::new(FractionMatched::zero()),
                denominator: lcm,
                len: matrix.len()
            })
        } else {
            let len = matrix.len();
            Ok(Self::Approximate {
                distances : matrix.distances.into_iter().map(|f| {
                    match f.borrow() {
                        Fraction::Approx(g) => Arc::new(FractionMatched::Approximate(*g)),
                        _ => unreachable!()
                    }}).collect::<Vec<_>>(),
                zero: Arc::new(FractionMatched::zero()),
                len: len
            })
        }
    }

    /**
     * Return the lenghth of one side of the matrix
     */
    pub fn len(&self) -> usize {
        match self {
            MatchedDistances::Exact { len, .. } => *len,
            MatchedDistances::Approximate { len, .. } => *len,
        }
    }


    fn get_index(i: usize, j: usize) -> usize {
        (i * (i-1)) / 2 + j
    }

    pub fn get(&self, i: usize, j: usize) -> &Arc<FractionMatched> {
        if i == j {
            &self.zero()
        } else {
            let index = if i > j { Self::get_index(i, j) } else { Self::get_index(j, i)};
            match self {
                MatchedDistances::Exact { distances, .. } => &distances[index],
                MatchedDistances::Approximate { distances, .. } => &distances[index]
            }
        }
    }

    fn zero(&self) -> &Arc<FractionMatched> {
        match self {
            MatchedDistances::Exact { zero, .. } => zero,
            MatchedDistances::Approximate { zero, ..} => zero
        }
    }

    pub fn to_fraction(&self, matched_fraction: FractionMatched) -> Fraction {
        match (self, matched_fraction) {
            (MatchedDistances::Exact { denominator, .. }, FractionMatched::Exact(numerator)) => 
                Fraction::Exact(GenericFraction::Rational(Sign::Plus, Ratio::new(numerator, denominator.clone()))),
            (MatchedDistances::Approximate { .. }, FractionMatched::Approximate(f)) 
                => Fraction::Approx(f),
            _ => Fraction::CannotCombineExactAndApprox
        }
    }
}