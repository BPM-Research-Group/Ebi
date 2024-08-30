use std::{borrow::Borrow, ops::{AddAssign, Mul}, sync::Arc};

use fraction::{BigUint, GenericFraction, Integer, One, Ratio, Sign, Zero};
use anyhow::{anyhow, Result};
use num_integer::lcm;

use super::fraction::Fraction;

#[derive(Clone)]
pub enum FractionMatched {
    Exact(BigUint),
    Approximate(f64),
    CannotCombineExactAndApprox
}

impl FractionMatched {

    pub fn create(fractions: &Vec<Arc<Fraction>>) -> Result<(Vec<Arc<Self>>, BigUint)> {
        let denominators = fractions.iter().filter_map(|f| match f.as_ref() {
            Fraction::Exact(GenericFraction::Rational(Sign::Plus, r)) => Some(r.denom()),
            _ => unreachable!("Cannot apply matched fractions to approximate arithmetic or negative numbers."),
        }).collect::<Vec<_>>();

        let lcm = Self::lcm(&denominators)?;

        let result = fractions.into_iter().map(|f| match f.as_ref() {
            Fraction::Exact(GenericFraction::Rational(Sign::Plus, r)) => {
                Arc::new(FractionMatched::Exact(
                    r.numer() * &lcm / r.denom(),
                ))
            },
            _ => unreachable!()
        }).collect::<Vec<_>>();

        Ok((result, lcm))
    }

    pub fn zero() -> Self {
        if Fraction::is_exaxt_globally() {
            Self::Exact(BigUint::zero())
        } else {
            Self::Approximate(0.0)
        }
    }

    pub fn lcm(numbers: &[&BigUint]) -> Result<BigUint> {
        if numbers.is_empty() {
            return Err(anyhow!("cannot compute lcm on empty list"));
        }
    
        let mut it = numbers.iter();
        let mut result = it.next().unwrap().to_owned().to_owned();
        while let Some(number) = it.next() {
            result = result.lcm(number);
        }
    
        Ok(result)
    }

    /**
     * Returns whether the two given fractions are either both exact or both approximate
     */
    pub(crate) fn matches(&self, rhs: &Self) -> bool {
        match (self, rhs) {
            (Self::Exact(_), Self::Exact(_)) => true,
            (Self::Approximate(_), Self::Approximate(_)) => true,
            _ => false
        }
    }

}

impl <T> AddAssign<T> for FractionMatched where T: Borrow<FractionMatched> {
    fn add_assign(&mut self, rhs: T) {

        if self.matches(rhs.borrow()) {
            match (self, rhs.borrow()) {
                (Self::Exact(x), Self::Exact(y)) => x.add_assign(y),
                (Self::Approximate(x), Self::Approximate(y)) => x.add_assign(y),
                _ => {}
            };
        } else {
            *self = Self::CannotCombineExactAndApprox
        }
    }
}

impl AddAssign<&Arc<FractionMatched>> for FractionMatched {
    fn add_assign(&mut self, rhs: &Arc<FractionMatched>) {

        if self.matches(rhs.borrow()) {
            match (self, rhs.borrow()) {
                (Self::Exact(x), Self::Exact(y)) => x.add_assign(y),
                (Self::Approximate(x), Self::Approximate(y)) => x.add_assign(y),
                _ => {}
            };
        } else {
            *self = Self::CannotCombineExactAndApprox
        }
    }
}

impl Mul<u64> for FractionMatched {
    type Output = FractionMatched;

    fn mul(self, rhs: u64) -> Self::Output {
        match self {
            FractionMatched::Exact(mut x) => {x *= rhs; FractionMatched::Exact(x)},
            FractionMatched::Approximate(mut x) => {x *= rhs as f64; FractionMatched::Approximate(x)},
            FractionMatched::CannotCombineExactAndApprox => FractionMatched::CannotCombineExactAndApprox,
        }
    }
}