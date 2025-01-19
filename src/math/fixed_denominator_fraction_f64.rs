use std::{borrow::Borrow, ops::{AddAssign, Mul}, sync::Arc};

use fraction::{BigUint, GenericFraction, Integer, Ratio, Sign};
use anyhow::{anyhow, Result};

use super::{fraction::Fraction, fraction_f64::FractionF64};

#[derive(Clone)]
pub struct FixedDenominatorFractionF64(f64);

impl FixedDenominatorFractionF64 {

    pub fn create(fractions: &Vec<Arc<Fraction>>) -> Result<Vec<Arc<Self>>> {
        Ok(fractions.iter().map(|f| Arc::new(Self(f.0.clone()))).collect())
    }

    pub fn zero() -> Self {
        Self(0.0)
    }

    /**
     * Returns whether the two given fractions are either both exact or both approximate
     */
    pub(crate) fn matches(&self, rhs: &Self) -> bool {
        true
    }

    pub fn to_fraction(self) -> Fraction {
        FractionF64(self.0)
    }
}

impl <T> AddAssign<T> for FixedDenominatorFractionF64 where T: Borrow<FixedDenominatorFractionF64> {
    fn add_assign(&mut self, rhs: T) {
        self.0.add_assign(rhs.borrow().0);
    }
}

impl AddAssign<&Arc<FixedDenominatorFractionF64>> for FixedDenominatorFractionF64 {
    fn add_assign(&mut self, rhs: &Arc<FixedDenominatorFractionF64>) {
        self.0.add_assign(rhs.as_ref().0);
    }
}

impl Mul<u64> for FixedDenominatorFractionF64 {
    type Output = FixedDenominatorFractionF64;

    fn mul(self, rhs: u64) -> Self::Output {
        Self(self.0 * rhs as f64)
    }
}