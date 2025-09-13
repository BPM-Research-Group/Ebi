use std::{
    borrow::Borrow,
    ops::{AddAssign, Mul},
    sync::Arc,
};

use anyhow::Result;
use ebi_arithmetic::{MaybeExact, Zero, fraction::fraction_f64::FractionF64};

#[derive(Clone)]
pub struct FixedDenominatorFractionF64(f64);

impl FixedDenominatorFractionF64 {
    pub fn create(fractions: &Vec<Arc<FractionF64>>) -> Result<Vec<Arc<Self>>> {
        Ok(fractions
            .iter()
            .map(|f| Arc::new(Self(f.approx().unwrap())))
            .collect())
    }

    pub fn to_fraction(self) -> FractionF64 {
        FractionF64::from(self.0)
    }
}

impl Zero for FixedDenominatorFractionF64 {
    fn zero() -> Self {
        Self(0.0)
    }

    fn is_zero(&self) -> bool {
        self.0.is_zero()
    }
}

impl<T> AddAssign<T> for FixedDenominatorFractionF64
where
    T: Borrow<FixedDenominatorFractionF64>,
{
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
