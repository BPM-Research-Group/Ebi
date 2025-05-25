use std::{
    borrow::Borrow,
    ops::{AddAssign, Mul},
    sync::Arc,
};

use anyhow::{anyhow, Result};
use fraction::{BigUint, GenericFraction, Integer, Ratio, Sign};

use super::{fraction_exact::FractionExact, traits::Zero};

#[derive(Clone)]
pub struct FixedDenominatorFractionExact(BigUint, Arc<BigUint>);

impl FixedDenominatorFractionExact {
    pub fn create(fractions: &Vec<Arc<FractionExact>>) -> Result<Vec<Arc<Self>>> {
        match fractions.iter().next() {
            None => Ok(vec![]),
            Some(_) => {
                //exact mode
                let denominators = fractions
                    .iter()
                    .filter_map(|f| match f.as_ref() {
                        FractionExact(GenericFraction::Rational(Sign::Plus, r)) => {
                            Some(r.denom())
                        }
                        _ => None,
                    })
                    .collect::<Vec<_>>();

                let lowest_common_multiple = Arc::new(Self::lowest_common_multiple(&denominators)?);

                Ok(fractions
                    .iter()
                    .map(|f| match f.as_ref() {
                        FractionExact(GenericFraction::Rational(Sign::Plus, r)) => {
                            let mut x = r.numer() * lowest_common_multiple.as_ref();
                            x /= r.denom();
                            Arc::new(FixedDenominatorFractionExact(x, lowest_common_multiple.clone()))
                        }
                        _ => panic!("cannot combine exact and approximate arithmetic"),
                    })
                    .collect::<Vec<_>>())
            }
        }
    }

    pub fn lowest_common_multiple(numbers: &[&BigUint]) -> Result<BigUint> {
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

    pub fn to_fraction(self) -> FractionExact {
        match self {
            Self(numer, denom) => FractionExact(GenericFraction::Rational(
                Sign::Plus,
                Ratio::new(numer, denom.as_ref().clone()),
            ))
        }
    }
}

impl crate::math::traits::Zero for FixedDenominatorFractionExact {
    fn zero() -> Self {
        let zero = num::Zero::zero();
        let one = Arc::new(num::One::one());
        Self(zero, one)
    }

    fn is_zero(&self) -> bool {
        num::Zero::is_zero(&self.0)
    }
}

impl<T> AddAssign<T> for FixedDenominatorFractionExact
where
    T: Borrow<FixedDenominatorFractionExact>,
{
    fn add_assign(&mut self, rhs: T) {
        if self.is_zero() {
            *self = rhs.borrow().clone()
        } else if rhs.borrow().is_zero() {
            //do nothing
        } else {
            match (self, rhs.borrow()) {
                (FixedDenominatorFractionExact(x, _), FixedDenominatorFractionExact(y, _)) => x.add_assign(y),
            };
        }
    }
}

impl AddAssign<&Arc<FixedDenominatorFractionExact>> for FixedDenominatorFractionExact {
    fn add_assign(&mut self, rhs: &Arc<FixedDenominatorFractionExact>) {
        if self.is_zero() {
            *self = rhs.as_ref().clone()
        } else if <std::sync::Arc<FixedDenominatorFractionExact> as Borrow<FixedDenominatorFractionExact>>::borrow(rhs).is_zero() {
            //do nothing
        } else {
            match (self, rhs.borrow()) {
                (FixedDenominatorFractionExact(x, _), FixedDenominatorFractionExact(y, _)) => x.add_assign(y),
            };
        }
    }
}

impl Mul<u64> for FixedDenominatorFractionExact {
    type Output = FixedDenominatorFractionExact;

    fn mul(self, rhs: u64) -> Self::Output {
        if self.is_zero() {
            self
        } else {
            match self {
                FixedDenominatorFractionExact(mut x, denom) => {
                    x *= rhs;
                    FixedDenominatorFractionExact(x, denom)
                }
            }
        }
    }
}