use std::{
    borrow::Borrow,
    ops::{AddAssign, Mul},
    sync::Arc,
};

use anyhow::{Result, anyhow};
use ebi_arithmetic::{MaybeExact, One, Zero, fraction::fraction_exact::FractionExact};
use malachite::{Natural, base::num::arithmetic::traits::Lcm, rational::Rational};

#[derive(Clone)]
pub struct FixedDenominatorFractionExact(Natural, Arc<Natural>);

impl FixedDenominatorFractionExact {
    pub fn create(fractions: &Vec<Arc<FractionExact>>) -> Result<Vec<Arc<Self>>> {
        match fractions.iter().next() {
            None => Ok(vec![]),
            Some(_) => {
                //exact mode
                let denominators = fractions
                    .iter()
                    .map(|f| f.as_ref().exact_ref().unwrap().to_numerator())
                    .collect::<Vec<_>>();

                let lowest_common_multiple = Arc::new(Self::lowest_common_multiple(&denominators)?);

                Ok(fractions
                    .iter()
                    .map(|f| {
                        let r = f.as_ref().exact_ref().unwrap();
                        let mut x = r.numerator_ref() * lowest_common_multiple.as_ref();
                        x /= r.denominator_ref();
                        Arc::new(FixedDenominatorFractionExact(
                            x,
                            lowest_common_multiple.clone(),
                        ))
                    })
                    .collect::<Vec<_>>())
            }
        }
    }

    pub fn lowest_common_multiple(numbers: &[Natural]) -> Result<Natural> {
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
            Self(numer, denom) => {
                FractionExact::from(Rational::from(numer) / Rational::from(denom.as_ref()))
            }
        }
    }
}

impl Zero for FixedDenominatorFractionExact {
    fn zero() -> Self {
        let zero = Natural::zero();
        let one = Arc::new(Natural::one());
        Self(zero, one)
    }

    fn is_zero(&self) -> bool {
        self.0.is_zero()
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
                (FixedDenominatorFractionExact(x, _), FixedDenominatorFractionExact(y, _)) => {
                    x.add_assign(y)
                }
            };
        }
    }
}

impl AddAssign<&Arc<FixedDenominatorFractionExact>> for FixedDenominatorFractionExact {
    fn add_assign(&mut self, rhs: &Arc<FixedDenominatorFractionExact>) {
        if self.is_zero() {
            *self = rhs.as_ref().clone()
        } else if <std::sync::Arc<FixedDenominatorFractionExact> as Borrow<
            FixedDenominatorFractionExact,
        >>::borrow(rhs)
        .is_zero()
        {
            //do nothing
        } else {
            match (self, rhs.borrow()) {
                (FixedDenominatorFractionExact(x, _), FixedDenominatorFractionExact(y, _)) => {
                    x.add_assign(y)
                }
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
                    x *= Natural::from(rhs);
                    FixedDenominatorFractionExact(x, denom)
                }
            }
        }
    }
}
