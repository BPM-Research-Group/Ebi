use std::{
    borrow::Borrow,
    ops::{AddAssign, Mul},
    sync::Arc,
};

use anyhow::{Result, anyhow};
use ebi_arithmetic::{
    ebi_number::Zero, exact::is_exact_globally, fraction::fraction_enum::FractionEnum,
};
use malachite::base::num::arithmetic::traits::Lcm;
use malachite::{Natural, rational::Rational};

#[derive(Clone)]
pub enum FixedDenominatorFractionEnum {
    Zero, //an exact zero value for which the denominator is not known (yet)
    Exact(Natural, Arc<Natural>),
    Approximate(f64),
    CannotCombineExactAndApprox,
}

impl FixedDenominatorFractionEnum {
    pub fn create(fractions: &Vec<Arc<FractionEnum>>) -> Result<Vec<Arc<Self>>> {
        match fractions.iter().next() {
            None => Ok(vec![]),
            Some(x) => match x.as_ref() {
                FractionEnum::CannotCombineExactAndApprox => {
                    Err(anyhow!("cannot combine exact and approximate arithmetic"))
                }
                FractionEnum::Approx(_) => {
                    //approximate mode
                    Ok(fractions
                        .into_iter()
                        .map(|f| match f.as_ref() {
                            FractionEnum::Approx(f) => Arc::new(Self::Approximate(f.clone())),
                            _ => Arc::new(Self::CannotCombineExactAndApprox),
                        })
                        .collect::<Vec<_>>())
                }
                FractionEnum::Exact(_) => {
                    //exact mode
                    let denominators = fractions
                        .iter()
                        .filter_map(|f| match f.as_ref() {
                            FractionEnum::Exact(r) => Some(r.to_denominator()),
                            _ => None,
                        })
                        .collect::<Vec<_>>();

                    let lowest_common_multiple =
                        Arc::new(Self::lowest_common_multiple(&denominators)?);

                    Ok(fractions
                        .iter()
                        .map(|f| match f.as_ref() {
                            FractionEnum::Exact(r) => {
                                let mut x = r.to_numerator() * lowest_common_multiple.as_ref();
                                x /= r.to_denominator();
                                Arc::new(Self::Exact(x, lowest_common_multiple.clone()))
                            }
                            _ => Arc::new(Self::CannotCombineExactAndApprox),
                        })
                        .collect::<Vec<_>>())
                }
            },
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

    /**
     * Returns whether the two given fractions are either both exact or both approximate
     */
    pub(crate) fn matches(&self, rhs: &Self) -> bool {
        match (self, rhs) {
            (Self::Zero, Self::Exact(_, _)) => true,
            (Self::Exact(_, _), Self::Zero) => true,
            (Self::Zero, Self::Zero) => true,
            (Self::Exact(_, denom1), Self::Exact(_, denom2)) => denom1 == denom2,
            (Self::Approximate(_), Self::Approximate(_)) => true,
            _ => false,
        }
    }

    pub fn to_fraction(self) -> FractionEnum {
        match self {
            Self::Exact(numer, denom) => {
                FractionEnum::Exact(Rational::from(numer) / Rational::from(denom.as_ref()))
            }
            Self::Approximate(f) => FractionEnum::Approx(f),
            Self::Zero => FractionEnum::zero(),
            _ => FractionEnum::CannotCombineExactAndApprox,
        }
    }
}

impl Zero for FixedDenominatorFractionEnum {
    fn zero() -> Self {
        if is_exact_globally() {
            Self::Zero
        } else {
            Self::Approximate(0.0)
        }
    }

    fn is_zero(&self) -> bool {
        match self {
            FixedDenominatorFractionEnum::Zero => true,
            FixedDenominatorFractionEnum::Exact(x, _) => x.is_zero(),
            FixedDenominatorFractionEnum::Approximate(f) => f.is_zero(),
            FixedDenominatorFractionEnum::CannotCombineExactAndApprox => false,
        }
    }
}

impl<T> AddAssign<T> for FixedDenominatorFractionEnum
where
    T: Borrow<FixedDenominatorFractionEnum>,
{
    fn add_assign(&mut self, rhs: T) {
        if let Self::Zero = self {
            *self = rhs.borrow().clone()
        } else if let Self::Zero = rhs.borrow() {
            //do nothing
        } else if self.matches(rhs.borrow()) {
            match (self, rhs.borrow()) {
                (Self::Exact(x, _), Self::Exact(y, _)) => x.add_assign(y),
                (Self::Approximate(x), Self::Approximate(y)) => x.add_assign(y),
                _ => {}
            };
        } else {
            *self = Self::CannotCombineExactAndApprox
        }
    }
}

impl AddAssign<&Arc<FixedDenominatorFractionEnum>> for FixedDenominatorFractionEnum {
    fn add_assign(&mut self, rhs: &Arc<FixedDenominatorFractionEnum>) {
        if let Self::Zero = self {
            *self = rhs.as_ref().clone()
        } else if let Self::Zero = rhs.borrow() {
            //do nothing
        } else if self.matches(rhs.borrow()) {
            match (self, rhs.borrow()) {
                (Self::Exact(x, _), Self::Exact(y, _)) => x.add_assign(y),
                (Self::Approximate(x), Self::Approximate(y)) => x.add_assign(y),
                _ => {}
            };
        } else {
            *self = Self::CannotCombineExactAndApprox
        }
    }
}

impl Mul<u64> for FixedDenominatorFractionEnum {
    type Output = FixedDenominatorFractionEnum;

    fn mul(self, rhs: u64) -> Self::Output {
        match self {
            Self::Zero => Self::Zero,
            Self::Exact(mut x, denom) => {
                x *= Natural::from(rhs);
                Self::Exact(x, denom)
            }
            Self::Approximate(mut x) => {
                x *= rhs as f64;
                Self::Approximate(x)
            }
            Self::CannotCombineExactAndApprox => Self::CannotCombineExactAndApprox,
        }
    }
}
