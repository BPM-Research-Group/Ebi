use anyhow::{anyhow, Error, Result};
use fraction::{BigFraction, BigUint, GenericFraction, Sign};
use num::BigInt;
use num_bigint::{RandBigInt, ToBigUint};
use num_rational::Ratio;
use std::{
    borrow::Borrow,
    cmp::Ordering,
    f64,
    hash::Hash,
    iter::Sum,
    ops::{Add, AddAssign, Div, DivAssign, Mul, MulAssign, Neg, Sub, SubAssign},
    str::FromStr,
    sync::Arc,
};

use crate::ebi_framework::{ebi_output::EbiOutput, exportable::Exportable, infoable::Infoable};

use super::{
    fraction::{FractionNotParsedYet, MaybeExact, ChooseRandomly, UInt},
    traits::{One, Signed, Zero},
};

#[derive(Clone)]
pub struct FractionExact(pub fraction::BigFraction);

impl FractionExact {
    pub fn sqrt_abs(&self, decimal_places: u32) -> FractionExact {
        Self(self.0.sqrt_abs(decimal_places))
    }

    pub fn is_sign_negative(&self) -> bool {
        self.0.is_sign_negative()
    }

    pub fn is_sign_positive(&self) -> bool {
        self.0.is_sign_positive()
    }

    /// Returns true if the value is Infinity (does not matter positive or negative)
    pub fn is_infinite(&self) -> bool {
        self.0.is_infinite()
    }

    pub fn is_nan(&self) -> bool {
        self.0.is_nan()
    }

    pub fn infinity() -> Self {
        Self(BigFraction::infinity())
    }

    pub fn neg_infinity() -> Self {
        Self(BigFraction::neg_infinity())
    }

    pub fn nan() -> Self {
        Self(BigFraction::nan())
    }

    pub fn sign(&self) -> Option<Sign> {
        self.0.sign()
    }

    /**
     * 1/self
     */
    pub fn recip(&self) -> Self {
        Self(self.0.recip())
    }

    pub fn one_minus(mut self) -> Self {
        self.0 = self.0.neg();
        self.0.add_assign(1.to_biguint().unwrap());
        self
    }

    pub fn two() -> FractionExact {
        Self(GenericFraction::Rational(
            Sign::Plus,
            Ratio::new_raw(UInt::from(2u32), UInt::from(1u32)),
        ))
    }
}

impl ChooseRandomly for FractionExact {
    fn choose_randomly(fractions: &Vec<FractionExact>) -> Result<usize> {
        if fractions.is_empty() {
            return Err(anyhow!("cannot take an element of an empty list"));
        }

        //normalise the probabilities
        let mut probabilities: Vec<FractionExact> = fractions.iter().cloned().collect();
        let sum = probabilities
            .iter()
            .fold(FractionExact::zero(), |x, y| &x + y);
        probabilities.retain_mut(|v| {
            *v /= &sum;
            true
        });

        //select a random value
        let mut rng = rand::thread_rng();
        let rand_val = {
            //strategy: the highest denominator determines how much precision we need
            let max_denom = probabilities
                .iter()
                .map(|f| match f {
                    FractionExact(e) => e.denom().unwrap(),
                })
                .max()
                .unwrap();
            //Generate a random value with the number of bits of the highest denominator. Repeat until this value is <= the max denominator.
            let mut rand_val = rng.gen_biguint(max_denom.bits());
            while &rand_val > max_denom {
                rand_val = rng.gen_biguint(max_denom.bits());
            }
            //create the fraction from the random nominator and the max denominator
            FractionExact::try_from((rand_val, max_denom.clone())).unwrap()
        };

        let mut cum_prob = FractionExact::zero();
        for (index, value) in probabilities.iter().enumerate() {
            cum_prob += value;
            if rand_val < cum_prob {
                return Ok(index);
            }
        }
        Ok(probabilities.len() - 1)
    }
}

impl MaybeExact for FractionExact {
    type Approximate = f64;
    type Exact = fraction::BigFraction;

    fn is_exact(&self) -> bool {
        true
    }

    fn extract_approx(&self) -> Result<f64> {
        Err(anyhow!("cannot extract a float from a fraction"))
    }

    /**
     * This is a low-level function to extract an f64. Only use if you are sure that the fraction is exact.
     * May not be available in all compilation modes.
     */
    fn extract_exact(&self) -> Result<GenericFraction<BigUint>> {
        Ok(self.0.clone())
    }
}

impl One for FractionExact {
    fn one() -> Self {
        Self(GenericFraction::Rational(Sign::Plus, num::One::one()))
    }

    fn is_one(&self) -> bool {
        fraction::One::is_one(&self.0)
    }
}

impl Zero for FractionExact {
    fn zero() -> Self {
        Self(GenericFraction::Rational(Sign::Plus, num::Zero::zero()))
    }

    fn is_zero(&self) -> bool {
        fraction::Zero::is_zero(&self.0)
    }
}

impl Signed for FractionExact {
    fn abs(&self) -> Self {
        Self(self.0.abs())
    }

    fn is_positive(&self) -> bool {
        !self.0.is_zero() && fraction::Signed::is_positive(&self.0)
    }

    fn is_negative(&self) -> bool {
        fraction::Signed::is_negative(&self.0)
    }
}

impl FromStr for FractionExact {
    type Err = Error;

    fn from_str(s: &str) -> std::prelude::v1::Result<Self, Self::Err> {
        Ok(Self(BigFraction::from_str(s)?))
    }
}

impl TryFrom<&FractionNotParsedYet> for FractionExact {
    type Error = Error;

    fn try_from(value: &FractionNotParsedYet) -> std::result::Result<Self, Self::Error> {
        Self::from_str(&value.s)
    }
}

impl crate::ebi_framework::ebi_trait::FromEbiTraitObject for FractionExact {
    fn from_trait_object(object: crate::ebi_framework::ebi_input::EbiInput) -> Result<Box<Self>> {
        match object {
            crate::ebi_framework::ebi_input::EbiInput::Fraction(e) => Ok(Box::new(e)),
            _ => Err(anyhow!(
                "cannot read {} {} as a fraction",
                object.get_type().get_article(),
                object.get_type()
            )),
        }
    }
}

impl From<&FractionExact> for FractionExact {
    fn from(value: &FractionExact) -> Self {
        value.clone()
    }
}

impl From<Arc<FractionExact>> for FractionExact {
    fn from(value: Arc<FractionExact>) -> Self {
        Self(value.0.clone())
    }
}

impl From<&Arc<FractionExact>> for FractionExact {
    fn from(value: &Arc<FractionExact>) -> Self {
        match value.as_ref() {
            FractionExact(f) => FractionExact(f.clone()),
        }
    }
}

impl TryFrom<BigUint> for FractionExact {
    type Error = Error;

    fn try_from(value: BigUint) -> std::prelude::v1::Result<Self, Self::Error> {
        Ok(Self(GenericFraction::Rational(
            Sign::Plus,
            Ratio::new(value, UInt::from(1u32)),
        )))
    }
}

impl TryFrom<&BigUint> for FractionExact {
    type Error = Error;

    fn try_from(value: &BigUint) -> std::prelude::v1::Result<Self, Self::Error> {
        Ok(Self(GenericFraction::Rational(
            Sign::Plus,
            Ratio::new(value.clone(), UInt::from(1u32)),
        )))
    }
}

impl TryFrom<BigInt> for FractionExact {
    type Error = Error;

    fn try_from(value: BigInt) -> std::prelude::v1::Result<Self, Self::Error> {
        Ok(Self(GenericFraction::Rational(
            if value.is_negative() {
                Sign::Minus
            } else {
                Sign::Plus
            },
            Ratio::new(value.abs().to_biguint().unwrap(), UInt::from(1u32)),
        )))
    }
}

impl TryFrom<(BigUint, BigUint)> for FractionExact {
    type Error = Error;

    fn try_from(value: (BigUint, BigUint)) -> std::prelude::v1::Result<Self, Self::Error> {
        Ok(Self(GenericFraction::Rational(
            Sign::Plus,
            Ratio::new(value.0, value.1),
        )))
    }
}

impl Exportable for FractionExact {
    fn export_from_object(object: EbiOutput, f: &mut dyn std::io::Write) -> Result<()> {
        match object {
            EbiOutput::Fraction(fr) => fr.export(f),
            _ => unreachable!(),
        }
    }

    fn export(&self, f: &mut dyn std::io::Write) -> Result<()> {
        writeln!(f, "{}", self)?;
        Ok(writeln!(f, "Approximately {:.4}", self)?)
    }
}

impl Infoable for FractionExact {
    fn info(&self, f: &mut impl std::io::Write) -> Result<()> {
        self.0.info(f)?;
        writeln!(f, "")?;
        Ok(write!(f, "")?)
    }
}

impl std::fmt::Display for FractionExact {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(&self.0, f)
    }
}

impl std::fmt::Debug for FractionExact {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_tuple("Exact ").field(&self.0).finish()
    }
}

impl Add<&FractionExact> for &FractionExact {
    type Output = FractionExact;

    fn add(self, rhs: &FractionExact) -> Self::Output {
        match (self, rhs) {
            (FractionExact(x), FractionExact(y)) => FractionExact(x.add(y)),
        }
    }
}

impl<T> AddAssign<T> for FractionExact
where
    T: Borrow<FractionExact>,
{
    fn add_assign(&mut self, rhs: T) {
        let rhs = rhs.borrow();
        match (self, rhs) {
            (FractionExact(x), FractionExact(y)) => x.add_assign(y),
        }
    }
}

impl AddAssign<&Arc<FractionExact>> for FractionExact {
    fn add_assign(&mut self, rhs: &Arc<FractionExact>) {
        let rhs = rhs.borrow();
        match (self, rhs) {
            (FractionExact(x), FractionExact(y)) => x.add_assign(y),
        }
    }
}

impl Sub<&FractionExact> for &FractionExact {
    type Output = FractionExact;

    fn sub(self, rhs: &FractionExact) -> Self::Output {
        match (self, rhs) {
            (FractionExact(x), FractionExact(y)) => FractionExact(x.sub(y)),
        }
    }
}

impl<T> SubAssign<T> for FractionExact
where
    T: Borrow<FractionExact>,
{
    fn sub_assign(&mut self, rhs: T) {
        let rhs = rhs.borrow();
        match (self, rhs) {
            (FractionExact(x), FractionExact(y)) => x.sub_assign(y),
        }
    }
}

impl Mul<&FractionExact> for &FractionExact {
    type Output = FractionExact;

    fn mul(self, rhs: &FractionExact) -> Self::Output {
        match (self, rhs) {
            (FractionExact(x), FractionExact(y)) => FractionExact(x.mul(y)),
        }
    }
}

impl<T> MulAssign<T> for FractionExact
where
    T: Borrow<FractionExact>,
{
    fn mul_assign(&mut self, rhs: T) {
        let rhs = rhs.borrow();
        match (self, rhs) {
            (FractionExact(x), FractionExact(y)) => x.mul_assign(y),
        }
    }
}

impl Div<&FractionExact> for &FractionExact {
    type Output = FractionExact;

    fn div(self, rhs: &FractionExact) -> Self::Output {
        match (self, rhs) {
            (FractionExact(x), FractionExact(y)) => FractionExact(x.div(y)),
        }
    }
}

impl<T> DivAssign<T> for FractionExact
where
    T: Borrow<FractionExact>,
{
    fn div_assign(&mut self, rhs: T) {
        let rhs = rhs.borrow();
        match (self, rhs) {
            (FractionExact(x), FractionExact(y)) => x.div_assign(y),
        }
    }
}

impl Neg for FractionExact {
    type Output = FractionExact;

    fn neg(self) -> Self::Output {
        FractionExact(self.0.neg())
    }
}

impl<'a> Neg for &'a FractionExact {
    type Output = FractionExact;

    fn neg(self) -> Self::Output {
        match self {
            FractionExact(f) => FractionExact(f.neg()),
        }
    }
}

impl PartialEq for FractionExact {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (FractionExact(x), FractionExact(y)) => x == y,
        }
    }
}

impl Eq for FractionExact {}

impl PartialOrd for FractionExact {
    /**
     * Note that exact and approximate should not be compared.
     */
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        match (self, other) {
            (FractionExact(x), FractionExact(y)) => x.partial_cmp(y),
        }
    }
}

impl Ord for FractionExact {
    fn cmp(&self, other: &Self) -> Ordering {
        self.0.cmp(&other.0)
    }
}

impl Hash for FractionExact {
    /**
     * For good reasons, Rust does not support hashing of doubles. However, we need it to store distributions in a hashmap.
     * Approximate arithmetic is discouraged
     */
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        match self {
            FractionExact(f) => f.hash(state),
        }
    }
}

impl Sum for FractionExact {
    fn sum<I: Iterator<Item = Self>>(iter: I) -> Self {
        iter.fold(Self::zero(), |sum, f| &sum + &f)
    }
}

impl<'a> Sum<&'a FractionExact> for FractionExact {
    fn sum<I: Iterator<Item = &'a FractionExact>>(iter: I) -> Self {
        iter.fold(FractionExact::zero(), |sum, f| &sum + f)
    }
}

//======================== primitive types ========================//

macro_rules! from {
    ($t:ident) => {
        impl From<$t> for FractionExact {
            fn from(value: $t) -> Self {
                Self(GenericFraction::Rational(
                    Sign::Plus,
                    Ratio::new(value.to_biguint().unwrap(), UInt::from(1u32)),
                ))
            }
        }
    };
}

macro_rules! from_signed {
    ($t:ident) => {
        impl From<$t> for FractionExact {
            fn from(value: $t) -> Self {
                Self(GenericFraction::Rational(
                    if value.is_negative() {
                        Sign::Minus
                    } else {
                        Sign::Plus
                    },
                    Ratio::new(value.abs().to_biguint().unwrap(), UInt::from(1u32)),
                ))
            }
        }
    };
}

macro_rules! from_tuple_u_u {
    ($t:ident,$tt:ident) => {
        impl From<($t, $tt)> for FractionExact {
            fn from(value: ($t, $tt)) -> Self {
                FractionExact(GenericFraction::Rational(
                    Sign::Plus,
                    Ratio::new(UInt::from(value.0), UInt::from(value.1)),
                ))
            }
        }
    };
}

macro_rules! from_tuple_u_i {
    ($t:ident,$tt:ident) => {
        impl From<($t, $tt)> for FractionExact {
            fn from(value: ($t, $tt)) -> Self {
                let s1 = if value.1.is_negative() {
                    Sign::Minus
                } else {
                    Sign::Plus
                };
                FractionExact(GenericFraction::Rational(
                    s1,
                    Ratio::new(UInt::from(value.0), UInt::from(value.1.abs() as u128)),
                ))
            }
        }
    };
}

macro_rules! from_tuple_i_u {
    ($t:ident,$tt:ident) => {
        impl From<($t, $tt)> for FractionExact {
            fn from(value: ($t, $tt)) -> Self {
                let s1 = if value.0.is_negative() {
                    Sign::Minus
                } else {
                    Sign::Plus
                };
                Self(GenericFraction::Rational(
                    s1,
                    Ratio::new(UInt::from(value.0.abs() as u128), UInt::from(value.1)),
                ))
            }
        }
    };
}

macro_rules! from_tuple_i_i {
    ($t:ident,$tt:ident) => {
        impl From<($t, $tt)> for FractionExact {
            fn from(value: ($t, $tt)) -> Self {
                let s0 = if value.0.is_negative() {
                    Sign::Minus
                } else {
                    Sign::Plus
                };
                let s1 = if value.1.is_negative() {
                    Sign::Minus
                } else {
                    Sign::Plus
                };
                Self(GenericFraction::Rational(
                    s0 * s1,
                    Ratio::new(
                        UInt::from(value.0.abs() as u128),
                        UInt::from(value.1.abs() as u128),
                    ),
                ))
            }
        }
    };
}

macro_rules! add {
    ($t:ident) => {
        impl<'a> Add<$t> for &'a FractionExact {
            type Output = FractionExact;

            fn add(self, rhs: $t) -> Self::Output {
                let rhs = rhs.into();
                match (self, rhs) {
                    (FractionExact(x), FractionExact(y)) => FractionExact(x.add(y)),
                }
            }
        }
    };
}

macro_rules! add_assign {
    ($t:ident) => {
        impl AddAssign<$t> for FractionExact {
            fn add_assign(&mut self, rhs: $t) {
                let rhs = rhs.into();
                match (self, rhs) {
                    (FractionExact(x), FractionExact(y)) => x.add_assign(y),
                }
            }
        }
    };
}

macro_rules! sub {
    ($t:ident) => {
        impl<'a> Sub<$t> for &'a FractionExact {
            type Output = FractionExact;

            fn sub(self, rhs: $t) -> Self::Output {
                let rhs = rhs.into();
                match (self, rhs) {
                    (FractionExact(x), FractionExact(y)) => FractionExact(x.sub(y)),
                }
            }
        }
    };
}

macro_rules! sub_assign {
    ($t:ident) => {
        impl SubAssign<$t> for FractionExact {
            fn sub_assign(&mut self, rhs: $t) {
                let rhs = rhs.into();
                match (self, rhs) {
                    (FractionExact(x), FractionExact(y)) => x.sub_assign(y),
                }
            }
        }
    };
}

macro_rules! mul {
    ($t:ident) => {
        impl<'a> Mul<$t> for &'a FractionExact {
            type Output = FractionExact;

            fn mul(self, rhs: $t) -> Self::Output {
                let rhs = rhs.into();
                match (self, rhs) {
                    (FractionExact(x), FractionExact(y)) => FractionExact(x.mul(y)),
                }
            }
        }
    };
}

macro_rules! mul_assign {
    ($t:ident) => {
        impl MulAssign<$t> for FractionExact {
            fn mul_assign(&mut self, rhs: $t) {
                let rhs = rhs.into();
                match (self, rhs) {
                    (FractionExact(x), FractionExact(y)) => x.mul_assign(y),
                }
            }
        }
    };
}

macro_rules! div {
    ($t:ident) => {
        impl<'a> Div<$t> for &'a FractionExact {
            type Output = FractionExact;

            fn div(self, rhs: $t) -> Self::Output {
                let rhs = rhs.into();
                match (self, rhs) {
                    (FractionExact(x), FractionExact(y)) => FractionExact(x.div(y)),
                }
            }
        }
    };
}

macro_rules! div_assign {
    ($t:ident) => {
        impl DivAssign<$t> for FractionExact {
            fn div_assign(&mut self, rhs: $t) {
                let rhs = rhs.into();
                match (self, rhs) {
                    (FractionExact(x), FractionExact(y)) => x.div_assign(y),
                }
            }
        }
    };
}

macro_rules! ttype_tuple {
    ($t:ident) => {
        from_tuple_u_u!($t, usize);
        from_tuple_u_u!($t, u128);
        from_tuple_u_u!($t, u64);
        from_tuple_u_u!($t, u32);
        from_tuple_u_u!($t, u16);
        from_tuple_u_u!($t, u8);
        from_tuple_u_i!($t, i128);
        from_tuple_u_i!($t, i64);
        from_tuple_u_i!($t, i32);
        from_tuple_u_i!($t, i16);
        from_tuple_u_i!($t, i8);
    };
}

macro_rules! ttype_tuple_signed {
    ($t:ident) => {
        from_tuple_i_u!($t, usize);
        from_tuple_i_u!($t, u128);
        from_tuple_i_u!($t, u64);
        from_tuple_i_u!($t, u32);
        from_tuple_i_u!($t, u16);
        from_tuple_i_u!($t, u8);
        from_tuple_i_i!($t, i64);
        from_tuple_i_i!($t, i32);
        from_tuple_i_i!($t, i16);
        from_tuple_i_i!($t, i8);
    };
}

macro_rules! ttype {
    ($t:ident) => {
        from!($t);
        ttype_tuple!($t);
        add!($t);
        add_assign!($t);
        sub!($t);
        sub_assign!($t);
        mul!($t);
        mul_assign!($t);
        div!($t);
        div_assign!($t);
    };
}

macro_rules! ttype_signed {
    ($t:ident) => {
        from_signed!($t);
        ttype_tuple_signed!($t);
        add!($t);
        add_assign!($t);
        sub!($t);
        sub_assign!($t);
        mul!($t);
        mul_assign!($t);
        div!($t);
        div_assign!($t);
    };
}

ttype!(usize);
ttype!(u128);
ttype!(u64);
ttype!(u32);
ttype!(u16);
ttype!(u8);
ttype_signed!(i128);
ttype_signed!(i64);
ttype_signed!(i32);
ttype_signed!(i16);
ttype_signed!(i8);

#[cfg(test)]
mod tests {
    use std::ops::Neg;

    use crate::math::{
        fraction_exact::FractionExact,
        traits::{One, Signed, Zero},
    };

    #[test]
    fn fraction_neg() {
        let one = FractionExact::one();
        assert!(one.is_positive());
        let one = one.neg();
        assert!(one.is_negative());
    }

    #[test]
    fn fraction_exact() {
        let zero = FractionExact::one().one_minus();

        assert!(zero.is_zero());
    }
}
