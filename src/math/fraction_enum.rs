use anyhow::{anyhow, Error, Result};
use fraction::{BigFraction, BigUint, GenericFraction, Sign};
use num::{BigInt, One as NumOne};
use num_bigint::{RandBigInt, ToBigInt, ToBigUint};
use num_rational::Ratio;
use num_traits::ToPrimitive;
use rand::Rng;
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

use crate::{
    ebi_framework::{ebi_output::EbiOutput, exportable::Exportable, infoable::Infoable},
    math::fraction::EPSILON,
};

use super::{
    fraction::{ChooseRandomly, FractionNotParsedYet, MaybeExact, UInt, EXACT},
    traits::{One, Signed, Zero},
};

#[derive(Clone)]
pub enum FractionEnum {
    Exact(fraction::BigFraction),
    Approx(f64),
    CannotCombineExactAndApprox,
}

impl FractionEnum {
    pub(crate) fn create_exact() -> bool {
        EXACT.load(std::sync::atomic::Ordering::Relaxed)
    }

    /**
     * Returns whether the two given fractions are either both exact or both approximate
     */
    pub(crate) fn matches(&self, rhs: &Self) -> bool {
        match (self, rhs) {
            (FractionEnum::Exact(_), FractionEnum::Exact(_)) => true,
            (FractionEnum::Approx(_), FractionEnum::Approx(_)) => true,
            _ => false,
        }
    }

    pub fn sqrt_abs(&self, decimal_places: u32) -> FractionEnum {
        match self {
            FractionEnum::Exact(f) => FractionEnum::Exact(f.sqrt_abs(decimal_places)),
            FractionEnum::Approx(f) => FractionEnum::Approx(f.abs().sqrt()),
            FractionEnum::CannotCombineExactAndApprox => self.clone(),
        }
    }

    pub fn is_sign_negative(&self) -> bool {
        match self {
            FractionEnum::Exact(f) => f.is_sign_negative(),
            FractionEnum::Approx(f) => f.is_sign_negative(),
            FractionEnum::CannotCombineExactAndApprox => true,
        }
    }

    pub fn is_sign_positive(&self) -> bool {
        match self {
            FractionEnum::Exact(f) => f.is_sign_positive(),
            FractionEnum::Approx(f) => f.is_sign_positive(),
            FractionEnum::CannotCombineExactAndApprox => false,
        }
    }

    /// Returns true if the value is Infinity (does not matter positive or negative)
    pub fn is_infinite(&self) -> bool {
        match self {
            FractionEnum::Exact(f) => f.is_infinite(),
            FractionEnum::Approx(f) => f.is_infinite(),
            FractionEnum::CannotCombineExactAndApprox => false,
        }
    }

    pub fn is_nan(&self) -> bool {
        match self {
            FractionEnum::Exact(f) => f.is_nan(),
            FractionEnum::Approx(f) => f.is_nan(),
            FractionEnum::CannotCombineExactAndApprox => true,
        }
    }

    pub fn infinity() -> Self {
        if Self::create_exact() {
            FractionEnum::Exact(BigFraction::infinity())
        } else {
            FractionEnum::Approx(f64::INFINITY)
        }
    }

    pub fn neg_infinity() -> Self {
        if Self::create_exact() {
            FractionEnum::Exact(BigFraction::neg_infinity())
        } else {
            FractionEnum::Approx(f64::NEG_INFINITY)
        }
    }

    pub fn nan() -> Self {
        if Self::create_exact() {
            FractionEnum::Exact(BigFraction::nan())
        } else {
            FractionEnum::Approx(f64::NAN)
        }
    }

    pub fn sign(&self) -> Option<Sign> {
        match self {
            FractionEnum::Exact(f) => f.sign(),
            FractionEnum::Approx(f) => {
                if f.is_nan() {
                    None
                } else if <f64 as Zero>::is_zero(f) {
                    Some(Sign::Plus)
                } else if f.is_sign_positive() {
                    Some(Sign::Plus)
                } else {
                    Some(Sign::Minus)
                }
            }
            FractionEnum::CannotCombineExactAndApprox => None,
        }
    }

    /**
     * 1/self
     */
    pub fn recip(&self) -> Self {
        match self {
            FractionEnum::Exact(f) => FractionEnum::Exact(f.recip()),
            FractionEnum::Approx(f) => FractionEnum::Approx(f.recip()),
            FractionEnum::CannotCombineExactAndApprox => self.clone(),
        }
    }

    pub fn one_minus(self) -> Self {
        match self {
            FractionEnum::Exact(mut f) => {
                f = f.neg();
                f.add_assign(1.to_biguint().unwrap());
                FractionEnum::Exact(f)
            }
            FractionEnum::Approx(f) => FractionEnum::Approx(1.0 - f),
            Self::CannotCombineExactAndApprox => self,
        }
    }

    pub fn two() -> FractionEnum {
        if Self::create_exact() {
            FractionEnum::Exact(GenericFraction::Rational(
                Sign::Plus,
                Ratio::new_raw(UInt::from(2u32), UInt::from(1u32)),
            ))
        } else {
            FractionEnum::Approx(2.0)
        }
    }
}

impl MaybeExact for FractionEnum {
    type Approximate = f64;
    type Exact = BigFraction;

    fn is_exact(&self) -> bool {
        match self {
            FractionEnum::Exact(_) => true,
            FractionEnum::Approx(_) => false,
            FractionEnum::CannotCombineExactAndApprox => false,
        }
    }

    fn extract_approx(&self) -> Result<f64> {
        match self {
            FractionEnum::Exact(_) => Err(anyhow!("cannot extract a float from a fraction")),
            FractionEnum::Approx(f) => Ok(*f),
            FractionEnum::CannotCombineExactAndApprox => {
                Err(anyhow!("cannot combine exact and approximate arithmetic"))
            }
        }
    }

    fn extract_exact(&self) -> Result<GenericFraction<BigUint>> {
        match self {
            FractionEnum::Exact(generic_fraction) => Ok(generic_fraction.clone()),
            FractionEnum::Approx(_) => Err(anyhow!("cannot extract a fraction from a float")),
            FractionEnum::CannotCombineExactAndApprox => {
                Err(anyhow!("cannot combine exact and approximate arithmetic"))
            }
        }
    }
}

impl ChooseRandomly for FractionEnum {
    fn choose_randomly(fractions: &Vec<FractionEnum>) -> Result<usize> {
        if fractions.is_empty() {
            return Err(anyhow!("cannot take an element of an empty list"));
        }

        //normalise the probabilities
        let mut probabilities: Vec<FractionEnum> = fractions.iter().cloned().collect();
        let sum = probabilities
            .iter()
            .fold(FractionEnum::zero(), |x, y| &x + y);
        if sum == FractionEnum::CannotCombineExactAndApprox {
            return Err(anyhow!("cannot combine exact and approximate arithmetic"));
        }
        probabilities.retain_mut(|v| {
            *v /= &sum;
            true
        });

        //select a random value
        let mut rng = rand::thread_rng();
        let rand_val = if sum.is_exact() {
            //strategy: the highest denominator determines how much precision we need
            let temp_zero = BigUint::zero();
            let max_denom = probabilities
                .iter()
                .map(|f| {
                    if let FractionEnum::Exact(e) = f {
                        e.denom().unwrap()
                    } else {
                        &temp_zero
                    }
                })
                .max()
                .unwrap();
            //Generate a random value with the number of bits of the highest denominator. Repeat until this value is <= the max denominator.
            let mut rand_val = rng.gen_biguint(max_denom.bits());
            while &rand_val > max_denom {
                rand_val = rng.gen_biguint(max_denom.bits());
            }
            //create the fraction from the random nominator and the max denominator
            FractionEnum::try_from((rand_val, max_denom.clone())).unwrap()
        } else {
            FractionEnum::Approx(rng.gen_range(0.0..=1.0))
        };

        let mut cum_prob = FractionEnum::zero();
        for (index, value) in probabilities.iter().enumerate() {
            cum_prob += value;
            if rand_val < cum_prob {
                return Ok(index);
            }
        }
        Ok(probabilities.len() - 1)
    }
}

impl One for FractionEnum {
    fn one() -> Self {
        if Self::create_exact() {
            FractionEnum::Exact(GenericFraction::Rational(Sign::Plus, Ratio::one()))
        } else {
            FractionEnum::Approx(1.0)
        }
    }

    fn is_one(&self) -> bool {
        match self {
            FractionEnum::Exact(f) => fraction::One::is_one(f),
            FractionEnum::Approx(f) => (f - 1.0).abs() < EPSILON,
            Self::CannotCombineExactAndApprox => false,
        }
    }
}

impl Zero for FractionEnum {
    fn zero() -> Self {
        if Self::create_exact() {
            FractionEnum::Exact(GenericFraction::Rational(Sign::Plus, num::Zero::zero()))
        } else {
            FractionEnum::Approx(<f64 as Zero>::zero())
        }
    }

    fn is_zero(&self) -> bool {
        match self {
            FractionEnum::Exact(f) => fraction::Zero::is_zero(f),
            FractionEnum::Approx(f) => f.abs() - &EPSILON < 0.0,
            Self::CannotCombineExactAndApprox => false,
        }
    }
}

impl Signed for FractionEnum {
    fn abs(&self) -> Self {
        match self {
            FractionEnum::Exact(f) => FractionEnum::Exact(f.abs()),
            FractionEnum::Approx(f) => FractionEnum::Approx(f.abs()),
            FractionEnum::CannotCombineExactAndApprox => self.clone(),
        }
    }

    fn is_positive(&self) -> bool {
        match self {
            FractionEnum::Exact(f) => !num::Zero::is_zero(f) && fraction::Signed::is_positive(f),
            FractionEnum::Approx(f) => *f != 0f64 && f.is_positive(),
            FractionEnum::CannotCombineExactAndApprox => false,
        }
    }

    fn is_negative(&self) -> bool {
        match self {
            FractionEnum::Exact(f) => fraction::Signed::is_negative(f),
            FractionEnum::Approx(f) => *f != 0f64 && f.is_negative(),
            FractionEnum::CannotCombineExactAndApprox => false,
        }
    }
}

impl FromStr for FractionEnum {
    type Err = Error;

    fn from_str(s: &str) -> std::prelude::v1::Result<Self, Self::Err> {
        if Self::create_exact() {
            Ok(FractionEnum::Exact(BigFraction::from_str(s)?))
        } else {
            if let Ok(float) = f64::from_str(s) {
                Ok(FractionEnum::Approx(float))
            } else {
                let fraction = BigFraction::from_str(s)?;
                match fraction.to_f64() {
                    Some(f) => Ok(FractionEnum::Approx(f)),
                    None => Err(anyhow!("could not read fraction {} as float", s)),
                }
            }
        }
    }
}

impl TryFrom<&FractionNotParsedYet> for FractionEnum {
    type Error = Error;

    fn try_from(value: &FractionNotParsedYet) -> std::result::Result<Self, Self::Error> {
        Self::from_str(&value.s)
    }
}

impl crate::ebi_framework::ebi_trait::FromEbiTraitObject for FractionEnum {
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

impl From<&FractionEnum> for FractionEnum {
    fn from(value: &FractionEnum) -> Self {
        match value {
            FractionEnum::Exact(_) => value.clone(),
            FractionEnum::Approx(_) => value.clone(),
            FractionEnum::CannotCombineExactAndApprox => value.clone(),
        }
    }
}

impl From<Arc<FractionEnum>> for FractionEnum {
    fn from(value: Arc<FractionEnum>) -> Self {
        match value.as_ref() {
            FractionEnum::Exact(f) => FractionEnum::Exact(f.clone()),
            FractionEnum::Approx(f) => FractionEnum::Approx(f.clone()),
            FractionEnum::CannotCombineExactAndApprox => FractionEnum::CannotCombineExactAndApprox,
        }
    }
}

impl From<&Arc<FractionEnum>> for FractionEnum {
    fn from(value: &Arc<FractionEnum>) -> Self {
        match value.as_ref() {
            FractionEnum::Exact(f) => FractionEnum::Exact(f.clone()),
            FractionEnum::Approx(f) => FractionEnum::Approx(f.clone()),
            FractionEnum::CannotCombineExactAndApprox => FractionEnum::CannotCombineExactAndApprox,
        }
    }
}

impl TryFrom<BigUint> for FractionEnum {
    type Error = Error;

    fn try_from(value: BigUint) -> std::prelude::v1::Result<Self, Self::Error> {
        if Self::create_exact() {
            Ok(FractionEnum::Exact(GenericFraction::Rational(
                Sign::Plus,
                Ratio::new(value, UInt::from(1u32)),
            )))
        } else {
            if value < u64::MAX.to_biguint().unwrap() {
                Ok(FractionEnum::Approx(value.to_f64().unwrap()))
            } else {
                Err(anyhow!("value too large for approximate arithmetic"))
            }
        }
    }
}

impl TryFrom<&BigUint> for FractionEnum {
    type Error = Error;

    fn try_from(value: &BigUint) -> std::prelude::v1::Result<Self, Self::Error> {
        if Self::create_exact() {
            Ok(FractionEnum::Exact(GenericFraction::Rational(
                Sign::Plus,
                Ratio::new(value.clone(), UInt::from(1u32)),
            )))
        } else {
            if value < &u64::MAX.to_biguint().unwrap() {
                Ok(FractionEnum::Approx(value.to_f64().unwrap()))
            } else {
                Err(anyhow!("value too large for approximate arithmetic"))
            }
        }
    }
}

impl TryFrom<BigInt> for FractionEnum {
    type Error = Error;

    fn try_from(value: BigInt) -> std::prelude::v1::Result<Self, Self::Error> {
        if Self::create_exact() {
            Ok(FractionEnum::Exact(GenericFraction::Rational(
                if value.is_negative() {
                    Sign::Minus
                } else {
                    Sign::Plus
                },
                Ratio::new(value.abs().to_biguint().unwrap(), UInt::from(1u32)),
            )))
        } else {
            if value < u64::MAX.to_bigint().unwrap() {
                Ok(FractionEnum::Approx(value.to_f64().unwrap()))
            } else {
                Err(anyhow!("value too large for approximate arithmetic"))
            }
        }
    }
}

impl TryFrom<(BigUint, BigUint)> for FractionEnum {
    type Error = Error;

    fn try_from(value: (BigUint, BigUint)) -> std::prelude::v1::Result<Self, Self::Error> {
        if Self::create_exact() {
            Ok(FractionEnum::Exact(GenericFraction::Rational(
                Sign::Plus,
                Ratio::new(value.0, value.1),
            )))
        } else {
            if let (Some(numer), Some(denom)) = (value.0.to_u64(), value.1.to_u64()) {
                Ok(FractionEnum::Approx(numer as f64 / denom as f64))
            } else {
                Err(anyhow!("numbers too large for approximate arithmetic"))
            }
        }
    }
}

impl Exportable for FractionEnum {
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

impl Infoable for FractionEnum {
    fn info(&self, f: &mut impl std::io::Write) -> Result<()> {
        match self {
            FractionEnum::Exact(v) => {v.info(f)?; writeln!(f, "")?;},
            FractionEnum::Approx(v) => writeln!(f, "Approximate value\t{}", v)?,
            FractionEnum::CannotCombineExactAndApprox => writeln!(f, "Fraction is a result of combining exact and approximate arithmethic and therefore has no value.")?,
        };

        Ok(write!(f, "")?)
    }
}

impl std::fmt::Display for FractionEnum {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            FractionEnum::Exact(fr) => std::fmt::Display::fmt(&fr, f),
            FractionEnum::Approx(fr) => std::fmt::Display::fmt(&fr, f),
            FractionEnum::CannotCombineExactAndApprox => {
                write!(f, "cannot combine exact and approximate arithmatic")
            }
        }
    }
}

impl std::fmt::Debug for FractionEnum {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Exact(arg0) => f.debug_tuple("Exact ").field(arg0).finish(),
            Self::Approx(arg0) => f.debug_tuple("Approx ").field(arg0).finish(),
            Self::CannotCombineExactAndApprox => {
                write!(f, "cannot combine exact and approximate arithmatic")
            }
        }
    }
}

impl Add<&FractionEnum> for &FractionEnum {
    type Output = FractionEnum;

    fn add(self, rhs: &FractionEnum) -> Self::Output {
        match (self, rhs) {
            (FractionEnum::Exact(x), FractionEnum::Exact(y)) => FractionEnum::Exact(x.add(y)),
            (FractionEnum::Approx(x), FractionEnum::Approx(y)) => FractionEnum::Approx(x.add(y)),
            _ => FractionEnum::CannotCombineExactAndApprox,
        }
    }
}

impl<T> AddAssign<T> for FractionEnum
where
    T: Borrow<FractionEnum>,
{
    fn add_assign(&mut self, rhs: T) {
        let rhs = rhs.borrow();

        if self.matches(&rhs) {
            match (self, rhs) {
                (FractionEnum::Exact(x), FractionEnum::Exact(y)) => x.add_assign(y),
                (FractionEnum::Approx(x), FractionEnum::Approx(y)) => x.add_assign(y),
                _ => {}
            };
        } else {
            *self = FractionEnum::CannotCombineExactAndApprox
        }
    }
}

impl AddAssign<&Arc<FractionEnum>> for FractionEnum {
    fn add_assign(&mut self, rhs: &Arc<FractionEnum>) {
        let rhs = rhs.borrow();

        if self.matches(&rhs) {
            match (self, rhs) {
                (FractionEnum::Exact(x), FractionEnum::Exact(y)) => x.add_assign(y),
                (FractionEnum::Approx(x), FractionEnum::Approx(y)) => x.add_assign(y),
                _ => {}
            };
        } else {
            *self = FractionEnum::CannotCombineExactAndApprox
        }
    }
}

impl Sub<&FractionEnum> for &FractionEnum {
    type Output = FractionEnum;

    fn sub(self, rhs: &FractionEnum) -> Self::Output {
        match (self, rhs) {
            (FractionEnum::Exact(x), FractionEnum::Exact(y)) => FractionEnum::Exact(x.sub(y)),
            (FractionEnum::Approx(x), FractionEnum::Approx(y)) => FractionEnum::Approx(x.sub(y)),
            _ => FractionEnum::CannotCombineExactAndApprox,
        }
    }
}

impl<T> SubAssign<T> for FractionEnum
where
    T: Borrow<FractionEnum>,
{
    fn sub_assign(&mut self, rhs: T) {
        let rhs = rhs.borrow();
        if self.matches(&rhs) {
            match (self, rhs) {
                (FractionEnum::Exact(x), FractionEnum::Exact(y)) => x.sub_assign(y),
                (FractionEnum::Approx(x), FractionEnum::Approx(y)) => x.sub_assign(y),
                _ => {}
            }
        } else {
            *self = FractionEnum::CannotCombineExactAndApprox;
        }
    }
}

impl Mul<&FractionEnum> for &FractionEnum {
    type Output = FractionEnum;

    fn mul(self, rhs: &FractionEnum) -> Self::Output {
        match (self, rhs) {
            (FractionEnum::Exact(x), FractionEnum::Exact(y)) => FractionEnum::Exact(x.mul(y)),
            (FractionEnum::Approx(x), FractionEnum::Approx(y)) => FractionEnum::Approx(x.mul(y)),
            _ => FractionEnum::CannotCombineExactAndApprox,
        }
    }
}

impl<T> MulAssign<T> for FractionEnum
where
    T: Borrow<FractionEnum>,
{
    fn mul_assign(&mut self, rhs: T) {
        let rhs = rhs.borrow();
        if self.matches(&rhs) {
            match (self, rhs) {
                (FractionEnum::Exact(x), FractionEnum::Exact(y)) => x.mul_assign(y),
                (FractionEnum::Approx(x), FractionEnum::Approx(y)) => x.mul_assign(y),
                _ => {}
            }
        } else {
            *self = FractionEnum::CannotCombineExactAndApprox
        }
    }
}

impl Div<&FractionEnum> for &FractionEnum {
    type Output = FractionEnum;

    fn div(self, rhs: &FractionEnum) -> Self::Output {
        match (self, rhs) {
            (FractionEnum::Exact(x), FractionEnum::Exact(y)) => FractionEnum::Exact(x.div(y)),
            (FractionEnum::Approx(x), FractionEnum::Approx(y)) => FractionEnum::Approx(x.div(y)),
            _ => FractionEnum::CannotCombineExactAndApprox,
        }
    }
}

impl<T> DivAssign<T> for FractionEnum
where
    T: Borrow<FractionEnum>,
{
    fn div_assign(&mut self, rhs: T) {
        let rhs = rhs.borrow();
        if self.matches(&rhs) {
            match (self, rhs) {
                (FractionEnum::Exact(x), FractionEnum::Exact(y)) => x.div_assign(y),
                (FractionEnum::Approx(x), FractionEnum::Approx(y)) => x.div_assign(y),
                _ => {}
            }
        } else {
            *self = FractionEnum::CannotCombineExactAndApprox
        }
    }
}

impl Neg for FractionEnum {
    type Output = FractionEnum;

    fn neg(self) -> Self::Output {
        match self {
            FractionEnum::Exact(f) => FractionEnum::Exact(f.neg()),
            FractionEnum::Approx(f) => FractionEnum::Approx(f.neg()),
            Self::CannotCombineExactAndApprox => self.clone(),
        }
    }
}

impl<'a> Neg for &'a FractionEnum {
    type Output = FractionEnum;

    fn neg(self) -> Self::Output {
        match self {
            FractionEnum::Exact(f) => FractionEnum::Exact(f.neg()),
            FractionEnum::Approx(f) => FractionEnum::Approx(f.neg()),
            FractionEnum::CannotCombineExactAndApprox => self.clone(),
        }
    }
}

impl PartialEq for FractionEnum {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Exact(l0), Self::Exact(r0)) => l0 == r0,
            (Self::Approx(l0), Self::Approx(r0)) => l0 - EPSILON <= *r0 && *r0 <= l0 + EPSILON,
            _ => false,
        }
    }
}

impl Eq for FractionEnum {}

impl PartialOrd for FractionEnum {
    /**
     * Note that exact and approximate should not be compared.
     */
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        if !self.matches(other) {
            panic!("cannot compare exact and inexact arithmethic");
        }
        match (self, other) {
            (FractionEnum::Exact(x), FractionEnum::Exact(y)) => x.partial_cmp(y),
            (FractionEnum::Approx(x), FractionEnum::Approx(y)) => x.partial_cmp(y),
            _ => None,
        }
    }
}

impl Ord for FractionEnum {
    /**
     * Note that exact and approximate should not be compared.
     */
    fn cmp(&self, other: &Self) -> Ordering {
        if !self.matches(other) {
            panic!("cannot compare exact and inexact arithmethic");
        }
        match (self, other) {
            (FractionEnum::Exact(x), FractionEnum::Exact(y)) => x.cmp(y),
            (FractionEnum::Approx(x), FractionEnum::Approx(y)) => {
                if x.is_nan() && y.is_nan() {
                    Ordering::Equal
                } else if x.is_nan() {
                    Ordering::Less
                } else if y.is_nan() {
                    Ordering::Greater
                } else if x == &f64::INFINITY {
                    if y == &f64::INFINITY {
                        Ordering::Equal
                    } else {
                        Ordering::Greater
                    }
                } else if y == &f64::INFINITY {
                    Ordering::Less
                } else if x == &f64::NEG_INFINITY {
                    if y == &f64::NEG_INFINITY {
                        Ordering::Equal
                    } else {
                        Ordering::Less
                    }
                } else if y == &f64::NEG_INFINITY {
                    Ordering::Greater
                } else {
                    x.partial_cmp(y).unwrap()
                }
            }
            (FractionEnum::Exact(_), FractionEnum::Approx(_)) => Ordering::Greater,
            (FractionEnum::Exact(_), FractionEnum::CannotCombineExactAndApprox) => {
                Ordering::Greater
            }
            (FractionEnum::Approx(_), FractionEnum::Exact(_)) => Ordering::Less,
            (FractionEnum::Approx(_), FractionEnum::CannotCombineExactAndApprox) => {
                Ordering::Greater
            }
            (FractionEnum::CannotCombineExactAndApprox, FractionEnum::Exact(_)) => Ordering::Less,
            (FractionEnum::CannotCombineExactAndApprox, FractionEnum::Approx(_)) => Ordering::Less,
            (
                FractionEnum::CannotCombineExactAndApprox,
                FractionEnum::CannotCombineExactAndApprox,
            ) => Ordering::Less,
        }
    }
}

impl Hash for FractionEnum {
    /**
     * For good reasons, Rust does not support hashing of doubles. However, we need it to store distributions in a hashmap.
     * Approximate arithmetic is discouraged
     */
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        match self {
            FractionEnum::Exact(f) => f.hash(state),
            FractionEnum::Approx(f) => unsafe { std::mem::transmute::<f64, u64>(*f).hash(state) },
            Self::CannotCombineExactAndApprox => "cceaa".hash(state),
        }
    }
}

impl Sum for FractionEnum {
    fn sum<I: Iterator<Item = Self>>(iter: I) -> Self {
        iter.fold(Self::zero(), |sum, f| &sum + &f)
    }
}

impl<'a> Sum<&'a FractionEnum> for FractionEnum {
    fn sum<I: Iterator<Item = &'a FractionEnum>>(iter: I) -> Self {
        iter.fold(FractionEnum::zero(), |sum, f| &sum + f)
    }
}

//======================== primitive types ========================//

macro_rules! from {
    ($t:ident) => {
        impl From<$t> for FractionEnum {
            fn from(value: $t) -> Self {
                if Self::create_exact() {
                    FractionEnum::Exact(GenericFraction::Rational(
                        Sign::Plus,
                        Ratio::new(value.to_biguint().unwrap(), UInt::from(1u32)),
                    ))
                } else {
                    FractionEnum::Approx(value as f64)
                }
            }
        }
    };
}

macro_rules! from_signed {
    ($t:ident) => {
        impl From<$t> for FractionEnum {
            fn from(value: $t) -> Self {
                if Self::create_exact() {
                    FractionEnum::Exact(GenericFraction::Rational(
                        if value.is_negative() {
                            Sign::Minus
                        } else {
                            Sign::Plus
                        },
                        Ratio::new(value.abs().to_biguint().unwrap(), UInt::from(1u32)),
                    ))
                } else {
                    FractionEnum::Approx(value as f64)
                }
            }
        }
    };
}

macro_rules! from_tuple_u_u {
    ($t:ident,$tt:ident) => {
        impl From<($t, $tt)> for FractionEnum {
            fn from(value: ($t, $tt)) -> Self {
                if Self::create_exact() {
                    FractionEnum::Exact(GenericFraction::Rational(
                        Sign::Plus,
                        Ratio::new(UInt::from(value.0), UInt::from(value.1)),
                    ))
                } else {
                    FractionEnum::Approx(value.0 as f64 / value.1 as f64)
                }
            }
        }
    };
}

macro_rules! from_tuple_u_i {
    ($t:ident,$tt:ident) => {
        impl From<($t, $tt)> for FractionEnum {
            fn from(value: ($t, $tt)) -> Self {
                if Self::create_exact() {
                    let s1 = if value.1.is_negative() {
                        Sign::Minus
                    } else {
                        Sign::Plus
                    };
                    FractionEnum::Exact(GenericFraction::Rational(
                        s1,
                        Ratio::new(UInt::from(value.0), UInt::from(value.1.abs() as u128)),
                    ))
                } else {
                    FractionEnum::Approx(value.0 as f64 / value.1 as f64)
                }
            }
        }
    };
}

macro_rules! from_tuple_i_u {
    ($t:ident,$tt:ident) => {
        impl From<($t, $tt)> for FractionEnum {
            fn from(value: ($t, $tt)) -> Self {
                if Self::create_exact() {
                    let s1 = if value.0.is_negative() {
                        Sign::Minus
                    } else {
                        Sign::Plus
                    };
                    FractionEnum::Exact(GenericFraction::Rational(
                        s1,
                        Ratio::new(UInt::from(value.0.abs() as u128), UInt::from(value.1)),
                    ))
                } else {
                    FractionEnum::Approx(value.0 as f64 / value.1 as f64)
                }
            }
        }
    };
}

macro_rules! from_tuple_i_i {
    ($t:ident,$tt:ident) => {
        impl From<($t, $tt)> for FractionEnum {
            fn from(value: ($t, $tt)) -> Self {
                if Self::create_exact() {
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
                    FractionEnum::Exact(GenericFraction::Rational(
                        s0 * s1,
                        Ratio::new(
                            UInt::from(value.0.abs() as u128),
                            UInt::from(value.1.abs() as u128),
                        ),
                    ))
                } else {
                    FractionEnum::Approx(value.0 as f64 / value.1 as f64)
                }
            }
        }
    };
}

macro_rules! add {
    ($t:ident) => {
        impl<'a> Add<$t> for &'a FractionEnum {
            type Output = FractionEnum;

            fn add(self, rhs: $t) -> Self::Output {
                let rhs = rhs.into();
                match (self, rhs) {
                    (FractionEnum::Exact(x), FractionEnum::Exact(y)) => {
                        FractionEnum::Exact(x.add(y))
                    }
                    (FractionEnum::Approx(x), FractionEnum::Approx(y)) => {
                        FractionEnum::Approx(x.add(y))
                    }
                    _ => FractionEnum::CannotCombineExactAndApprox,
                }
            }
        }
    };
}

macro_rules! add_assign {
    ($t:ident) => {
        impl AddAssign<$t> for FractionEnum {
            fn add_assign(&mut self, rhs: $t) {
                let rhs = rhs.into();
                if self.matches(&rhs) {
                    match (self, rhs) {
                        (FractionEnum::Exact(x), FractionEnum::Exact(y)) => x.add_assign(y),
                        (FractionEnum::Approx(x), FractionEnum::Approx(y)) => x.add_assign(y),
                        _ => {}
                    };
                } else {
                    *self = FractionEnum::CannotCombineExactAndApprox
                }
            }
        }
    };
}

macro_rules! sub {
    ($t:ident) => {
        impl<'a> Sub<$t> for &'a FractionEnum {
            type Output = FractionEnum;

            fn sub(self, rhs: $t) -> Self::Output {
                let rhs = rhs.into();
                match (self, rhs) {
                    (FractionEnum::Exact(x), FractionEnum::Exact(y)) => {
                        FractionEnum::Exact(x.sub(y))
                    }
                    (FractionEnum::Approx(x), FractionEnum::Approx(y)) => {
                        FractionEnum::Approx(x.sub(y))
                    }
                    _ => FractionEnum::CannotCombineExactAndApprox,
                }
            }
        }
    };
}

macro_rules! sub_assign {
    ($t:ident) => {
        impl SubAssign<$t> for FractionEnum {
            fn sub_assign(&mut self, rhs: $t) {
                let rhs = rhs.into();
                if self.matches(&rhs) {
                    match (self, rhs) {
                        (FractionEnum::Exact(x), FractionEnum::Exact(y)) => x.sub_assign(y),
                        (FractionEnum::Approx(x), FractionEnum::Approx(y)) => x.sub_assign(y),
                        _ => {}
                    };
                } else {
                    *self = FractionEnum::CannotCombineExactAndApprox
                }
            }
        }
    };
}

macro_rules! mul {
    ($t:ident) => {
        impl<'a> Mul<$t> for &'a FractionEnum {
            type Output = FractionEnum;

            fn mul(self, rhs: $t) -> Self::Output {
                let rhs = rhs.into();
                match (self, rhs) {
                    (FractionEnum::Exact(x), FractionEnum::Exact(y)) => {
                        FractionEnum::Exact(x.mul(y))
                    }
                    (FractionEnum::Approx(x), FractionEnum::Approx(y)) => {
                        FractionEnum::Approx(x.mul(y))
                    }
                    _ => FractionEnum::CannotCombineExactAndApprox,
                }
            }
        }
    };
}

macro_rules! mul_assign {
    ($t:ident) => {
        impl MulAssign<$t> for FractionEnum {
            fn mul_assign(&mut self, rhs: $t) {
                let rhs = rhs.into();
                if self.matches(&rhs) {
                    match (self, rhs) {
                        (FractionEnum::Exact(x), FractionEnum::Exact(y)) => x.mul_assign(y),
                        (FractionEnum::Approx(x), FractionEnum::Approx(y)) => x.mul_assign(y),
                        _ => {}
                    };
                } else {
                    *self = FractionEnum::CannotCombineExactAndApprox
                }
            }
        }
    };
}

macro_rules! div {
    ($t:ident) => {
        impl<'a> Div<$t> for &'a FractionEnum {
            type Output = FractionEnum;

            fn div(self, rhs: $t) -> Self::Output {
                let rhs = rhs.into();
                match (self, rhs) {
                    (FractionEnum::Exact(x), FractionEnum::Exact(y)) => {
                        FractionEnum::Exact(x.div(y))
                    }
                    (FractionEnum::Approx(x), FractionEnum::Approx(y)) => {
                        FractionEnum::Approx(x.div(y))
                    }
                    _ => FractionEnum::CannotCombineExactAndApprox,
                }
            }
        }
    };
}

macro_rules! div_assign {
    ($t:ident) => {
        impl DivAssign<$t> for FractionEnum {
            fn div_assign(&mut self, rhs: $t) {
                let rhs = rhs.into();
                if self.matches(&rhs) {
                    match (self, rhs) {
                        (FractionEnum::Exact(x), FractionEnum::Exact(y)) => x.div_assign(y),
                        (FractionEnum::Approx(x), FractionEnum::Approx(y)) => x.div_assign(y),
                        _ => {}
                    };
                } else {
                    *self = FractionEnum::CannotCombineExactAndApprox
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
        fraction_enum::FractionEnum,
        traits::{One, Signed, Zero},
    };

    #[test]
    fn fraction_neg() {
        let one = FractionEnum::one();
        assert!(one.is_positive());
        let one = one.neg();
        assert!(one.is_negative());
    }

    #[test]
    fn fraction_exact() {
        let zero = FractionEnum::one().one_minus();

        assert!(zero.is_zero());
    }
}
