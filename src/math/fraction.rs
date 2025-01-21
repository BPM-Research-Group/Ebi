use anyhow::{anyhow, Error, Result};
use fraction::{BigFraction, BigInt, BigUint, GenericFraction, One, Sign, Zero};
use num_bigint::{RandBigInt, ToBigInt, ToBigUint};
use num_rational::Ratio;
use num_traits::{Signed, ToPrimitive};
use rand::Rng;
use std::{
    borrow::Borrow,
    cmp::Ordering,
    hash::Hash,
    iter::Sum,
    ops::{Add, AddAssign, Div, DivAssign, Mul, MulAssign, Neg, Sub, SubAssign},
    str::FromStr,
    sync::{atomic::AtomicBool, Arc},
};

use crate::ebi_framework::{
    ebi_input::EbiInput, ebi_output::EbiOutput, ebi_trait::FromEbiTraitObject,
    exportable::Exportable, infoable::Infoable,
};

use super::fraction_raw::FractionRaw;

static EXACT: AtomicBool = AtomicBool::new(true);
pub const APPROX_DIGITS: u64 = 5;

#[derive(Clone)]
pub enum Fraction {
    Exact(fraction::BigFraction),
    Approx(f64),
    CannotCombineExactAndApprox,
}
pub type UInt = fraction::BigUint;

impl Fraction {
    /**
     * Enables or disables exact arithmetic for all future calls to Fraction.
     * Exact arithmetic cannot be combined with approximate arithmetic, and if this nevertheless occurs, the results will be set to Fraction::CannotCombineExactAndApprox
     */
    pub fn set_exact_globally(exact: bool) {
        EXACT.store(exact, std::sync::atomic::Ordering::Relaxed);
    }

    pub fn is_exaxt_globally() -> bool {
        EXACT.load(std::sync::atomic::Ordering::Relaxed)
    }

    pub(crate) fn create_exact() -> bool {
        EXACT.load(std::sync::atomic::Ordering::Relaxed)
    }

    pub(crate) fn is_exact(&self) -> bool {
        match self {
            Fraction::Exact(_) => true,
            Fraction::Approx(_) => false,
            Fraction::CannotCombineExactAndApprox => false,
        }
    }

    /**
     * Returns whether the two given fractions are either both exact or both approximate
     */
    pub(crate) fn matches(&self, rhs: &Self) -> bool {
        match (self, rhs) {
            (Fraction::Exact(_), Fraction::Exact(_)) => true,
            (Fraction::Approx(_), Fraction::Approx(_)) => true,
            _ => false,
        }
    }

    pub fn sqrt_abs(&self, decimal_places: u32) -> Fraction {
        match self {
            Fraction::Exact(f) => Fraction::Exact(f.sqrt_abs(decimal_places)),
            Fraction::Approx(f) => Fraction::Approx(f.abs().sqrt()),
            Fraction::CannotCombineExactAndApprox => self.clone(),
        }
    }

    pub fn is_sign_negative(&self) -> bool {
        match self {
            Fraction::Exact(f) => f.is_sign_negative(),
            Fraction::Approx(f) => f.is_sign_negative(),
            Fraction::CannotCombineExactAndApprox => true,
        }
    }

    pub fn is_sign_positive(&self) -> bool {
        match self {
            Fraction::Exact(f) => f.is_sign_positive(),
            Fraction::Approx(f) => f.is_sign_positive(),
            Fraction::CannotCombineExactAndApprox => false,
        }
    }
    /// Returns true if the number is negative and false if the number is zero or positive.
    pub fn is_negative(&self) -> bool {
        match self {
            Fraction::Exact(f) => f.is_negative(),
            Fraction::Approx(f) => *f != 0f64 && f.is_negative(),
            Fraction::CannotCombineExactAndApprox => false,
        }
    }

    /// Returns true if the number is positive and false if the number is zero or negative.
    pub fn is_positive(&self) -> bool {
        match self {
            Fraction::Exact(f) => !f.is_zero() && f.is_positive(),
            Fraction::Approx(f) => *f != 0f64 && f.is_positive(),
            Fraction::CannotCombineExactAndApprox => false,
        }
    }

    /// Returns true if the value is Infinity (does not matter positive or negative)
    pub fn is_infinite(&self) -> bool {
        match self {
            Fraction::Exact(f) => f.is_infinite(),
            Fraction::Approx(f) => f.is_infinite(),
            Fraction::CannotCombineExactAndApprox => false,
        }
    }

    pub fn is_nan(&self) -> bool {
        match self {
            Fraction::Exact(f) => f.is_nan(),
            Fraction::Approx(f) => f.is_nan(),
            Fraction::CannotCombineExactAndApprox => true,
        }
    }

    pub fn infinity() -> Self {
        if Self::create_exact() {
            Fraction::Exact(BigFraction::infinity())
        } else {
            Fraction::Approx(f64::INFINITY)
        }
    }

    pub fn neg_infinity() -> Self {
        if Self::create_exact() {
            Fraction::Exact(BigFraction::neg_infinity())
        } else {
            Fraction::Approx(f64::NEG_INFINITY)
        }
    }

    pub fn nan() -> Self {
        if Self::create_exact() {
            Fraction::Exact(BigFraction::nan())
        } else {
            Fraction::Approx(f64::NAN)
        }
    }

    pub fn abs(&self) -> Self {
        match self {
            Fraction::Exact(f) => Fraction::Exact(f.abs()),
            Fraction::Approx(f) => Fraction::Approx(f.abs()),
            Fraction::CannotCombineExactAndApprox => self.clone(),
        }
    }

    pub fn sign(&self) -> Option<Sign> {
        match self {
            Fraction::Exact(f) => f.sign(),
            Fraction::Approx(f) => {
                if f.is_nan() {
                    None
                } else if f.is_zero() {
                    Some(Sign::Plus)
                } else if f.is_sign_positive() {
                    Some(Sign::Plus)
                } else {
                    Some(Sign::Minus)
                }
            }
            Fraction::CannotCombineExactAndApprox => None,
        }
    }

    /**
     * 1/self
     */
    pub fn recip(&self) -> Self {
        match self {
            Fraction::Exact(f) => Fraction::Exact(f.recip()),
            Fraction::Approx(f) => Fraction::Approx(f.recip()),
            Fraction::CannotCombineExactAndApprox => self.clone(),
        }
    }

    /**
     * Return a random index from 0 (inclusive) to the length of the list (exclusive).
     * The likelihood of each index to be returned is proportional to the value of the fraction at that index.
     *
     * The fractions do not need to sum to 1.
     */
    pub fn choose_randomly(fractions: &Vec<Fraction>) -> Result<usize> {
        if fractions.is_empty() {
            return Err(anyhow!("cannot take an element of an empty list"));
        }

        //normalise the probabilities
        let mut probabilities: Vec<Fraction> = fractions.iter().cloned().collect();
        let sum = probabilities.iter().fold(Fraction::zero(), |x, y| &x + y);
        if sum == Fraction::CannotCombineExactAndApprox {
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
                    if let Fraction::Exact(e) = f {
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
            Fraction::try_from((rand_val, max_denom.clone())).unwrap()
        } else {
            Fraction::Approx(rng.gen_range(0.0..=1.0))
        };

        let mut cum_prob = Fraction::zero();
        for (index, value) in probabilities.iter().enumerate() {
            cum_prob += value;
            if rand_val < cum_prob {
                return Ok(index);
            }
        }
        Ok(probabilities.len() - 1)
    }

    pub fn zero() -> Self {
        if Self::create_exact() {
            Fraction::Exact(GenericFraction::Rational(Sign::Plus, Ratio::zero()))
        } else {
            Fraction::Approx(f64::zero())
        }
    }

    pub fn is_zero(&self) -> bool {
        match self {
            Fraction::Exact(f) => f.is_zero(),
            Fraction::Approx(f) => f.abs() - &f64::EPSILON < 0.0,
            Self::CannotCombineExactAndApprox => false,
        }
    }

    pub fn one_minus(self) -> Self {
        match self {
            Fraction::Exact(mut f) => {
                f = f.neg();
                f.add_assign(1.to_biguint().unwrap());
                Fraction::Exact(f)
            }
            Fraction::Approx(f) => Fraction::Approx(1.0 - f),
            Self::CannotCombineExactAndApprox => self,
        }
    }

    pub fn one() -> Self {
        if Self::create_exact() {
            Fraction::Exact(GenericFraction::Rational(Sign::Plus, Ratio::one()))
        } else {
            Fraction::Approx(1.0)
        }
    }

    pub fn is_one(&self) -> bool {
        match self {
            Fraction::Exact(f) => f.is_one(),
            Fraction::Approx(f) => (f - 1.0).abs() - &f64::EPSILON < 0.0,
            Self::CannotCombineExactAndApprox => false,
        }
    }

    pub fn two() -> Fraction {
        if Self::create_exact() {
            Fraction::Exact(GenericFraction::Rational(
                Sign::Plus,
                Ratio::new_raw(UInt::from(2u32), UInt::from(1u32)),
            ))
        } else {
            Fraction::Approx(2.0)
        }
    }

    pub fn denom(&self) -> Option<&BigUint> {
        match self {
            Fraction::Exact(fraction) => fraction.denom(), // Returns the denominator for BigFraction
            Fraction::Approx(_) => None,                   // No meaningful denominator for Approx
            Fraction::CannotCombineExactAndApprox => None, // No meaningful denominator here either
        }
    }
}

impl FromStr for Fraction {
    type Err = Error;

    fn from_str(s: &str) -> std::prelude::v1::Result<Self, Self::Err> {
        if Self::create_exact() {
            Ok(Fraction::Exact(BigFraction::from_str(s)?))
        } else {
            if let Ok(float) = f64::from_str(s) {
                Ok(Fraction::Approx(float))
            } else {
                let fraction = BigFraction::from_str(s)?;
                match fraction.to_f64() {
                    Some(f) => Ok(Fraction::Approx(f)),
                    None => Err(anyhow!("could not read fraction {} as float", s)),
                }
            }
        }
    }
}

impl TryFrom<&FractionNotParsedYet> for Fraction {
    type Error = Error;

    fn try_from(value: &FractionNotParsedYet) -> std::result::Result<Self, Self::Error> {
        Self::from_str(&value.s)
    }
}

impl FromEbiTraitObject for Fraction {
    fn from_trait_object(object: EbiInput) -> Result<Box<Self>> {
        match object {
            EbiInput::Fraction(e) => Ok(Box::new(e)),
            _ => Err(anyhow!(
                "cannot read {} {} as a fraction",
                object.get_type().get_article(),
                object.get_type()
            )),
        }
    }
}

impl From<&Fraction> for Fraction {
    fn from(value: &Fraction) -> Self {
        match value {
            Fraction::Exact(_) => value.clone(),
            Fraction::Approx(_) => value.clone(),
            Fraction::CannotCombineExactAndApprox => value.clone(),
        }
    }
}

impl From<Arc<Fraction>> for Fraction {
    fn from(value: Arc<Fraction>) -> Self {
        match value.as_ref() {
            Fraction::Exact(f) => Fraction::Exact(f.clone()),
            Fraction::Approx(f) => Fraction::Approx(f.clone()),
            Fraction::CannotCombineExactAndApprox => Fraction::CannotCombineExactAndApprox,
        }
    }
}

impl From<&Arc<Fraction>> for Fraction {
    fn from(value: &Arc<Fraction>) -> Self {
        match value.as_ref() {
            Fraction::Exact(f) => Fraction::Exact(f.clone()),
            Fraction::Approx(f) => Fraction::Approx(f.clone()),
            Fraction::CannotCombineExactAndApprox => Fraction::CannotCombineExactAndApprox,
        }
    }
}

impl From<FractionRaw> for Fraction {
    fn from(value: FractionRaw) -> Self {
        Fraction::Exact(GenericFraction::Rational(
            value.sign,
            Ratio::new(value.a, value.b),
        ))
    }
}

impl TryFrom<BigUint> for Fraction {
    type Error = Error;

    fn try_from(value: BigUint) -> std::prelude::v1::Result<Self, Self::Error> {
        if Self::create_exact() {
            Ok(Fraction::Exact(GenericFraction::Rational(
                Sign::Plus,
                Ratio::new(value, UInt::from(1u32)),
            )))
        } else {
            if value < u64::MAX.to_biguint().unwrap() {
                Ok(Fraction::Approx(value.to_f64().unwrap()))
            } else {
                Err(anyhow!("value too large for approximate arithmetic"))
            }
        }
    }
}

impl TryFrom<&BigUint> for Fraction {
    type Error = Error;

    fn try_from(value: &BigUint) -> std::prelude::v1::Result<Self, Self::Error> {
        if Self::create_exact() {
            Ok(Fraction::Exact(GenericFraction::Rational(
                Sign::Plus,
                Ratio::new(value.clone(), UInt::from(1u32)),
            )))
        } else {
            if value < &u64::MAX.to_biguint().unwrap() {
                Ok(Fraction::Approx(value.to_f64().unwrap()))
            } else {
                Err(anyhow!("value too large for approximate arithmetic"))
            }
        }
    }
}

impl TryFrom<(BigUint, BigUint)> for Fraction {
    type Error = Error;

    fn try_from(value: (BigUint, BigUint)) -> std::prelude::v1::Result<Self, Self::Error> {
        if Self::create_exact() {
            Ok(Fraction::Exact(GenericFraction::Rational(
                Sign::Plus,
                Ratio::new(value.0, value.1),
            )))
        } else {
            if let (Some(numer), Some(denom)) = (value.0.to_u64(), value.1.to_u64()) {
                Ok(Fraction::Approx(numer as f64 / denom as f64))
            } else {
                Err(anyhow!("numbers too large for approximate arithmetic"))
            }
        }
    }
}

impl Exportable for Fraction {
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

impl Infoable for Fraction {
    fn info(&self, f: &mut impl std::io::Write) -> Result<()> {
        match self {
            Fraction::Exact(v) => {v.info(f)?; writeln!(f, "")?;},
            Fraction::Approx(v) => writeln!(f, "Approximate value\t{}", v)?,
            Fraction::CannotCombineExactAndApprox => writeln!(f, "Fraction is a result of combining exact and approximate arithmethic and therefore has no value.")?,
        };

        Ok(write!(f, "")?)
    }
}

impl std::fmt::Display for Fraction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Fraction::Exact(fr) => std::fmt::Display::fmt(&fr, f),
            Fraction::Approx(fr) => std::fmt::Display::fmt(&fr, f),
            Fraction::CannotCombineExactAndApprox => {
                write!(f, "cannot combine exact and approximate arithmatic")
            }
        }
    }
}

impl From<BigInt> for Fraction {
    fn from(value: BigInt) -> Self {
        if value.is_negative() {
            // Handle negative BigInt values if necessary
            panic!("Cannot convert a negative BigInt to Fraction::Exact with BigUint denominator");
        }

        // Safely convert BigInt to BigUint
        let numerator = value
            .to_biguint()
            .expect("BigInt must be non-negative to convert to BigUint");

        Fraction::Exact(BigFraction::new(numerator, BigUint::from(1u8)))
    }
}

impl std::fmt::Debug for Fraction {
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

impl Add<&Fraction> for &Fraction {
    type Output = Fraction;

    fn add(self, rhs: &Fraction) -> Self::Output {
        match (self, rhs) {
            (Fraction::Exact(x), Fraction::Exact(y)) => Fraction::Exact(x.add(y)),
            (Fraction::Approx(x), Fraction::Approx(y)) => Fraction::Approx(x.add(y)),
            _ => Fraction::CannotCombineExactAndApprox,
        }
    }
}

impl<T> AddAssign<T> for Fraction
where
    T: Borrow<Fraction>,
{
    fn add_assign(&mut self, rhs: T) {
        let rhs = rhs.borrow();

        if self.matches(&rhs) {
            match (self, rhs) {
                (Fraction::Exact(x), Fraction::Exact(y)) => x.add_assign(y),
                (Fraction::Approx(x), Fraction::Approx(y)) => x.add_assign(y),
                _ => {}
            };
        } else {
            *self = Fraction::CannotCombineExactAndApprox
        }
    }
}

impl AddAssign<&Arc<Fraction>> for Fraction {
    fn add_assign(&mut self, rhs: &Arc<Fraction>) {
        let rhs = rhs.borrow();

        if self.matches(&rhs) {
            match (self, rhs) {
                (Fraction::Exact(x), Fraction::Exact(y)) => x.add_assign(y),
                (Fraction::Approx(x), Fraction::Approx(y)) => x.add_assign(y),
                _ => {}
            };
        } else {
            *self = Fraction::CannotCombineExactAndApprox
        }
    }
}

impl Sub<&Fraction> for &Fraction {
    type Output = Fraction;

    fn sub(self, rhs: &Fraction) -> Self::Output {
        match (self, rhs) {
            (Fraction::Exact(x), Fraction::Exact(y)) => Fraction::Exact(x.sub(y)),
            (Fraction::Approx(x), Fraction::Approx(y)) => Fraction::Approx(x.sub(y)),
            _ => Fraction::CannotCombineExactAndApprox,
        }
    }
}

impl Sub for Fraction {
    type Output = Fraction;

    fn sub(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Fraction::Exact(x), Fraction::Exact(y)) => Fraction::Exact(x - y),
            (Fraction::Approx(x), Fraction::Approx(y)) => Fraction::Approx(x - y),
            _ => Fraction::CannotCombineExactAndApprox,
        }
    }
}

impl<T> SubAssign<T> for Fraction
where
    T: Borrow<Fraction>,
{
    fn sub_assign(&mut self, rhs: T) {
        let rhs = rhs.borrow();
        if self.matches(&rhs) {
            match (self, rhs) {
                (Fraction::Exact(x), Fraction::Exact(y)) => x.sub_assign(y),
                (Fraction::Approx(x), Fraction::Approx(y)) => x.sub_assign(y),
                _ => {}
            }
        } else {
            *self = Fraction::CannotCombineExactAndApprox;
        }
    }
}

impl Mul<&Fraction> for &Fraction {
    type Output = Fraction;

    fn mul(self, rhs: &Fraction) -> Self::Output {
        match (self, rhs) {
            (Fraction::Exact(x), Fraction::Exact(y)) => Fraction::Exact(x.mul(y)),
            (Fraction::Approx(x), Fraction::Approx(y)) => Fraction::Approx(x.mul(y)),
            _ => Fraction::CannotCombineExactAndApprox,
        }
    }
}

impl<T> MulAssign<T> for Fraction
where
    T: Borrow<Fraction>,
{
    fn mul_assign(&mut self, rhs: T) {
        let rhs = rhs.borrow();
        if self.matches(&rhs) {
            match (self, rhs) {
                (Fraction::Exact(x), Fraction::Exact(y)) => x.mul_assign(y),
                (Fraction::Approx(x), Fraction::Approx(y)) => x.mul_assign(y),
                _ => {}
            }
        } else {
            *self = Fraction::CannotCombineExactAndApprox
        }
    }
}

impl Div<&Fraction> for &Fraction {
    type Output = Fraction;

    fn div(self, rhs: &Fraction) -> Self::Output {
        match (self, rhs) {
            (Fraction::Exact(x), Fraction::Exact(y)) => Fraction::Exact(x.div(y)),
            (Fraction::Approx(x), Fraction::Approx(y)) => Fraction::Approx(x.div(y)),
            _ => Fraction::CannotCombineExactAndApprox,
        }
    }
}

impl Div for Fraction {
    type Output = Fraction;

    fn div(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Fraction::Exact(x), Fraction::Exact(y)) => Fraction::Exact(x.div(y)),
            (Fraction::Approx(x), Fraction::Approx(y)) => Fraction::Approx(x.div(y)),
            _ => Fraction::CannotCombineExactAndApprox,
        }
    }
}

impl<T> DivAssign<T> for Fraction
where
    T: Borrow<Fraction>,
{
    fn div_assign(&mut self, rhs: T) {
        let rhs = rhs.borrow();
        if self.matches(&rhs) {
            match (self, rhs) {
                (Fraction::Exact(x), Fraction::Exact(y)) => x.div_assign(y),
                (Fraction::Approx(x), Fraction::Approx(y)) => x.div_assign(y),
                _ => {}
            }
        } else {
            *self = Fraction::CannotCombineExactAndApprox
        }
    }
}

impl Neg for Fraction {
    type Output = Fraction;

    fn neg(self) -> Self::Output {
        match self {
            Fraction::Exact(f) => Fraction::Exact(f.neg()),
            Fraction::Approx(f) => Fraction::Approx(f.neg()),
            Self::CannotCombineExactAndApprox => self.clone(),
        }
    }
}

impl<'a> Neg for &'a Fraction {
    type Output = Fraction;

    fn neg(self) -> Self::Output {
        match self {
            Fraction::Exact(f) => Fraction::Exact(f.neg()),
            Fraction::Approx(f) => Fraction::Approx(f.neg()),
            Fraction::CannotCombineExactAndApprox => self.clone(),
        }
    }
}

impl PartialEq for Fraction {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Exact(l0), Self::Exact(r0)) => l0 == r0,
            (Self::Approx(l0), Self::Approx(r0)) => {
                l0 - f64::EPSILON <= *r0 && *r0 <= l0 + f64::EPSILON
            }
            _ => false,
        }
    }
}

impl Eq for Fraction {}

impl PartialOrd for Fraction {
    /**
     * Note that exact and approximate should not be compared.
     */
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        if !self.matches(other) {
            panic!("cannot compare exact and inexact arithmethic");
        }
        match (self, other) {
            (Fraction::Exact(x), Fraction::Exact(y)) => x.partial_cmp(y),
            (Fraction::Approx(x), Fraction::Approx(y)) => x.partial_cmp(y),
            _ => None,
        }
    }
}

impl Ord for Fraction {
    /**
     * Note that exact and approximate should not be compared.
     */
    fn cmp(&self, other: &Self) -> Ordering {
        if !self.matches(other) {
            panic!("cannot compare exact and inexact arithmethic");
        }
        match (self, other) {
            (Fraction::Exact(x), Fraction::Exact(y)) => x.cmp(y),
            (Fraction::Approx(x), Fraction::Approx(y)) => {
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
            (Fraction::Exact(_), Fraction::Approx(_)) => Ordering::Greater,
            (Fraction::Exact(_), Fraction::CannotCombineExactAndApprox) => Ordering::Greater,
            (Fraction::Approx(_), Fraction::Exact(_)) => Ordering::Less,
            (Fraction::Approx(_), Fraction::CannotCombineExactAndApprox) => Ordering::Greater,
            (Fraction::CannotCombineExactAndApprox, Fraction::Exact(_)) => Ordering::Less,
            (Fraction::CannotCombineExactAndApprox, Fraction::Approx(_)) => Ordering::Less,
            (Fraction::CannotCombineExactAndApprox, Fraction::CannotCombineExactAndApprox) => {
                Ordering::Less
            }
        }
    }
}

impl Hash for Fraction {
    /**
     * For good reasons, Rust does not support hashing of doubles. However, we need it to store distributions in a hashmap.
     * Approximate arithmetic is discouraged
     */
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        match self {
            Fraction::Exact(f) => f.hash(state),
            Fraction::Approx(f) => unsafe { std::mem::transmute::<f64, u64>(*f).hash(state) },
            Self::CannotCombineExactAndApprox => "cceaa".hash(state),
        }
    }
}

impl Sum for Fraction {
    fn sum<I: Iterator<Item = Self>>(iter: I) -> Self {
        iter.fold(Self::zero(), |sum, f| &sum + &f)
    }
}

impl<'a> Sum<&'a Fraction> for Fraction {
    fn sum<I: Iterator<Item = &'a Fraction>>(iter: I) -> Self {
        iter.fold(Fraction::zero(), |sum, f| &sum + f)
    }
}

impl ToBigUint for Fraction {
    fn to_biguint(&self) -> Option<BigUint> {
        match self {
            Fraction::Exact(fraction) => {
                if fraction.denom() == Some(&BigUint::from(1u32)) {
                    fraction.numer().cloned() // Return the numerator as `BigUint`
                } else {
                    None // Not an integer if denominator != 1
                }
            }
            Fraction::Approx(_) | Fraction::CannotCombineExactAndApprox => None,
        }
    }
}

impl ToBigInt for Fraction {
    fn to_bigint(&self) -> Option<BigInt> {
        match self {
            Fraction::Exact(fraction) => {
                if fraction.denom() == Some(&BigUint::from(1u32)) {
                    fraction.numer().map(|n| n.clone().into()) // Convert numerator to BigInt
                } else {
                    None // Not an integer if denominator != 1
                }
            }
            Fraction::Approx(_) | Fraction::CannotCombineExactAndApprox => None,
        }
    }
}

impl Fraction {
    pub fn to_i64(&self) -> Option<i64> {
        match self {
            Fraction::Exact(fraction) => {
                if fraction.denom() == Some(&BigUint::from(1u32)) {
                    fraction
                        .numer()
                        .map(|n| n.clone().to_i64().expect("Numerator to big for conversion"))
                } else {
                    None // Not an integer if denominator != 1
                }
            }
            Fraction::Approx(_) | Fraction::CannotCombineExactAndApprox => None,
        }
    }
}

impl Fraction {
    pub fn to_i128(&self) -> Option<i128> {
        match self {
            Fraction::Exact(fraction) => {
                if fraction.denom() == Some(&BigUint::from(1u32)) {
                    fraction.numer().map(|n| {
                        n.clone()
                            .to_i128()
                            .expect("Numerator to big for conversion")
                    })
                } else {
                    None // Not an integer if denominator != 1
                }
            }
            Fraction::Approx(_) | Fraction::CannotCombineExactAndApprox => None,
        }
    }
}

impl Fraction {
    pub fn to_f64(&self) -> Option<f64> {
        match self {
            Fraction::Approx(value) => Some(value.clone()),
            _ => None,
        }
    }
}

//======================== primitive types ========================//

macro_rules! from {
    ($t:ident) => {
        impl From<$t> for Fraction {
            fn from(value: $t) -> Self {
                if Self::create_exact() {
                    Fraction::Exact(GenericFraction::Rational(
                        Sign::Plus,
                        Ratio::new(value.to_biguint().unwrap(), UInt::from(1u32)),
                    ))
                } else {
                    Fraction::Approx(value as f64)
                }
            }
        }
    };
}

macro_rules! from_signed {
    ($t:ident) => {
        impl From<$t> for Fraction {
            fn from(value: $t) -> Self {
                if Self::create_exact() {
                    Fraction::Exact(GenericFraction::Rational(
                        if value.is_negative() {
                            Sign::Minus
                        } else {
                            Sign::Plus
                        },
                        Ratio::new(value.abs().to_biguint().unwrap(), UInt::from(1u32)),
                    ))
                } else {
                    Fraction::Approx(value as f64)
                }
            }
        }
    };
}

macro_rules! from_tuple_u_u {
    ($t:ident,$tt:ident) => {
        impl From<($t, $tt)> for Fraction {
            fn from(value: ($t, $tt)) -> Self {
                if Self::create_exact() {
                    Fraction::Exact(GenericFraction::Rational(
                        Sign::Plus,
                        Ratio::new(UInt::from(value.0), UInt::from(value.1)),
                    ))
                } else {
                    Fraction::Approx(value.0 as f64 / value.1 as f64)
                }
            }
        }
    };
}

macro_rules! from_tuple_u_i {
    ($t:ident,$tt:ident) => {
        impl From<($t, $tt)> for Fraction {
            fn from(value: ($t, $tt)) -> Self {
                if Self::create_exact() {
                    let s1 = if value.1.is_negative() {
                        Sign::Minus
                    } else {
                        Sign::Plus
                    };
                    Fraction::Exact(GenericFraction::Rational(
                        s1,
                        Ratio::new(UInt::from(value.0), UInt::from(value.1.abs() as u128)),
                    ))
                } else {
                    Fraction::Approx(value.0 as f64 / value.1 as f64)
                }
            }
        }
    };
}

macro_rules! from_tuple_i_u {
    ($t:ident,$tt:ident) => {
        impl From<($t, $tt)> for Fraction {
            fn from(value: ($t, $tt)) -> Self {
                if Self::create_exact() {
                    let s1 = if value.0.is_negative() {
                        Sign::Minus
                    } else {
                        Sign::Plus
                    };
                    Fraction::Exact(GenericFraction::Rational(
                        s1,
                        Ratio::new(UInt::from(value.0.abs() as u128), UInt::from(value.1)),
                    ))
                } else {
                    Fraction::Approx(value.0 as f64 / value.1 as f64)
                }
            }
        }
    };
}

macro_rules! from_tuple_i_i {
    ($t:ident,$tt:ident) => {
        impl From<($t, $tt)> for Fraction {
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
                    Fraction::Exact(GenericFraction::Rational(
                        s0 * s1,
                        Ratio::new(
                            UInt::from(value.0.abs() as u128),
                            UInt::from(value.1.abs() as u128),
                        ),
                    ))
                } else {
                    Fraction::Approx(value.0 as f64 / value.1 as f64)
                }
            }
        }
    };
}

macro_rules! add {
    ($t:ident) => {
        impl<'a> Add<$t> for &'a Fraction {
            type Output = Fraction;

            fn add(self, rhs: $t) -> Self::Output {
                let rhs = rhs.into();
                match (self, rhs) {
                    (Fraction::Exact(x), Fraction::Exact(y)) => Fraction::Exact(x.add(y)),
                    (Fraction::Approx(x), Fraction::Approx(y)) => Fraction::Approx(x.add(y)),
                    _ => Fraction::CannotCombineExactAndApprox,
                }
            }
        }
    };
}

macro_rules! add_assign {
    ($t:ident) => {
        impl AddAssign<$t> for Fraction {
            fn add_assign(&mut self, rhs: $t) {
                let rhs = rhs.into();
                if self.matches(&rhs) {
                    match (self, rhs) {
                        (Fraction::Exact(x), Fraction::Exact(y)) => x.add_assign(y),
                        (Fraction::Approx(x), Fraction::Approx(y)) => x.add_assign(y),
                        _ => {}
                    };
                } else {
                    *self = Fraction::CannotCombineExactAndApprox
                }
            }
        }
    };
}

macro_rules! sub {
    ($t:ident) => {
        impl<'a> Sub<$t> for &'a Fraction {
            type Output = Fraction;

            fn sub(self, rhs: $t) -> Self::Output {
                let rhs = rhs.into();
                match (self, rhs) {
                    (Fraction::Exact(x), Fraction::Exact(y)) => Fraction::Exact(x.sub(y)),
                    (Fraction::Approx(x), Fraction::Approx(y)) => Fraction::Approx(x.sub(y)),
                    _ => Fraction::CannotCombineExactAndApprox,
                }
            }
        }
    };
}

macro_rules! sub_assign {
    ($t:ident) => {
        impl SubAssign<$t> for Fraction {
            fn sub_assign(&mut self, rhs: $t) {
                let rhs = rhs.into();
                if self.matches(&rhs) {
                    match (self, rhs) {
                        (Fraction::Exact(x), Fraction::Exact(y)) => x.sub_assign(y),
                        (Fraction::Approx(x), Fraction::Approx(y)) => x.sub_assign(y),
                        _ => {}
                    };
                } else {
                    *self = Fraction::CannotCombineExactAndApprox
                }
            }
        }
    };
}

macro_rules! mul {
    ($t:ident) => {
        impl<'a> Mul<$t> for &'a Fraction {
            type Output = Fraction;

            fn mul(self, rhs: $t) -> Self::Output {
                let rhs = rhs.into();
                match (self, rhs) {
                    (Fraction::Exact(x), Fraction::Exact(y)) => Fraction::Exact(x.mul(y)),
                    (Fraction::Approx(x), Fraction::Approx(y)) => Fraction::Approx(x.mul(y)),
                    _ => Fraction::CannotCombineExactAndApprox,
                }
            }
        }
    };
}

macro_rules! mul_assign {
    ($t:ident) => {
        impl MulAssign<$t> for Fraction {
            fn mul_assign(&mut self, rhs: $t) {
                let rhs = rhs.into();
                if self.matches(&rhs) {
                    match (self, rhs) {
                        (Fraction::Exact(x), Fraction::Exact(y)) => x.mul_assign(y),
                        (Fraction::Approx(x), Fraction::Approx(y)) => x.mul_assign(y),
                        _ => {}
                    };
                } else {
                    *self = Fraction::CannotCombineExactAndApprox
                }
            }
        }
    };
}

macro_rules! div {
    ($t:ident) => {
        impl<'a> Div<$t> for &'a Fraction {
            type Output = Fraction;

            fn div(self, rhs: $t) -> Self::Output {
                let rhs = rhs.into();
                match (self, rhs) {
                    (Fraction::Exact(x), Fraction::Exact(y)) => Fraction::Exact(x.div(y)),
                    (Fraction::Approx(x), Fraction::Approx(y)) => Fraction::Approx(x.div(y)),
                    _ => Fraction::CannotCombineExactAndApprox,
                }
            }
        }
    };
}

macro_rules! div_assign {
    ($t:ident) => {
        impl DivAssign<$t> for Fraction {
            fn div_assign(&mut self, rhs: $t) {
                let rhs = rhs.into();
                if self.matches(&rhs) {
                    match (self, rhs) {
                        (Fraction::Exact(x), Fraction::Exact(y)) => x.div_assign(y),
                        (Fraction::Approx(x), Fraction::Approx(y)) => x.div_assign(y),
                        _ => {}
                    };
                } else {
                    *self = Fraction::CannotCombineExactAndApprox
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

//======================== cli parsing ========================//

#[derive(Clone)]
pub struct FractionNotParsedYet {
    s: String,
}

impl FromStr for FractionNotParsedYet {
    type Err = Error;

    fn from_str(s: &str) -> std::prelude::v1::Result<Self, Self::Err> {
        Ok(Self { s: s.to_string() })

        // if Self::create_exact() {
        //     Ok(Fraction::Exact(BigFraction::from_str(s)?))
        // } else {
        //     if let Ok(float) = f64::from_str(s) {
        //         Ok(Fraction::Approx(float))
        //     } else {
        //         let fraction = BigFraction::from_str(s)?;
        //         match fraction.to_f64() {
        //             Some(f) => Ok(Fraction::Approx(f)),
        //             None => Err(anyhow!("could not read fraction {} as float", s)),
        //         }
        //     }
        // }
    }
}
