use anyhow::{anyhow, Error, Result};
use fraction::{BigFraction, BigUint, GenericFraction, Integer, Sign};
use num::{One as NumOne, Zero as NumZero};
use num_bigint::{ToBigInt, ToBigUint};
use num_traits::Pow;
use std::{
    fmt::Display,
    io::Write,
    mem,
    ops::{Add, AddAssign, DivAssign, MulAssign, Neg, Sub, SubAssign},
};

use crate::ebi_framework::{ebi_output::EbiOutput, exportable::Exportable, infoable::Infoable};

use super::{
    fraction::{Fraction, UInt, APPROX_DIGITS},
    traits::{One, Zero},
};

#[derive(Clone)]
pub enum LogDivEnum {
    Exact(BigFraction, UInt), //fraction cannot be negative, but zero and NaN are ok, UInt is the denominator
    Approx(f64),
    CannotCombineExactAndApprox,
}

impl LogDivEnum {
    /**
     * Returns whether the two given logdivs are either both exact or both approximate
     */
    pub(crate) fn matches(&self, rhs: &Self) -> bool {
        match (self, rhs) {
            (Self::Exact(_, _), Self::Exact(_, _)) => true,
            (Self::Approx(_), Self::Approx(_)) => true,
            _ => false,
        }
    }

    /**
     * Returns whether the two given logdivs are either both exact or both approximate
     */
    pub(crate) fn matches_f(&self, rhs: &Fraction) -> bool {
        match (self, rhs.is_exact()) {
            (Self::Exact(_, _), true) => true,
            (Self::Approx(_), false) => true,
            _ => false,
        }
    }

    pub fn log2_div(log_of: Fraction, divide_by: u64) -> Self {
        if log_of.is_sign_negative() {
            return Self::nan(&log_of);
        }

        match log_of {
            Fraction::Exact(f) => Self::Exact(f, divide_by.into()),
            Fraction::Approx(f) => Self::Approx(f.log2() / divide_by as f64),
            Fraction::CannotCombineExactAndApprox => Self::CannotCombineExactAndApprox,
        }
    }

    pub fn log2(log_of: Fraction) -> Self {
        if log_of.is_sign_negative() {
            return Self::nan(&log_of);
        }

        match log_of {
            Fraction::Exact(f) => Self::Exact(f, 1u32.into()),
            Fraction::Approx(f) => Self::Approx(f.log2()),
            Fraction::CannotCombineExactAndApprox => Self::CannotCombineExactAndApprox,
        }
    }

    pub fn approximate(&self) -> Result<Fraction> {
        if self.is_zero() {
            return Ok(Fraction::zero());
        } else if self.is_infinite() {
            return Ok(Fraction::infinity());
        } else if self.is_nan() {
            return Ok(Fraction::nan());
        }

        match self {
            LogDivEnum::Exact(ab_log, c_denom) => {
                let raw: FractionRaw = ab_log.clone().try_into().unwrap();
                let mut approx = raw.approximate_log2().unwrap();
                approx /= Fraction::try_from(c_denom)?;
                Ok(approx)
            }
            LogDivEnum::Approx(f) => Ok(Fraction::Approx(*f)),
            LogDivEnum::CannotCombineExactAndApprox => Ok(Fraction::CannotCombineExactAndApprox),
        }
    }

    pub fn n_log_n(n: &Fraction) -> Self {
        if n.is_sign_negative() {
            return Self::nan(n);
        }
        if n.is_infinite() {
            return Self::infinity();
        }
        if n.is_nan() {
            return Self::nan(n);
        }

        match n {
            Fraction::Exact(f) => Self::Exact(
                Self::power_f_u(f, f.numer().unwrap()),
                f.denom().unwrap().clone(),
            ),
            Fraction::Approx(f) => Self::Approx(f * f.log2()),
            Fraction::CannotCombineExactAndApprox => Self::CannotCombineExactAndApprox,
        }
    }

    fn nan(f: &Fraction) -> Self {
        Self::nan_b(f.is_exact())
    }

    fn nan_b(exact: bool) -> Self {
        if exact {
            Self::Exact(BigFraction::nan(), 1.to_biguint().unwrap())
        } else {
            Self::Approx(f64::NAN)
        }
    }

    pub fn is_nan(&self) -> bool {
        match self {
            LogDivEnum::Exact(f, _) => f.is_nan(),
            LogDivEnum::Approx(f) => f.is_nan(),
            LogDivEnum::CannotCombineExactAndApprox => false,
        }
    }

    pub fn neg_infinity() -> Self {
        Self::Approx(f64::NEG_INFINITY)
    }

    pub fn infinity() -> Self {
        Self::Approx(f64::INFINITY)
    }

    pub fn is_infinite(&self) -> bool {
        match self {
            LogDivEnum::Exact(f, _) => f.is_infinite(),
            LogDivEnum::Approx(f) => f.is_infinite(),
            LogDivEnum::CannotCombineExactAndApprox => false,
        }
    }

    pub fn power_s_u(base: usize, power: &UInt) -> BigUint {
        base.to_bigint().unwrap().pow(power).to_biguint().unwrap()
    }

    /**
     * Internally uses i32 powers for approximate arithmetic
     */
    pub fn power_f_u(base: &BigFraction, power: &UInt) -> BigFraction {
        match base {
            GenericFraction::Rational(sign, ratio) => {
                let numer = ratio.numer().to_bigint().unwrap();
                let numer_pow = numer.pow(power).to_biguint().unwrap();

                let denom = ratio.denom().to_bigint().unwrap();
                let denom_pow = denom.pow(power).to_biguint().unwrap();

                let frac = BigFraction::new(numer_pow, denom_pow);
                if sign.is_positive() || (sign.is_negative() && power.is_even()) {
                    //result is positive
                    frac
                } else {
                    //result is negative
                    frac.neg()
                }
            }
            GenericFraction::Infinity(x) => match x {
                fraction::Sign::Plus => BigFraction::infinity(),
                fraction::Sign::Minus => BigFraction::neg_infinity(),
            },
            GenericFraction::NaN => BigFraction::nan(),
        }
    }

    pub(crate) fn is_exact(&self) -> bool {
        match self {
            LogDivEnum::Exact(_, _) => true,
            LogDivEnum::Approx(_) => false,
            LogDivEnum::CannotCombineExactAndApprox => false,
        }
    }
}

impl Zero for LogDivEnum {
    fn zero() -> Self {
        if Fraction::create_exact() {
            Self::Exact(BigFraction::one(), UInt::from(1u32))
        } else {
            Self::Approx(0.0)
        }
    }

    fn is_zero(&self) -> bool {
        match self {
            LogDivEnum::Exact(f, _) => f.is_one(),
            LogDivEnum::Approx(f) => <f64 as Zero>::is_zero(f),
            LogDivEnum::CannotCombineExactAndApprox => false,
        }
    }
}

impl PartialEq for LogDivEnum {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Exact(l0, l1), Self::Exact(r0, r1)) => {
                if self.is_zero() && other.is_zero() {
                    return true;
                }
                l0 == r0 && l1 == r1
            }
            (Self::Approx(l0), Self::Approx(r0)) => {
                l0 - f64::EPSILON <= *r0 && *r0 <= l0 + f64::EPSILON
            }
            _ => false,
        }
    }
}

impl Eq for LogDivEnum {}

impl From<Fraction> for LogDivEnum {
    fn from(value: Fraction) -> Self {
        match value {
            Fraction::Exact(f) => {
                if f.is_zero() {
                    Self::zero()
                } else if f.is_sign_negative() {
                    Self::nan_b(true)
                } else {
                    match f {
                        GenericFraction::Rational(_, f) => {
                            let g = BigFraction::new(Self::power_s_u(2, f.numer()), BigUint::one());
                            Self::Exact(g, f.denom().clone())
                        }
                        GenericFraction::Infinity(_) => Self::infinity(),
                        GenericFraction::NaN => Self::nan_b(true),
                    }
                }
            }
            Fraction::Approx(f) => Self::Approx(f),
            Fraction::CannotCombineExactAndApprox => Self::CannotCombineExactAndApprox,
        }
    }
}

impl Add for LogDivEnum {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (LogDivEnum::Exact(ab_log, c_denom), LogDivEnum::Exact(rhs_ab_log, rhs_c_denom)) => {
                Self::Exact(
                    Self::power_f_u(&ab_log, &rhs_c_denom) * Self::power_f_u(&rhs_ab_log, &c_denom),
                    c_denom * rhs_c_denom,
                )
            }
            (LogDivEnum::Approx(f), LogDivEnum::Approx(g)) => Self::Approx(f.add(g)),
            _ => LogDivEnum::CannotCombineExactAndApprox,
        }
    }
}

impl Add<Fraction> for LogDivEnum {
    type Output = LogDivEnum;

    fn add(self, rhs: Fraction) -> Self::Output {
        self + LogDivEnum::from(rhs)
    }
}

impl AddAssign for LogDivEnum {
    fn add_assign(&mut self, rhs: Self) {
        if !self.matches(&rhs) {
            *self = LogDivEnum::CannotCombineExactAndApprox
        } else {
            match (self, rhs) {
                (
                    LogDivEnum::Exact(ab_log, c_denom),
                    LogDivEnum::Exact(rhs_ab_log, rhs_c_denom),
                ) => {
                    *ab_log = Self::power_f_u(&ab_log, &rhs_c_denom)
                        * Self::power_f_u(&rhs_ab_log, &c_denom);
                    *c_denom *= &rhs_c_denom;
                }
                (LogDivEnum::Approx(f), LogDivEnum::Approx(g)) => f.add_assign(g),
                _ => {}
            }
        }
    }
}

impl Sub for LogDivEnum {
    type Output = Self;

    fn sub(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (LogDivEnum::Exact(ab_log, c_denom), LogDivEnum::Exact(rhs_ab_log, rhs_c_denom)) => {
                Self::Exact(
                    Self::power_f_u(&ab_log, &rhs_c_denom) / Self::power_f_u(&rhs_ab_log, &c_denom),
                    c_denom * rhs_c_denom,
                )
            }
            (LogDivEnum::Approx(f), LogDivEnum::Approx(g)) => Self::Approx(f.sub(g)),
            _ => LogDivEnum::CannotCombineExactAndApprox,
        }
    }
}

impl SubAssign for LogDivEnum {
    fn sub_assign(&mut self, rhs: Self) {
        if !self.matches(&rhs) {
            *self = LogDivEnum::CannotCombineExactAndApprox
        } else {
            match (self, rhs) {
                (
                    LogDivEnum::Exact(ab_log, c_denom),
                    LogDivEnum::Exact(rhs_ab_log, rhs_c_denom),
                ) => {
                    *ab_log = Self::power_f_u(&ab_log, &rhs_c_denom)
                        / Self::power_f_u(&rhs_ab_log, &c_denom);
                    *c_denom *= &rhs_c_denom;
                }
                (LogDivEnum::Approx(f), LogDivEnum::Approx(g)) => f.sub_assign(g),
                _ => {}
            }
        }
    }
}

impl MulAssign<Fraction> for LogDivEnum {
    fn mul_assign(&mut self, rhs: Fraction) {
        if !self.matches_f(&rhs) {
            *self = LogDivEnum::CannotCombineExactAndApprox
        } else {
            match (self, rhs) {
                (LogDivEnum::Exact(ab_log, c), Fraction::Exact(f)) => {
                    *ab_log = Self::power_f_u(&ab_log, &f.numer().unwrap());
                    *c *= f.denom().unwrap()
                }
                (LogDivEnum::Approx(f), Fraction::Approx(g)) => *f *= g,
                _ => {}
            }
        }
    }
}

impl MulAssign<&Fraction> for LogDivEnum {
    fn mul_assign(&mut self, rhs: &Fraction) {
        if !self.matches_f(rhs) {
            *self = Self::CannotCombineExactAndApprox
        }
        match (self, rhs) {
            (LogDivEnum::Exact(ab_log, c), Fraction::Exact(f)) => {
                *ab_log = Self::power_f_u(&ab_log, &f.numer().unwrap());
                *c *= f.denom().unwrap()
            }
            (LogDivEnum::Approx(f), Fraction::Approx(g)) => *f *= g,
            _ => {}
        }
    }
}

impl MulAssign<usize> for LogDivEnum {
    fn mul_assign(&mut self, rhs: usize) {
        match self {
            LogDivEnum::Exact(ab_log, _) => {
                *ab_log = Self::power_f_u(&ab_log, &rhs.to_biguint().unwrap())
            }
            LogDivEnum::Approx(f) => *f *= rhs as f64,
            _ => {}
        }
    }
}

impl MulAssign<u64> for LogDivEnum {
    fn mul_assign(&mut self, rhs: u64) {
        match self {
            LogDivEnum::Exact(ab_log, _) => {
                *ab_log = Self::power_f_u(&ab_log, &rhs.to_biguint().unwrap())
            }
            LogDivEnum::Approx(f) => *f *= rhs as f64,
            _ => {}
        }
    }
}

impl DivAssign<usize> for LogDivEnum {
    fn div_assign(&mut self, rhs: usize) {
        self.mul_assign(Fraction::from(rhs).recip())
    }
}

impl DivAssign<Fraction> for LogDivEnum {
    fn div_assign(&mut self, rhs: Fraction) {
        self.mul_assign(rhs.recip())
    }
}

impl DivAssign<&Fraction> for LogDivEnum {
    fn div_assign(&mut self, rhs: &Fraction) {
        self.mul_assign(rhs.recip())
    }
}

impl Exportable for LogDivEnum {
    fn export_from_object(object: EbiOutput, f: &mut dyn Write) -> Result<()> {
        match object {
            EbiOutput::LogDiv(fr) => fr.export(f),
            _ => unreachable!(),
        }
    }

    fn export(&self, f: &mut dyn Write) -> Result<()> {
        if self.is_exact() {
            writeln!(f, "{}", self)?;
        }
        let a = self.approximate();
        match a {
            Ok(a) => Ok(writeln!(f, "Approximately {:.4}", a)?),
            Err(_) => Ok(writeln!(f, "no approximation available")?),
        }
    }
}

impl Infoable for LogDivEnum {
    fn info(&self, f: &mut impl Write) -> Result<()> {
        match self {
            LogDivEnum::Exact(ab_log, c_denom) => {
                write!(f, "log(")?;
                ab_log.info(f)?;
                writeln!(f, ") / {} bits", c_denom.bits())?;
            }
            LogDivEnum::Approx(fr) => writeln!(f, "logdiv with value {}", fr)?,
            LogDivEnum::CannotCombineExactAndApprox => {
                writeln!(f, "cannot combine exact and approximate arithmetic")?
            }
        };

        Ok(write!(f, "")?)
    }
}

impl Display for LogDivEnum {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LogDivEnum::Exact(ab_log, c_denom) => write!(f, "log({})/{}", ab_log, c_denom),
            LogDivEnum::Approx(fr) => write!(f, "{}", fr),
            LogDivEnum::CannotCombineExactAndApprox => {
                write!(f, "cannot combine exact and approximate arithmetic")
            }
        }
    }
}

impl std::fmt::Debug for LogDivEnum {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LogDivEnum::Exact(ab_log, c_denom) => {
                write!(f, "logdiv of ({:?}/{}) bits", ab_log, c_denom.bits())
            }
            LogDivEnum::Approx(fr) => write!(f, "logdiv approx {}", fr),
            LogDivEnum::CannotCombineExactAndApprox => {
                write!(f, "cannot combine exact and approximate arithmetic")
            }
        }
    }
}

pub struct FractionRaw {
    //  a / b
    pub(crate) sign: Sign,
    pub(crate) a: BigUint,
    pub(crate) b: BigUint,
}

impl FractionRaw {
    pub fn invert(&mut self) -> Result<()> {
        if self.a.is_zero() {
            Err(anyhow!("divide by zero"))
        } else {
            mem::swap(&mut self.a, &mut self.b);
            Ok(())
        }
    }

    pub fn div_2(&mut self) {
        if self.a.is_even() {
            self.a >>= 1;
        } else {
            self.b <<= 1;
        }
    }

    pub fn is_less_than_eq_half(&self) -> bool {
        self.sign.is_negative() || &self.a << 1 <= self.b
    }

    pub fn is_greater_than_two(&self) -> bool {
        // log::info!("compare {}/{}, b << 1 = {}", self.a, self.b, &self.b << 1);
        self.sign.is_positive() && self.a > &self.b << 1
    }

    pub fn is_greater_than_one(&self) -> bool {
        self.sign.is_positive() && self.a > self.b
    }

    pub fn is_one(&self) -> bool {
        self.sign.is_positive() && self.a == self.b
    }

    pub fn approximate_log2(mut self) -> Result<Fraction> {
        // log::info!("approximate log2 of {}/{} bits", self.a.bits(), self.b.bits());

        if self.a.is_zero() || self.sign.is_negative() {
            return Err(anyhow!("cannot take logarithm of zero or negative number"));
        }

        if self.is_less_than_eq_half() {
            // log::debug!("invert fraction");
            self.invert()?;
            return Ok(-self.approximate_log2()?);
        }

        let mut result = 0;

        // log::debug!("approximate_log2 {} / {}", self.a, self.b);

        //bring self to <= 1
        while self.is_greater_than_one() {
            result += 1;
            self.div_2();
        }

        // log::debug!("result before comma {}", result);

        // log::debug!("left to compute log({:.5})", Fraction::new(self.a.clone(), self.b.clone()));

        //Now 0.5 < self < 1

        // let sqrt2 = Self::sqrt2();
        let mut matissa = Fraction::zero();

        //https://math.stackexchange.com/questions/1706939/approximation-log-2x
        self.invert()?;
        for digit in 0..(APPROX_DIGITS as f64 * 3.3).floor() as u32 {
            //use 3.3 bits for every digit in the outcome
            //avoid operations on too many bits by truncating: 4 times as many decimals as digits in the outcome should be more than enough.
            self.truncate((APPROX_DIGITS as f64 * 3.3 * 4.0).ceil() as u64);

            if self.is_one() {
                break;
            }
            self.square();
            if self.is_greater_than_two() {
                self.div_2();
                matissa -= &Fraction::one() / 2u32.pow(digit + 1);
                // log::debug!("report 1 -- next {}", self);
            } else {
                // log::debug!("report 0 -- next {}", self);
            }
        }

        // log::debug!("matissa is {:.4}", matissa);

        matissa += result;

        Ok(matissa)
    }

    pub fn zero() -> Self {
        Self {
            sign: Sign::Plus,
            a: BigUint::zero(),
            b: BigUint::one(),
        }
    }

    pub fn sqrt2() -> Self {
        Self {
            sign: Sign::Plus,
            a: BigUint::from(14142135623730950488u64),
            b: BigUint::from(10000000000000000000u64),
        }
    }

    pub fn square(&mut self) {
        // log::info!("square with {}/{} bits", self.a.bits(), self.b.bits());
        self.a *= self.a.clone();
        self.b *= self.b.clone();
    }

    pub fn truncate(&mut self, bits: u64) {
        let min_bits = self.a.bits().min(self.b.bits());
        if min_bits > bits {
            self.a >>= min_bits - bits;
            self.b >>= min_bits - bits;
        }
    }
}

impl Neg for FractionRaw {
    type Output = Self;

    fn neg(self) -> Self::Output {
        Self {
            sign: -self.sign,
            a: self.a,
            b: self.b,
        }
    }
}

impl AddAssign<u64> for FractionRaw {
    fn add_assign(&mut self, rhs: u64) {
        let x = &self.b * rhs;
        if self.sign.is_negative() {
            if self.a > x {
                self.a -= x;
            } else {
                self.a = x - &self.a;
                self.sign = Sign::Plus;
            }
        } else {
            self.a += x;
        }
    }
}

impl TryFrom<BigFraction> for FractionRaw {
    type Error = Error;

    fn try_from(value: BigFraction) -> std::prelude::v1::Result<Self, Self::Error> {
        if value.is_sign_negative() {
            return Err(anyhow!("negative number"));
        } else if value.is_nan() {
            return Err(anyhow!("nan"));
        } else if value.is_infinite() {
            return Err(anyhow!("infinite"));
        }

        Ok(Self {
            sign: value.sign().unwrap(),
            a: value.numer().unwrap().clone(),
            b: value.denom().unwrap().clone(),
        })
    }
}

impl Display for FractionRaw {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{:.4}",
            Fraction::Exact(BigFraction::new(self.a.clone(), self.b.clone()))
        )
    }
}

#[cfg(test)]
mod tests {
    use crate::math::{log_div_enum::LogDivEnum, traits::Zero};

    #[test]
    fn zero_log_div() {
        let mut zero = LogDivEnum::zero();
        zero /= 2;
        assert_eq!(zero, LogDivEnum::zero());
    }
}