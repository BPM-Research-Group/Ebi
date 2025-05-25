use anyhow::Result;
use num_bigint::ToBigInt;
use num_traits::Pow;
use std::{
    fmt::Display,
    io::Write,
    ops::{Add, AddAssign, DivAssign, MulAssign, Neg, Sub, SubAssign},
};

use fraction::{BigFraction, BigUint, GenericFraction, Integer};

use crate::ebi_framework::{ebi_output::EbiOutput, exportable::Exportable, infoable::Infoable};

use super::{fraction::{MaybeExact, UInt}, fraction_f64::FractionF64, traits::{One, Zero}};

#[derive(Clone)]
pub struct LogDivF64(FractionF64);

impl LogDivF64 {
    pub fn log2_div(log_of: FractionF64, divide_by: u64) -> Self {
        if log_of.is_sign_negative() {
            return Self::nan(&log_of);
        }

        Self(FractionF64(log_of.0.log2() / divide_by as f64))
    }

    pub fn log2(log_of: FractionF64) -> Self {
        if log_of.is_sign_negative() {
            return Self::nan(&log_of);
        }

        Self(FractionF64(log_of.0.log2()))
    }

    pub fn approximate(&self) -> Result<FractionF64> {
        if self.is_zero() {
            return Ok(FractionF64::zero());
        } else if self.is_infinite() {
            return Ok(FractionF64::infinity());
        } else if self.is_nan() {
            return Ok(FractionF64::nan());
        }

        Ok(self.0)
    }

    pub fn n_log_n(n: &FractionF64) -> Self {
        if n.is_sign_negative() {
            return Self::nan(n);
        }
        if n.is_infinite() {
            return Self::infinity();
        }
        if n.is_nan() {
            return Self::nan(n);
        }

        Self(FractionF64(n.0 * n.0.log2()))
    }

    fn nan(f: &FractionF64) -> Self {
        Self::nan_b(f.is_exact())
    }

    fn nan_b(_exact: bool) -> Self {
        Self(FractionF64(f64::NAN))
    }

    pub fn is_nan(&self) -> bool {
        self.0.is_nan()
    }

    pub fn neg_infinity() -> Self {
        Self(FractionF64(f64::NEG_INFINITY))
    }

    pub fn infinity() -> Self {
        Self(FractionF64(f64::INFINITY))
    }

    pub fn is_infinite(&self) -> bool {
        self.0.is_infinite()
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
        false
    }
}

impl Zero for LogDivF64 {
    fn zero() -> Self {
        Self(0.0.into())
    }

    fn is_zero(&self) -> bool {
        self.0.is_zero()
    }
}

impl One for LogDivF64 {
    fn one() -> Self {
        Self(1.0.into())
    }

    fn is_one(&self) -> bool {
        self.0.is_one()
    }
}

impl PartialEq for LogDivF64 {
    fn eq(&self, other: &Self) -> bool {
        self.0 .0 - f64::EPSILON <= other.0 .0 && other.0 .0 <= self.0 .0 + f64::EPSILON
    }
}

impl Eq for LogDivF64 {}

impl From<FractionF64> for LogDivF64 {
    fn from(value: FractionF64) -> Self {
        Self(value)
    }
}

impl Add for LogDivF64 {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        Self(self.0.add(&rhs.0))
    }
}

impl Add<FractionF64> for LogDivF64 {
    type Output = LogDivF64;

    fn add(self, rhs: FractionF64) -> Self::Output {
        self + LogDivF64::from(rhs)
    }
}

impl AddAssign for LogDivF64 {
    fn add_assign(&mut self, rhs: Self) {
        self.0.add_assign(rhs.0)
    }
}

impl Sub for LogDivF64 {
    type Output = Self;

    fn sub(self, rhs: Self) -> Self::Output {
        Self(self.0.sub(&rhs.0))
    }
}

impl SubAssign for LogDivF64 {
    fn sub_assign(&mut self, rhs: Self) {
        self.0.sub_assign(rhs.0)
    }
}

impl MulAssign<FractionF64> for LogDivF64 {
    fn mul_assign(&mut self, rhs: FractionF64) {
        self.0 *= rhs
    }
}

impl MulAssign<&FractionF64> for LogDivF64 {
    fn mul_assign(&mut self, rhs: &FractionF64) {
        self.0 *= rhs
    }
}

impl MulAssign<usize> for LogDivF64 {
    fn mul_assign(&mut self, rhs: usize) {
        self.0 *= rhs
    }
}

impl MulAssign<u64> for LogDivF64 {
    fn mul_assign(&mut self, rhs: u64) {
        self.0 *= rhs
    }
}

impl DivAssign<usize> for LogDivF64 {
    fn div_assign(&mut self, rhs: usize) {
        self.mul_assign(FractionF64::from(rhs).recip())
    }
}

impl DivAssign<FractionF64> for LogDivF64 {
    fn div_assign(&mut self, rhs: FractionF64) {
        self.mul_assign(rhs.recip())
    }
}

impl DivAssign<&FractionF64> for LogDivF64 {
    fn div_assign(&mut self, rhs: &FractionF64) {
        self.mul_assign(rhs.recip())
    }
}

impl Exportable for LogDivF64 {
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

impl Infoable for LogDivF64 {
    fn info(&self, f: &mut impl Write) -> Result<()> {
        writeln!(f, "logdiv with value {}", self.0)?;
        Ok(write!(f, "")?)
    }
}

impl Display for LogDivF64 {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl std::fmt::Debug for LogDivF64 {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "logdiv approx {}", self.0)
    }
}

#[cfg(test)]
mod tests {
    use crate::math::{log_div_f64::LogDivF64, traits::Zero};


    #[test]
    fn zero_log_div() {
        let mut zero = LogDivF64::zero();
        zero /= 2;
        assert_eq!(zero, LogDivF64::zero());
    }
}