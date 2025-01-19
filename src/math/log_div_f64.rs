use std::{fmt::Display, io::Write, ops::{Add, AddAssign, DivAssign, MulAssign, Sub, SubAssign}};

use fraction::{BigFraction, BigUint, GenericFraction, Zero};

use crate::ebi_framework::{ebi_output::EbiOutput, exportable::Exportable, infoable::Infoable};

use super::{fraction::UInt, fraction_f64::FractionF64};

pub struct LogDivF64(FractionF64);

impl LogDivF64 {

    /**
     * Returns whether the two given logdivs are either both exact or both approximate
     */
    pub(crate) fn matches(&self, rhs: &Self) -> bool {
        true
    }

     /**
     * Returns whether the two given logdivs are either both exact or both approximate
     */
    pub(crate) fn matches_f(&self, rhs: &FractionF64) -> bool {
        true
    }

    pub fn log2_div(log_of: FractionF64, divide_by: u64) -> Self {
        if log_of.is_sign_negative() {
            return Self::nan(&log_of);
        }
        
        Self(log_of.log2() / divide_by as f64)
    }

    pub fn log2(log_of: FractionF64) -> Self {
        if log_of.is_sign_negative() {
            return Self::nan(&log_of);
        }

        Self(log_of.log2())
    }

    pub fn approximate(&self) -> Result<FractionF64> {
        if self.is_zero() {
            return Ok(FractionF64::zero());
        } else if self.is_infinite() {
            return Ok(FractionF64::infinity());
        } else if self.is_nan() {
            return Ok(FractionF64::nan());
        }

        Ok(FractionF64(self.0))
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

        Self(n * n.log2())
    }

    fn nan(f: &FractionF64) -> Self {
        Self::nan_b(f.is_exact())
    }

    fn nan_b(exact: bool) -> Self {
        Self(f64::NAN)
    }

    pub fn is_nan(&self) -> bool {
        self.0.is_nan()
    }

    pub fn neg_infinity() -> Self {
        Self(f64::NEG_INFINITY)
    }

    pub fn infinity() -> Self {
        Self(f64::INFINITY)
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
            },
            GenericFraction::Infinity(x) => match x {
                fraction::Sign::Plus => BigFraction::infinity(),
                fraction::Sign::Minus => BigFraction::neg_infinity(),
            },
            GenericFraction::NaN => BigFraction::nan()
        }
    }

    pub(crate) fn is_exact(&self) -> bool {
        false
    }
}

impl Zero for LogDivF64 {
    fn zero() -> Self {
        Self(0.0)
    }

    fn is_zero(&self) -> bool {
        self.0.is_zero()
    }
}

impl PartialEq for LogDivF64 {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Exact(l0, l1), Self::Exact(r0, r1)) => {
                if self.is_zero() && other.is_zero() {
                    return true;
                }
                l0 == r0 && l1 == r1
            },
            (Self::Approx(l0), Self::Approx(r0)) => {
                l0 - f64::EPSILON <= *r0 && *r0 <= l0 + f64::EPSILON
            },
            _ => false,
        }
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
        Self(self.0.add(rhs.0))
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
        if !self.matches(&rhs) {
            *self = LogDivF64::CannotCombineExactAndApprox
        } else {
            match (self, rhs) {
                (LogDivF64::Exact(ab_log, c_denom), LogDivF64::Exact(rhs_ab_log, rhs_c_denom)) => {
                    *ab_log = Self::power_f_u(&ab_log, &rhs_c_denom) * Self::power_f_u(&rhs_ab_log, &c_denom);
                    *c_denom *= &rhs_c_denom;
                },
                (LogDivF64::Approx(f), LogDivF64::Approx(g)) => f.add_assign(g),
                _ => {}
            }
        }
    }
}

impl Sub for LogDivF64 {
    type Output = Self;

    fn sub(self, rhs: Self) -> Self::Output { 
        match (self, rhs) {
            (LogDivF64::Exact(ab_log, c_denom), LogDivF64::Exact(rhs_ab_log, rhs_c_denom)) => 
                Self::Exact(
                    Self::power_f_u(&ab_log, &rhs_c_denom) / Self::power_f_u(&rhs_ab_log, &c_denom), 
                    c_denom * rhs_c_denom),
            (LogDivF64::Approx(f), LogDivF64::Approx(g)) => Self::Approx(f.sub(g)),
            _ => LogDivF64::CannotCombineExactAndApprox
        }
    }
}

impl SubAssign for LogDivF64 {
    fn sub_assign(&mut self, rhs: Self) {
        if !self.matches(&rhs) {
            *self = LogDivF64::CannotCombineExactAndApprox
        } else {
            match (self, rhs) {
                (LogDivF64::Exact(ab_log, c_denom), LogDivF64::Exact(rhs_ab_log, rhs_c_denom)) => {
                    *ab_log = Self::power_f_u(&ab_log, &rhs_c_denom) / Self::power_f_u(&rhs_ab_log, &c_denom);
                    *c_denom *= &rhs_c_denom;
                },
                (LogDivF64::Approx(f), LogDivF64::Approx(g)) => f.sub_assign(g),
                _ => {}
            }
        }
    }
}

impl MulAssign<Fraction> for LogDivF64 {
    fn mul_assign(&mut self, rhs: Fraction) {
        if !self.matches_f(&rhs) {
            *self = LogDivF64::CannotCombineExactAndApprox
        } else {
            match (self, rhs) {
                (LogDivF64::Exact(ab_log, c), Fraction::Exact(f)) => {
                    *ab_log = Self::power_f_u(&ab_log, &f.numer().unwrap());
                    *c *= f.denom().unwrap()
                },
                (LogDivF64::Approx(f), Fraction::Approx(g)) => *f *= g,
                _ => {}
            }
        }
    }
}

impl MulAssign<&Fraction> for LogDivF64 {
    fn mul_assign(&mut self, rhs: &Fraction) {
        if !self.matches_f(rhs) {
            *self = Self::CannotCombineExactAndApprox
        }
        match (self, rhs) {
            (LogDivF64::Exact(ab_log, c), Fraction::Exact(f)) => {
                *ab_log = Self::power_f_u(&ab_log, &f.numer().unwrap());
                *c *= f.denom().unwrap()
            },
            (LogDivF64::Approx(f), Fraction::Approx(g)) => *f *= g,
            _ => {}
        }
    }
}

impl MulAssign<usize> for LogDivF64 {
    fn mul_assign(&mut self, rhs: usize) {
        match self {
            LogDivF64::Exact(ab_log, _) => *ab_log = Self::power_f_u(&ab_log, &rhs.to_biguint().unwrap()),
            LogDivF64::Approx(f) => *f *= rhs as f64,
            _ => {}
        }
    }
}

impl MulAssign<u64> for LogDivF64 {
    fn mul_assign(&mut self, rhs: u64) {
        match self {
            LogDivF64::Exact(ab_log, _) => *ab_log = Self::power_f_u(&ab_log, &rhs.to_biguint().unwrap()),
            LogDivF64::Approx(f) => *f *= rhs as f64,
            _ => {}
        }
    }
}

impl DivAssign<usize> for LogDivF64 {
    fn div_assign(&mut self, rhs: usize) {
        self.mul_assign(Fraction::from(rhs).recip())
    }
}

impl DivAssign<Fraction> for LogDivF64 {
    fn div_assign(&mut self, rhs: Fraction) {
        self.mul_assign(rhs.recip())
    }
}

impl DivAssign<&Fraction> for LogDivF64 {
    fn div_assign(&mut self, rhs: &Fraction) {
        self.mul_assign(rhs.recip())
    }
}

impl Exportable for LogDivF64 {
    fn export_from_object(object: EbiOutput, f: &mut dyn Write) -> Result<()> {
        match object {
            EbiOutput::LogDiv(fr) => fr.export(f),
            _ => unreachable!()
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
        match self {
            LogDivF64::Exact(ab_log, c_denom) => {
                write!(f, "log(")?;
                ab_log.info(f)?;
                writeln!(f, ") / {} bits", c_denom.bits())?;
            },
            LogDivF64::Approx(fr) => 
                writeln!(f, "logdiv with value {}", fr)?,
            LogDivF64::CannotCombineExactAndApprox => 
                writeln!(f, "cannot combine exact and approximate arithmetic")?
        };

        Ok(write!(f, "")?)
    }
}

impl Display for LogDivF64 {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LogDivF64::Exact(ab_log, c_denom) => 
                write!(f, "log({})/{}", ab_log, c_denom),
            LogDivF64::Approx(fr) => 
                write!(f, "{}", fr),
            LogDivF64::CannotCombineExactAndApprox => 
                write!(f, "cannot combine exact and approximate arithmetic")
        }
    }
}

impl std::fmt::Debug for LogDivF64 {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LogDivF64::Exact(ab_log, c_denom) => 
                write!(f, "logdiv of ({:?}/{}) bits", ab_log, c_denom.bits()),
            LogDivF64::Approx(fr) => 
                write!(f, "logdiv approx {}", fr),
            LogDivF64::CannotCombineExactAndApprox => 
                write!(f, "cannot combine exact and approximate arithmetic")
        }
        
    }
}