use std::{fmt::Display, io::Write, ops::{Add, AddAssign, DivAssign, MulAssign, Neg, Sub, SubAssign}};
use anyhow::Result;
use fraction::{BigFraction, BigUint, GenericFraction, Integer, One, Zero};
use num_bigint::{ToBigInt, ToBigUint};
use num_traits::Pow;



use crate::ebi_framework::{ebi_output::EbiOutput, exportable::Exportable, infoable::Infoable};

use super::{fraction::{Fraction, UInt}, fraction_raw::FractionRaw};

#[derive(Clone)]
pub enum LogDiv {
    Exact(BigFraction, UInt), //fraction cannot be negative, but zero and NaN are ok, UInt is the denominator
    Approx(f64),
    CannotCombineExactAndApprox
}

impl LogDiv {

    /**
     * Returns whether the two given logdivs are either both exact or both approximate
     */
    pub(crate) fn matches(&self, rhs: &Self) -> bool {
        match (self, rhs) {
            (Self::Exact(_, _), Self::Exact(_, _)) => true,
            (Self::Approx(_), Self::Approx(_)) => true,
            _ => false
        }
    }

     /**
     * Returns whether the two given logdivs are either both exact or both approximate
     */
    pub(crate) fn matches_f(&self, rhs: &Fraction) -> bool {
        match (self, rhs) {
            (Self::Exact(_, _), Fraction::Exact(_)) => true,
            (Self::Approx(_), Fraction::Approx(_)) => true,
            _ => false
        }
    }

    pub fn log2_div(log_of: Fraction, divide_by: u64) -> Self {
        if log_of.is_sign_negative() {
            return Self::nan(&log_of);
        }
        
        match log_of {
            Fraction::Exact(f) => Self::Exact(f, divide_by.into()),
            Fraction::Approx(f) => Self::Approx(f.log2() / divide_by as f64),
            Fraction::CannotCombineExactAndApprox => Self::CannotCombineExactAndApprox
        }
    }

    pub fn log2(log_of: Fraction) -> Self {
        if log_of.is_sign_negative() {
            return Self::nan(&log_of);
        }

        match log_of {
            Fraction::Exact(f) => Self::Exact(f, 1u32.into()),
            Fraction::Approx(f) => Self::Approx(f.log2()),
            Fraction::CannotCombineExactAndApprox => Self::CannotCombineExactAndApprox
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
            LogDiv::Exact(ab_log, c_denom) => {
                let raw: FractionRaw = ab_log.clone().try_into().unwrap();
                let mut approx = raw.approximate_log2().unwrap();
                approx /= Fraction::try_from(c_denom)?;
                Ok(approx)
            },
            LogDiv::Approx(f) => Ok(Fraction::Approx(*f)),
            LogDiv::CannotCombineExactAndApprox => Ok(Fraction::CannotCombineExactAndApprox),
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
            Fraction::Exact(f) => 
                Self::Exact(Self::power_f_u(f, f.numer().unwrap()), f.denom().unwrap().clone()),
            Fraction::Approx(f) => Self::Approx(f * f.log2()),
            Fraction::CannotCombineExactAndApprox => Self::CannotCombineExactAndApprox
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
            LogDiv::Exact(f, _) => f.is_nan(),
            LogDiv::Approx(f) => f.is_nan(),
            LogDiv::CannotCombineExactAndApprox => false
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
            LogDiv::Exact(f, _) => f.is_infinite(),
            LogDiv::Approx(f) => f.is_infinite(),
            LogDiv::CannotCombineExactAndApprox => false
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
            },
            GenericFraction::Infinity(x) => match x {
                fraction::Sign::Plus => BigFraction::infinity(),
                fraction::Sign::Minus => BigFraction::neg_infinity(),
            },
            GenericFraction::NaN => BigFraction::nan()
        }
    }

    pub(crate) fn is_exact(&self) -> bool {
        match self {
            LogDiv::Exact(_, _) => true,
            LogDiv::Approx(_) => false,
            LogDiv::CannotCombineExactAndApprox => false
        }
    }
}

impl Zero for LogDiv {
    fn zero() -> Self {
        if Fraction::create_exact() {
            Self::Exact(BigFraction::one(), UInt::from(1u32))
        } else {
            Self::Approx(0.0)
        }
    }

    fn is_zero(&self) -> bool {
        match self {
            LogDiv::Exact(f, _) => f.is_one(),
            LogDiv::Approx(f) => f.is_zero(),
            LogDiv::CannotCombineExactAndApprox => false
        }
    }
}

impl From<Fraction> for LogDiv {
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
                        },
                        GenericFraction::Infinity(_) => Self::infinity(),
                        GenericFraction::NaN => Self::nan_b(true),
                    }
                }
            },
            Fraction::Approx(f) => Self::Approx(f),
            Fraction::CannotCombineExactAndApprox => Self::CannotCombineExactAndApprox
        }
    }
}

impl Add for LogDiv {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (LogDiv::Exact(ab_log, c_denom), LogDiv::Exact(rhs_ab_log, rhs_c_denom)) => 
                Self::Exact(
                    Self::power_f_u(&ab_log, &rhs_c_denom) * Self::power_f_u(&rhs_ab_log, &c_denom), 
                    c_denom * rhs_c_denom),
            (LogDiv::Approx(f), LogDiv::Approx(g)) => Self::Approx(f.add(g)),
            _ => LogDiv::CannotCombineExactAndApprox
        }
    }
}

impl Add<Fraction> for LogDiv {
    type Output = LogDiv;

    fn add(self, rhs: Fraction) -> Self::Output {
        self + LogDiv::from(rhs)
    }
}

impl AddAssign for LogDiv {
    fn add_assign(&mut self, rhs: Self) {
        if !self.matches(&rhs) {
            *self = LogDiv::CannotCombineExactAndApprox
        } else {
            match (self, rhs) {
                (LogDiv::Exact(ab_log, c_denom), LogDiv::Exact(rhs_ab_log, rhs_c_denom)) => {
                    *ab_log = Self::power_f_u(&ab_log, &rhs_c_denom) * Self::power_f_u(&rhs_ab_log, &c_denom);
                    *c_denom *= &rhs_c_denom;
                },
                (LogDiv::Approx(f), LogDiv::Approx(g)) => f.add_assign(g),
                _ => {}
            }
        }
    }
}

impl Sub for LogDiv {
    type Output = Self;

    fn sub(self, rhs: Self) -> Self::Output { 
        match (self, rhs) {
            (LogDiv::Exact(ab_log, c_denom), LogDiv::Exact(rhs_ab_log, rhs_c_denom)) => 
                Self::Exact(
                    Self::power_f_u(&ab_log, &rhs_c_denom) / Self::power_f_u(&rhs_ab_log, &c_denom), 
                    c_denom * rhs_c_denom),
            (LogDiv::Approx(f), LogDiv::Approx(g)) => Self::Approx(f.sub(g)),
            _ => LogDiv::CannotCombineExactAndApprox
        }
    }
}

impl SubAssign for LogDiv {
    fn sub_assign(&mut self, rhs: Self) {
        if !self.matches(&rhs) {
            *self = LogDiv::CannotCombineExactAndApprox
        } else {
            match (self, rhs) {
                (LogDiv::Exact(ab_log, c_denom), LogDiv::Exact(rhs_ab_log, rhs_c_denom)) => {
                    *ab_log = Self::power_f_u(&ab_log, &rhs_c_denom) / Self::power_f_u(&rhs_ab_log, &c_denom);
                    *c_denom *= &rhs_c_denom;
                },
                (LogDiv::Approx(f), LogDiv::Approx(g)) => f.sub_assign(g),
                _ => {}
            }
        }
    }
}

impl MulAssign<Fraction> for LogDiv {
    fn mul_assign(&mut self, rhs: Fraction) {
        if !self.matches_f(&rhs) {
            *self = LogDiv::CannotCombineExactAndApprox
        } else {
            match (self, rhs) {
                (LogDiv::Exact(ab_log, c), Fraction::Exact(f)) => {
                    *ab_log = Self::power_f_u(&ab_log, &f.numer().unwrap());
                    *c *= f.denom().unwrap()
                },
                (LogDiv::Approx(f), Fraction::Approx(g)) => *f *= g,
                _ => {}
            }
        }
    }
}

impl MulAssign<&Fraction> for LogDiv {
    fn mul_assign(&mut self, rhs: &Fraction) {
        if !self.matches_f(rhs) {
            *self = Self::CannotCombineExactAndApprox
        }
        match (self, rhs) {
            (LogDiv::Exact(ab_log, c), Fraction::Exact(f)) => {
                *ab_log = Self::power_f_u(&ab_log, &f.numer().unwrap());
                *c *= f.denom().unwrap()
            },
            (LogDiv::Approx(f), Fraction::Approx(g)) => *f *= g,
            _ => {}
        }
    }
}

impl MulAssign<usize> for LogDiv {
    fn mul_assign(&mut self, rhs: usize) {
        match self {
            LogDiv::Exact(ab_log, _) => *ab_log = Self::power_f_u(&ab_log, &rhs.to_biguint().unwrap()),
            LogDiv::Approx(f) => *f *= rhs as f64,
            _ => {}
        }
    }
}

impl MulAssign<u64> for LogDiv {
    fn mul_assign(&mut self, rhs: u64) {
        match self {
            LogDiv::Exact(ab_log, _) => *ab_log = Self::power_f_u(&ab_log, &rhs.to_biguint().unwrap()),
            LogDiv::Approx(f) => *f *= rhs as f64,
            _ => {}
        }
    }
}

impl DivAssign<usize> for LogDiv {
    fn div_assign(&mut self, rhs: usize) {
        self.mul_assign(Fraction::from(rhs).recip())
    }
}

impl DivAssign<Fraction> for LogDiv {
    fn div_assign(&mut self, rhs: Fraction) {
        self.mul_assign(rhs.recip())
    }
}

impl DivAssign<&Fraction> for LogDiv {
    fn div_assign(&mut self, rhs: &Fraction) {
        self.mul_assign(rhs.recip())
    }
}

impl Exportable for LogDiv {
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

impl Infoable for LogDiv {
    fn info(&self, f: &mut impl Write) -> Result<()> {
        match self {
            LogDiv::Exact(ab_log, c_denom) => {
                write!(f, "log(")?;
                ab_log.info(f)?;
                writeln!(f, ") / {} bits", c_denom.bits())?;
            },
            LogDiv::Approx(fr) => 
                writeln!(f, "logdiv with value {}", fr)?,
            LogDiv::CannotCombineExactAndApprox => 
                writeln!(f, "cannot combine exact and approximate arithmetic")?
        };

        Ok(write!(f, "")?)
    }
}

impl Display for LogDiv {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LogDiv::Exact(ab_log, c_denom) => 
                write!(f, "log({})/{}", ab_log, c_denom),
            LogDiv::Approx(fr) => 
                write!(f, "{}", fr),
            LogDiv::CannotCombineExactAndApprox => 
                write!(f, "cannot combine exact and approximate arithmetic")
        }
    }
}

impl std::fmt::Debug for LogDiv {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LogDiv::Exact(ab_log, c_denom) => 
                write!(f, "logdiv of ({:?}/{}) bits", ab_log, c_denom.bits()),
            LogDiv::Approx(fr) => 
                write!(f, "logdiv approx {}", fr),
            LogDiv::CannotCombineExactAndApprox => 
                write!(f, "cannot combine exact and approximate arithmetic")
        }
        
    }
}