use anyhow::{Error, Result, anyhow};
use ebi_arithmetic::{
    Fraction, Recip, Signed,
    ebi_number::{One, Zero},
    exact::MaybeExact,
    fraction::{fraction::APPROX_DIGITS, fraction_exact::FractionExact},
};
use ebi_objects::Infoable;
use fraction::Sign;
use malachite::{
    Natural,
    base::num::{
        arithmetic::traits::{Parity, Pow, Sign as MSign},
        basic::traits::Two,
        logic::traits::SignificantBits,
    },
    rational::Rational,
};
use std::{
    cmp::Ordering,
    fmt::Display,
    io::Write,
    mem,
    ops::{Add, AddAssign, DivAssign, MulAssign, Neg, Sub, SubAssign},
};

#[derive(Clone)]
pub struct LogDivExact((Rational, Natural));

impl LogDivExact {
    pub fn log2_div(log_of: Fraction, divide_by: u64) -> Result<Self> {
        if log_of.is_negative() {
            return Err(anyhow!("cannot take log of negative value"));
        }

        let f = log_of.exact().unwrap();
        Ok(LogDivExact((f, divide_by.into())))
    }

    pub fn log2(log_of: Fraction) -> Result<Self> {
        if log_of.is_negative() {
            return Err(anyhow!("cannot take log of negative value"));
        }

        let f = log_of.exact().unwrap();
        Ok(LogDivExact((f, 1u32.into())))
    }

    pub fn approximate(&self) -> Fraction {
        if self.is_zero() {
            return Fraction::zero();
        }

        match self {
            LogDivExact((ab_log, c_denom)) => {
                let raw: FractionRaw = ab_log.clone().try_into().unwrap();
                let mut approx = raw.approximate_log2().unwrap();
                approx /= FractionExact::from(c_denom.clone());
                approx
            }
        }
    }

    pub fn n_log_n(n: &Fraction) -> Result<Self> {
        if n.is_negative() {
            return Err(anyhow!("cannot take log of negative value"));
        }

        let f = n.exact_ref().unwrap();
        Ok(LogDivExact((
            Self::power_f_u(f, f.numerator_ref()),
            f.to_denominator(),
        )))
    }

    pub fn power_s_u(base: usize, power: &Natural) -> Natural {
        let p: u64 = power
            .try_into()
            .expect("overflow of u64: this is beyond the capability of computers");
        Natural::from(base).pow(p)
    }

    /**
     * Internally uses i32 powers for approximate arithmetic
     */
    pub fn power_f_u(base: &Rational, power: &Natural) -> Rational {
        // log::debug!("power_f_u of {} and {}", base, power);

        let p: u64 = power
            .try_into()
            .expect("overflow of u64: this is beyond the capability of computers");

        base.pow(p)
    }

    pub fn export(&self, f: &mut dyn Write) -> Result<()> {
        if self.is_exact() {
            writeln!(f, "{}", self)?;
        }
        Ok(writeln!(f, "Approximately {:.4}", self.approximate())?)
    }
}

impl MaybeExact for LogDivExact {
    type Approximate = f64;
    type Exact = (Rational, Natural);

    fn is_exact(&self) -> bool {
        true
    }

    fn approx_ref(&self) -> Result<&Self::Approximate> {
        Err(anyhow!("cannot extract a float from fractions"))
    }

    fn exact_ref(&self) -> Result<&Self::Exact> {
        Ok(&self.0)
    }

    fn approx(self) -> Result<Self::Approximate> {
        Err(anyhow!("cannot extract a float from fractions"))
    }

    fn exact(self) -> Result<Self::Exact> {
        Ok(self.0)
    }
}

impl Zero for LogDivExact {
    fn zero() -> Self {
        LogDivExact((Rational::one(), Natural::one()))
    }

    fn is_zero(&self) -> bool {
        match self {
            LogDivExact((f, _)) => f.is_one(),
        }
    }
}

impl One for LogDivExact {
    fn one() -> Self {
        Self((Rational::TWO, Natural::one()))
    }

    fn is_one(&self) -> bool {
        /*
         * logdiv = 1 <=> c = log(a/b)
         */
        match self {
            LogDivExact((f, c)) => {
                //if f.denom is not 1 (given that f is always reduced), then a/b is not an integer and thus the result is false

                if !f.denominator_ref().is_one() {
                    return false;
                }

                //extract a
                let a = f.numerator_ref();

                //left to check: 2^c = a
                match a.trailing_zeros() {
                    Some(zeroes) => {
                        //the number of trailing zeroes must be exactly c, i.e. all lower bits are 0
                        if &zeroes != c {
                            return false;
                        };

                        //the number of bits necessary (= the highest bit) must be zeroes + 1, i.e.a is not too high
                        return a.significant_bits() == c + Natural::one();
                    }
                    None => return false,
                }
            }
        }
    }
}

impl PartialEq for LogDivExact {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self((l0, l1)), Self((r0, r1))) => {
                if self.is_zero() && other.is_zero() {
                    return true;
                }
                l0 == r0 && l1 == r1
            }
        }
    }
}

impl Eq for LogDivExact {}

impl TryFrom<Fraction> for LogDivExact {
    type Error = Error;

    fn try_from(value: Fraction) -> Result<Self> {
        let f = value.exact()?;
        if f.is_zero() {
            Ok(Self::zero())
        } else if f.is_negative() {
            Err(anyhow!("cannot take log of negative value"))
        } else {
            let g = Rational::from(Self::power_s_u(2, f.numerator_ref()));
            Ok(Self((g, f.to_denominator())))
        }
    }
}

impl Add for LogDivExact {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (LogDivExact((ab_log, c_denom)), LogDivExact((rhs_ab_log, rhs_c_denom))) => Self((
                Self::power_f_u(&ab_log, &rhs_c_denom) * Self::power_f_u(&rhs_ab_log, &c_denom),
                c_denom * rhs_c_denom,
            )),
        }
    }
}

impl Add<Fraction> for LogDivExact {
    type Output = LogDivExact;

    fn add(self, rhs: Fraction) -> Self::Output {
        if !rhs.is_zero() {
            self + LogDivExact::try_from(rhs).unwrap()
        } else {
            self
        }
    }
}

impl AddAssign for LogDivExact {
    fn add_assign(&mut self, rhs: Self) {
        match (self, rhs) {
            (LogDivExact((ab_log, c_denom)), LogDivExact((rhs_ab_log, rhs_c_denom))) => {
                *ab_log =
                    Self::power_f_u(&ab_log, &rhs_c_denom) * Self::power_f_u(&rhs_ab_log, &c_denom);
                *c_denom *= &rhs_c_denom;
            }
        }
    }
}

impl Sub for LogDivExact {
    type Output = Self;

    fn sub(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (LogDivExact((ab_log, c_denom)), LogDivExact((rhs_ab_log, rhs_c_denom))) => Self((
                Self::power_f_u(&ab_log, &rhs_c_denom) / Self::power_f_u(&rhs_ab_log, &c_denom),
                c_denom * rhs_c_denom,
            )),
        }
    }
}

impl SubAssign for LogDivExact {
    fn sub_assign(&mut self, rhs: Self) {
        match (self, rhs) {
            (LogDivExact((ab_log, c_denom)), LogDivExact((rhs_ab_log, rhs_c_denom))) => {
                *ab_log =
                    Self::power_f_u(&ab_log, &rhs_c_denom) / Self::power_f_u(&rhs_ab_log, &c_denom);
                *c_denom *= &rhs_c_denom;
            }
        }
    }
}

impl MulAssign<FractionExact> for LogDivExact {
    fn mul_assign(&mut self, rhs: FractionExact) {
        let LogDivExact((ab_log, c)) = self;
        let f = rhs.exact_ref().unwrap();
        *ab_log = Self::power_f_u(&ab_log, &f.numerator_ref());
        *c *= f.denominator_ref();
    }
}

impl MulAssign<&FractionExact> for LogDivExact {
    fn mul_assign(&mut self, rhs: &FractionExact) {
        let LogDivExact((ab_log, c)) = self;
        let f = rhs.exact_ref().unwrap();
        *ab_log = Self::power_f_u(&ab_log, f.numerator_ref());
        *c *= f.denominator_ref();
    }
}

impl MulAssign<usize> for LogDivExact {
    fn mul_assign(&mut self, rhs: usize) {
        let LogDivExact((ab_log, _)) = self;
        *ab_log = Self::power_f_u(&ab_log, &Natural::from(rhs))
    }
}

impl MulAssign<u64> for LogDivExact {
    fn mul_assign(&mut self, rhs: u64) {
        let LogDivExact((ab_log, _)) = self;
        *ab_log = Self::power_f_u(&ab_log, &Natural::from(rhs))
    }
}

impl DivAssign<usize> for LogDivExact {
    fn div_assign(&mut self, rhs: usize) {
        self.mul_assign(Fraction::from(rhs).recip())
    }
}

impl DivAssign<Fraction> for LogDivExact {
    fn div_assign(&mut self, rhs: Fraction) {
        self.mul_assign(rhs.recip())
    }
}

impl DivAssign<&Fraction> for LogDivExact {
    fn div_assign(&mut self, rhs: &Fraction) {
        self.mul_assign(rhs.clone().recip())
    }
}

impl Infoable for LogDivExact {
    fn info(&self, f: &mut impl Write) -> Result<()> {
        match self {
            LogDivExact((ab_log, c_denom)) => {
                write!(f, "log(")?;
                ab_log.info(f)?;
                writeln!(f, ") / {} bits", c_denom.significant_bits())?;
            }
        };

        Ok(write!(f, "")?)
    }
}

impl Display for LogDivExact {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LogDivExact((ab_log, c_denom)) => write!(f, "log({})/{}", ab_log, c_denom),
        }
    }
}

impl std::fmt::Debug for LogDivExact {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LogDivExact((ab_log, c_denom)) => {
                write!(
                    f,
                    "logdiv of ({:?}/{}) bits",
                    ab_log,
                    c_denom.significant_bits()
                )
            }
        }
    }
}

pub struct FractionRaw {
    //  a / b
    pub(crate) sign: Sign,
    pub(crate) a: Natural,
    pub(crate) b: Natural,
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
        if self.a.even() {
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
            a: Natural::zero(),
            b: Natural::one(),
        }
    }

    pub fn square(&mut self) {
        // log::info!("square with {}/{} bits", self.a.bits(), self.b.bits());
        self.a *= self.a.clone();
        self.b *= self.b.clone();
    }

    pub fn truncate(&mut self, bits: u64) {
        let min_bits = self.a.significant_bits().min(self.b.significant_bits());
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
        let x = &self.b * Natural::from(rhs);
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

impl TryFrom<Rational> for FractionRaw {
    type Error = Error;

    fn try_from(value: Rational) -> std::prelude::v1::Result<Self, Self::Error> {
        if value.is_negative() {
            return Err(anyhow!("negative number"));
        }

        Ok(Self {
            sign: if value.sign() == Ordering::Less {
                Sign::Minus
            } else {
                Sign::Plus
            },
            a: value.to_numerator(),
            b: value.to_denominator(),
        })
    }
}

impl Display for FractionRaw {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{:.4}",
            FractionExact::from(Rational::from_naturals(self.a.clone(), self.b.clone()))
        )
    }
}

#[cfg(test)]
mod tests {
    use ebi_arithmetic::ebi_number::Zero;

    use crate::math::log_div_exact::LogDivExact;

    #[test]
    fn zero_log_div() {
        let mut zero = LogDivExact::zero();
        zero /= 2;
        assert_eq!(zero, LogDivExact::zero());
    }
}
