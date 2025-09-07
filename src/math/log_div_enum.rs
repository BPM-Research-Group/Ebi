use anyhow::{Error, Result, anyhow};
use ebi_arithmetic::{
    Recip, Signed,
    ebi_number::{One, Zero},
    exact::{MaybeExact, is_exact_globally},
    fraction::{fraction::APPROX_DIGITS, fraction_enum::FractionEnum},
};
use ebi_objects::Infoable;
use fraction::Sign;
use malachite::{
    Natural,
    base::num::{
        arithmetic::traits::{Parity, Pow},
        basic::traits::Two,
        logic::traits::{BitAccess, SignificantBits},
    },
    rational::Rational,
};
use std::{
    fmt::Display,
    io::Write,
    mem,
    ops::{Add, AddAssign, DivAssign, MulAssign, Neg, Sub, SubAssign},
};

#[derive(Clone)]
pub enum LogDivEnum {
    Exact((Rational, Natural)), //fraction cannot be negative, but zero is ok
    Approx(f64),
    CannotCombineExactAndApprox,
}

impl LogDivEnum {
    /**
     * Returns whether the two given logdivs are either both exact or both approximate
     */
    pub(crate) fn matches(&self, rhs: &Self) -> bool {
        match (self, rhs) {
            (Self::Exact(_), Self::Exact(_)) => true,
            (Self::Approx(_), Self::Approx(_)) => true,
            _ => false,
        }
    }

    /**
     * Returns whether the two given logdivs are either both exact or both approximate
     */
    pub(crate) fn matches_f(&self, rhs: &FractionEnum) -> bool {
        match (self, rhs.is_exact()) {
            (Self::Exact(_), true) => true,
            (Self::Approx(_), false) => true,
            _ => false,
        }
    }

    pub fn log2_div(log_of: FractionEnum, divide_by: u64) -> Result<Self> {
        if log_of.is_negative() {
            return Err(anyhow!("cannot take log of negative value"));
        }

        Ok(match log_of {
            FractionEnum::Exact(f) => Self::Exact((f, divide_by.into())),
            FractionEnum::Approx(f) => Self::Approx(f.log2() / divide_by as f64),
            FractionEnum::CannotCombineExactAndApprox => Self::CannotCombineExactAndApprox,
        })
    }

    pub fn log2(log_of: FractionEnum) -> Result<Self> {
        if log_of.is_negative() {
            return Err(anyhow!("cannot take log of negative value"));
        }

        Ok(match log_of {
            FractionEnum::Exact(f) => Self::Exact((f, 1u32.into())),
            FractionEnum::Approx(f) => Self::Approx(f.log2()),
            FractionEnum::CannotCombineExactAndApprox => Self::CannotCombineExactAndApprox,
        })
    }

    pub fn approximate(&self) -> FractionEnum {
        if self.is_zero() {
            return FractionEnum::zero();
        }

        match self {
            LogDivEnum::Exact((ab_log, c_denom)) => {
                let raw: FractionRaw = ab_log.clone().try_into().unwrap();
                let mut approx = raw.approximate_log2().unwrap();
                approx /= FractionEnum::Exact(Rational::from(c_denom));
                approx
            }
            LogDivEnum::Approx(f) => FractionEnum::Approx(*f),
            LogDivEnum::CannotCombineExactAndApprox => FractionEnum::CannotCombineExactAndApprox,
        }
    }

    pub fn n_log_n(n: &FractionEnum) -> Result<Self> {
        // log::debug!("n log n of {}", n);

        if n.is_negative() {
            return Err(anyhow!("cannot take log of negative value"));
        }

        Ok(match n {
            FractionEnum::Exact(f) => Self::Exact((
                Self::power_f_u(f, f.numerator_ref()),
                f.to_denominator().clone(),
            )),
            FractionEnum::Approx(f) => Self::Approx(f * f.log2()),
            FractionEnum::CannotCombineExactAndApprox => Self::CannotCombineExactAndApprox,
        })
    }

    pub fn power_s_u(base: usize, power: &Natural) -> Natural {
        let p: u64 = power
            .try_into()
            .expect("overflow of u64: this is beyond the capability of computers");
        Natural::from(base).pow(p)
    }

    pub fn power_2_u(power: &Natural) -> Natural {
        if let Ok(p) = power.try_into() {
            let mut result = Natural::zero();
            result.set_bit(p);
            result
        } else {
            let p: u64 = power
                .try_into()
                .expect("overflow of u64: this is beyond the capability of computers");
            Natural::TWO.pow(p)
        }
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

impl MaybeExact for LogDivEnum {
    type Approximate = f64;
    type Exact = (Rational, Natural);

    fn is_exact(&self) -> bool {
        match self {
            LogDivEnum::Exact(_) => true,
            LogDivEnum::Approx(_) => false,
            LogDivEnum::CannotCombineExactAndApprox => false,
        }
    }

    fn approx_ref(&self) -> Result<&Self::Approximate> {
        match self {
            LogDivEnum::Exact(_) => Err(anyhow!("cannot extract a float from fractions")),
            LogDivEnum::Approx(f) => Ok(f),
            LogDivEnum::CannotCombineExactAndApprox => {
                Err(anyhow!("cannot combine exact and approximate arithmetic"))
            }
        }
    }

    fn exact_ref(&self) -> Result<&<LogDivEnum as MaybeExact>::Exact> {
        match self {
            LogDivEnum::Exact(a) => Ok(a),
            LogDivEnum::Approx(_) => Err(anyhow!("cannot extract fractions from a float")),
            LogDivEnum::CannotCombineExactAndApprox => {
                Err(anyhow!("cannot combine exact and approximate arithmetic"))
            }
        }
    }

    fn approx(self) -> Result<Self::Approximate> {
        match self {
            LogDivEnum::Exact(_) => Err(anyhow!("cannot extract a float from fractions")),
            LogDivEnum::Approx(f) => Ok(f),
            LogDivEnum::CannotCombineExactAndApprox => {
                Err(anyhow!("cannot combine exact and approximate arithmetic"))
            }
        }
    }

    fn exact(self) -> Result<<LogDivEnum as MaybeExact>::Exact> {
        match self {
            LogDivEnum::Exact(a) => Ok(a),
            LogDivEnum::Approx(_) => Err(anyhow!("cannot extract fractions from a float")),
            LogDivEnum::CannotCombineExactAndApprox => {
                Err(anyhow!("cannot combine exact and approximate arithmetic"))
            }
        }
    }
}

impl Zero for LogDivEnum {
    fn zero() -> Self {
        if is_exact_globally() {
            Self::Exact((Rational::one(), Natural::one()))
        } else {
            Self::Approx(0.0)
        }
    }

    fn is_zero(&self) -> bool {
        match self {
            LogDivEnum::Exact((f, _)) => f.is_one(),
            LogDivEnum::Approx(f) => <f64 as Zero>::is_zero(f),
            LogDivEnum::CannotCombineExactAndApprox => false,
        }
    }
}

impl One for LogDivEnum {
    fn one() -> Self {
        if is_exact_globally() {
            Self::Exact((Rational::from(2), Natural::one()))
        } else {
            Self::Approx(1.0)
        }
    }

    fn is_one(&self) -> bool {
        /*
         * logdiv = 1 <=> c = log(a/b)
         */
        match self {
            LogDivEnum::Exact((f, c)) => {
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
            LogDivEnum::Approx(f) => One::is_one(f),
            _ => false,
        }
    }
}

impl PartialEq for LogDivEnum {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Exact((l0, l1)), Self::Exact((r0, r1))) => {
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

impl TryFrom<FractionEnum> for LogDivEnum {
    type Error = Error;

    fn try_from(value: FractionEnum) -> Result<Self> {
        match value {
            FractionEnum::Exact(f) => {
                if f.is_zero() {
                    Ok(Self::zero())
                } else if f.is_negative() {
                    Err(anyhow!("cannot take log of negative value"))
                } else {
                    let g = Rational::from(Self::power_2_u(f.numerator_ref()));
                    Ok(Self::Exact((g, f.to_denominator().clone())))
                }
            }
            FractionEnum::Approx(f) => Ok(Self::Approx(f)),
            FractionEnum::CannotCombineExactAndApprox => {
                Err(anyhow!("cannot combine exact and approximate arithmetic"))
            }
        }
    }
}

impl Add for LogDivEnum {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (
                LogDivEnum::Exact((ab_log, c_denom)),
                LogDivEnum::Exact((rhs_ab_log, rhs_c_denom)),
            ) => Self::Exact((
                Self::power_f_u(&ab_log, &rhs_c_denom) * Self::power_f_u(&rhs_ab_log, &c_denom),
                c_denom * rhs_c_denom,
            )),
            (LogDivEnum::Approx(f), LogDivEnum::Approx(g)) => Self::Approx(f.add(g)),
            _ => LogDivEnum::CannotCombineExactAndApprox,
        }
    }
}

impl Add<FractionEnum> for LogDivEnum {
    type Output = Result<LogDivEnum>;

    fn add(self, rhs: FractionEnum) -> Self::Output {
        Ok(self + LogDivEnum::try_from(rhs)?)
    }
}

impl AddAssign for LogDivEnum {
    fn add_assign(&mut self, rhs: Self) {
        if !self.matches(&rhs) {
            *self = LogDivEnum::CannotCombineExactAndApprox
        } else {
            match (self, rhs) {
                (
                    LogDivEnum::Exact((ab_log, c_denom)),
                    LogDivEnum::Exact((rhs_ab_log, rhs_c_denom)),
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
            (
                LogDivEnum::Exact((ab_log, c_denom)),
                LogDivEnum::Exact((rhs_ab_log, rhs_c_denom)),
            ) => Self::Exact((
                Self::power_f_u(&ab_log, &rhs_c_denom) / Self::power_f_u(&rhs_ab_log, &c_denom),
                c_denom * rhs_c_denom,
            )),
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
                    LogDivEnum::Exact((ab_log, c_denom)),
                    LogDivEnum::Exact((rhs_ab_log, rhs_c_denom)),
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

impl MulAssign<FractionEnum> for LogDivEnum {
    fn mul_assign(&mut self, rhs: FractionEnum) {
        if !self.matches_f(&rhs) {
            *self = LogDivEnum::CannotCombineExactAndApprox
        } else {
            match (self, rhs) {
                (LogDivEnum::Exact((ab_log, c)), FractionEnum::Exact(f)) => {
                    *ab_log = Self::power_f_u(&ab_log, f.numerator_ref());
                    *c *= f.denominator_ref();
                }
                (LogDivEnum::Approx(f), FractionEnum::Approx(g)) => *f *= g,
                _ => {}
            }
        }
    }
}

impl MulAssign<&FractionEnum> for LogDivEnum {
    fn mul_assign(&mut self, rhs: &FractionEnum) {
        if !self.matches_f(rhs) {
            *self = Self::CannotCombineExactAndApprox
        }
        match (self, rhs) {
            (LogDivEnum::Exact((ab_log, c)), FractionEnum::Exact(f)) => {
                *ab_log = Self::power_f_u(&ab_log, f.numerator_ref());
                *c *= f.denominator_ref();
            }
            (LogDivEnum::Approx(f), FractionEnum::Approx(g)) => *f *= g,
            _ => {}
        }
    }
}

impl MulAssign<usize> for LogDivEnum {
    fn mul_assign(&mut self, rhs: usize) {
        match self {
            LogDivEnum::Exact((ab_log, _)) => {
                *ab_log = Self::power_f_u(&ab_log, &Natural::from(rhs))
            }
            LogDivEnum::Approx(f) => *f *= rhs as f64,
            _ => {}
        }
    }
}

impl MulAssign<u64> for LogDivEnum {
    fn mul_assign(&mut self, rhs: u64) {
        match self {
            LogDivEnum::Exact((ab_log, _)) => {
                *ab_log = Self::power_f_u(&ab_log, &Natural::from(rhs))
            }
            LogDivEnum::Approx(f) => *f *= rhs as f64,
            _ => {}
        }
    }
}

impl DivAssign<usize> for LogDivEnum {
    fn div_assign(&mut self, rhs: usize) {
        self.mul_assign(FractionEnum::from(rhs).recip())
    }
}

impl DivAssign<FractionEnum> for LogDivEnum {
    fn div_assign(&mut self, rhs: FractionEnum) {
        self.mul_assign(rhs.recip())
    }
}

impl DivAssign<&FractionEnum> for LogDivEnum {
    fn div_assign(&mut self, rhs: &FractionEnum) {
        self.mul_assign(rhs.clone().recip())
    }
}

impl Infoable for LogDivEnum {
    fn info(&self, f: &mut impl Write) -> Result<()> {
        match self {
            LogDivEnum::Exact((ab_log, c_denom)) => {
                write!(f, "log(")?;
                ab_log.info(f)?;
                writeln!(f, ") / {} bits", c_denom.significant_bits())?;
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
            LogDivEnum::Exact((ab_log, c_denom)) => write!(f, "log({})/{}", ab_log, c_denom),
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
            LogDivEnum::Exact((ab_log, c_denom)) => {
                write!(
                    f,
                    "logdiv of ({:?}/{}) bits",
                    ab_log,
                    c_denom.significant_bits()
                )
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

    pub fn approximate_log2(mut self) -> Result<FractionEnum> {
        // log::info!("approximate log2 of {}/{} bits", self.a.bits(), self.b.bits());

        if self.a.is_zero() || self.sign == Sign::Minus {
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
        let mut matissa = FractionEnum::zero();

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
                matissa -= &FractionEnum::one() / 2u32.pow(digit + 1);
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
        if self.sign == Sign::Minus {
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
            sign: if value.is_negative() {
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
            FractionEnum::Exact(Rational::from(self.a.clone()) / Rational::from(self.b.clone()))
        )
    }
}

#[cfg(test)]
mod tests {
    use ebi_arithmetic::ebi_number::Zero;

    use crate::math::log_div_enum::LogDivEnum;

    #[test]
    fn zero_log_div() {
        let mut zero = LogDivEnum::zero();
        zero /= 2;
        assert_eq!(zero, LogDivEnum::zero());
    }
}
