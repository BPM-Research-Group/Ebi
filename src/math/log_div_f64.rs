use anyhow::{Result, anyhow};
use ebi_arithmetic::{MaybeExact, One, Recip, Signed, Zero, fraction::fraction_f64::FractionF64};
use ebi_objects::Infoable;
use malachite::{Natural, base::num::arithmetic::traits::Pow, rational::Rational};
use std::{
    fmt::Display,
    io::Write,
    ops::{Add, AddAssign, DivAssign, MulAssign, Sub, SubAssign},
};


#[derive(Clone, PartialEq, Eq)]
pub struct LogDivF64(FractionF64);

impl LogDivF64 {
    pub fn log2_div(log_of: FractionF64, divide_by: u64) -> Result<Self> {
        if log_of.is_negative() {
            return Err(anyhow!("cannot take log of negative value"));
        }

        Ok(Self(FractionF64::from(
            log_of.approx().unwrap().log2() / divide_by as f64,
        )))
    }

    pub fn log2(log_of: FractionF64) -> Result<Self> {
        if log_of.is_negative() {
            return Err(anyhow!("cannot take log of negative value"));
        }

        Ok(Self(FractionF64::from(
            log_of.approx().unwrap().log2(),
        )))
    }

    pub fn approximate(&self) -> FractionF64 {
        if self.is_zero() {
            return FractionF64::zero();
        }

        self.0
    }

    pub fn n_log_n(n: &FractionF64) -> Result<Self> {
        if n.is_negative() {
            return Err(anyhow!("cannot take log of negative value"));
        }

        let n0 = n.approx().unwrap();
        Ok(Self(FractionF64::from(n0 * n0.log2())))
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

    pub(crate) fn is_exact(&self) -> bool {
        false
    }

    pub fn export(&self, f: &mut dyn Write) -> Result<()> {
        if self.is_exact() {
            writeln!(f, "{}", self)?;
        }
        Ok(writeln!(f, "Approximately {:.4}", self.approximate())?)
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

impl From<FractionF64> for LogDivF64 {
    fn from(value: FractionF64) -> Self {
        Self(value)
    }
}

impl Add for LogDivF64 {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        let f = rhs.0.approx().unwrap();
        Self(self.0.add(f))
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
        let f = rhs.0.approx().unwrap();
        Self(self.0.sub(f))
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
    use ebi_arithmetic::ebi_number::Zero;

    use crate::math::log_div_f64::LogDivF64;

    #[test]
    fn zero_log_div() {
        let mut zero = LogDivF64::zero();
        zero /= 2;
        assert_eq!(zero, LogDivF64::zero());
    }
}
