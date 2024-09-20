use std::{fmt::Display, mem, ops::{AddAssign, Neg}};

use anyhow::{anyhow, Error, Result};
use fraction::{BigFraction, BigUint, Integer, One, Sign, Zero};

use super::fraction::{Fraction, APPROX_DIGITS};

pub struct FractionRaw { //  a / b
    pub(crate) sign: Sign,
    pub(crate) a: BigUint,
    pub(crate) b: BigUint
}

impl FractionRaw {
    pub fn invert(&mut self) -> Result<()>{
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
        for digit in 0..(APPROX_DIGITS as f64 * 3.3).floor() as u32 { //use 3.3 bits for every digit in the outcome
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
            b: value.denom().unwrap().clone()
        })
    }
}

impl Display for FractionRaw {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:.4}", Fraction::Exact(BigFraction::new(self.a.clone(), self.b.clone())))
    }
}