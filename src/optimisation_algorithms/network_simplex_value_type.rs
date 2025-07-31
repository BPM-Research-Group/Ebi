use ebi_arithmetic::{fraction::Fraction, fraction_f64::FractionF64};
use num::BigInt;

pub trait MulWithFloat {
    fn mul_with_float(self, rhs: &f64) -> Self;
}

impl MulWithFloat for f64 {
    fn mul_with_float(self, rhs: &f64) -> Self {
        self * rhs
    }
}

impl MulWithFloat for i64 {
    fn mul_with_float(self, _rhs: &f64) -> Self {
        // this should never occur. it is necessary to make network simplex work on both integers and floats
        panic!("Cannot multiply values of different types");
    }
}

impl MulWithFloat for i128 {
    fn mul_with_float(self, _rhs: &f64) -> Self {
        // this should never occur. it is necessary to make network simplex work on both integers and floats
        panic!("Cannot multiply values of different types");
    }
}

impl MulWithFloat for BigInt {
    fn mul_with_float(self, _rhs: &f64) -> Self {
        // this should never occur. it is necessary to make network simplex work on both integers and floats
        panic!("Cannot multiply values of different types");
    }
}

impl MulWithFloat for FractionF64 {
    fn mul_with_float(self, rhs: &f64) -> Self {
        self * *rhs
    }
}

pub trait ToBigInt {
    fn to_big_int(&self) -> BigInt;
}

impl ToBigInt for f64 {
    // this should never occur. it is necessary to make network simplex work on both integers and floats
    fn to_big_int(&self) -> BigInt {
        panic!("Cannot multiply values of different types");
    }
}

impl ToBigInt for i64 {
    fn to_big_int(&self) -> BigInt {
        BigInt::from(*self)
    }
}

impl ToBigInt for i128 {
    fn to_big_int(&self) -> BigInt {
        BigInt::from(*self)
    }
}

impl ToBigInt for BigInt {
    fn to_big_int(&self) -> BigInt {
        self.clone()
    }
}

impl ToBigInt for Fraction {
    fn to_big_int(&self) -> BigInt {
        panic!("Cannot multiply values of different types");
    }
}
