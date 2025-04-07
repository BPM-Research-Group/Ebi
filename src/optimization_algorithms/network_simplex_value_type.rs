use num::BigInt;

use crate::math::fraction::Fraction;

pub trait IsFloat {
    fn is_float(&self) -> bool;
}

impl IsFloat for f64 {
    fn is_float(&self) -> bool {
        true
    }
}

impl IsFloat for i64 {
    fn is_float(&self) -> bool {
        false
    }
}

impl IsFloat for i128 {
    fn is_float(&self) -> bool {
        false
    }
}

impl IsFloat for BigInt {
    fn is_float(&self) -> bool {
        false
    }
}

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
