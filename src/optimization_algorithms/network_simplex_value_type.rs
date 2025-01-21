use num::BigInt;

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
