use num::{BigInt, Float, One as NumOne, Signed as NumSigned, Zero as NumZero};

use super::fraction::EPSILON;

pub trait One: Sized {
    fn one() -> Self;

    fn set_one(&mut self) {
        *self = One::one();
    }

    fn is_one(&self) -> bool;
}

pub trait Zero: Sized {
    fn zero() -> Self;

    fn set_zero(&mut self) {
        *self = Zero::zero();
    }

    fn is_zero(&self) -> bool;
}

pub trait Signed: Sized {
    fn abs(&self) -> Self;

    /// Returns true if the number is positive and false if the number is zero or negative.
    fn is_positive(&self) -> bool;

    /// Returns true if the number is negative and false if the number is zero or positive.
    fn is_negative(&self) -> bool;
}


// ============ implementations ============

impl Zero for i64 {
    fn zero() -> Self {
        0
    }

    fn is_zero(&self) -> bool {
        self == &0i64
    }
}

impl One for i64 {
    fn one() -> Self {
        1
    }

    fn is_one(&self) -> bool {
        self == &1i64
    }
}

impl Signed for i64 {
    fn abs(&self) -> Self {
        NumSigned::abs(&self)
    }

    fn is_positive(&self) -> bool {
        NumSigned::is_positive(self)
    }

    fn is_negative(&self) -> bool {
        NumSigned::is_negative(self)
    }
}

impl Zero for i128 {
    fn zero() -> Self {
        0
    }

    fn is_zero(&self) -> bool {
        self == &0i128
    }
}

impl One for i128 {
    fn one() -> Self {
        1
    }

    fn is_one(&self) -> bool {
        self == &1i128
    }
}

impl Signed for i128 {
    fn abs(&self) -> Self {
        NumSigned::abs(&self)
    }

    fn is_positive(&self) -> bool {
        NumSigned::is_positive(self)
    }

    fn is_negative(&self) -> bool {
        NumSigned::is_negative(self)
    }
}

impl One for BigInt {
    fn one() -> Self {
        <BigInt as NumOne>::one()
    }

    fn is_one(&self) -> bool {
        <BigInt as NumOne>::is_one(&self)
    }
}

impl Zero for BigInt {
    fn zero() -> Self {
        <BigInt as NumZero>::zero()
    }

    fn is_zero(&self) -> bool {
        <BigInt as NumZero>::is_zero(&self)
    }
}

impl Signed for BigInt {
    fn abs(&self) -> Self {
        <BigInt as NumSigned>::abs(&self)
    }

    fn is_positive(&self) -> bool {
        <BigInt as NumSigned>::is_positive(&self)
    }

    fn is_negative(&self) -> bool {
        <BigInt as NumSigned>::is_negative(&self)
    }
}

impl One for f64 {
    fn one() -> Self {
        1.0
    }

    fn is_one(&self) -> bool {
        (self - 1.0).abs() - &EPSILON < 0.0
    }
}

impl Zero for f64 {
    fn zero() -> Self {
        0.0
    }

    fn is_zero(&self) -> bool {
        <f64 as Signed>::abs(&self) - &EPSILON < 0.0
    }
}

impl Signed for f64 {
    fn abs(&self) -> Self {
        <f64 as Float>::abs(*self)
    }

    fn is_positive(&self) -> bool {
        self > &EPSILON
    }

    fn is_negative(&self) -> bool {
        self < &-EPSILON
    }
}
