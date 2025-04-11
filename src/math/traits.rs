use fraction::BigFraction;
use num::{BigInt, BigUint, Float, One as NumOne, Signed as NumSigned, Zero as NumZero};

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

impl Zero for BigUint {
    fn zero() -> Self {
        num::Zero::zero()
    }

    fn is_zero(&self) -> bool {
        num::Zero::is_zero(self)
    }
}

impl One for BigUint {
    fn one() -> Self {
        num::One::one()
    }

    fn is_one(&self) -> bool {
        num::One::is_one(self)
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

impl Zero for BigFraction {
    fn zero() -> Self {
        num::Zero::zero()
    }

    fn is_zero(&self) -> bool {
        num::Zero::is_zero(self)
    }
}

impl One for BigFraction {
    fn one() -> Self {
        num::One::one()
    }

    fn is_one(&self) -> bool {
        num::One::is_one(self)
    }
}

macro_rules! ttype_signed {
    ($t:ident) => {
        ttype!($t);
        impl Signed for $t {
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
    };
}

macro_rules! ttype {
    ($t:ident) => {
        impl Zero for $t {
            fn zero() -> Self {
                0
            }

            fn is_zero(&self) -> bool {
                num::Zero::is_zero(self)
            }
        }

        impl One for $t {
            fn one() -> Self {
                1
            }
        
            fn is_one(&self) -> bool {
                num::One::is_one(self)
            }
        }
    };
}

ttype!(usize);
ttype!(u128);
ttype!(u16);
ttype!(u32);
ttype!(u64);
ttype!(u8);
ttype_signed!(i128);
ttype_signed!(i16);
ttype_signed!(i32);
ttype_signed!(i64);
ttype_signed!(i8);