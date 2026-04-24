use std::ops::{Mul, Neg};

#[derive(Eq, PartialEq, Copy, Clone, Debug)]
pub enum Sign {
    Plus,
    Minus,
}

impl Sign {
    pub fn is_negative(&self) -> bool {
        match self {
            Sign::Plus => false,
            Sign::Minus => true,
        }
    }

    pub fn is_positive(&self) -> bool {
        match self {
            Sign::Plus => true,
            Sign::Minus => false,
        }
    }
}

impl Neg for Sign {
    type Output = Self;

    fn neg(self) -> Self::Output {
        match self {
            Sign::Plus => Sign::Minus,
            Sign::Minus => Sign::Plus,
        }
    }
}

impl Mul for Sign {
    type Output = Self;

    fn mul(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Sign::Plus, Sign::Plus) => Sign::Plus,
            (Sign::Plus, Sign::Minus) => Sign::Minus,
            (Sign::Minus, Sign::Plus) => Sign::Minus,
            (Sign::Minus, Sign::Minus) => Sign::Plus,
        }
    }
}