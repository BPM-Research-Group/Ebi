use ebi_arithmetic::Fraction;
use std::fmt::Display;

#[derive(Eq, Clone)]
pub struct ConstFraction(usize, usize);

impl ConstFraction {
    pub fn to_fraction(&self) -> Fraction {
        Fraction::from((self.0, self.1))
    }

    pub const fn of(numerator: usize, denominator: usize) -> ConstFraction {
        Self(numerator, denominator)
    }

    pub const fn one() -> ConstFraction {
        Self(1, 1)
    }

    pub const fn zero() -> ConstFraction {
        Self(0, 1)
    }
}

impl Display for ConstFraction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.1 == 1 {
            write!(f, "{}", self.0)
        } else {
            write!(f, "{}/{}", self.0, self.1)
        }
    }
}

impl PartialEq for ConstFraction {
    fn eq(&self, other: &Self) -> bool {
        self.to_fraction() == other.to_fraction()
    }
}

impl PartialEq<Fraction> for ConstFraction {
    fn eq(&self, other: &Fraction) -> bool {
        self.to_fraction().eq(other)
    }
}

impl PartialOrd<Fraction> for ConstFraction {
    fn partial_cmp(&self, other: &Fraction) -> Option<std::cmp::Ordering> {
        self.to_fraction().partial_cmp(other)
    }
}
