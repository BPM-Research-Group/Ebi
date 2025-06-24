use anyhow::{Error, Result};
use std::{fmt::Display, str::FromStr, sync::atomic::AtomicBool};

#[cfg(any(
    all(
        not(feature = "exactarithmetic"),
        not(feature = "approximatearithmetic")
    ),
    all(feature = "exactarithmetic", feature = "approximatearithmetic")
))]
pub type Fraction = super::fraction_enum::FractionEnum;

#[cfg(all(not(feature = "exactarithmetic"), feature = "approximatearithmetic"))]
pub type Fraction = super::fraction_f64::FractionF64;

#[cfg(all(feature = "exactarithmetic", not(feature = "approximatearithmetic")))]
pub type Fraction = super::fraction_exact::FractionExact;

//======================== fraction tools ========================//

#[cfg(any(
    all(
        not(feature = "exactarithmetic"),
        not(feature = "approximatearithmetic")
    ),
    all(feature = "exactarithmetic", feature = "approximatearithmetic")
))]
pub type FractionRandomCache = super::fraction_enum::FractionRandomCacheEnum;

#[cfg(all(not(feature = "exactarithmetic"), feature = "approximatearithmetic"))]
pub type FractionRandomCache = super::fraction_f64::FractionRandomCacheF64;

#[cfg(all(feature = "exactarithmetic", not(feature = "approximatearithmetic")))]
pub type FractionRandomCache = super::fraction_exact::FractionRandomCacheExact;

pub trait ChooseRandomly {
    /**
     * Return a random index from 0 (inclusive) to the length of the list (exclusive).
     * The likelihood of each index to be returned is proportional to the value of the fraction at that index.
     *
     * The fractions do not need to sum to 1.
     */
    fn choose_randomly(fractions: &Vec<Self>) -> Result<usize>
    where
        Self: Sized;

    fn choose_randomly_create_cache<'a>(
        fractions: impl Iterator<Item = &'a Self>,
    ) -> Result<FractionRandomCache>
    where
        Self: Sized,
        Self: 'a;

    fn choose_randomly_cached(cache: &FractionRandomCache) -> usize
    where
        Self: Sized;
}

pub trait MaybeExact {
    type Approximate;
    type Exact;

    fn is_exact(&self) -> bool;

    /**
     * This is a low-level function to extract an f64. Will only succeed if the fraction is approximate.
     */
    fn extract_approx(&self) -> Result<Self::Approximate>;

    /**
     * This is a low-level function to extract an exact value. Will only succeed if the fraction is exact.
     */
    fn extract_exact(&self) -> Result<&Self::Exact>;
}

//======================== exactness tools ========================//
pub type UInt = fraction::BigUint;
pub static EXACT: AtomicBool = AtomicBool::new(true);
pub const APPROX_DIGITS: u64 = 5;
pub const EPSILON: f64 = 1e-13;

/**
 * Enables or disables exact arithmetic for all future calls to Fraction.
 * Exact arithmetic cannot be combined with approximate arithmetic, and if this nevertheless occurs, the results will be set to FractionEnum::CannotCombineExactAndApprox
 */
pub fn set_exact_globally(exact: bool) {
    EXACT.store(exact, std::sync::atomic::Ordering::Relaxed);
}

pub fn is_exaxt_globally() -> bool {
    if cfg!(any(
        all(
            feature = "exactarithmetic",
            feature = "approximatearithmetic"
        ),
        all(
            not(feature = "exactarithmetic"),
            not(feature = "approximatearithmetic")
        )
    )) {
        EXACT.load(std::sync::atomic::Ordering::Relaxed)
    } else if cfg!(feature = "exactarithmetic") {
        true
    } else {
        false
    }
}

//======================== cli parsing ========================//

#[derive(Clone)]
pub struct FractionNotParsedYet {
    pub s: String,
}

impl FromStr for FractionNotParsedYet {
    type Err = Error;

    fn from_str(s: &str) -> std::prelude::v1::Result<Self, Self::Err> {
        Ok(Self { s: s.to_string() })
    }
}

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
