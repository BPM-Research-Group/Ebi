use anyhow::{Error, Result};
use std::{str::FromStr, sync::atomic::AtomicBool};

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
    fn extract_exact(&self) -> Result<Self::Exact>;
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
    if cfg!(any(all(
        feature = "exactarithmetic",
        feature = "approximatearithmetic"
    ), all(not(feature = "exactarithmetic"), not(feature="approximatearithmetic")))) {
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
