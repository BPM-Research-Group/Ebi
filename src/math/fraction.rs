use anyhow::{anyhow, Error, Result};
use fraction::{BigFraction, BigInt, BigUint, GenericFraction, One, Sign, Zero};
use num_bigint::{RandBigInt, ToBigInt, ToBigUint};
use num_rational::Ratio;
use num_traits::{Signed, ToPrimitive};
use rand::Rng;
use std::{
    borrow::Borrow,
    cmp::Ordering,
    hash::Hash,
    iter::Sum,
    ops::{Add, AddAssign, Div, DivAssign, Mul, MulAssign, Neg, Sub, SubAssign},
    str::FromStr,
    sync::{atomic::AtomicBool, Arc},
};

use crate::ebi_framework::{
    ebi_input::EbiInput, ebi_output::EbiOutput, ebi_trait::FromEbiTraitObject,
    exportable::Exportable, infoable::Infoable,
};

#[cfg(feature = "withoutexactarithmetic")]
pub type Fraction = super::fraction_f64::FractionF64;

//======================== exactness tools ========================//
pub type UInt = fraction::BigUint;
pub static EXACT: AtomicBool = AtomicBool::new(true);
pub const APPROX_DIGITS: u64 = 5;

/**
 * Enables or disables exact arithmetic for all future calls to Fraction.
 * Exact arithmetic cannot be combined with approximate arithmetic, and if this nevertheless occurs, the results will be set to FractionEnum::CannotCombineExactAndApprox
 */
pub fn set_exact_globally(exact: bool) {
    EXACT.store(exact, std::sync::atomic::Ordering::Relaxed);
}

pub fn is_exaxt_globally() -> bool {
    if !cfg!(feature = "withoutexactarithmetic") {
        EXACT.load(std::sync::atomic::Ordering::Relaxed)
    } else {
        false
    }

    pub fn denom(&self) -> Option<&BigUint> {
        match self {
            Fraction::Exact(fraction) => fraction.denom(), // Returns the denominator for BigFraction
            Fraction::Approx(_) => None,                   // No meaningful denominator for Approx
            Fraction::CannotCombineExactAndApprox => None, // No meaningful denominator here either
        }
    }
}

//======================== cli parsing ========================//

#[derive(Clone)]
pub struct FractionNotParsedYet {
    pub s: String
}

impl FromStr for FractionNotParsedYet {
    type Err = Error;

    fn from_str(s: &str) -> std::prelude::v1::Result<Self, Self::Err> {
        Ok(Self {
            s: s.to_string()
        })
    }
}
