use anyhow::Error;
use std::{str::FromStr, sync::atomic::AtomicBool};

#[cfg(not(any(feature = "exact", feature = "approximate")))]
compile_error!("At least one of the features \"exact\" and \"approximate\" must be enabled.");

#[cfg(all(feature = "exact", feature = "approximate"))]
pub type Fraction = super::fraction_enum::FractionEnum;

#[cfg(all(not(feature = "exact"), feature = "approximate"))]
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
    if cfg!(feature = "exact") {
        EXACT.load(std::sync::atomic::Ordering::Relaxed)
    } else {
        false
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