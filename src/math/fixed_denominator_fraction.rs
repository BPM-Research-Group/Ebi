/**
 * A fixed denominator fraction is a fraction on which a limited set of operations is available.
 * In return, the denominator is fixed and operations are much cheaper.
 */

#[cfg(not(any(feature = "exact", feature = "approximate")))]
compile_error!("At least one of the features \"exact\" and \"approximate\" must be enabled.");

#[cfg(all(feature = "exact", feature = "approximate"))]
pub type FixedDenominatorFraction = super::fixed_denominator_fraction_enum::FixedDenominatorFractionEnum;

#[cfg(all(not(feature = "exact"), feature = "approximate"))]
pub type FixedDenominatorFraction = super::fixed_denominator_fraction_f64::FixedDenominatorFractionF64;