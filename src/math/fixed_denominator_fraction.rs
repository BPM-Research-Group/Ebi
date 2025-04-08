/**
 * A fixed denominator fraction is a fraction on which a limited set of operations is available.
 * In return, the denominator is fixed and operations are much cheaper.
 */

 #[cfg(all(not(feature = "withoutexactarithmetic"), not(feature = "withoutapproximatearithmetic")))]
pub type FixedDenominatorFraction = super::fixed_denominator_fraction_enum::FixedDenominatorFractionEnum;

#[cfg(feature = "withoutexactarithmetic")]
pub type FixedDenominatorFraction = super::fixed_denominator_fraction_f64::FixedDenominatorFractionF64;

#[cfg(feature = "withoutapproximatearithmetic")]
pub type FixedDenominatorFraction = super::fixed_denominator_fraction_exact::FixedDenominatorFractionExact;