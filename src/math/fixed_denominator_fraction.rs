/**
 * A fixed denominator fraction is a fraction on which a limited set of operations is available.
 * In return, the denominator is fixed and operations are much cheaper.
 */

#[cfg(any(
    all(
        not(feature = "eexactarithmetic"),
        not(feature = "eapproximatearithmetic")
    ),
    all(feature = "eexactarithmetic", feature = "eapproximatearithmetic")
))]
pub type FixedDenominatorFraction =
    super::fixed_denominator_fraction_enum::FixedDenominatorFractionEnum;

#[cfg(all(not(feature = "eexactarithmetic"), feature = "eapproximatearithmetic"))]
pub type FixedDenominatorFraction =
    super::fixed_denominator_fraction_f64::FixedDenominatorFractionF64;

#[cfg(all(feature = "eexactarithmetic", not(feature = "eapproximatearithmetic")))]
pub type FixedDenominatorFraction =
    super::fixed_denominator_fraction_exact::FixedDenominatorFractionExact;
