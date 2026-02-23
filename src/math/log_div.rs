#[cfg(any(
    all(
        not(feature = "eexactarithmetic"),
        not(feature = "eapproximatearithmetic")
    ),
    all(feature = "eexactarithmetic", feature = "eapproximatearithmetic")
))]
pub type LogDiv = super::log_div_enum::LogDivEnum;

#[cfg(all(not(feature = "eexactarithmetic"), feature = "eapproximatearithmetic"))]
pub type LogDiv = super::log_div_f64::LogDivF64;

#[cfg(all(feature = "eexactarithmetic", not(feature = "eapproximatearithmetic")))]
pub type LogDiv = super::log_div_exact::LogDivExact;
