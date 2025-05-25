#[cfg(any(
    all(
        not(feature = "exactarithmetic"),
        not(feature = "approximatearithmetic")
    ),
    all(feature = "exactarithmetic", feature = "approximatearithmetic")
))]
pub type LogDiv = super::log_div_enum::LogDivEnum;

#[cfg(all(not(feature = "exactarithmetic"), feature = "approximatearithmetic"))]
pub type LogDiv = super::log_div_f64::LogDivF64;

#[cfg(all(feature = "exactarithmetic", not(feature = "approximatearithmetic")))]
pub type LogDiv = super::log_div_exact::LogDivExact;
