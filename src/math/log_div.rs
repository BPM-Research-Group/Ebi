#[cfg(all(not(feature = "withoutexactarithmetic"), not(feature = "withoutapproximatearithmetic")))]
pub type LogDiv = super::log_div_enum::LogDivEnum;

#[cfg(feature = "withoutexactarithmetic")]
pub type LogDiv = super::log_div_f64::LogDivF64;

#[cfg(feature = "withoutapproximatearithmetic")]
pub type LogDiv = super::log_div_exact::LogDivExact;