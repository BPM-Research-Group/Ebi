#[cfg(not(feature = "withoutexactarithmetic"))]
pub type LogDiv = super::log_div_enum::LogDivEnum;

#[cfg(feature = "withoutexactarithmetic")]
pub type LogDiv = super::log_div_f64::LogDivF64;