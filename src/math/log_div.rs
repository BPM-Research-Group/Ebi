#[cfg(not(any(feature = "exact", feature = "approximate")))]
compile_error!("At least one of the features \"exact\" and \"approximate\" must be enabled.");

#[cfg(all(feature = "exact", feature = "approximate"))]
pub type LogDiv = super::log_div_enum::LogDivEnum;

#[cfg(all(not(feature = "exact"), feature = "approximate"))]
pub type LogDiv = super::log_div_f64::LogDivF64;