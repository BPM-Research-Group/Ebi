use ebi_arithmetic::{Fraction, OneMinus, Sqrt};
use std::fmt::Display;

use super::log_div::LogDiv;

#[derive(PartialEq, Eq, Debug, Clone)]
pub struct RootLogDiv {
    one_minus: bool,
    log_div: LogDiv,
}

impl RootLogDiv {
    pub fn sqrt(log_div: LogDiv) -> Self {
        Self {
            one_minus: false,
            log_div: log_div,
        }
    }

    pub fn one_minus(mut self) -> Self {
        self.one_minus = !self.one_minus;
        self
    }

    pub fn approximate(&self) -> Fraction {
        let f = self.log_div.approximate();
        let mut r = f.approx_abs_sqrt(6);
        if self.one_minus {
            r = r.one_minus();
        }
        r
    }

    pub fn export(&self, f: &mut dyn std::io::Write) -> anyhow::Result<()> {
        if self.one_minus {
            write!(f, "1-")?;
        }
        writeln!(f, "√({})", self.log_div)?;
        Ok(writeln!(f, "Approximately {:.4}", self.approximate())?)
    }
}

impl Display for RootLogDiv {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.one_minus {
            write!(f, "1-")?;
        }
        write!(f, "√({})", self.log_div)
    }
}
