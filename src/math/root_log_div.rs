use std::fmt::Display;
use anyhow::Result;

use crate::ebi_framework::{ebi_output::EbiOutput, exportable::Exportable};

use super::{fraction::Fraction, log_div::LogDiv};

#[derive(PartialEq, Eq, Debug, Clone)]
pub struct RootLogDiv {
    one_minus: bool,
    log_div: LogDiv
}

impl RootLogDiv {
    pub fn sqrt(log_div: LogDiv) -> Self {
        Self {
            one_minus: false,
            log_div: log_div
        }
    }

    pub fn one_minus(mut self) -> Self {
        self.one_minus = !self.one_minus;
        self
    }

    pub fn approximate(&self) -> Result<Fraction> {
        let f = self.log_div.approximate()?;
        let mut r = f.sqrt_abs(10);
        if self.one_minus {
            r = r.one_minus();
        }
        Ok(r)
    }
}

impl Exportable for RootLogDiv {
    fn export_from_object(object: EbiOutput, f: &mut dyn std::io::Write) -> Result<()> {
        match object {
            EbiOutput::RootLogDiv(fr) => fr.export(f),
            _ => unreachable!()
        }
    }
    
    fn export(&self, f: &mut dyn std::io::Write) -> anyhow::Result<()> {
        if self.one_minus {
            write!(f, "1-")?;
        }
        writeln!(f, "√({})", self.log_div)?;

        match self.approximate() {
            Ok(a) => Ok(writeln!(f, "Approximately {:.4}", a)?),
            Err(_) => Ok(writeln!(f, "no approximation available")?),
        }
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