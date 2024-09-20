use fraction::BigFraction;
use anyhow::Result;

pub trait Infoable {
    fn info(&self, f: &mut impl std::io::Write) -> Result<()>;
}

impl Infoable for String {
    fn info(&self, f: &mut impl std::io::Write) -> Result<()> {
        Ok(writeln!(f, "Length\t{}", self.len())?)
    }
}

impl Infoable for BigFraction {
    fn info(&self, f: &mut impl std::io::Write) -> Result<()> {
        match self {
            fraction::GenericFraction::Rational(_, ratio) => write!(f, "{} bits / {} bits", ratio.numer().bits(), ratio.denom().bits())?,
            fraction::GenericFraction::Infinity(sign) => write!(f, "{} infinity", sign.to_string())?,
            fraction::GenericFraction::NaN => write!(f, "NaN")?,
        }
        Ok(write!(f, "")?)
    }
}