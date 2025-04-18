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

#[cfg(test)]
mod tests {
    use std::str::FromStr;

    use fraction::BigFraction;

    use crate::ebi_framework::{ebi_input::EbiInput, infoable::Infoable};

    #[test]
    fn all_infoable() {
        for (input, _, _) in crate::tests::get_all_test_files() {
            if let EbiInput::Object(object, _) = input {
                let mut f = vec![];
                object.info(&mut f).unwrap();
            }
        }
    }

    #[test]
    fn default_infoable() {
        let x = "x".to_string();
        let mut f = vec![];
        x.info(&mut f).unwrap();

        BigFraction::infinity().info(&mut f).unwrap();
        BigFraction::nan().info(&mut f).unwrap();
        BigFraction::from_str("1/2").unwrap().info(&mut f).unwrap();
    }
}