use anyhow::{Result, anyhow};
use ebi_arithmetic::{Fraction, OneMinus, Signed, Sqrt, Zero};
use ebi_objects::Infoable;
use fraction::Sign;
use std::{
    fmt::{Debug, Display},
    io::Write,
    ops::{Div, Mul, Neg},
};

#[derive(Clone)]
pub struct ContainsRoot {
    root: Root,
    one_minus: bool,
}

impl ContainsRoot {
    pub fn of(root: Root) -> Self {
        Self {
            root: root,
            one_minus: false,
        }
    }

    pub fn one_minus(root: Root) -> Self {
        Self {
            root: root,
            one_minus: true,
        }
    }

    pub fn approximate(&self) -> String {
        let mut x = self.root.r.clone().approx_abs_sqrt(6);
        if self.one_minus {
            x = x.one_minus();
        }
        if self.root.sign.is_positive() {
            format!("{:.4}", x)
        } else {
            format!("-{:.4}", x)
        }
    }

    pub fn export(&self, f: &mut dyn Write) -> Result<()> {
        writeln!(f, "{}", self)?;
        Ok(writeln!(f, "Approximately {:.4}", self.approximate())?)
    }
}

impl Infoable for ContainsRoot {
    fn info(&self, f: &mut impl Write) -> Result<()> {
        if self.one_minus {
            write!(f, "1-")?;
        }
        write!(f, "√")?;
        self.root.r.info(f)?;
        Ok(writeln!(f, "")?)
    }
}

impl Display for ContainsRoot {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.one_minus {
            write!(f, "1-")?;
        }
        self.root.fmt(f)
    }
}

impl Debug for ContainsRoot {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.one_minus {
            write!(f, "1-")?;
        }
        self.root.fmt(f)
    }
}

#[derive(Clone)]
pub struct Root {
    sign: Sign,
    r: Fraction,
}

impl Root {
    pub fn of(r: Fraction) -> Result<Self> {
        if !r.is_not_negative() {
            return Err(anyhow!("cannot take the root of a negative number"));
        }
        Ok(Self {
            sign: Sign::Plus,
            r: r,
        })
    }

    // pub fn approximate(&self) -> String {
    //     let x = self.r.sqrt_abs(6);
    //     if self.sign.is_positive() {
    //         format!("{:.4}", x)
    //     } else {
    //         format!("-{:.4}", x)
    //     }
    // }

    pub fn is_zero(&self) -> bool {
        self.r.is_zero()
    }
}

impl From<Fraction> for Root {
    fn from(value: Fraction) -> Self {
        Self {
            sign: if value.is_negative() {
                Sign::Minus
            } else {
                Sign::Plus
            },
            r: &value * &value,
        }
    }
}

impl std::fmt::Debug for Root {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.sign.is_positive() {
            write!(f, "√{}", self.r)
        } else {
            write!(f, "-√{}", self.r)
        }
    }
}

impl Neg for Root {
    type Output = Root;

    fn neg(self) -> Self::Output {
        Self {
            sign: -self.sign,
            r: self.r,
        }
    }
}

impl Mul for Root {
    type Output = Root;

    fn mul(self, rhs: Self) -> Self::Output {
        Self {
            sign: self.sign * rhs.sign,
            r: &self.r * &rhs.r,
        }
    }
}

impl Div for Root {
    type Output = Root;

    fn div(self, rhs: Self) -> Self::Output {
        Self {
            sign: self.sign * rhs.sign,
            r: &self.r / &rhs.r,
        }
    }
}
