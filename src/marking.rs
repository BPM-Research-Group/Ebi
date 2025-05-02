use anyhow::{Result, anyhow};
use std::{
    cmp::Ordering,
    fmt::{Debug, Display, Formatter},
    ops::MulAssign,
};

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct Marking {
    pub(crate) place2token: Vec<u64>, //for each place: number of tokens in that place
}

impl Marking {
    pub fn new(size: usize) -> Self {
        Marking {
            place2token: vec![0; size],
        }
    }

    pub fn get_place2token(&self) -> &Vec<u64> {
        &self.place2token
    }

    pub fn from_vec(place2token: Vec<u64>) -> Self {
        Marking {
            place2token: place2token,
        }
    }

    pub fn increase(&mut self, place: usize, amount: u64) -> Result<()> {
        if self.place2token[place] == u64::MAX - amount {
            return Err(anyhow!(
                "tried to put too many places in a marking for place {}",
                place
            ));
        }

        self.place2token[place] += amount;
        Ok(())
    }

    pub fn decrease(&mut self, place: usize, amount: u64) -> Result<()> {
        if self.place2token[place] < amount {
            return Err(anyhow!(
                "tried to obtain a negative number of places in a marking for place {}",
                place
            ));
        }
        self.place2token[place] -= amount;
        Ok(())
    }

    pub fn add_place(&mut self) {
        self.place2token.push(0);
    }

    /**
     * Returns whether all places are at least equal, and at least one has a larger number of tokens.
     */
    pub fn is_larger_than(&self, other: &Self) -> bool {
        let mut at_least_one_larger = false;
        for (me, you) in self.place2token.iter().zip(other.place2token.iter()) {
            match me.partial_cmp(you) {
                Some(Ordering::Equal) => {}
                Some(Ordering::Greater) => {
                    return false;
                }
                Some(Ordering::Less) => at_least_one_larger = true,
                None => return false,
            }
        }

        at_least_one_larger
    }

    /**
     * Returns whether all places are at least equal.
     */
    pub fn is_larger_than_or_equal_to(&self, other: &Self) -> bool {
        for (me, you) in self.place2token.iter().zip(other.place2token.iter()) {
            match me.partial_cmp(you) {
                Some(Ordering::Equal) => {}
                Some(Ordering::Greater) => {
                    return false;
                }
                Some(Ordering::Less) => {}
                None => return false,
            }
        }
        return true;
    }
}

impl From<Vec<u64>> for Marking {
    fn from(value: Vec<u64>) -> Self {
        Self { place2token: value }
    }
}

impl Display for Marking {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{{")?;
        let mut first = true;
        for (place, multiplicity) in self.place2token.iter().enumerate() {
            if *multiplicity > 0 {
                if !first {
                    write!(f, ", ")?;
                }
                first = false;
                write!(f, "{}:{}", place, multiplicity)?;
            }
        }
        write!(f, "}}")
    }
}

impl Debug for Marking {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{{")?;
        let mut first = true;
        for (place, multiplicity) in self.place2token.iter().enumerate() {
            if *multiplicity > 0 {
                if !first {
                    write!(f, ", ")?;
                }
                first = false;
                write!(f, "{}:{}", place, multiplicity)?;
            }
        }
        write!(f, "}}")
    }
}

impl MulAssign<u64> for Marking {
    fn mul_assign(&mut self, rhs: u64) {
        self.place2token.iter_mut().for_each(|x| *x *= rhs);
    }
}
