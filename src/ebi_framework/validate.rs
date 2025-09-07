use anyhow::Result;
use std::io::BufRead;

use ebi_objects::Importable;

pub trait Validate: Importable {
    fn validate(reader: &mut dyn BufRead) -> Result<()>
    where
        Self: Sized,
    {
        match Self::import(reader) {
            Ok(_) => Ok(()),
            Err(x) => Err(x),
        }
    }
}

impl<T> Validate for T where T: Importable {}
