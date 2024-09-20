use std::io::BufRead;
use anyhow::Result;

use super::ebi_object::EbiObject;

pub trait Importable {
    fn import_as_object(reader: &mut dyn BufRead) -> Result<EbiObject>;
    fn import(reader: &mut dyn BufRead) -> Result<Self> where Self: Sized;
}