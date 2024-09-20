use anyhow::Result;

use super::ebi_output::EbiOutput;

pub trait Exportable {
    fn export_from_object(object: EbiOutput, f: &mut dyn std::io::Write) -> Result<()>;

    fn export(&self, f: &mut dyn std::io::Write) -> Result<()>;
}

impl Exportable for String {
    fn export_from_object(_: EbiOutput, _: &mut dyn std::io::Write) -> Result<()> {
        unreachable!()
    }

    fn export(&self, f: &mut dyn std::io::Write) -> Result<()> {
        Ok(writeln!(f, "{}", self)?)
    }
}

impl Exportable for usize {
    fn export_from_object(_: EbiOutput, _: &mut dyn std::io::Write) -> Result<()> {
        unreachable!()
    }

    fn export(&self, f: &mut dyn std::io::Write) -> Result<()> {
        Ok(writeln!(f, "{}", self)?)
    }
}