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

impl Exportable for bool {
    fn export_from_object(_: EbiOutput, _: &mut dyn std::io::Write) -> Result<()> {
        unreachable!()
    }

    fn export(&self, f: &mut dyn std::io::Write) -> Result<()> {
        Ok(writeln!(f, "{}", self)?)
    }
}

#[cfg(test)]
mod tests {
    use super::Exportable;

    #[test]
    fn default_exportable() {
        let mut f = vec![];
        0usize.export(&mut f).unwrap();

        "a".to_string().export(&mut f).unwrap();
    }

    #[test]
    #[should_panic]
    fn unreachable_string() {
        let mut f = vec![];
        String::export_from_object(crate::ebi_framework::ebi_output::EbiOutput::Usize(1), &mut f).unwrap();
    }

    #[test]
    #[should_panic]
    fn unreachable_usize() {
        let mut f = vec![];
        usize::export_from_object(crate::ebi_framework::ebi_output::EbiOutput::Usize(1), &mut f).unwrap();
    }
}