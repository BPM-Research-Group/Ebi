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
    use std::fs::{self, File};

    use crate::{ebi_framework::{ebi_file_handler::EBI_FILE_HANDLERS, infoable::Infoable}, multiple_reader::MultipleReader};

    #[test]
    fn all_infoable() {
        let files = fs::read_dir("./testfiles").unwrap();
        for path in files {
            let file = path.unwrap();
            println!("file {:?}", file.file_name());

            let mut reader = MultipleReader::from_file(File::open(file.path()).unwrap());

            //look for file handlers that should accept this file
            for file_handler in EBI_FILE_HANDLERS {
                println!("\tfile handler {}", file_handler);
                if !file.file_name().into_string().unwrap().contains("invalid")
                    && file
                        .file_name()
                        .into_string()
                        .unwrap()
                        .ends_with(&(".".to_string() + file_handler.file_extension))
                {
                    //file handler should be able to accept this file

                    for importer in file_handler.object_importers {
                        println!("\t\timporter {}", importer);
                        let object = (importer.get_importer())(&mut reader.get().unwrap()).unwrap();
                        let mut f = vec![];
                        object.info(&mut f).unwrap();
                    }
                }
            }
        }
    }
}