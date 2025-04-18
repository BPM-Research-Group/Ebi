use std::{fmt::{self, Display}, io::BufRead, str::FromStr};
use anyhow::{Result, Error};
use chrono::{DateTime, FixedOffset};
use serde::{Deserialize, Serialize};

use crate::ebi_framework::{ebi_file_handler::EbiFileHandler, ebi_input::{self, EbiObjectImporter}, ebi_object::EbiObject, ebi_output::{EbiObjectExporter, EbiOutput}, exportable::Exportable, importable::Importable, infoable::Infoable};

pub const HEADER: &str = "executions";

pub const FORMAT_SPECIFICATION: &str = "not yet finalised.";

pub const EBI_EXECUTIONS: EbiFileHandler = EbiFileHandler {
    name: "executions",
    article: "",
    file_extension: "exs",
    format_specification: &FORMAT_SPECIFICATION,
    validator: ebi_input::validate::<Executions>,
    trait_importers: &[
        
    ],
    object_importers: &[
        EbiObjectImporter::Executions(Executions::import_as_object)
    ],
    object_exporters: &[ 
        EbiObjectExporter::Executions(Executions::export_from_object)
    ],
    java_object_handlers: &[],
};

#[derive(Serialize, Deserialize, Clone)]
pub struct Executions {
    executions: Vec<Execution>
}

impl Importable for Executions {
    fn import_as_object(reader: &mut dyn BufRead) -> Result<EbiObject> {
        Ok(EbiObject::Executions(Self::import(reader)?))
    }

    fn import(reader: &mut dyn BufRead) -> Result<Self> where Self: Sized {
        Ok(serde_json::from_reader(reader)?)
    }
}

impl Infoable for Executions {
    fn info(&self, f: &mut impl std::io::Write) -> Result<()> {
        writeln!(f, "Number of executions\t\t{}", self.executions.len())?;
        Ok(write!(f, "")?)
    }
}

impl Display for Executions {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let x = serde_json::to_string(self).unwrap();
        write!(f, "{}", x)
    }
}

impl FromStr for Executions {
    type Err = Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut reader = std::io::Cursor::new(s);
        Self::import(&mut reader)
    }
}

impl Exportable for Executions {
    fn export_from_object(object: EbiOutput, f: &mut dyn std::io::Write) -> Result<()> {
        match object {
            EbiOutput::Object(EbiObject::Executions(exe)) => exe.export(f),
            _ => unreachable!()
        }
    }

    fn export(&self, f: &mut dyn std::io::Write) -> Result<()> {
        Ok(write!(f, "{}", self)?)
    }
}

impl From<Vec<Execution>> for Executions {
    fn from(value: Vec<Execution>) -> Self {
        Self {
            executions: value
        }
    }
}

#[derive(Serialize, Deserialize, Clone)]
pub struct Execution {
    pub transition: usize,
    pub enabled_transitions_at_enablement: Option<Vec<usize>>,
    pub time_of_enablement: Option<DateTime<FixedOffset>>,
    pub time_of_execution: Option<DateTime<FixedOffset>>,
    pub features_at_enablement: Option<Vec<usize>>
}

impl Display for Execution {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let x = serde_json::to_string(self).unwrap();
        write!(f, "{}", x)
    }
}