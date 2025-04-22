use anyhow::Result;
use flate2::{Compression, bufread::GzDecoder, write::GzEncoder};
use std::io::{BufRead, BufReader, Write};

use crate::{
    ebi_framework::{
        ebi_file_handler::EbiFileHandler,
        ebi_input::{self, EbiObjectImporter, EbiTraitImporter},
        ebi_object::EbiObject,
        ebi_output::{EbiObjectExporter, EbiOutput},
        exportable::Exportable,
        importable::Importable,
    },
    ebi_traits::{
        ebi_trait_event_log::EbiTraitEventLog,
        ebi_trait_finite_language::EbiTraitFiniteLanguage,
        ebi_trait_finite_stochastic_language::EbiTraitFiniteStochasticLanguage,
        ebi_trait_iterable_language::EbiTraitIterableLanguage,
        ebi_trait_iterable_stochastic_language::EbiTraitIterableStochasticLanguage,
        ebi_trait_queriable_stochastic_language::EbiTraitQueriableStochasticLanguage,
        ebi_trait_semantics::{EbiTraitSemantics, ToSemantics},
        ebi_trait_stochastic_deterministic_semantics::{
            EbiTraitStochasticDeterministicSemantics, ToStochasticDeterministicSemantics,
        },
        ebi_trait_stochastic_semantics::{EbiTraitStochasticSemantics, ToStochasticSemantics},
    },
};

use super::{
    event_log::EventLog, finite_language::FiniteLanguage,
    finite_stochastic_language::FiniteStochasticLanguage,
};

pub const FORMAT_SPECIFICATION: &str = "A compressed event log is a gzipped event log file in the IEEE XES format~\\cite{DBLP:journals/cim/AcamporaVSAGV17}.
Parsing is performed by the Rust4PM crate~\\cite{DBLP:conf/bpm/KustersA24}.";

pub const EBI_COMPRESSED_EVENT_LOG: EbiFileHandler = EbiFileHandler {
    name: "compressed event log",
    article: "a",
    file_extension: "xes.gz",
    format_specification: &FORMAT_SPECIFICATION,
    validator: ebi_input::validate::<CompressedEventLog>,
    trait_importers: &[
        EbiTraitImporter::IterableLanguage(CompressedEventLog::read_as_iterable_language),
        EbiTraitImporter::FiniteLanguage(CompressedEventLog::read_as_finite_language),
        EbiTraitImporter::FiniteStochasticLanguage(
            CompressedEventLog::read_as_finite_stochastic_language,
        ),
        EbiTraitImporter::QueriableStochasticLanguage(
            CompressedEventLog::read_as_queriable_stochastic_language,
        ),
        EbiTraitImporter::IterableStochasticLanguage(
            CompressedEventLog::read_as_iterable_stochastic_language,
        ),
        EbiTraitImporter::EventLog(CompressedEventLog::read_as_event_log),
        EbiTraitImporter::StochasticDeterministicSemantics(
            CompressedEventLog::import_as_stochastic_deterministic_semantics,
        ),
        EbiTraitImporter::StochasticSemantics(CompressedEventLog::import_as_stochastic_semantics),
        EbiTraitImporter::Semantics(CompressedEventLog::import_as_semantics),
    ],
    object_importers: &[EbiObjectImporter::EventLog(
        CompressedEventLog::import_as_object,
    )],
    object_exporters: &[EbiObjectExporter::EventLog(
        CompressedEventLog::export_from_object,
    )],
    java_object_handlers: &[],
};

pub struct CompressedEventLog {
    pub(crate) log: EventLog,
}

impl CompressedEventLog {
    pub fn read_as_finite_language(
        reader: &mut dyn BufRead,
    ) -> Result<Box<dyn EbiTraitFiniteLanguage>> {
        let event_log = Self::import(reader)?;
        Ok(Box::new(Into::<FiniteLanguage>::into(event_log.log)))
    }

    pub fn read_as_finite_stochastic_language(
        reader: &mut dyn BufRead,
    ) -> Result<Box<dyn EbiTraitFiniteStochasticLanguage>> {
        let event_log = Self::import(reader)?;
        Ok(Box::new(Into::<FiniteStochasticLanguage>::into(Into::<
            FiniteStochasticLanguage,
        >::into(
            event_log.log,
        ))))
    }

    pub fn read_as_queriable_stochastic_language(
        reader: &mut dyn BufRead,
    ) -> Result<Box<dyn EbiTraitQueriableStochasticLanguage>> {
        let event_log = Self::import(reader)?;
        Ok(Box::new(Into::<FiniteStochasticLanguage>::into(
            event_log.log,
        )))
    }

    pub fn read_as_iterable_language(
        reader: &mut dyn BufRead,
    ) -> Result<Box<dyn EbiTraitIterableLanguage>> {
        let event_log = Self::import(reader)?;
        Ok(Box::new(Into::<FiniteLanguage>::into(event_log.log)))
    }

    pub fn read_as_iterable_stochastic_language(
        reader: &mut dyn BufRead,
    ) -> Result<Box<dyn EbiTraitIterableStochasticLanguage>> {
        let event_log = Self::import(reader)?;
        Ok(Box::new(Into::<FiniteStochasticLanguage>::into(
            Into::<FiniteStochasticLanguage>::into(event_log.log),
        )))
    }

    pub fn read_as_event_log(reader: &mut dyn BufRead) -> Result<Box<dyn EbiTraitEventLog>> {
        let event_log = Self::import(reader)?;
        Ok(Box::new(event_log.log))
    }
}

impl Importable for CompressedEventLog {
    fn import_as_object(reader: &mut dyn BufRead) -> Result<EbiObject> {
        Ok(EbiObject::EventLog(Self::import(reader)?.log))
    }

    fn import(reader: &mut dyn BufRead) -> anyhow::Result<Self>
    where
        Self: Sized,
    {
        let dec = GzDecoder::new(reader);
        let mut reader2 = BufReader::new(dec);
        let log = EventLog::import(&mut reader2)?;
        Ok(Self { log: log })
    }
}

impl Exportable for CompressedEventLog {
    fn export_from_object(object: EbiOutput, f: &mut dyn Write) -> Result<()> {
        match object {
            EbiOutput::Object(EbiObject::EventLog(log)) => Self::export(&Self { log: log }, f),
            _ => unreachable!(),
        }
    }

    fn export(&self, f: &mut dyn std::io::Write) -> Result<()> {
        let mut writer = GzEncoder::new(f, Compression::best());
        self.log.export(&mut writer)
    }
}

impl ToSemantics for CompressedEventLog {
    fn to_semantics(self) -> EbiTraitSemantics {
        self.log.to_semantics()
    }
}

impl ToStochasticSemantics for CompressedEventLog {
    fn to_stochastic_semantics(self) -> EbiTraitStochasticSemantics {
        self.log.to_stochastic_semantics()
    }
}

impl ToStochasticDeterministicSemantics for CompressedEventLog {
    fn to_stochastic_deterministic_semantics(self) -> EbiTraitStochasticDeterministicSemantics {
        self.log.to_stochastic_deterministic_semantics()
    }
}
