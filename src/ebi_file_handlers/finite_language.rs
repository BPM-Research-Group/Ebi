use crate::ebi_framework::{
    ebi_file_handler::EbiFileHandler,
    ebi_input::{EbiInput, EbiObjectImporter, EbiTraitImporter},
    ebi_output::EbiObjectExporter,
    ebi_trait::FromEbiTraitObject,
    object_importers::ImportAsDeterministicFiniteAutomatonObject,
    trait_importers::{
        ImportAsActivitiesTrait, ImportAsFiniteLanguageTrait, ImportAsIterableLanguageTrait,
        ImportAsSemanticsTrait,
    },
    validate::Validate,
};
use anyhow::{Result, anyhow};
use ebi_objects::{EbiObject, Exportable, FiniteLanguage, Importable};

pub const EBI_FINITE_LANGUAGE: EbiFileHandler = EbiFileHandler {
    name: "finite language",
    article: "a",
    file_extension: "lang",
    is_binary: false,
    format_specification: FiniteLanguage::FILE_FORMAT_SPECIFICATION_LATEX,
    validator: Some(FiniteLanguage::validate),
    trait_importers: &[
        EbiTraitImporter::Activities(
            FiniteLanguage::import_as_activities_trait,
            FiniteLanguage::IMPORTER_PARAMETERS,
        ),
        EbiTraitImporter::IterableLanguage(
            FiniteLanguage::import_as_iterable_language_trait,
            FiniteLanguage::IMPORTER_PARAMETERS,
        ),
        EbiTraitImporter::FiniteLanguage(
            FiniteLanguage::import_as_finite_language_trait,
            FiniteLanguage::IMPORTER_PARAMETERS,
        ),
        EbiTraitImporter::Semantics(
            FiniteLanguage::import_as_semantics_trait,
            FiniteLanguage::IMPORTER_PARAMETERS,
        ),
    ],
    object_importers: &[
        EbiObjectImporter::FiniteLanguage(
            FiniteLanguage::import_as_object,
            FiniteLanguage::IMPORTER_PARAMETERS,
        ),
        EbiObjectImporter::DeterministicFiniteAutomaton(
            FiniteLanguage::import_as_deterministic_finite_automaton_object,
            &[],
        ),
    ],
    object_exporters: &[
        EbiObjectExporter::EventLog(FiniteLanguage::export_from_object),
        EbiObjectExporter::EventLogTraceAttributes(FiniteLanguage::export_from_object),
        EbiObjectExporter::EventLogXes(FiniteLanguage::export_from_object),
        EbiObjectExporter::EventLogCsv(FiniteLanguage::export_from_object),
        EbiObjectExporter::FiniteLanguage(FiniteLanguage::export_from_object),
        EbiObjectExporter::FiniteStochasticLanguage(FiniteLanguage::export_from_object),
    ],
    object_exporters_fallible: &[],
    java_object_handlers: &[],
};

impl FromEbiTraitObject for FiniteLanguage {
    fn from_trait_object(object: EbiInput) -> Result<Box<Self>> {
        match object {
            EbiInput::Object(EbiObject::FiniteLanguage(e), _) => Ok(Box::new(e)),
            _ => Err(anyhow!(
                "cannot read {} {} as a finite language",
                object.get_type().get_article(),
                object.get_type()
            )),
        }
    }
}

#[cfg(test)]
mod tests {
    use std::fs;

    use crate::{
        ebi_framework::trait_importers::ToSemanticsTrait,
        ebi_traits::ebi_trait_semantics::EbiTraitSemantics,
    };

    use super::FiniteLanguage;

    #[test]
    fn lang_empty() {
        let fin = fs::read_to_string("testfiles/empty.lang").unwrap();
        let log = fin.parse::<FiniteLanguage>().unwrap();

        if let EbiTraitSemantics::Usize(semantics) = log.to_semantics_trait() {
            assert!(semantics.get_initial_state().is_none());
        }
    }
}
