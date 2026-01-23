use crate::ebi_framework::{
    ebi_file_handler::EbiFileHandler,
    ebi_input::{EbiInput, EbiObjectImporter, EbiTraitImporter},
    ebi_output::EbiObjectExporter,
    ebi_trait::FromEbiTraitObject,
    object_importers::{
        ImportAsStochasticDeterministicFiniteAutomatonObject,
        ImportAsStochasticNondeterministicFiniteAutomatonObject,
    },
    trait_importers::{
        ImportAsActivitiesTrait, ImportAsFiniteLanguageTrait,
        ImportAsFiniteStochasticLanguageTrait, ImportAsIterableLanguageTrait,
        ImportAsIterableStochasticLanguageTrait, ImportAsQueriableStochasticLanguageTrait,
        ImportAsSemanticsTrait, ImportAsStochasticDeterministicSemanticsTrait,
        ImportAsStochasticSemanticsTrait,
    },
    validate::Validate,
};
use anyhow::{Result, anyhow};
use ebi_objects::{EbiObject, Exportable, FiniteStochasticLanguage, Importable};

pub const EBI_FINITE_STOCHASTIC_LANGUAGE: EbiFileHandler = EbiFileHandler {
    name: "finite stochastic language",
    article: "a",
    file_extension: "slang",
    is_binary: false,
    format_specification: FiniteStochasticLanguage::FILE_FORMAT_SPECIFICATION_LATEX,
    validator: Some(FiniteStochasticLanguage::validate),
    trait_importers: &[
        EbiTraitImporter::Activities(
            FiniteStochasticLanguage::import_as_activities_trait,
            FiniteStochasticLanguage::IMPORTER_PARAMETERS,
        ),
        EbiTraitImporter::IterableLanguage(
            FiniteStochasticLanguage::import_as_iterable_language_trait,
            FiniteStochasticLanguage::IMPORTER_PARAMETERS,
        ),
        EbiTraitImporter::FiniteLanguage(
            FiniteStochasticLanguage::import_as_finite_language_trait,
            FiniteStochasticLanguage::IMPORTER_PARAMETERS,
        ),
        EbiTraitImporter::FiniteStochasticLanguage(
            FiniteStochasticLanguage::import_as_finite_stochastic_language_trait,
            FiniteStochasticLanguage::IMPORTER_PARAMETERS,
        ),
        EbiTraitImporter::QueriableStochasticLanguage(
            FiniteStochasticLanguage::import_as_queriable_stochastic_language_trait,
            FiniteStochasticLanguage::IMPORTER_PARAMETERS,
        ),
        EbiTraitImporter::IterableStochasticLanguage(
            FiniteStochasticLanguage::import_as_iterable_stochastic_language_trait,
            FiniteStochasticLanguage::IMPORTER_PARAMETERS,
        ),
        EbiTraitImporter::StochasticSemantics(
            FiniteStochasticLanguage::import_as_stochastic_semantics_trait,
            FiniteStochasticLanguage::IMPORTER_PARAMETERS,
        ),
        EbiTraitImporter::StochasticDeterministicSemantics(
            FiniteStochasticLanguage::import_as_stochastic_deterministic_semantics_trait,
            FiniteStochasticLanguage::IMPORTER_PARAMETERS,
        ),
        EbiTraitImporter::Semantics(
            FiniteStochasticLanguage::import_as_semantics_trait,
            FiniteStochasticLanguage::IMPORTER_PARAMETERS,
        ),
    ],
    object_importers: &[
        EbiObjectImporter::FiniteStochasticLanguage(
            FiniteStochasticLanguage::import_as_object,
            FiniteStochasticLanguage::IMPORTER_PARAMETERS,
        ),
        EbiObjectImporter::StochasticDeterministicFiniteAutomaton(
            FiniteStochasticLanguage::import_as_stochastic_deterministic_finite_automaton_object,
            FiniteStochasticLanguage::IMPORTER_PARAMETERS,
        ),
        EbiObjectImporter::StochasticNondeterministicFiniteAutomaton(
            FiniteStochasticLanguage::import_as_stochastic_nondeterministic_finite_automaton_object,
            FiniteStochasticLanguage::IMPORTER_PARAMETERS,
        ),
    ],
    object_exporters: &[
        EbiObjectExporter::FiniteStochasticLanguage(FiniteStochasticLanguage::export_from_object),
        EbiObjectExporter::EventLog(FiniteStochasticLanguage::export_from_object),
        EbiObjectExporter::EventLogCsv(FiniteStochasticLanguage::export_from_object),
        EbiObjectExporter::EventLogXes(FiniteStochasticLanguage::export_from_object),
        EbiObjectExporter::EventLogTraceAttributes(FiniteStochasticLanguage::export_from_object),
    ],
    object_exporters_fallible: &[],
    java_object_handlers: &[],
};

impl FromEbiTraitObject for FiniteStochasticLanguage {
    fn from_trait_object(object: EbiInput) -> Result<Box<Self>> {
        match object {
            EbiInput::Object(EbiObject::FiniteStochasticLanguage(e), _) => Ok(Box::new(e)),
            _ => Err(anyhow!(
                "cannot read {} {} as a finite stochastic language",
                object.get_type().get_article(),
                object.get_type()
            )),
        }
    }
}

#[cfg(test)]
mod tests {
    use std::fs;

    use ebi_objects::{
        FiniteStochasticLanguage, NumberOfTraces,
        ebi_arithmetic::{Fraction, Zero},
    };

    use crate::ebi_traits::ebi_trait_finite_stochastic_language::EbiTraitFiniteStochasticLanguage;

    #[test]
    fn empty_slang() {
        let fin = fs::read_to_string("testfiles/empty.slang").unwrap();
        let mut slang = fin.parse::<FiniteStochasticLanguage>().unwrap();
        slang.normalise();

        assert_eq!(slang.number_of_traces(), 0);
        assert_eq!(slang.get_probability_sum(), Fraction::zero());
    }
}
