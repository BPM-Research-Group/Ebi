use ebi_objects::{
    EbiObject, Exportable, FiniteStochasticLanguage, Importable,
    ebi_objects::finite_stochastic_language::FORMAT_SPECIFICATION,
};
use anyhow::{anyhow, Result};

use crate::{
    ebi_framework::{
        ebi_file_handler::EbiFileHandler,
        ebi_input::{EbiInput, EbiObjectImporter, EbiTraitImporter},
        ebi_output::EbiObjectExporter,
        ebi_trait::FromEbiTraitObject,
        object_importers::ToStochasticDeterministicFiniteAutomatonObject,
        validate::Validate,
    },
    ebi_traits::{
        ebi_trait_activities::ToActivities, ebi_trait_finite_language::ToFiniteLanguage,
        ebi_trait_finite_stochastic_language::ToFiniteStochasticLanguage,
        ebi_trait_iterable_language::ToIterableLanguage,
        ebi_trait_iterable_stochastic_language::ToIterableStochasticLanguage,
        ebi_trait_queriable_stochastic_language::ToQueriableStochasticLanguage,
        ebi_trait_semantics::ToSemantics,
        ebi_trait_stochastic_deterministic_semantics::ToStochasticDeterministicSemantics,
        ebi_trait_stochastic_semantics::ToStochasticSemantics,
    },
};

pub const EBI_FINITE_STOCHASTIC_LANGUAGE: EbiFileHandler = EbiFileHandler {
    name: "finite stochastic language",
    article: "a",
    file_extension: "slang",
    is_binary: false,
    format_specification: &FORMAT_SPECIFICATION,
    validator: Some(FiniteStochasticLanguage::validate),
    trait_importers: &[
        EbiTraitImporter::Activities(FiniteStochasticLanguage::import_as_activities),
        EbiTraitImporter::IterableLanguage(FiniteStochasticLanguage::import_as_iterable_language),
        EbiTraitImporter::FiniteLanguage(FiniteStochasticLanguage::import_as_finite_language),
        EbiTraitImporter::FiniteStochasticLanguage(
            FiniteStochasticLanguage::import_as_finite_stochastic_language,
        ),
        EbiTraitImporter::QueriableStochasticLanguage(
            FiniteStochasticLanguage::import_as_queriable_stochastic_language,
        ),
        EbiTraitImporter::IterableStochasticLanguage(
            FiniteStochasticLanguage::import_as_iterable_stochastic_language,
        ),
        EbiTraitImporter::StochasticSemantics(
            FiniteStochasticLanguage::import_as_stochastic_semantics,
        ),
        EbiTraitImporter::StochasticDeterministicSemantics(
            FiniteStochasticLanguage::import_as_stochastic_deterministic_semantics,
        ),
        EbiTraitImporter::Semantics(FiniteStochasticLanguage::import_as_semantics),
    ],
    object_importers: &[
        EbiObjectImporter::FiniteStochasticLanguage(FiniteStochasticLanguage::import_as_object),
        EbiObjectImporter::StochasticDeterministicFiniteAutomaton(
            FiniteStochasticLanguage::import_as_stochastic_deterministic_finite_automaton_object,
        ),
    ],
    object_exporters: &[
        EbiObjectExporter::FiniteStochasticLanguage(FiniteStochasticLanguage::export_from_object),
        EbiObjectExporter::EventLog(FiniteStochasticLanguage::export_from_object),
    ],
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

    use ebi_arithmetic::{Fraction, Zero};
    use ebi_objects::{FiniteStochasticLanguage, IndexTrace};

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
