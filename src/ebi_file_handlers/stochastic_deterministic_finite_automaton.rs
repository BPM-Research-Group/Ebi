use anyhow::{Result, anyhow};
use ebi_objects::{
    EbiObject, Exportable, Importable, StochasticDeterministicFiniteAutomaton,
    ebi_objects::stochastic_deterministic_finite_automaton::FORMAT_SPECIFICATION,
};

use crate::{
    ebi_framework::{
        ebi_file_handler::EbiFileHandler,
        ebi_input::{EbiInput, EbiObjectImporter, EbiTraitImporter},
        ebi_output::EbiObjectExporter,
        ebi_trait::FromEbiTraitObject,
        object_importers::ToLabelledPetriNetObject,
        validate::Validate,
    },
    ebi_traits::{
        ebi_trait_activities::ToActivities, ebi_trait_graphable::ToGraphable,
        ebi_trait_queriable_stochastic_language::ToQueriableStochasticLanguage,
        ebi_trait_semantics::ToSemantics,
        ebi_trait_stochastic_deterministic_semantics::ToStochasticDeterministicSemantics,
        ebi_trait_stochastic_semantics::ToStochasticSemantics,
    },
};

pub const EBI_STOCHASTIC_DETERMINISTIC_FINITE_AUTOMATON: EbiFileHandler = EbiFileHandler {
    name: "stochastic deterministic finite automaton",
    article: "a",
    file_extension: "sdfa",
    is_binary: false,
    format_specification: &FORMAT_SPECIFICATION,
    validator: Some(StochasticDeterministicFiniteAutomaton::validate),
    trait_importers: &[
        EbiTraitImporter::Activities(StochasticDeterministicFiniteAutomaton::import_as_activities),
        EbiTraitImporter::QueriableStochasticLanguage(
            StochasticDeterministicFiniteAutomaton::import_as_queriable_stochastic_language,
        ),
        EbiTraitImporter::StochasticDeterministicSemantics(
            StochasticDeterministicFiniteAutomaton::import_as_stochastic_deterministic_semantics,
        ),
        EbiTraitImporter::StochasticSemantics(
            StochasticDeterministicFiniteAutomaton::import_as_stochastic_semantics,
        ),
        EbiTraitImporter::Semantics(StochasticDeterministicFiniteAutomaton::import_as_semantics),
        EbiTraitImporter::Graphable(StochasticDeterministicFiniteAutomaton::import_as_graphable),
    ],
    object_importers: &[
        EbiObjectImporter::StochasticDeterministicFiniteAutomaton(
            StochasticDeterministicFiniteAutomaton::import_as_object,
        ),
        EbiObjectImporter::LabelledPetriNet(
            StochasticDeterministicFiniteAutomaton::import_as_labelled_petri_net_object,
        ),
    ],
    object_exporters: &[
        EbiObjectExporter::StochasticDeterministicFiniteAutomaton(
            StochasticDeterministicFiniteAutomaton::export_from_object,
        ),
        EbiObjectExporter::FiniteStochasticLanguage(
            StochasticDeterministicFiniteAutomaton::export_from_object,
        ),
        EbiObjectExporter::EventLog(StochasticDeterministicFiniteAutomaton::export_from_object),
    ],
    java_object_handlers: &[],
};

impl FromEbiTraitObject for StochasticDeterministicFiniteAutomaton {
    fn from_trait_object(object: EbiInput) -> Result<Box<Self>> {
        match object {
            EbiInput::Object(EbiObject::StochasticDeterministicFiniteAutomaton(e), _) => {
                Ok(Box::new(e))
            }
            _ => Err(anyhow!(
                "cannot read {} {} as a stochastic deterministic finite automaton",
                object.get_type().get_article(),
                object.get_type()
            )),
        }
    }
}

#[cfg(test)]
mod tests {
    use std::fs;

    use crate::ebi_traits::ebi_trait_semantics::{EbiTraitSemantics, ToSemantics};

    use super::StochasticDeterministicFiniteAutomaton;

    #[test]
    fn sdfa_empty() {
        let fin = fs::read_to_string("testfiles/empty.sdfa").unwrap();
        let dfa = fin
            .parse::<StochasticDeterministicFiniteAutomaton>()
            .unwrap();

        if let EbiTraitSemantics::Usize(semantics) = dfa.to_semantics() {
            assert!(semantics.get_initial_state().is_none());
        } else {
            assert!(false);
        }
    }
}
