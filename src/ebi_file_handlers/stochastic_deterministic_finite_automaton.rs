use crate::ebi_framework::{
    ebi_file_handler::EbiFileHandler,
    ebi_input::{EbiInput, EbiObjectImporter, EbiTraitImporter},
    ebi_output::EbiObjectExporter,
    ebi_trait::FromEbiTraitObject,
    object_importers::{
        ImportAsLabelledPetriNetObject, ImportAsStochasticLabelledPetriNetObject, ImportAsStochasticNondeterministicFiniteAutomatonObject
    },
    trait_importers::{
        ImportAsActivitiesTrait, ImportAsGraphableTrait, ImportAsQueriableStochasticLanguageTrait,
        ImportAsSemanticsTrait, ImportAsStochasticDeterministicSemanticsTrait,
        ImportAsStochasticSemanticsTrait,
    },
    validate::Validate,
};
use anyhow::{Result, anyhow};
use ebi_objects::{EbiObject, Exportable, Importable, StochasticDeterministicFiniteAutomaton};

pub const EBI_STOCHASTIC_DETERMINISTIC_FINITE_AUTOMATON: EbiFileHandler = EbiFileHandler {
    name: "stochastic deterministic finite automaton",
    article: "a",
    file_extension: "sdfa",
    is_binary: false,
    format_specification: StochasticDeterministicFiniteAutomaton::FILE_FORMAT_SPECIFICATION_LATEX,
    validator: Some(StochasticDeterministicFiniteAutomaton::validate),
    trait_importers: &[
        EbiTraitImporter::Activities(
            StochasticDeterministicFiniteAutomaton::import_as_activities_trait,
            StochasticDeterministicFiniteAutomaton::IMPORTER_PARAMETERS,
        ),
        EbiTraitImporter::QueriableStochasticLanguage(
            StochasticDeterministicFiniteAutomaton::import_as_queriable_stochastic_language_trait,
            StochasticDeterministicFiniteAutomaton::IMPORTER_PARAMETERS,
        ),
        EbiTraitImporter::StochasticDeterministicSemantics(
            StochasticDeterministicFiniteAutomaton::import_as_stochastic_deterministic_semantics_trait,
            StochasticDeterministicFiniteAutomaton::IMPORTER_PARAMETERS,
        ),
        EbiTraitImporter::StochasticSemantics(
            StochasticDeterministicFiniteAutomaton::import_as_stochastic_semantics_trait,
            StochasticDeterministicFiniteAutomaton::IMPORTER_PARAMETERS,
        ),
        EbiTraitImporter::Semantics(
            StochasticDeterministicFiniteAutomaton::import_as_semantics_trait,
            StochasticDeterministicFiniteAutomaton::IMPORTER_PARAMETERS,
        ),
        EbiTraitImporter::Graphable(
            StochasticDeterministicFiniteAutomaton::import_as_graphable_trait,
            StochasticDeterministicFiniteAutomaton::IMPORTER_PARAMETERS,
        ),
    ],
    object_importers: &[
        EbiObjectImporter::StochasticDeterministicFiniteAutomaton(
            StochasticDeterministicFiniteAutomaton::import_as_object,
            StochasticDeterministicFiniteAutomaton::IMPORTER_PARAMETERS,
        ),
        EbiObjectImporter::StochasticNondeterministicFiniteAutomaton(
            StochasticDeterministicFiniteAutomaton::import_as_stochastic_nondeterministic_finite_automaton_object,
            StochasticDeterministicFiniteAutomaton::IMPORTER_PARAMETERS,
        ),
        EbiObjectImporter::LabelledPetriNet(
            StochasticDeterministicFiniteAutomaton::import_as_labelled_petri_net_object,
            StochasticDeterministicFiniteAutomaton::IMPORTER_PARAMETERS,
        ),
        EbiObjectImporter::StochasticLabelledPetriNet(
            StochasticDeterministicFiniteAutomaton::import_as_stochastic_labelled_petri_net_object,
            StochasticDeterministicFiniteAutomaton::IMPORTER_PARAMETERS,
        ),
        EbiObjectImporter::StochasticNondeterministicFiniteAutomaton(
            StochasticDeterministicFiniteAutomaton::import_as_stochastic_nondeterministic_finite_automaton_object,
            StochasticDeterministicFiniteAutomaton::IMPORTER_PARAMETERS,
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
        EbiObjectExporter::EventLogTraceAttributes(
            StochasticDeterministicFiniteAutomaton::export_from_object,
        ),
        EbiObjectExporter::EventLogXes(StochasticDeterministicFiniteAutomaton::export_from_object),
        EbiObjectExporter::EventLogCsv(StochasticDeterministicFiniteAutomaton::export_from_object),
    ],
    object_exporters_fallible: &[],
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

    use crate::{
        ebi_framework::trait_importers::ToSemanticsTrait,
        ebi_traits::ebi_trait_semantics::EbiTraitSemantics,
    };

    use super::StochasticDeterministicFiniteAutomaton;

    #[test]
    fn sdfa_empty() {
        let fin = fs::read_to_string("testfiles/empty.sdfa").unwrap();
        let dfa = fin
            .parse::<StochasticDeterministicFiniteAutomaton>()
            .unwrap();

        if let EbiTraitSemantics::Usize(semantics) = dfa.to_semantics_trait() {
            assert!(semantics.get_initial_state().is_none());
        } else {
            assert!(false);
        }
    }
}
