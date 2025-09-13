use ebi_objects::{
    DeterministicFiniteAutomaton, Exportable, Importable,
    ebi_objects::deterministic_finite_automaton::FORMAT_SPECIFICATION,
};

use crate::{
    ebi_framework::{
        ebi_file_handler::EbiFileHandler,
        ebi_input::{EbiObjectImporter, EbiTraitImporter},
        ebi_output::EbiObjectExporter,
        object_importers::ToLabelledPetriNetObject,
        validate::Validate,
    },
    ebi_traits::{
        ebi_trait_activities::ToActivities, ebi_trait_graphable::ToGraphable,
        ebi_trait_semantics::ToSemantics,
    },
};

pub const EBI_DETERMINISTIC_FINITE_AUTOMATON: EbiFileHandler = EbiFileHandler {
    name: "deterministic finite automaton",
    article: "a",
    file_extension: "dfa",
    is_binary: false,
    format_specification: FORMAT_SPECIFICATION,
    validator: Some(DeterministicFiniteAutomaton::validate),
    trait_importers: &[
        EbiTraitImporter::Activities(DeterministicFiniteAutomaton::import_as_activities),
        EbiTraitImporter::Semantics(DeterministicFiniteAutomaton::import_as_semantics),
        EbiTraitImporter::Graphable(DeterministicFiniteAutomaton::import_as_graphable),
    ],
    object_importers: &[
        EbiObjectImporter::DeterministicFiniteAutomaton(
            DeterministicFiniteAutomaton::import_as_object,
        ),
        EbiObjectImporter::LabelledPetriNet(
            DeterministicFiniteAutomaton::import_as_labelled_petri_net_object,
        ),
    ],
    object_exporters: &[
        EbiObjectExporter::DeterministicFiniteAutomaton(
            DeterministicFiniteAutomaton::export_from_object,
        ),
        EbiObjectExporter::FiniteLanguage(DeterministicFiniteAutomaton::export_from_object),
        EbiObjectExporter::StochasticDeterministicFiniteAutomaton(
            DeterministicFiniteAutomaton::export_from_object,
        ),
        EbiObjectExporter::EventLog(DeterministicFiniteAutomaton::export_from_object),
        EbiObjectExporter::FiniteStochasticLanguage(
            DeterministicFiniteAutomaton::export_from_object,
        ),
    ],
    java_object_handlers: &[],
};

#[cfg(test)]
mod tests {
    use std::fs;

    use ebi_objects::{DeterministicFiniteAutomaton, HasActivityKey};

    use crate::{ebi_traits::ebi_trait_semantics::{EbiTraitSemantics, ToSemantics}, semantics::semantics::Semantics};

    #[test]
    fn insert_wrong_edge() {
        let mut dfa = DeterministicFiniteAutomaton::new();
        let state = dfa.get_initial_state().unwrap();
        let activity = dfa.activity_key_mut().process_activity("a");

        dfa.get_sources();

        assert!(dfa.add_transition(state, activity, state).is_ok());
        assert!(dfa.add_transition(state, activity, state).is_err());
    }

    #[test]
    fn dfa_empty() {
        let fin = fs::read_to_string("testfiles/empty.dfa").unwrap();
        let dfa = fin.parse::<DeterministicFiniteAutomaton>().unwrap();

        if let EbiTraitSemantics::Usize(semantics) = dfa.to_semantics() {
            assert!(semantics.get_initial_state().is_none());
        } else {
            assert!(false);
        }
    }
}
