use crate::ebi_framework::{
    ebi_file_handler::EbiFileHandler,
    ebi_input::{EbiObjectImporter, EbiTraitImporter},
    ebi_output::EbiObjectExporter,
    object_importers::ImportAsLabelledPetriNetObject,
    trait_importers::{ImportAsActivitiesTrait, ImportAsGraphableTrait, ImportAsSemanticsTrait},
    validate::Validate,
};
use ebi_objects::{DeterministicFiniteAutomaton, Exportable, Importable};

pub const EBI_DETERMINISTIC_FINITE_AUTOMATON: EbiFileHandler = EbiFileHandler {
    name: "deterministic finite automaton",
    article: "a",
    file_extension: "dfa",
    is_binary: false,
    format_specification: DeterministicFiniteAutomaton::FILE_FORMAT_SPECIFICATION_LATEX,
    validator: Some(DeterministicFiniteAutomaton::validate),
    trait_importers: &[
        EbiTraitImporter::Activities(
            DeterministicFiniteAutomaton::import_as_activities_trait,
            DeterministicFiniteAutomaton::IMPORTER_PARAMETERS,
        ),
        EbiTraitImporter::Semantics(
            DeterministicFiniteAutomaton::import_as_semantics_trait,
            DeterministicFiniteAutomaton::IMPORTER_PARAMETERS,
        ),
        EbiTraitImporter::Graphable(
            DeterministicFiniteAutomaton::import_as_graphable_trait,
            DeterministicFiniteAutomaton::IMPORTER_PARAMETERS,
        ),
    ],
    object_importers: &[
        EbiObjectImporter::DeterministicFiniteAutomaton(
            DeterministicFiniteAutomaton::import_as_object,
            DeterministicFiniteAutomaton::IMPORTER_PARAMETERS,
        ),
        EbiObjectImporter::LabelledPetriNet(
            DeterministicFiniteAutomaton::import_as_labelled_petri_net_object,
            DeterministicFiniteAutomaton::IMPORTER_PARAMETERS,
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
        EbiObjectExporter::EventLogTraceAttributes(
            DeterministicFiniteAutomaton::export_from_object,
        ),
        EbiObjectExporter::EventLogCsv(DeterministicFiniteAutomaton::export_from_object),
        EbiObjectExporter::EventLogXes(DeterministicFiniteAutomaton::export_from_object),
        EbiObjectExporter::FiniteStochasticLanguage(
            DeterministicFiniteAutomaton::export_from_object,
        ),
    ],
    object_exporters_fallible: &[],
    java_object_handlers: &[],
};

#[cfg(test)]
mod tests {
    use crate::{
        ebi_framework::trait_importers::ToSemanticsTrait,
        ebi_traits::ebi_trait_semantics::EbiTraitSemantics, semantics::semantics::Semantics,
    };
    use ebi_objects::{DeterministicFiniteAutomaton, HasActivityKey};
    use std::fs;

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

        if let EbiTraitSemantics::Usize(semantics) = dfa.to_semantics_trait() {
            assert!(semantics.get_initial_state().is_none());
        } else {
            assert!(false);
        }
    }
}
