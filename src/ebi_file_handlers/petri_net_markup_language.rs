use ebi_objects::{
    Exportable, Importable, PetriNetMarkupLanguage,
    ebi_objects::petri_net_markup_language::FORMAT_SPECIFICATION,
};

use crate::{
    ebi_framework::{
        ebi_file_handler::EbiFileHandler,
        ebi_input::{EbiObjectImporter, EbiTraitImporter},
        ebi_output::EbiObjectExporter,
        validate::Validate,
    },
    ebi_traits::{
        ebi_trait_activities::ToActivities, ebi_trait_graphable::ToGraphable,
        ebi_trait_semantics::ToSemantics,
    },
};

pub const EBI_PETRI_NET_MARKUP_LANGUAGE: EbiFileHandler = EbiFileHandler {
    name: "Petri net markup language",
    article: "a",
    file_extension: "pnml",
    is_binary: false,
    format_specification: &FORMAT_SPECIFICATION,
    validator: Some(PetriNetMarkupLanguage::validate),
    trait_importers: &[
        EbiTraitImporter::Activities(PetriNetMarkupLanguage::import_as_activities),
        EbiTraitImporter::Semantics(PetriNetMarkupLanguage::import_as_semantics),
        EbiTraitImporter::Graphable(PetriNetMarkupLanguage::import_as_graphable),
    ],
    object_importers: &[EbiObjectImporter::LabelledPetriNet(
        PetriNetMarkupLanguage::import_as_object,
    )],
    object_exporters: &[
        EbiObjectExporter::DeterministicFiniteAutomaton(PetriNetMarkupLanguage::export_from_object),
        EbiObjectExporter::DirectlyFollowsModel(PetriNetMarkupLanguage::export_from_object),
        EbiObjectExporter::StochasticDirectlyFollowsModel(
            PetriNetMarkupLanguage::export_from_object,
        ),
        EbiObjectExporter::DirectlyFollowsGraph(PetriNetMarkupLanguage::export_from_object),
        EbiObjectExporter::LabelledPetriNet(PetriNetMarkupLanguage::export_from_object),
        EbiObjectExporter::ProcessTree(PetriNetMarkupLanguage::export_from_object),
        EbiObjectExporter::StochasticProcessTree(PetriNetMarkupLanguage::export_from_object),
        EbiObjectExporter::StochasticLabelledPetriNet(PetriNetMarkupLanguage::export_from_object),
        EbiObjectExporter::StochasticDeterministicFiniteAutomaton(
            PetriNetMarkupLanguage::export_from_object,
        ),
    ],
    java_object_handlers: &[], //java translations covered by LabelledPetrinet
};

#[cfg(test)]
mod tests {
    use std::fs::File;

    use ebi_objects::PetriNetMarkupLanguage;

    use crate::{
        ebi_traits::ebi_trait_semantics::{EbiTraitSemantics, ToSemantics}, multiple_reader::MultipleReader,
    };

    #[test]
    fn pnml_empty() {
        let mut reader = MultipleReader::from_file(File::open("testfiles/empty.pnml").unwrap());
        let semantics =
            PetriNetMarkupLanguage::import_as_semantics(&mut reader.get().unwrap()).unwrap();

        if let EbiTraitSemantics::Marking(semantics) = semantics {
            let state = semantics.get_initial_state().unwrap();
            assert_eq!(semantics.get_enabled_transitions(&state).len(), 0);
        } else {
            assert!(false);
        }
    }
}
