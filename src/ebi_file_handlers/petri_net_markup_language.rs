use crate::ebi_framework::{
    ebi_file_handler::EbiFileHandler,
    ebi_input::{EbiObjectImporter, EbiTraitImporter},
    ebi_output::EbiObjectExporter,
    trait_importers::{ImportAsActivitiesTrait, ImportAsGraphableTrait, ImportAsSemanticsTrait},
    validate::Validate,
};
use ebi_objects::{Exportable, Importable, PetriNetMarkupLanguage};

pub const EBI_PETRI_NET_MARKUP_LANGUAGE: EbiFileHandler = EbiFileHandler {
    name: "Petri net markup language",
    article: "a",
    file_extension: "pnml",
    is_binary: false,
    format_specification: PetriNetMarkupLanguage::FILE_FORMAT_SPECIFICATION_LATEX,
    validator: Some(PetriNetMarkupLanguage::validate),
    trait_importers: &[
        EbiTraitImporter::Activities(
            PetriNetMarkupLanguage::import_as_activities_trait,
            PetriNetMarkupLanguage::IMPORTER_PARAMETERS,
        ),
        EbiTraitImporter::Semantics(
            PetriNetMarkupLanguage::import_as_semantics_trait,
            PetriNetMarkupLanguage::IMPORTER_PARAMETERS,
        ),
        EbiTraitImporter::Graphable(
            PetriNetMarkupLanguage::import_as_graphable_trait,
            PetriNetMarkupLanguage::IMPORTER_PARAMETERS,
        ),
    ],
    object_importers: &[EbiObjectImporter::LabelledPetriNet(
        PetriNetMarkupLanguage::import_as_object,
        PetriNetMarkupLanguage::IMPORTER_PARAMETERS,
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
    object_exporters_fallible: &[],
    java_object_handlers: &[], //java translations covered by LabelledPetrinet
};

#[cfg(test)]
mod tests {
    use crate::{
        ebi_framework::trait_importers::ImportAsSemanticsTrait,
        ebi_traits::ebi_trait_semantics::EbiTraitSemantics, multiple_reader::MultipleReader,
    };
    use ebi_objects::{Importable, PetriNetMarkupLanguage};
    use std::fs::File;

    #[test]
    fn pnml_empty() {
        let mut reader = MultipleReader::from_file(File::open("testfiles/empty.pnml").unwrap());
        let semantics = PetriNetMarkupLanguage::import_as_semantics_trait(
            &mut reader.get().unwrap(),
            &PetriNetMarkupLanguage::default_importer_parameter_values(),
        )
        .unwrap();

        if let EbiTraitSemantics::Marking(semantics) = semantics {
            let state = semantics.get_initial_state().unwrap();
            assert_eq!(semantics.get_enabled_transitions(&state).len(), 0);
        } else {
            assert!(false);
        }
    }
}
