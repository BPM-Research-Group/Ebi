use crate::ebi_framework::{
    ebi_file_handler::EbiFileHandler,
    ebi_input::{EbiObjectImporter, EbiTraitImporter},
    ebi_output::EbiObjectExporter,
    object_importers::ImportAsLabelledPetriNetObject,
    trait_importers::{ImportAsActivitiesTrait, ImportAsGraphableTrait, ImportAsSemanticsTrait},
    validate::Validate,
};
use ebi_objects::{Exportable, Importable, ProcessTreeMarkupLanguage};

pub const EBI_PROCESS_TREE_MARKUP_LANGUAGE: EbiFileHandler = EbiFileHandler {
    name: "process tree markup language",
    article: "a",
    file_extension: "ptml",
    is_binary: false,
    format_specification: ProcessTreeMarkupLanguage::FILE_FORMAT_SPECIFICATION_LATEX,
    validator: Some(ProcessTreeMarkupLanguage::validate),
    trait_importers: &[
        EbiTraitImporter::Activities(
            ProcessTreeMarkupLanguage::import_as_activities_trait,
            ProcessTreeMarkupLanguage::IMPORTER_PARAMETERS,
        ),
        EbiTraitImporter::Semantics(
            ProcessTreeMarkupLanguage::import_as_semantics_trait,
            ProcessTreeMarkupLanguage::IMPORTER_PARAMETERS,
        ),
        EbiTraitImporter::Graphable(
            ProcessTreeMarkupLanguage::import_as_graphable_trait,
            ProcessTreeMarkupLanguage::IMPORTER_PARAMETERS,
        ),
    ],
    object_importers: &[
        EbiObjectImporter::ProcessTree(ProcessTreeMarkupLanguage::import_as_object, &[]),
        EbiObjectImporter::LabelledPetriNet(
            ProcessTreeMarkupLanguage::import_as_labelled_petri_net_object,
            ProcessTreeMarkupLanguage::IMPORTER_PARAMETERS,
        ),
    ],
    object_exporters: &[
        EbiObjectExporter::ProcessTree(ProcessTreeMarkupLanguage::export_from_object),
        EbiObjectExporter::StochasticProcessTree(ProcessTreeMarkupLanguage::export_from_object),
    ],
    object_exporters_fallible: &[],
    java_object_handlers: &[], //java object handlers are through processtree
};

#[cfg(test)]
mod tests {
    use std::fs::{self};

    use ebi_objects::ProcessTreeMarkupLanguage;

    use crate::semantics::semantics::Semantics;

    #[test]
    fn nested_ptml() {
        let fin = fs::read_to_string("testfiles/valid nested.ptml").unwrap();
        let ptml = fin.parse::<ProcessTreeMarkupLanguage>().unwrap();

        let sem = ptml.tree;
        let state = sem.get_initial_state().unwrap();
        assert_eq!(sem.get_enabled_transitions(&state).len(), 1);
    }
}
