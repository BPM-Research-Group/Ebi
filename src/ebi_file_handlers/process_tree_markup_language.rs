use ebi_objects::{
    Exportable, Importable, ProcessTreeMarkupLanguage,
    ebi_objects::process_tree_markup_language::FORMAT_SPECIFICATION,
};

use crate::{
    ebi_framework::{
        ebi_file_handler::EbiFileHandler,
        ebi_input::{EbiObjectImporter, EbiTraitImporter},
        ebi_output::EbiObjectExporter,
        object_importers::ToLabelledPetriNetObject,
        validate::Validate,
    },
    ebi_traits::{ebi_trait_graphable::ToGraphable, ebi_trait_semantics::ToSemantics},
};

pub const EBI_PROCESS_TREE_MARKUP_LANGUAGE: EbiFileHandler = EbiFileHandler {
    name: "process tree markup language",
    article: "a",
    file_extension: "ptml",
    is_binary: false,
    format_specification: &FORMAT_SPECIFICATION,
    validator: Some(ProcessTreeMarkupLanguage::validate),
    trait_importers: &[
        EbiTraitImporter::Semantics(ProcessTreeMarkupLanguage::import_as_semantics),
        EbiTraitImporter::Graphable(ProcessTreeMarkupLanguage::import_as_graphable),
    ],
    object_importers: &[
        EbiObjectImporter::ProcessTree(ProcessTreeMarkupLanguage::import_as_object),
        EbiObjectImporter::LabelledPetriNet(
            ProcessTreeMarkupLanguage::import_as_labelled_petri_net_object,
        ),
    ],
    object_exporters: &[
        EbiObjectExporter::ProcessTree(ProcessTreeMarkupLanguage::export_from_object),
        EbiObjectExporter::StochasticProcessTree(ProcessTreeMarkupLanguage::export_from_object),
    ],
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
