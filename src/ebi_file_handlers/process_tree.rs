use anyhow::{Result, anyhow};
use ebi_objects::{
    EbiObject, Exportable, Importable, ProcessTree, ebi_objects::process_tree::FORMAT_SPECIFICATION,
};

use crate::{
    ebi_framework::{
        ebi_file_handler::EbiFileHandler,
        ebi_input::{EbiInput, EbiObjectImporter, EbiTraitImporter},
        ebi_output::EbiObjectExporter,
        ebi_trait::FromEbiTraitObject,
        object_importers::ToLabelledPetriNetObject,
        prom_link::JavaObjectHandler,
        validate::Validate,
    },
    ebi_traits::{
        ebi_trait_activities::ToActivities, ebi_trait_graphable::ToGraphable,
        ebi_trait_semantics::ToSemantics,
    },
};

pub const EBI_PROCESS_TREE: EbiFileHandler = EbiFileHandler {
    name: "process tree",
    article: "a",
    file_extension: "ptree",
    is_binary: false,
    format_specification: &FORMAT_SPECIFICATION,
    validator: Some(ProcessTree::validate),
    trait_importers: &[
        EbiTraitImporter::Activities(ProcessTree::import_as_activities),
        EbiTraitImporter::Semantics(ProcessTree::import_as_semantics),
        EbiTraitImporter::Graphable(ProcessTree::import_as_graphable),
    ],
    object_importers: &[
        EbiObjectImporter::ProcessTree(ProcessTree::import_as_object),
        EbiObjectImporter::LabelledPetriNet(ProcessTree::import_as_labelled_petri_net_object),
    ],
    object_exporters: &[EbiObjectExporter::ProcessTree(
        ProcessTree::export_from_object,
    )],
    java_object_handlers: &[JavaObjectHandler {
        name: "ProcessTree",
        java_class: "org.processmining.plugins.InductiveMiner.efficienttree.EfficientTree",
        translator_ebi_to_java: Some(
            "org.processmining.ebi.objects.EbiProcessTree.EbiString2EfficientTree",
        ),
        translator_java_to_ebi: Some(
            "org.processmining.ebi.objects.EbiProcessTree.EfficientTree2EbiString",
        ),
        input_gui: None,
    }],
};

impl FromEbiTraitObject for ProcessTree {
    fn from_trait_object(object: EbiInput) -> Result<Box<Self>> {
        match object {
            EbiInput::Object(EbiObject::ProcessTree(e), _) => Ok(Box::new(e)),
            _ => Err(anyhow!(
                "cannot read {} {} as a process tree",
                object.get_type().get_article(),
                object.get_type()
            )),
        }
    }
}
