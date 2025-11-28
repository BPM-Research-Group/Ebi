use crate::{
    ebi_framework::{
        ebi_file_handler::EbiFileHandler,
        ebi_input::{EbiInput, EbiObjectImporter, EbiTraitImporter},
        ebi_output::EbiObjectExporter,
        ebi_trait::FromEbiTraitObject,
        object_importers::ImportAsLabelledPetriNetObject,
        trait_importers::{
            ImportAsActivitiesTrait, ImportAsGraphableTrait, ImportAsSemanticsTrait,
        },
        validate::Validate,
    },
    prom::java_object_handler::JavaObjectHandler,
};
use anyhow::{Result, anyhow};
use ebi_objects::{EbiObject, Exportable, Importable, ProcessTree};

pub const EBI_PROCESS_TREE: EbiFileHandler = EbiFileHandler {
    name: "process tree",
    article: "a",
    file_extension: "ptree",
    is_binary: false,
    format_specification: ProcessTree::FILE_FORMAT_SPECIFICATION_LATEX,
    validator: Some(ProcessTree::validate),
    trait_importers: &[
        EbiTraitImporter::Activities(
            ProcessTree::import_as_activities_trait,
            ProcessTree::IMPORTER_PARAMETERS,
        ),
        EbiTraitImporter::Semantics(
            ProcessTree::import_as_semantics_trait,
            ProcessTree::IMPORTER_PARAMETERS,
        ),
        EbiTraitImporter::Graphable(
            ProcessTree::import_as_graphable_trait,
            ProcessTree::IMPORTER_PARAMETERS,
        ),
    ],
    object_importers: &[
        EbiObjectImporter::ProcessTree(
            ProcessTree::import_as_object,
            ProcessTree::IMPORTER_PARAMETERS,
        ),
        EbiObjectImporter::LabelledPetriNet(
            ProcessTree::import_as_labelled_petri_net_object,
            ProcessTree::IMPORTER_PARAMETERS,
        ),
    ],
    object_exporters: &[
        EbiObjectExporter::ProcessTree(ProcessTree::export_from_object),
        EbiObjectExporter::StochasticProcessTree(ProcessTree::export_from_object),
    ],
    object_exporters_fallible: &[],
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
