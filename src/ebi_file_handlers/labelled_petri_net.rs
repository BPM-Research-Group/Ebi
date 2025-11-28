use crate::{
    ebi_framework::{
        ebi_file_handler::EbiFileHandler,
        ebi_input::{EbiInput, EbiObjectImporter, EbiTraitImporter},
        ebi_output::EbiObjectExporter,
        ebi_trait::FromEbiTraitObject,
        trait_importers::{
            ImportAsActivitiesTrait, ImportAsGraphableTrait, ImportAsSemanticsTrait,
        },
        validate::Validate,
    },
    prom::java_object_handler::JavaObjectHandler,
};
use anyhow::{Ok, Result, anyhow};
use ebi_objects::{EbiObject, Exportable, Importable, LabelledPetriNet};

pub const EBI_LABELLED_PETRI_NET: EbiFileHandler = EbiFileHandler {
    name: "labelled Petri net",
    article: "a",
    file_extension: "lpn",
    is_binary: false,
    format_specification: LabelledPetriNet::FILE_FORMAT_SPECIFICATION_LATEX,
    validator: Some(LabelledPetriNet::validate),
    trait_importers: &[
        EbiTraitImporter::Activities(
            LabelledPetriNet::import_as_activities_trait,
            LabelledPetriNet::IMPORTER_PARAMETERS,
        ),
        EbiTraitImporter::Semantics(
            LabelledPetriNet::import_as_semantics_trait,
            LabelledPetriNet::IMPORTER_PARAMETERS,
        ),
        EbiTraitImporter::Graphable(
            LabelledPetriNet::import_as_graphable_trait,
            LabelledPetriNet::IMPORTER_PARAMETERS,
        ),
    ],
    object_importers: &[EbiObjectImporter::LabelledPetriNet(
        LabelledPetriNet::import_as_object,
        LabelledPetriNet::IMPORTER_PARAMETERS,
    )],
    object_exporters: &[
        EbiObjectExporter::DeterministicFiniteAutomaton(LabelledPetriNet::export_from_object),
        EbiObjectExporter::DirectlyFollowsGraph(LabelledPetriNet::export_from_object),
        EbiObjectExporter::DirectlyFollowsModel(LabelledPetriNet::export_from_object),
        EbiObjectExporter::StochasticDirectlyFollowsModel(LabelledPetriNet::export_from_object),
        EbiObjectExporter::LabelledPetriNet(LabelledPetriNet::export_from_object),
        EbiObjectExporter::ProcessTree(LabelledPetriNet::export_from_object),
        EbiObjectExporter::StochasticProcessTree(LabelledPetriNet::export_from_object),
        EbiObjectExporter::StochasticLabelledPetriNet(LabelledPetriNet::export_from_object),
        EbiObjectExporter::StochasticDeterministicFiniteAutomaton(
            LabelledPetriNet::export_from_object,
        ),
    ],
    object_exporters_fallible: &[],
    java_object_handlers: &[
        JavaObjectHandler {
            name: "PetriNet",
            translator_ebi_to_java: Some(
                "org.processmining.ebi.objects.EbiLabelledPetriNet.EbiString2Petrinet",
            ),
            translator_java_to_ebi: None, //Some("org.processmining.ebi.objects.EbiLabelledPetriNet.PetriNet2EbiString"),
            java_class: "org.processmining.models.graphbased.directed.petrinet.Petrinet",
            input_gui: None,
        },
        JavaObjectHandler {
            name: "AcceptingPetriNet",
            translator_ebi_to_java: None,
            translator_java_to_ebi: Some(
                "org.processmining.ebi.objects.EbiLabelledPetriNet.AcceptingPetriNet2EbiString",
            ),
            java_class: "org.processmining.acceptingpetrinet.models.AcceptingPetriNet",
            input_gui: None,
        },
    ],
};

impl FromEbiTraitObject for LabelledPetriNet {
    fn from_trait_object(object: EbiInput) -> Result<Box<Self>> {
        match object {
            EbiInput::Object(EbiObject::LabelledPetriNet(e), _) => Ok(Box::new(e)),
            _ => Err(anyhow!(
                "cannot read {} {} as a labelled Petri net",
                object.get_type().get_article(),
                object.get_type()
            )),
        }
    }
}
