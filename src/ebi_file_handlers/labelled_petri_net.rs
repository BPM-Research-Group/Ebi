use anyhow::{Ok, Result, anyhow};
use ebi_objects::{ebi_objects::labelled_petri_net::FORMAT_SPECIFICATION, EbiObject, Exportable, Importable, LabelledPetriNet};

use crate::{
    ebi_framework::{
        ebi_file_handler::EbiFileHandler,
        ebi_input::{EbiInput, EbiObjectImporter, EbiTraitImporter},
        ebi_output::EbiObjectExporter,
        ebi_trait::FromEbiTraitObject,
        prom_link::JavaObjectHandler,
        validate::Validate,
    },
    ebi_traits::{
        ebi_trait_activities::ToActivities, ebi_trait_graphable::ToGraphable,
        ebi_trait_semantics::ToSemantics,
    },
};

pub const EBI_LABELLED_PETRI_NET: EbiFileHandler = EbiFileHandler {
    name: "labelled Petri net",
    article: "a",
    file_extension: "lpn",
    is_binary: false,
    format_specification: &FORMAT_SPECIFICATION,
    validator: Some(LabelledPetriNet::validate),
    trait_importers: &[
        EbiTraitImporter::Activities(LabelledPetriNet::import_as_activities),
        EbiTraitImporter::Semantics(LabelledPetriNet::import_as_semantics),
        EbiTraitImporter::Graphable(LabelledPetriNet::import_as_graphable),
    ],
    object_importers: &[EbiObjectImporter::LabelledPetriNet(
        LabelledPetriNet::import_as_object,
    )],
    object_exporters: &[
        EbiObjectExporter::DeterministicFiniteAutomaton(LabelledPetriNet::export_from_object),
        EbiObjectExporter::DirectlyFollowsGraph(LabelledPetriNet::export_from_object),
        EbiObjectExporter::DirectlyFollowsModel(LabelledPetriNet::export_from_object),
        EbiObjectExporter::StochasticDirectlyFollowsModel(LabelledPetriNet::export_from_object),
        EbiObjectExporter::LabelledPetriNet(LabelledPetriNet::export_from_object),
        EbiObjectExporter::ProcessTree(LabelledPetriNet::export_from_object),
        EbiObjectExporter::StochasticLabelledPetriNet(LabelledPetriNet::export_from_object),
        EbiObjectExporter::StochasticDeterministicFiniteAutomaton(
            LabelledPetriNet::export_from_object,
        ),
    ],
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
