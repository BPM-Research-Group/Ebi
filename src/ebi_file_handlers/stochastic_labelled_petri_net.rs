use crate::{
    ebi_framework::{
        ebi_file_handler::EbiFileHandler,
        ebi_input::{EbiInput, EbiObjectImporter, EbiTraitImporter},
        ebi_output::EbiObjectExporter,
        ebi_trait::FromEbiTraitObject,
        object_importers::ImportAsLabelledPetriNetObject,
        trait_importers::{
            ImportAsActivitiesTrait, ImportAsGraphableTrait,
            ImportAsQueriableStochasticLanguageTrait, ImportAsSemanticsTrait,
            ImportAsStochasticDeterministicSemanticsTrait, ImportAsStochasticSemanticsTrait,
        },
        validate::Validate,
    },
    prom::java_object_handler::JavaObjectHandler,
};
use anyhow::{Result, anyhow};
use ebi_objects::{EbiObject, Exportable, Importable, StochasticLabelledPetriNet};

pub const EBI_STOCHASTIC_LABELLED_PETRI_NET: EbiFileHandler = EbiFileHandler {
    name: "stochastic labelled Petri net",
    article: "a",
    file_extension: "slpn",
    is_binary: false,
    format_specification: StochasticLabelledPetriNet::FILE_FORMAT_SPECIFICATION_LATEX,
    validator: Some(StochasticLabelledPetriNet::validate),
    trait_importers: &[
        EbiTraitImporter::Activities(
            StochasticLabelledPetriNet::import_as_activities_trait,
            StochasticLabelledPetriNet::IMPORTER_PARAMETERS,
        ),
        EbiTraitImporter::QueriableStochasticLanguage(
            StochasticLabelledPetriNet::import_as_queriable_stochastic_language_trait,
            StochasticLabelledPetriNet::IMPORTER_PARAMETERS,
        ),
        EbiTraitImporter::StochasticDeterministicSemantics(
            StochasticLabelledPetriNet::import_as_stochastic_deterministic_semantics_trait,
            StochasticLabelledPetriNet::IMPORTER_PARAMETERS,
        ),
        EbiTraitImporter::StochasticSemantics(
            StochasticLabelledPetriNet::import_as_stochastic_semantics_trait,
            StochasticLabelledPetriNet::IMPORTER_PARAMETERS,
        ),
        EbiTraitImporter::Semantics(
            StochasticLabelledPetriNet::import_as_semantics_trait,
            StochasticLabelledPetriNet::IMPORTER_PARAMETERS,
        ),
        EbiTraitImporter::Graphable(
            StochasticLabelledPetriNet::import_as_graphable_trait,
            StochasticLabelledPetriNet::IMPORTER_PARAMETERS,
        ),
    ],
    object_importers: &[
        EbiObjectImporter::StochasticLabelledPetriNet(
            StochasticLabelledPetriNet::import_as_object,
            StochasticLabelledPetriNet::IMPORTER_PARAMETERS,
        ),
        EbiObjectImporter::LabelledPetriNet(
            StochasticLabelledPetriNet::import_as_labelled_petri_net_object,
            StochasticLabelledPetriNet::IMPORTER_PARAMETERS,
        ),
    ],
    object_exporters: &[
        EbiObjectExporter::StochasticLabelledPetriNet(
            StochasticLabelledPetriNet::export_from_object,
        ),
        EbiObjectExporter::StochasticNondeterministicFiniteAutomaton(
            StochasticLabelledPetriNet::export_from_object,
        ),
        EbiObjectExporter::StochasticDeterministicFiniteAutomaton(
            StochasticLabelledPetriNet::export_from_object,
        ),
        EbiObjectExporter::StochasticDirectlyFollowsModel(
            StochasticLabelledPetriNet::export_from_object,
        ),
        EbiObjectExporter::DirectlyFollowsGraph(StochasticLabelledPetriNet::export_from_object),
    ],
    object_exporters_fallible: &[],
    java_object_handlers: &[JavaObjectHandler {
        name: "StochasticLabelledPetriNet",
        translator_ebi_to_java: Some(
            "org.processmining.ebi.objects.EbiStochasticLabelledPetriNet.EbiString2StochasticLabelledPetriNet",
        ),
        translator_java_to_ebi: Some(
            "org.processmining.ebi.objects.EbiStochasticLabelledPetriNet.StochasticLabelledPetriNet2EbiString",
        ),
        java_class: "org.processmining.stochasticlabelledpetrinets.StochasticLabelledPetriNetSimpleWeights",
        input_gui: None,
    }],
};

impl FromEbiTraitObject for StochasticLabelledPetriNet {
    fn from_trait_object(object: EbiInput) -> Result<Box<Self>> {
        match object {
            EbiInput::Object(EbiObject::StochasticLabelledPetriNet(e), _) => Ok(Box::new(e)),
            _ => Err(anyhow!(
                "cannot read {} {} as a stochastic labelled Petri net",
                object.get_type().get_article(),
                object.get_type()
            )),
        }
    }
}

#[cfg(test)]
mod tests {
    use std::fs;

    use ebi_objects::StochasticLabelledPetriNet;

    use crate::semantics::semantics::Semantics;

    #[test]
    fn empty_slpn() {
        let fin = fs::read_to_string("testfiles/empty_net.slpn").unwrap();
        let slpn = fin.parse::<StochasticLabelledPetriNet>().unwrap();

        assert_eq!(slpn.get_number_of_places(), 0);
        assert_eq!(slpn.get_number_of_transitions(), 0);
    }
}
