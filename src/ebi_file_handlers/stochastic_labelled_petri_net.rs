use ebi_objects::{
    Exportable, Importable, StochasticLabelledPetriNet,
    ebi_objects::stochastic_labelled_petri_net::FORMAT_SPECIFICATION,
};

use crate::{
    ebi_framework::{
        ebi_file_handler::EbiFileHandler,
        ebi_input::{EbiObjectImporter, EbiTraitImporter},
        ebi_output::EbiObjectExporter,
        object_importers::ToLabelledPetriNetObject,
        prom_link::JavaObjectHandler,
        validate::Validate,
    },
    ebi_traits::{
        ebi_trait_activities::ToActivities, ebi_trait_graphable::ToGraphable,
        ebi_trait_queriable_stochastic_language::ToQueriableStochasticLanguage,
        ebi_trait_semantics::ToSemantics,
        ebi_trait_stochastic_deterministic_semantics::ToStochasticDeterministicSemantics,
        ebi_trait_stochastic_semantics::ToStochasticSemantics,
    },
};

pub const EBI_STOCHASTIC_LABELLED_PETRI_NET: EbiFileHandler = EbiFileHandler {
    name: "stochastic labelled Petri net",
    article: "a",
    file_extension: "slpn",
    is_binary: false,
    format_specification: &FORMAT_SPECIFICATION,
    validator: Some(StochasticLabelledPetriNet::validate),
    trait_importers: &[
        EbiTraitImporter::Activities(StochasticLabelledPetriNet::import_as_activities),
        EbiTraitImporter::QueriableStochasticLanguage(
            StochasticLabelledPetriNet::import_as_queriable_stochastic_language,
        ),
        EbiTraitImporter::StochasticDeterministicSemantics(
            StochasticLabelledPetriNet::import_as_stochastic_deterministic_semantics,
        ),
        EbiTraitImporter::StochasticSemantics(
            StochasticLabelledPetriNet::import_as_stochastic_semantics,
        ),
        EbiTraitImporter::Semantics(StochasticLabelledPetriNet::import_as_semantics),
        EbiTraitImporter::Graphable(StochasticLabelledPetriNet::import_as_graphable),
    ],
    object_importers: &[
        EbiObjectImporter::StochasticLabelledPetriNet(StochasticLabelledPetriNet::import_as_object),
        EbiObjectImporter::LabelledPetriNet(
            StochasticLabelledPetriNet::import_as_labelled_petri_net_object,
        ),
    ],
    object_exporters: &[
        EbiObjectExporter::StochasticLabelledPetriNet(
            StochasticLabelledPetriNet::export_from_object,
        ),
        EbiObjectExporter::StochasticDirectlyFollowsModel(
            StochasticLabelledPetriNet::export_from_object,
        ),
        EbiObjectExporter::DirectlyFollowsGraph(StochasticLabelledPetriNet::export_from_object),
    ],
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
