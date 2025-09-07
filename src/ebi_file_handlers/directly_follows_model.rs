use ebi_objects::{
    DirectlyFollowsModel, Exportable, Importable,
    ebi_objects::directly_follows_model::FORMAT_SPECIFICATION,
};

use crate::{
    ebi_framework::{
        ebi_file_handler::EbiFileHandler,
        ebi_input::{EbiObjectImporter, EbiTraitImporter},
        ebi_output::EbiObjectExporter,
        object_importers::ToLabelledPetriNetObject,
        validate::Validate,
    },
    ebi_traits::{
        ebi_trait_activities::ToActivities, ebi_trait_graphable::ToGraphable,
        ebi_trait_semantics::ToSemantics,
    },
};

pub const EBI_DIRECTLY_FOLLOWS_MODEL: EbiFileHandler = EbiFileHandler {
    name: "directly follows model",
    article: "a",
    file_extension: "dfm",
    is_binary: false,
    format_specification: &FORMAT_SPECIFICATION,
    validator: Some(DirectlyFollowsModel::validate),
    trait_importers: &[
        EbiTraitImporter::Activities(DirectlyFollowsModel::import_as_activities),
        EbiTraitImporter::Semantics(DirectlyFollowsModel::import_as_semantics),
        EbiTraitImporter::Graphable(DirectlyFollowsModel::import_as_graphable),
    ],
    object_importers: &[
        EbiObjectImporter::DirectlyFollowsModel(DirectlyFollowsModel::import_as_object),
        EbiObjectImporter::LabelledPetriNet(
            DirectlyFollowsModel::import_as_labelled_petri_net_object,
        ),
    ],
    object_exporters: &[
        EbiObjectExporter::DirectlyFollowsModel(DirectlyFollowsModel::export_from_object),
        EbiObjectExporter::StochasticDirectlyFollowsModel(DirectlyFollowsModel::export_from_object),
        EbiObjectExporter::DirectlyFollowsGraph(DirectlyFollowsModel::export_from_object),
    ],
    java_object_handlers: &[],
};

#[cfg(test)]
mod tests {
    use std::fs;

    use ebi_objects::{ActivityKey, DirectlyFollowsModel, HasActivityKey, TranslateActivityKey};

    #[test]
    fn activity_key_translating() {
        let fin = fs::read_to_string("testfiles/a-b_star.dfm").unwrap();
        let mut dfm = fin.parse::<DirectlyFollowsModel>().unwrap();

        let mut activity_key = ActivityKey::new();
        let x = activity_key.process_activity("xyz");

        dfm.translate_using_activity_key(&mut activity_key);

        assert_eq!(
            dfm.activity_key().get_activity_label(&x),
            activity_key.get_activity_label(&x)
        );
    }
}
