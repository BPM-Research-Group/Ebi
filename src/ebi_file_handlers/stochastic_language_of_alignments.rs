use anyhow::{Result, anyhow};
use ebi_objects::{
    EbiObject, Exportable, Importable, StochasticLanguageOfAlignments,
    ebi_objects::stochastic_language_of_alignments::FORMAT_SPECIFICATION,
};

use crate::{
    ebi_framework::{
        ebi_file_handler::EbiFileHandler,
        ebi_input::{EbiInput, EbiObjectImporter, EbiTraitImporter},
        ebi_output::EbiObjectExporter,
        ebi_trait::FromEbiTraitObject, validate::Validate,
    },
    ebi_traits::ebi_trait_activities::ToActivities,
};

pub const EBI_STOCHASTIC_LANGUAGE_OF_ALIGNMENTS: EbiFileHandler = EbiFileHandler {
    name: "stochastic language of alignments",
    article: "a",
    file_extension: "sali",
    is_binary: false,
    format_specification: &FORMAT_SPECIFICATION,
    validator: Some(StochasticLanguageOfAlignments::validate),
    trait_importers: &[EbiTraitImporter::Activities(
        StochasticLanguageOfAlignments::import_as_activities,
    )],
    object_importers: &[EbiObjectImporter::StochasticLanguageOfAlignments(
        StochasticLanguageOfAlignments::import_as_object,
    )],
    object_exporters: &[EbiObjectExporter::StochasticLanguageOfAlignments(
        StochasticLanguageOfAlignments::export_from_object,
    )],
    java_object_handlers: &[],
};

impl FromEbiTraitObject for StochasticLanguageOfAlignments {
    fn from_trait_object(object: EbiInput) -> Result<Box<Self>> {
        match object {
            EbiInput::Object(EbiObject::StochasticLanguageOfAlignments(e), _) => Ok(Box::new(e)),
            _ => Err(anyhow!(
                "cannot read {} {} as a finite language",
                object.get_type().get_article(),
                object.get_type()
            )),
        }
    }
}
