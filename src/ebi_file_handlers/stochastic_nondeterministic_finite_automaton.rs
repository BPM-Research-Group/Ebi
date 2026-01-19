use crate::ebi_framework::{
    ebi_file_handler::EbiFileHandler,
    ebi_input::{EbiInput, EbiObjectImporter},
    ebi_output::EbiObjectExporter,
    ebi_trait::FromEbiTraitObject,
    validate::Validate,
};
use anyhow::{Result, anyhow};
use ebi_objects::{EbiObject, Exportable, Importable, StochasticNondeterministicFiniteAutomaton};

pub const EBI_STOCHASTIC_NONDETERMINISTIC_FINITE_AUTOMATON: EbiFileHandler = EbiFileHandler {
    name: "stochastic non-deterministic finite automaton",
    article: "a",
    file_extension: "snfa",
    is_binary: false,
    format_specification:
        StochasticNondeterministicFiniteAutomaton::FILE_FORMAT_SPECIFICATION_LATEX,
    validator: Some(StochasticNondeterministicFiniteAutomaton::validate),
    trait_importers: &[],
    object_importers: &[
        EbiObjectImporter::StochasticNondeterministicFiniteAutomaton(
            StochasticNondeterministicFiniteAutomaton::import_as_object,
            StochasticNondeterministicFiniteAutomaton::IMPORTER_PARAMETERS,
        ),
    ],
    object_exporters: &[
        EbiObjectExporter::StochasticNondeterministicFiniteAutomaton(
            StochasticNondeterministicFiniteAutomaton::export_from_object,
        ),
    ],
    object_exporters_fallible: &[],
    java_object_handlers: &[],
};

impl FromEbiTraitObject for StochasticNondeterministicFiniteAutomaton {
    fn from_trait_object(object: EbiInput) -> Result<Box<Self>> {
        match object {
            EbiInput::Object(EbiObject::StochasticNondeterministicFiniteAutomaton(e), _) => {
                Ok(Box::new(e))
            }
            _ => Err(anyhow!(
                "cannot read {} {} as a stochastic non-deterministic finite automaton",
                object.get_type().get_article(),
                object.get_type()
            )),
        }
    }
}
