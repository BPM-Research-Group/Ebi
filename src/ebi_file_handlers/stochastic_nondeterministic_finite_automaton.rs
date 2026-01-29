use crate::ebi_framework::{
    ebi_file_handler::EbiFileHandler,
    ebi_input::{EbiInput, EbiObjectImporter, EbiTraitImporter},
    ebi_output::EbiObjectExporter,
    ebi_trait::FromEbiTraitObject,
    object_importers::ImportAsStochasticLabelledPetriNetObject,
    trait_importers::{
        ImportAsActivitiesTrait, ImportAsGraphableTrait, ImportAsSemanticsTrait,
        ImportAsStochasticDeterministicSemanticsTrait, ImportAsStochasticSemanticsTrait,
    },
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
    trait_importers: &[
        EbiTraitImporter::Activities(
            StochasticNondeterministicFiniteAutomaton::import_as_activities_trait,
            StochasticNondeterministicFiniteAutomaton::IMPORTER_PARAMETERS,
        ),
        // EbiTraitImporter::QueriableStochasticLanguage(
        //     StochasticNondeterministicFiniteAutomaton::import_as_queriable_stochastic_language_trait,
        //     StochasticNondeterministicFiniteAutomaton::IMPORTER_PARAMETERS,
        // ),
        EbiTraitImporter::StochasticDeterministicSemantics(
            StochasticNondeterministicFiniteAutomaton::import_as_stochastic_deterministic_semantics_trait,
            StochasticNondeterministicFiniteAutomaton::IMPORTER_PARAMETERS,
        ),
        EbiTraitImporter::StochasticSemantics(
            StochasticNondeterministicFiniteAutomaton::import_as_stochastic_semantics_trait,
            StochasticNondeterministicFiniteAutomaton::IMPORTER_PARAMETERS,
        ),
        EbiTraitImporter::Semantics(
            StochasticNondeterministicFiniteAutomaton::import_as_semantics_trait,
            StochasticNondeterministicFiniteAutomaton::IMPORTER_PARAMETERS,
        ),
        EbiTraitImporter::Graphable(
            StochasticNondeterministicFiniteAutomaton::import_as_graphable_trait,
            StochasticNondeterministicFiniteAutomaton::IMPORTER_PARAMETERS,
        ),
    ],
    object_importers: &[
        EbiObjectImporter::StochasticNondeterministicFiniteAutomaton(
            StochasticNondeterministicFiniteAutomaton::import_as_object,
            StochasticNondeterministicFiniteAutomaton::IMPORTER_PARAMETERS,
        ),
        EbiObjectImporter::StochasticLabelledPetriNet(
            StochasticNondeterministicFiniteAutomaton::import_as_stochastic_labelled_petri_net_object,
            StochasticNondeterministicFiniteAutomaton::IMPORTER_PARAMETERS,
        ),
    ],
    object_exporters: &[
        EbiObjectExporter::StochasticNondeterministicFiniteAutomaton(
            StochasticNondeterministicFiniteAutomaton::export_from_object,
        ),
        EbiObjectExporter::StochasticDeterministicFiniteAutomaton(StochasticNondeterministicFiniteAutomaton::export_from_object),
        EbiObjectExporter::FiniteStochasticLanguage(StochasticNondeterministicFiniteAutomaton::export_from_object),
        EbiObjectExporter::StochasticProcessTree(StochasticNondeterministicFiniteAutomaton::export_from_object),
        EbiObjectExporter::EventLog(StochasticNondeterministicFiniteAutomaton::export_from_object),
        EbiObjectExporter::EventLogTraceAttributes(StochasticNondeterministicFiniteAutomaton::export_from_object),
        EbiObjectExporter::EventLogXes(StochasticNondeterministicFiniteAutomaton::export_from_object),
        EbiObjectExporter::EventLogCsv(StochasticNondeterministicFiniteAutomaton::export_from_object),
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
