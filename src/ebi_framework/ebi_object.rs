use anyhow::Result;
use std::{
    collections::{BTreeSet, HashSet},
    fmt::Display,
};
use strum_macros::EnumIter;

use crate::{
    ebi_objects::{
        deterministic_finite_automaton::DeterministicFiniteAutomaton,
        directly_follows_model::DirectlyFollowsModel, event_log::EventLog, executions::Executions,
        finite_language::FiniteLanguage, finite_stochastic_language::FiniteStochasticLanguage,
        labelled_petri_net::LabelledPetriNet, language_of_alignments::LanguageOfAlignments,
        process_tree::ProcessTree,
        stochastic_deterministic_finite_automaton::StochasticDeterministicFiniteAutomaton,
        stochastic_labelled_petri_net::StochasticLabelledPetriNet,
        stochastic_language_of_alignments::StochasticLanguageOfAlignments,
        stochastic_process_tree::StochasticProcessTree,
    },
    ebi_traits::{
        ebi_trait_event_log::EbiTraitEventLog, ebi_trait_finite_language::EbiTraitFiniteLanguage,
        ebi_trait_finite_stochastic_language::EbiTraitFiniteStochasticLanguage,
        ebi_trait_graphable::EbiTraitGraphable,
        ebi_trait_iterable_language::EbiTraitIterableLanguage,
        ebi_trait_iterable_stochastic_language::EbiTraitIterableStochasticLanguage,
        ebi_trait_queriable_stochastic_language::EbiTraitQueriableStochasticLanguage,
        ebi_trait_semantics::EbiTraitSemantics,
        ebi_trait_stochastic_deterministic_semantics::EbiTraitStochasticDeterministicSemantics,
        ebi_trait_stochastic_semantics::EbiTraitStochasticSemantics,
    },
};

use super::{
    ebi_command::{EBI_COMMANDS, EbiCommand},
    ebi_file_handler::{EBI_FILE_HANDLERS, EbiFileHandler},
    ebi_input::EbiInputType,
    ebi_output::EbiOutputType,
    ebi_trait::EbiTrait,
    infoable::Infoable,
    prom_link::JavaObjectHandler,
};

#[derive(PartialEq, Clone, EnumIter, Hash, Default, Debug)]
pub enum EbiObjectType {
    #[default]
    LanguageOfAlignments,
    StochasticLanguageOfAlignments,
    StochasticDeterministicFiniteAutomaton,
    DeterministicFiniteAutomaton,
    DirectlyFollowsModel,
    EventLog,
    FiniteLanguage,
    FiniteStochasticLanguage,
    LabelledPetriNet,
    StochasticLabelledPetriNet,
    ProcessTree,
    StochasticProcessTree,
    Executions,
}

impl EbiObjectType {
    pub fn get_article(&self) -> &str {
        match self {
            EbiObjectType::LabelledPetriNet => "a",
            EbiObjectType::StochasticLabelledPetriNet => "a",
            EbiObjectType::FiniteStochasticLanguage => "a",
            EbiObjectType::StochasticDeterministicFiniteAutomaton => "a",
            EbiObjectType::EventLog => "an",
            EbiObjectType::FiniteLanguage => "a",
            EbiObjectType::DirectlyFollowsModel => "a",
            EbiObjectType::LanguageOfAlignments => "",
            EbiObjectType::StochasticLanguageOfAlignments => "an",
            EbiObjectType::DeterministicFiniteAutomaton => "a",
            EbiObjectType::ProcessTree => "a",
            EbiObjectType::StochasticProcessTree => "a",
            EbiObjectType::Executions => "",
        }
    }

    pub fn get_applicable_commands(&self) -> BTreeSet<Vec<&'static EbiCommand>> {
        let mut result = EBI_COMMANDS.get_command_paths();
        result.retain(|path| {
            if let EbiCommand::Command { input_types, .. } = path[path.len() - 1] {
                for input_typess in input_types.iter() {
                    for input_typesss in input_typess.iter() {
                        if input_typesss == &&EbiInputType::AnyObject
                            || input_typesss == &&EbiInputType::Object(self.clone())
                        {
                            return true;
                        }
                    }
                }
            }
            false
        });
        result
    }

    pub fn get_file_handlers(&self) -> Vec<&'static EbiFileHandler> {
        let mut result = vec![];
        for file_handler in EBI_FILE_HANDLERS.iter() {
            for importer in file_handler.object_importers {
                if &importer.get_type() == self {
                    result.push(file_handler);
                    break;
                }
            }
        }
        result
    }

    pub fn get_java_object_handlers_that_can_export(&self) -> HashSet<JavaObjectHandler> {
        EbiOutputType::ObjectType(self.clone()).get_java_object_handlers_that_can_export()
    }

    pub fn get_java_object_handlers_that_can_import(&self) -> HashSet<JavaObjectHandler> {
        EbiInputType::Object(self.clone()).get_java_object_handlers_that_can_import()
    }
}

impl Eq for EbiObjectType {}

impl Display for EbiObjectType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                EbiObjectType::LabelledPetriNet => "labelled Petri net",
                EbiObjectType::StochasticLabelledPetriNet => "stochastic labelled Petri net",
                EbiObjectType::FiniteStochasticLanguage => "finite stochastic language",
                EbiObjectType::StochasticDeterministicFiniteAutomaton =>
                    "stochastic deterministic finite automaton",
                EbiObjectType::EventLog => "event log",
                EbiObjectType::FiniteLanguage => "finite language",
                EbiObjectType::DirectlyFollowsModel => "directly follows model",
                EbiObjectType::LanguageOfAlignments => "alignments",
                EbiObjectType::StochasticLanguageOfAlignments =>
                    "stochastic language of alignments",
                EbiObjectType::DeterministicFiniteAutomaton => "deterministic finite automaton",
                EbiObjectType::ProcessTree => "process tree",
                EbiObjectType::StochasticProcessTree => "stochastic process tree",
                EbiObjectType::Executions => "executions",
            }
        )
    }
}

#[derive(Clone)]
pub enum EbiObject {
    LabelledPetriNet(LabelledPetriNet),
    StochasticLabelledPetriNet(StochasticLabelledPetriNet),
    FiniteStochasticLanguage(FiniteStochasticLanguage),
    StochasticDeterministicFiniteAutomaton(StochasticDeterministicFiniteAutomaton),
    EventLog(EventLog),
    FiniteLanguage(FiniteLanguage),
    DirectlyFollowsModel(DirectlyFollowsModel),
    LanguageOfAlignments(LanguageOfAlignments),
    StochasticLanguageOfAlignments(StochasticLanguageOfAlignments),
    DeterministicFiniteAutomaton(DeterministicFiniteAutomaton),
    ProcessTree(ProcessTree),
    StochasticProcessTree(StochasticProcessTree),
    Executions(Executions),
}

impl EbiObject {
    pub fn get_type(&self) -> EbiObjectType {
        match self {
            EbiObject::LabelledPetriNet(_) => EbiObjectType::LabelledPetriNet,
            EbiObject::StochasticLabelledPetriNet(_) => EbiObjectType::StochasticLabelledPetriNet,
            EbiObject::FiniteStochasticLanguage(_) => EbiObjectType::FiniteStochasticLanguage,
            EbiObject::StochasticDeterministicFiniteAutomaton(_) => {
                EbiObjectType::StochasticDeterministicFiniteAutomaton
            }
            EbiObject::EventLog(_) => EbiObjectType::EventLog,
            EbiObject::FiniteLanguage(_) => EbiObjectType::FiniteLanguage,
            EbiObject::DirectlyFollowsModel(_) => EbiObjectType::DirectlyFollowsModel,
            EbiObject::LanguageOfAlignments(_) => EbiObjectType::LanguageOfAlignments,
            EbiObject::StochasticLanguageOfAlignments(_) => {
                EbiObjectType::StochasticLanguageOfAlignments
            }
            EbiObject::DeterministicFiniteAutomaton(_) => {
                EbiObjectType::DeterministicFiniteAutomaton
            }
            EbiObject::ProcessTree(_) => EbiObjectType::ProcessTree,
            EbiObject::StochasticProcessTree(_) => EbiObjectType::StochasticProcessTree,
            EbiObject::Executions(_) => EbiObjectType::Executions,
        }
    }
}

impl Display for EbiObject {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            EbiObject::LabelledPetriNet(o) => write!(f, "{}", o),
            EbiObject::StochasticLabelledPetriNet(o) => write!(f, "{}", o),
            EbiObject::FiniteStochasticLanguage(o) => write!(f, "{}", o),
            EbiObject::StochasticDeterministicFiniteAutomaton(o) => write!(f, "{}", o),
            EbiObject::EventLog(o) => write!(f, "{}", o),
            EbiObject::FiniteLanguage(o) => write!(f, "{}", o),
            EbiObject::DirectlyFollowsModel(o) => write!(f, "{}", o),
            EbiObject::LanguageOfAlignments(o) => write!(f, "{}", o),
            EbiObject::StochasticLanguageOfAlignments(o) => write!(f, "{}", o),
            EbiObject::DeterministicFiniteAutomaton(o) => write!(f, "{}", o),
            EbiObject::ProcessTree(o) => write!(f, "{}", o),
            EbiObject::StochasticProcessTree(o) => write!(f, "{}", o),
            EbiObject::Executions(o) => write!(f, "{}", o),
        }
    }
}

impl Infoable for EbiObject {
    fn info(&self, f: &mut impl std::io::Write) -> Result<()> {
        match self {
            EbiObject::LabelledPetriNet(o) => o.info(f),
            EbiObject::StochasticLabelledPetriNet(o) => o.info(f),
            EbiObject::FiniteStochasticLanguage(o) => o.info(f),
            EbiObject::StochasticDeterministicFiniteAutomaton(o) => o.info(f),
            EbiObject::EventLog(o) => o.info(f),
            EbiObject::FiniteLanguage(o) => o.info(f),
            EbiObject::DirectlyFollowsModel(o) => o.info(f),
            EbiObject::LanguageOfAlignments(o) => o.info(f),
            EbiObject::StochasticLanguageOfAlignments(o) => o.info(f),
            EbiObject::DeterministicFiniteAutomaton(o) => o.info(f),
            EbiObject::ProcessTree(o) => o.info(f),
            EbiObject::StochasticProcessTree(o) => o.info(f),
            EbiObject::Executions(o) => o.info(f),
        }
    }
}

pub enum EbiTraitObject {
    EventLog(Box<dyn EbiTraitEventLog>),
    IterableLanguage(Box<dyn EbiTraitIterableLanguage>),
    FiniteLanguage(Box<dyn EbiTraitFiniteLanguage>),
    FiniteStochasticLanguage(Box<dyn EbiTraitFiniteStochasticLanguage>),
    IterableStochasticLanguage(Box<dyn EbiTraitIterableStochasticLanguage>),
    QueriableStochasticLanguage(Box<dyn EbiTraitQueriableStochasticLanguage>),
    StochasticDeterministicSemantics(EbiTraitStochasticDeterministicSemantics),
    StochasticSemantics(EbiTraitStochasticSemantics),
    Semantics(EbiTraitSemantics),
    Graphable(Box<dyn EbiTraitGraphable>),
}

impl EbiTraitObject {
    pub fn get_trait(&self) -> EbiTrait {
        match self {
            EbiTraitObject::EventLog(_) => EbiTrait::EventLog,
            EbiTraitObject::IterableLanguage(_) => EbiTrait::IterableLanguage,
            EbiTraitObject::FiniteLanguage(_) => EbiTrait::FiniteLanguage,
            EbiTraitObject::FiniteStochasticLanguage(_) => EbiTrait::FiniteStochasticLanguage,
            EbiTraitObject::IterableStochasticLanguage(_) => EbiTrait::IterableStochasticLanguage,
            EbiTraitObject::QueriableStochasticLanguage(_) => EbiTrait::QueriableStochasticLanguage,
            EbiTraitObject::StochasticDeterministicSemantics(_) => {
                EbiTrait::StochasticDeterministicSemantics
            }
            EbiTraitObject::StochasticSemantics(_) => EbiTrait::StochasticSemantics,
            EbiTraitObject::Semantics(_) => EbiTrait::Semantics,
            EbiTraitObject::Graphable(_) => EbiTrait::Graphable,
        }
    }
}

#[cfg(test)]
mod tests {

    use strum::IntoEnumIterator;

    use crate::ebi_framework::{ebi_input::EbiInput, ebi_trait::FromEbiTraitObject};

    use super::EbiObjectType;

    #[test]
    fn object_types() {
        for object_type in EbiObjectType::iter() {
            object_type.get_article();
            object_type.get_file_handlers();
            object_type.get_java_object_handlers_that_can_export();
            object_type.get_java_object_handlers_that_can_import();
        }

        let _ = String::from_trait_object(EbiInput::String("xyz".to_string()));
    }

    #[test]
    fn objects() {
        for (input, _, _) in crate::tests::get_all_test_files() {
            if let EbiInput::Object(object, _) = input {
                object.get_type();
                object.to_string();
            } else if let EbiInput::Trait(object, _) = input {
                object.get_trait();
            }
        }
    }
}
