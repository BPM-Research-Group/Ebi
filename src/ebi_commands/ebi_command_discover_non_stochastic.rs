use anyhow::Context;
use ebi_objects::{EbiObject, EbiObjectType, HasActivityKey};

use crate::{
    ebi_framework::{
        ebi_command::EbiCommand,
        ebi_input::{EbiInput, EbiInputType},
        ebi_output::{EbiOutput, EbiOutputType},
        ebi_trait::EbiTrait,
        ebi_trait_object::EbiTraitObject,
    },
    ebi_traits::ebi_trait_finite_language::EbiTraitFiniteLanguage,
    techniques::{
        flower_miner::{FlowerMinerDFA, FlowerMinerTree},
        prefix_tree_miner::{PrefixTreeMinerDFA, PrefixTreeMinerTree},
    },
};

pub const EBI_DISCOVER_NON_STOCHASTIC: EbiCommand = EbiCommand::Group {
    name_short: "dins",
    name_long: Some("discover-non-stochastic"),
    explanation_short: "Discover a non-stochastic process model.",
    explanation_long: None,
    children: &[
        &EBI_DISCOVER_NON_STOCHASTIC_FLOWER,
        &EBI_DISCOVER_NON_STOCHASTIC_PREFIX,
    ],
};

pub const EBI_DISCOVER_NON_STOCHASTIC_FLOWER: EbiCommand = EbiCommand::Group {
    name_short: "flw",
    name_long: Some("flower"),
    explanation_short: "Discover a model that supports any trace with the activities of the log or model.",
    explanation_long: None,
    children: &[
        &EBI_DISCOVER_NON_STOCHASTIC_FLOWER_DFA,
        &EBI_DISCOVER_NON_STOCHASTIC_FLOWER_TREE,
    ],
};

pub const EBI_DISCOVER_NON_STOCHASTIC_FLOWER_DFA: EbiCommand = EbiCommand::Command {
    name_short: "dfa",
    name_long: Some("deterministic-finite-automaton"),
    library_name: "ebi_commands::ebi_command_discover_non_stochastic::EBI_DISCOVER_NON_STOCHASTIC_FLOWER_DFA",
    explanation_short: "Discover a DFA that supports any trace with the activities of the log.",
    explanation_long: None,
    latex_link: None,
    cli_command: None,
    exact_arithmetic: true,
    input_types: &[&[
        &EbiInputType::Trait(EbiTrait::FiniteLanguage),
        &EbiInputType::Trait(EbiTrait::Activities),
    ]],
    input_names: &["FILE"],
    input_helps: &["A file with activities."],
    execute: |mut inputs, _| {
        Ok(EbiOutput::Object(EbiObject::DeterministicFiniteAutomaton(
            match inputs.remove(0) {
                EbiInput::Trait(EbiTraitObject::FiniteLanguage(lang), _) => {
                    let lang: Box<dyn HasActivityKey> = lang;
                    lang.mine_flower_dfa()
                        .with_context(|| format!("cannot compute flower model"))?
                }
                EbiInput::Trait(EbiTraitObject::Activities(lang), _) => {
                    let lang: Box<dyn HasActivityKey> = lang;
                    lang.mine_flower_dfa()
                        .with_context(|| format!("cannot compute flower model"))?
                }
                _ => unreachable!(),
            },
        )))
    },
    output_type: &EbiOutputType::ObjectType(EbiObjectType::DeterministicFiniteAutomaton),
};

pub const EBI_DISCOVER_NON_STOCHASTIC_FLOWER_TREE: EbiCommand = EbiCommand::Command {
    name_short: "ptree",
    name_long: Some("process-tree"),
    library_name: "ebi_commands::ebi_command_discover_non_stochastic::EBI_DISCOVER_NON_STOCHASTIC_FLOWER_TREE",
    explanation_short: "Discover a process tree that supports any trace with the activities of the log.",
    explanation_long: None,
    latex_link: None,
    cli_command: None,
    exact_arithmetic: true,
    input_types: &[&[
        &EbiInputType::Trait(EbiTrait::FiniteLanguage),
        &EbiInputType::Trait(EbiTrait::Activities),
    ]],
    input_names: &["FILE"],
    input_helps: &["A file with activities."],
    execute: |mut inputs, _| {
        Ok(EbiOutput::Object(EbiObject::ProcessTree(
            match inputs.remove(0) {
                EbiInput::Trait(EbiTraitObject::FiniteLanguage(lang), _) => lang.mine_flower_tree(),
                EbiInput::Trait(EbiTraitObject::Activities(lang), _) => {
                    let lang: Box<dyn HasActivityKey> = lang;
                    lang.mine_flower_tree()
                }
                _ => unreachable!(),
            },
        )))
    },
    output_type: &EbiOutputType::ObjectType(EbiObjectType::ProcessTree),
};

pub const EBI_DISCOVER_NON_STOCHASTIC_PREFIX: EbiCommand = EbiCommand::Group {
    name_short: "pfxt",
    name_long: Some("prefix-tree"),
    explanation_short: "Discover a prefix tree model of the log.",
    explanation_long: None,
    children: &[
        &EBI_DISCOVER_NON_STOCHASTIC_TREE_DFA,
        &EBI_DISCOVER_NON_STOCHASTIC_TREE_TREE,
    ],
};

pub const EBI_DISCOVER_NON_STOCHASTIC_TREE_DFA: EbiCommand = EbiCommand::Command {
    name_short: "dfa",
    name_long: Some("deterministic-finite-automaton"),
    library_name: "ebi_commands::ebi_command_discover_non_stochastic::EBI_DISCOVER_NON_STOCHASTIC_TREE_DFA",
    explanation_short: "Discover a DFA that is a prefix tree of the log.",
    explanation_long: None,
    latex_link: None,
    cli_command: None,
    exact_arithmetic: true,
    input_types: &[&[&EbiInputType::Trait(EbiTrait::FiniteLanguage)]],
    input_names: &["LANG"],
    input_helps: &["A finite language."],
    execute: |mut inputs, _| {
        let lpn = inputs.remove(0).to_type::<dyn EbiTraitFiniteLanguage>()?;
        Ok(EbiOutput::Object(EbiObject::DeterministicFiniteAutomaton(
            lpn.mine_prefix_tree_dfa(),
        )))
    },
    output_type: &EbiOutputType::ObjectType(EbiObjectType::DeterministicFiniteAutomaton),
};

pub const EBI_DISCOVER_NON_STOCHASTIC_TREE_TREE: EbiCommand = EbiCommand::Command {
    name_short: "tree",
    name_long: Some("process-tree"),
    library_name: "ebi_commands::ebi_command_discover_non_stochastic::EBI_DISCOVER_NON_STOCHASTIC_TREE_TREE",
    explanation_short: "Discover a process tree that is a prefix tree of the log.",
    explanation_long: None,
    latex_link: None,
    cli_command: None,
    exact_arithmetic: true,
    input_types: &[&[&EbiInputType::Trait(EbiTrait::FiniteLanguage)]],
    input_names: &["LANG"],
    input_helps: &["A finite language."],
    execute: |mut inputs, _| {
        let lpn = inputs.remove(0).to_type::<dyn EbiTraitFiniteLanguage>()?;
        Ok(EbiOutput::Object(EbiObject::ProcessTree(
            lpn.mine_prefix_tree_tree(),
        )))
    },
    output_type: &EbiOutputType::ObjectType(EbiObjectType::ProcessTree),
};
