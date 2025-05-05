use anyhow::{Context, anyhow};

use crate::{
    ebi_framework::{
        ebi_command::EbiCommand,
        ebi_input::EbiInputType,
        ebi_object::{EbiObject, EbiObjectType},
        ebi_output::{EbiOutput, EbiOutputType},
        ebi_trait::EbiTrait,
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
    explanation_short: "Discover a model that supports any trace with the activities of the log.",
    explanation_long: None,
    children: &[
        &EBI_DISCOVER_NON_STOCHASTIC_FLOWER_DFA,
        &EBI_DISCOVER_NON_STOCHASTIC_FLOWER_TREE,
    ],
};

pub const EBI_DISCOVER_NON_STOCHASTIC_FLOWER_DFA: EbiCommand = EbiCommand::Command {
    name_short: "dfa",
    name_long: Some("deterministic-finite-automaton"),
    explanation_short: "Discover a DFA that supports any trace with the activities of the log.",
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
            lpn.mine_flower_dfa()
                .with_context(|| anyhow!("cannot discover flower model"))?,
        )))
    },
    output_type: &EbiOutputType::ObjectType(EbiObjectType::DeterministicFiniteAutomaton),
};

pub const EBI_DISCOVER_NON_STOCHASTIC_FLOWER_TREE: EbiCommand = EbiCommand::Command {
    name_short: "ptree",
    name_long: Some("process-tree"),
    explanation_short: "Discover a process tree that supports any trace with the activities of the log.",
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
            lpn.mine_flower_tree(),
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
