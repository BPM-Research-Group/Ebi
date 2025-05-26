use anyhow::{Context, anyhow};

use crate::{
    ebi_framework::{
        ebi_command::EbiCommand,
        ebi_input::EbiInputType,
        ebi_object::{EbiObject, EbiObjectType},
        ebi_output::{EbiOutput, EbiOutputType},
        ebi_trait::EbiTrait,
    },
    ebi_traits::{
        ebi_trait_finite_language::EbiTraitFiniteLanguage,
        ebi_trait_finite_stochastic_language::EbiTraitFiniteStochasticLanguage,
    },
    math::fraction::Fraction,
    techniques::{
        directly_follows_model_miner::DirectlyFollowsModelMinerFiltering,
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
        &EBI_DISCOVER_NON_STOCHASTIC_DIRECTLY_FOLLOWS,
        &EBI_DISCOVER_NON_STOCHASTIC_FLOWER,
        &EBI_DISCOVER_NON_STOCHASTIC_PREFIX,
    ],
};

pub const EBI_DISCOVER_NON_STOCHASTIC_DIRECTLY_FOLLOWS: EbiCommand = EbiCommand::Command {
    name_short: "dfm",
    name_long: Some("directly-follows-model"),
    library_name: "ebi_commands::ebi_command_discover_non_stochastic::EBI_DISCOVER_NON_STOCHASTIC_DIRECTLY_FOLLOWS",
    explanation_short: "Discover a directly follows model.",
    explanation_long: None,
    latex_link: Some("~\\cite{DBLP:conf/icpm/LeemansPW19}"),
    cli_command: None,
    exact_arithmetic: true,
    input_types: &[
        &[&EbiInputType::Trait(EbiTrait::FiniteStochasticLanguage)],
        &[&EbiInputType::Fraction],
    ],
    input_names: &["LANG", "MIN_FITNESS"],
    input_helps: &[
        "A finite stochastic language.",
        "The minimum fitness of the resulting model.",
    ],
    execute: |mut inputs, _| {
        let mut lang = inputs
            .remove(0)
            .to_type::<dyn EbiTraitFiniteStochasticLanguage>()?;
        let minimum_fitness = inputs.remove(0).to_type::<Fraction>()?;
        Ok(EbiOutput::Object(EbiObject::DirectlyFollowsModel(
            lang.mine_directly_follows_model_filtering(&minimum_fitness)?.into(),
        )))
    },
    output_type: &EbiOutputType::ObjectType(EbiObjectType::DirectlyFollowsModel),
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
    library_name: "ebi_commands::ebi_command_discover_non_stochastic::EBI_DISCOVER_NON_STOCHASTIC_FLOWER_DFA",
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
    library_name: "ebi_commands::ebi_command_discover_non_stochastic::EBI_DISCOVER_NON_STOCHASTIC_FLOWER_TREE",
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
