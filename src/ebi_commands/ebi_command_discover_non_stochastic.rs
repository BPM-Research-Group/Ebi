use crate::{
    ebi_framework::{
        ebi_command::EbiCommand,
        ebi_input::{EbiInput, EbiInputType},
        ebi_output::{
            EbiOutput,
            EbiOutputType::{self},
        },
        ebi_trait::EbiTrait,
        ebi_trait_object::EbiTraitObject,
    },
    ebi_traits::{
        ebi_trait_finite_language::EbiTraitFiniteLanguage,
        ebi_trait_finite_stochastic_language::EbiTraitFiniteStochasticLanguage,
    },
    techniques::{
        flower_miner::{FlowerMinerDFA, FlowerMinerTree},
        inductive_miner::{InductiveMiner, InductiveMinerInfrequent},
        prefix_tree_miner::{PrefixTreeMinerDFA, PrefixTreeMinerTree},
        split_miner::SplitMiner,
        trace_model_miner::TraceModelMinerTree,
    },
};
use ebi_objects::{
    EbiObject, EbiObjectType, HasActivityKey,
    anyhow::{Context, anyhow},
    ebi_arithmetic::{ConstFraction, Fraction},
};

pub const EBI_DISCOVER_NON_STOCHASTIC: EbiCommand = EbiCommand::Group {
    name_short: "dins",
    name_long: Some("discover-non-stochastic"),
    explanation_short: "Discover a non-stochastic process model.",
    explanation_long: None,
    children: &[
        &EBI_DISCOVER_NON_STOCHASTIC_FLOWER,
        &EBI_DISCOVER_NON_STOCHASTIC_PREFIX,
        &EBI_DISCOVER_NON_STOCHASTIC_TRACE_MODEL,
        &EBI_DISCOVER_NON_STOCHASTIC_INDUCTIVE_MINER,
        &EBI_DISCOVER_NON_STOCHASTIC_INDUCTIVE_MINER_INFREQUENT,
        &EBI_DISCOVER_NON_STOCHASTIC_SPLIT_MINER,
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
                object => {
                    return Err(anyhow!(
                        "Unsupported object {:?} provided.",
                        object.get_type()
                    ));
                }
            },
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
                object => {
                    return Err(anyhow!(
                        "Unsupported object {:?} provided.",
                        object.get_type()
                    ));
                }
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
    name_short: "ptree",
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

pub const EBI_DISCOVER_NON_STOCHASTIC_TRACE_MODEL: EbiCommand = EbiCommand::Command {
    name_short: "tm",
    name_long: Some("trace-model"),
    explanation_short: "Discover a model that is a choice between all traces of the model.",
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
            lpn.mine_trace_model_tree(),
        )))
    },
    output_type: &EbiOutputType::ObjectType(EbiObjectType::ProcessTree),
};

pub const EBI_DISCOVER_NON_STOCHASTIC_INDUCTIVE_MINER: EbiCommand = EbiCommand::Command {
    name_short: "im",
    name_long: Some("inductive-miner"),
    explanation_short: "Discover a process tree using the Inductive Miner algorithm.",
    explanation_long: Some("Discover a process tree using the Inductive Miner.
    Please note that this implementation closely follows the thesis version; it is not equivalent to the ProM implementation."),
    latex_link: Some("\\cite{DBLP:series/lnbip/Leemans22}"),
    cli_command: None,
    exact_arithmetic: true,
    input_types: &[
        &[&EbiInputType::Trait(EbiTrait::FiniteLanguage)],
    ],
    input_names: &["LANG"],
    input_helps: &[
        "A finite language."
    ],
    execute: |mut inputs, _| {
        let lang = inputs.remove(0).to_type::<dyn EbiTraitFiniteLanguage>()?;
            Ok(EbiOutput::Object(EbiObject::ProcessTree(
                lang.inductive_miner(),
            )))
    },
    output_type: &EbiOutputType::ObjectType(EbiObjectType::ProcessTree),
};

pub const EBI_DISCOVER_NON_STOCHASTIC_INDUCTIVE_MINER_INFREQUENT: EbiCommand = EbiCommand::Command {
    name_short: "imf",
    name_long: Some("inductive-miner-infrequent"),
    explanation_short: "Discover a process tree using the Inductive Miner-infrequent algorithm.",
    explanation_long: Some("Discover a process tree using the Inductive Miner-infrequent algorithm. 
    If the given noise parameter is 0, then behaves as the Inductive Miner algorithm.
    Please note that this implementation closely follows the thesis version; it is not equivalent to the ProM implementation."),
    latex_link: Some("\\cite{DBLP:series/lnbip/Leemans22}"),
    cli_command: None,
    exact_arithmetic: true,
    input_types: &[
        &[&EbiInputType::Trait(EbiTrait::FiniteStochasticLanguage)],
        &[&EbiInputType::Fraction(
            Some(ConstFraction::zero()),
            Some(ConstFraction::one()),
            Some(ConstFraction::of(2, 10)),
        )],
    ],
    input_names: &["SLANG", "NOISE"],
    input_helps: &[
        "A finite stochastic language.",
        "The amount of noise filtering, where 0 means no noise filtering is applied, and 1 means that maximum noise filtering is applied.",
    ],
    execute: |mut inputs, _| {
        let slang = inputs.remove(0).to_type::<dyn EbiTraitFiniteStochasticLanguage>()?;
        let noise = inputs.remove(0).to_type::<Fraction>()?;
            Ok(EbiOutput::Object(EbiObject::ProcessTree(
                slang.inductive_miner_infrequent(&noise),
            )))
    },
    output_type: &EbiOutputType::ObjectType(EbiObjectType::ProcessTree),
};

pub const EBI_DISCOVER_NON_STOCHASTIC_SPLIT_MINER: EbiCommand = EbiCommand::Command {
    name_short: "sm",
    name_long: Some("split-miner"),
    explanation_short: "Apply a simplified version of the Split Miner algorithm.",
    explanation_long: Some(
        "Apply a simplified version of the Split Miner algorithm.
        In this implementation, it is not necessary to have a unique start and end activity.
        For now, this implementation does not reduce splits or SESE fragments (Algorithms 8 and 9).",
    ),
    latex_link: Some("\\cite{DBLP:journals/kais/AugustoCDRP19}"),
    cli_command: None,
    exact_arithmetic: true,
    input_types: &[&[&EbiInputType::Trait(EbiTrait::FiniteStochasticLanguage)]],
    input_names: &["LOG"],
    input_helps: &["The event log."],
    execute: |mut inputs, _| {
        let log = inputs
            .remove(0)
            .to_type::<dyn EbiTraitFiniteStochasticLanguage>()?;
        Ok(EbiOutput::Object(
            EbiObject::BusinessProcessModelAndNotation(log.split_miner()?),
        ))
    },
    output_type: &EbiOutputType::ObjectType(EbiObjectType::BusinessProcessModelAndNotation),
};
