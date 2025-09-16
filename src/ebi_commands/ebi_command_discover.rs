use anyhow::{Context, anyhow};
use ebi_arithmetic::Fraction;
use ebi_objects::{EbiObject, EbiObjectType, LabelledPetriNet, ProcessTree};

use crate::{
    ebi_framework::{
        ebi_command::EbiCommand,
        ebi_input::{EbiInput, EbiInputType},
        ebi_output::{EbiOutput, EbiOutputType},
        ebi_trait::EbiTrait, ebi_trait_object::EbiTraitObject,
    },
    ebi_traits::ebi_trait_finite_stochastic_language::EbiTraitFiniteStochasticLanguage,
    math::constant_fraction::ConstFraction,
    techniques::{
        alignment_stochastic_miner::AlignmentMiner,
        directly_follows_model_miner::DirectlyFollowsModelMinerFiltering,
        occurrences_stochastic_miner::{
            OccurrencesStochasticMinerLPN, OccurrencesStochasticMinerTree,
        },
        uniform_stochastic_miner::{UniformStochasticMinerLPN, UniformStochasticMinerTree},
    },
};

pub const EBI_DISCOVER: EbiCommand = EbiCommand::Group {
    name_short: "disc",
    name_long: Some("discover"),
    explanation_short: "Discover a stochastic process model.",
    explanation_long: None,
    children: &[
        &EBI_DISCOVER_ALIGNMENTS,
        &EBI_DISCOVER_DIRECTLY_FOLLOWS,
        &EBI_DISCOVER_OCCURRENCE,
        &EBI_DISCOVER_UNIFORM,
    ],
};

pub const EBI_DISCOVER_ALIGNMENTS: EbiCommand = EbiCommand::Command {
    name_short: "ali",
    name_long: Some("alignments"),
    library_name: "ebi_commands::ebi_command_discover::EBI_DISCOVER_ALIGNMENTS",
    explanation_short: "Give each transition a weight that matches the aligned occurrences of its label.",
    explanation_long: Some(
        "Give each transition a weight that matches the aligned occurrences of its label. The model must be livelock-free.",
    ),
    latex_link: Some("~\\cite{DBLP:conf/icpm/BurkeLW20}"),
    cli_command: None,
    exact_arithmetic: true,
    input_types: &[
        &[&EbiInputType::Trait(EbiTrait::FiniteStochasticLanguage)],
        &[&EbiInputType::Object(EbiObjectType::LabelledPetriNet)],
    ],
    input_names: &["FILE_1", "FILE_2"],
    input_helps: &[
        "A finite stochastic language (log) to get the occurrences from.",
        "A labelled Petri net with the control flow.",
    ],
    execute: |mut inputs, _| {
        let language = inputs
            .remove(0)
            .to_type::<dyn EbiTraitFiniteStochasticLanguage>()?;
        let lpn = inputs.remove(0).to_type::<LabelledPetriNet>()?;
        Ok(EbiOutput::Object(EbiObject::StochasticLabelledPetriNet(
            lpn.mine_stochastic_alignment(language)?,
        )))
    },
    output_type: &EbiOutputType::ObjectType(EbiObjectType::StochasticLabelledPetriNet),
};

pub const EBI_DISCOVER_DIRECTLY_FOLLOWS: EbiCommand = EbiCommand::Command {
    name_short: "dfg",
    name_long: Some("directly-follows-graph"),
    library_name: "ebi_commands::ebi_command_discover::EBI_DISCOVER_DIRECTLY_FOLLOWS",
    explanation_short: "Discover a directly follows graph.",
    explanation_long: None,
    latex_link: Some("~\\cite{DBLP:conf/icpm/LeemansPW19}"),
    cli_command: None,
    exact_arithmetic: true,
    input_types: &[
        &[
            &EbiInputType::Trait(EbiTrait::EventLog),
            &EbiInputType::Trait(EbiTrait::FiniteStochasticLanguage),
        ],
        &[&EbiInputType::Fraction(
            Some(ConstFraction::zero()),
            Some(ConstFraction::one()),
            Some(ConstFraction::one()),
        )],
    ],
    input_names: &["LANG", "MIN_FITNESS"],
    input_helps: &[
        "A finite stochastic language.",
        "The minimum fraction of traces that should fit the resulting model.",
    ],
    execute: |mut inputs, _| {
        let lang = inputs.remove(0);
        let minimum_fitness = inputs
            .remove(0)
            .to_type::<Fraction>()
            .with_context(|| "reading minimum fitness")?;
        match lang {
            EbiInput::Trait(EbiTraitObject::EventLog(mut log), _) => {
                Ok(EbiOutput::Object(EbiObject::DirectlyFollowsGraph(
                    log.mine_directly_follows_model_filtering(&minimum_fitness)?,
                )))
            }
            EbiInput::Trait(EbiTraitObject::FiniteStochasticLanguage(mut slang), _) => {
                Ok(EbiOutput::Object(EbiObject::DirectlyFollowsGraph(
                    slang.mine_directly_follows_model_filtering(&minimum_fitness)?,
                )))
            }
            _ => Err(anyhow!(
                "can only apply directly follows model miner to event log or finite stochastic language"
            )),
        }
    },
    output_type: &EbiOutputType::ObjectType(EbiObjectType::DirectlyFollowsGraph),
};

pub const EBI_DISCOVER_OCCURRENCE: EbiCommand = EbiCommand::Command { 
    name_short: "occ", 
    name_long: Some("occurrence"), 
    library_name: "ebi_commands::ebi_command_discover::EBI_DISCOVER_OCCURRENCE",
    explanation_short: "Give each transition a weight that matches the occurrences of its label; silent transitions get a weight of 1.", 
    explanation_long: None, 
    latex_link: Some("~\\cite{DBLP:conf/icpm/BurkeLW20}"), 
    cli_command: None, 
    exact_arithmetic: true, 
    input_types: &[ 
        &[ &EbiInputType::Trait(EbiTrait::FiniteStochasticLanguage)], 
        &[ &EbiInputType::Object(EbiObjectType::LabelledPetriNet)]
    ], 
    input_names: &[ "FILE_1", "FILE_2" ], 
    input_helps: &[ "A finite stochastic language (log) to get the occurrences from.", "A labelled Petri net with the control flow." ], 
    execute: |mut inputs, _| {
        let language = inputs
            .remove(0)
            .to_type::<dyn EbiTraitFiniteStochasticLanguage>()?;
        let lpn = inputs.remove(0).to_type::<LabelledPetriNet>()?;
        Ok(EbiOutput::Object(EbiObject::StochasticLabelledPetriNet(
            lpn.mine_occurrences_stochastic_lpn(language),
        )))
    },
    output_type: &EbiOutputType::ObjectType(EbiObjectType::StochasticLabelledPetriNet),
};

pub const EBI_DISCOVER_OCCURRENCE_PTREE: EbiCommand = EbiCommand::Command {
    name_short: "ptree",
    name_long: Some("process-tree"),
    library_name: "ebi_commands::ebi_command_discover::EBI_DISCOVER_OCCURRENCE_PTREE",
    explanation_short: "Give each leaf a weight that matches the occurrences of its label; silent leaves get a weight of 1.",
    explanation_long: None,
    latex_link: Some("~\\cite{DBLP:conf/icpm/BurkeLW20}"),
    cli_command: None,
    exact_arithmetic: true,
    input_types: &[
        &[&EbiInputType::Trait(EbiTrait::FiniteStochasticLanguage)],
        &[&EbiInputType::Object(EbiObjectType::ProcessTree)],
    ],
    input_names: &["LANG", "TREE"],
    input_helps: &[
        "A finite stochastic language (log) to get the occurrences from.",
        "A process tree with the control flow.",
    ],
    execute: |mut inputs, _| {
        let language = inputs
            .remove(0)
            .to_type::<dyn EbiTraitFiniteStochasticLanguage>()?;
        let lpn = inputs.remove(0).to_type::<ProcessTree>()?;
        Ok(EbiOutput::Object(EbiObject::StochasticProcessTree(
            lpn.mine_occurrences_stochastic_tree(language),
        )))
    },
    output_type: &EbiOutputType::ObjectType(EbiObjectType::StochasticProcessTree),
};

pub const EBI_DISCOVER_UNIFORM: EbiCommand = EbiCommand::Command { 
    name_short: "uni", 
    name_long: Some("uniform"), 
    library_name: "ebi_commands::ebi_command_discover::EBI_DISCOVER_UNIFORM",
    explanation_short: "Give each transition a weight of 1.", 
    explanation_long: None, 
    latex_link: None, 
    cli_command: None, 
    exact_arithmetic: true, 
    input_types: &[ 
        &[ &EbiInputType::Object(EbiObjectType::LabelledPetriNet)]
    ], 
    input_names: &["LPN_FILE" ], 
    input_helps: &[ "A labelled Petri net." ], 
    execute: |mut inputs, _| {
        let lpn = inputs.remove(0).to_type::<LabelledPetriNet>()?;
        Ok(EbiOutput::Object(EbiObject::StochasticLabelledPetriNet(
            lpn.mine_uniform_stochastic_lpn(),
        )))
    },
    output_type: &EbiOutputType::ObjectType(EbiObjectType::StochasticLabelledPetriNet),
};

pub const EBI_DISCOVER_UNIFORM_PTREE: EbiCommand = EbiCommand::Command {
    name_short: "ptree",
    name_long: Some("process-tree"),
    library_name: "ebi_commands::ebi_command_discover::EBI_DISCOVER_UNIFORM_PTREE",
    explanation_short: "Give each leaf a weight of 1 in a process tree.",
    explanation_long: None,
    latex_link: None,
    cli_command: None,
    exact_arithmetic: true,
    input_types: &[&[&EbiInputType::Object(EbiObjectType::ProcessTree)]],
    input_names: &["TREE"],
    input_helps: &["A process tree."],
    execute: |mut inputs, _| {
        let tree = inputs.remove(0).to_type::<ProcessTree>()?;
        Ok(EbiOutput::Object(EbiObject::StochasticProcessTree(
            tree.mine_uniform_stochastic_tree(),
        )))
    },
    output_type: &EbiOutputType::ObjectType(EbiObjectType::StochasticProcessTree),
};
