use crate::{
    ebi_framework::{
        ebi_command::EbiCommand,
        ebi_input::{EbiInput, EbiInputType},
        ebi_output::{EbiOutput, EbiOutputType},
        ebi_trait::EbiTrait,
        ebi_trait_object::EbiTraitObject,
    },
    ebi_traits::{
        ebi_trait_event_log::EbiTraitEventLog,
        ebi_trait_finite_stochastic_language::EbiTraitFiniteStochasticLanguage,
    },
    techniques::{
        alergia::Alergia,
        alignment_stochastic_miner::AlignmentMiner,
        directly_follows_model_miner::DirectlyFollowsModelMinerFiltering,
        gaspd::Gaspd,
        occurrences_stochastic_miner::{
            OccurrencesStochasticMinerBPMN, OccurrencesStochasticMinerLPN,
            OccurrencesStochasticMinerTree,
        },
        random_stochastic_miner::{RandomMinerSBPMN, RandomMinerSTREE},
        uniform_stochastic_miner::{
            UniformStochasticBusinessProcessModelAndNotation, UniformStochasticMinerLPN,
            UniformStochasticMinerTree,
        },
    },
};
use ebi_objects::{
    BusinessProcessModelAndNotation, EbiObject, EbiObjectType, LabelledPetriNet, ProcessTree,
    anyhow::{Context, anyhow},
    ebi_arithmetic::{ConstFraction, Fraction, Random},
};

pub const EBI_DISCOVER: EbiCommand = EbiCommand::Group {
    name_short: "disc",
    name_long: Some("discover"),
    explanation_short: "Discover a stochastic process model.",
    explanation_long: None,
    children: &[
        &EBI_DISCOVER_ALERGIA,
        &EBI_DISCOVER_ALIGNMENTS,
        &EBI_DISCOVER_DIRECTLY_FOLLOWS,
        &EBI_DISCOVER_GASPD,
        &EBI_DISCOVER_OCCURRENCE,
        &EBI_DISCOVER_RANDOM,
        &EBI_DISCOVER_UNIFORM,

    ],
};

pub const EBI_DISCOVER_ALERGIA: EbiCommand = EbiCommand::Command {
    name_short: "al",
    name_long: Some("alergia"),
    explanation_short: "Discover an SDFA from an event log using ALERGIA.",
    explanation_long: Some("Learns a Stochastic Deterministic Finite Automaton (SDFA) from an \
                        event log using the ALERGIA algorithm, which iteratively merges \
                        statistically similar states of the log's frequency prefix tree."),
    latex_link: Some("~\\cite{DBLP:conf/icgi/CarrascoO94}"),
    cli_command: None,
    exact_arithmetic: true,
    input_types: &[
        &[&EbiInputType::Trait(EbiTrait::EventLog)],
        &[&EbiInputType::Fraction(
            Some(ConstFraction::zero()),
            Some(ConstFraction::one()),
            Some(ConstFraction::one()),
        )],
        &[&EbiInputType::Usize(Some(0), None, Some(30))],
    ],
    input_names: &["LOG", "FILTER", "VISITS"],
    input_helps: &[
        "An event log to discover the model from.",
        "The fraction of most-frequent trace variants to retain before learning (the rest are filtered out).",
        "The minimum number of times a state must be visited before it is eligible to be merged.",
    ],
    execute: |mut inputs, _| {
        let log = inputs.remove(0)
            .to_type::<dyn EbiTraitEventLog>()?;
        let filter_frequency = *inputs.remove(0)
            .to_type::<Fraction>().unwrap();
        let min_visits = *inputs.remove(0)
            .to_type::<usize>().unwrap();
        Ok(EbiOutput::Object(EbiObject::StochasticDeterministicFiniteAutomaton(log.alergia(filter_frequency, min_visits)?)))
    },
    output_type: &EbiOutputType::ObjectType(EbiObjectType::StochasticDeterministicFiniteAutomaton),
};

pub const EBI_DISCOVER_ALIGNMENTS: EbiCommand = EbiCommand::Group {
    name_short: "ali",
    name_long: Some("alignments"),
    explanation_short: "Give each transition a weight that matches the aligned occurrences of its label.",
    explanation_long: Some(
        "Give each transition a weight that matches the aligned occurrences of its label. The model must be livelock-free.",
    ),
    children: &[&EBI_DISCOVER_ALIGNMENTS_BPMN, &EBI_DISCOVER_ALIGNMENTS_SLPN],
};

pub const EBI_DISCOVER_ALIGNMENTS_BPMN: EbiCommand = EbiCommand::Command {
    name_short: "sbpmn",
    name_long: Some("stochastic-business-process-model-and-notation"),
    explanation_short: "Give each transition a weight that matches the aligned occurrences of its label.",
    explanation_long: Some(
        "Give each transition a weight that matches the aligned occurrences of its label. The model must be livelock-free.",
    ),
    latex_link: Some("~\\cite{DBLP:conf/icpm/BurkeLW20}"),
    cli_command: None,
    exact_arithmetic: true,
    input_types: &[
        &[&EbiInputType::Trait(EbiTrait::FiniteStochasticLanguage)],
        &[&EbiInputType::Object(
            EbiObjectType::BusinessProcessModelAndNotation,
        )],
    ],
    input_names: &["SLANG", "BPMN"],
    input_helps: &[
        "A finite stochastic language (log) to get the occurrences from.",
        "A business process model and notation with the control flow.",
    ],
    execute: |mut inputs, _| {
        let language = inputs
            .remove(0)
            .to_type::<dyn EbiTraitFiniteStochasticLanguage>()?;
        let bpmn = inputs
            .remove(0)
            .to_type::<BusinessProcessModelAndNotation>()?;
        Ok(EbiOutput::Object(
            EbiObject::StochasticBusinessProcessModelAndNotation(
                bpmn.mine_stochastic_alignment(language)
                    .with_context(|| anyhow!("Discovering using alignments."))?,
            ),
        ))
    },
    output_type: &EbiOutputType::ObjectType(
        EbiObjectType::StochasticBusinessProcessModelAndNotation,
    ),
};

pub const EBI_DISCOVER_ALIGNMENTS_SLPN: EbiCommand = EbiCommand::Command {
    name_short: "slpn",
    name_long: Some("stochastic-labelled-Petri-nets"),
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
    input_names: &["SLANG", "LPN"],
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
    input_names: &["SLANG", "MIN_FITNESS"],
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
pub const EBI_DISCOVER_GASPD: EbiCommand = EbiCommand::Command {
    name_short: "gaspd",
    name_long: Some("genetic-algorithm-for-stochastic-process-discovery"),
    explanation_short: "Selects a model based on a family of SDFMs from an event log using GASPD.",
    explanation_long: Some("Learns a family of Stochastic Directly Follows Models (SDFMs) \
                           from an event log by evolving a population of ALERGIA parameter \
                           settings (confidence factor, filter frequency, minimum visits) \
                           via a genetic search, retaining the Pareto-optimal trade-offs \
                           between model simplicity, relevance and adhesion. Returns the \
                           best model according to the given preferences for simplicity, relevance and adhesion."),
    latex_link: Some("~\\cite{DBLP:conf/caise/AlkhammashPM24}"),
    cli_command: None,
    exact_arithmetic: false,
    input_types: &[
        &[&EbiInputType::Trait(EbiTrait::EventLog)],
        &[&EbiInputType::Usize(Some(0),None,Some(50))],
        &[&EbiInputType::Usize(Some(0),None,Some(50))],
        &[&EbiInputType::Usize(Some(0),None,Some(3))],
        &[&EbiInputType::Fraction(Some(ConstFraction::zero()), Some(ConstFraction::one()), Some(ConstFraction::zero()))],
        &[&EbiInputType::Fraction(Some(ConstFraction::zero()), Some(ConstFraction::one()), Some(ConstFraction::zero()))],
    ],
    input_names: &["LOG", "POP", "GENS", "PARENTS", "SIMP", "REL"],
    input_helps: &[
        "The event log to discover a family of models from.",
        "The number of candidate ALERGIA parameter settings in the initial population.",
        "The number of generations the genetic search runs for.",
        "The number of parents selected from the Pareto frontier to produce each new generation.",
        "The preference (weight) for simplicity, between 0 and 1. SIMP + REL must be at most 1.",
        "The preference (weight) for relevance, between 0 and 1. SIMP + REL must be at most 1."
    ],
    execute: |mut inputs, _| {
        let mut log = inputs.remove(0)
            .to_type::<dyn EbiTraitEventLog>()?;
        let population_size = *inputs.remove(0)
            .to_type::<usize>().unwrap();
        let generation_limit = *inputs.remove(0)
            .to_type::<usize>().unwrap();
        let number_of_parents = *inputs.remove(0)
            .to_type::<usize>().unwrap();
        let w_s = *inputs.remove(0)
            .to_type::<Fraction>().unwrap();
        let w_r = *inputs.remove(0)
            .to_type::<Fraction>().unwrap();

        let res = log.gaspd(generation_limit, number_of_parents, population_size, w_s, w_r).unwrap();
        Ok(EbiOutput::Object(EbiObject::StochasticDirectlyFollowsModel(res)))
    },
    output_type: &EbiOutputType::ObjectType(EbiObjectType::StochasticDirectlyFollowsModel),
};

pub const EBI_DISCOVER_OCCURRENCE: EbiCommand = EbiCommand::Group {
    name_short: "occ",
    name_long: Some("occurrence"),
    explanation_short: "Give each transition a weight that matches the occurrences of its label; silent transitions get a weight of 1.",
    explanation_long: None,
    children: &[
        &EBI_DISCOVER_OCCURRENCE_SBPMN,
        &EBI_DISCOVER_OCCURRENCE_SLPN,
        &EBI_DISCOVER_OCCURRENCE_SPTREE,
    ],
};

pub const EBI_DISCOVER_OCCURRENCE_SBPMN: EbiCommand = EbiCommand::Command {
    name_short: "sbpmn",
    name_long: Some("stochastic-business-process-model-and-notation"),
    explanation_short: "Give each sequence flow to a task a weight that matches the occurrences of its label; other sequence flows get a weight of 1.",
    explanation_long: None,
    latex_link: Some("~\\cite{DBLP:conf/icpm/BurkeLW20}"),
    cli_command: None,
    exact_arithmetic: true,
    input_types: &[
        &[&EbiInputType::Trait(EbiTrait::FiniteStochasticLanguage)],
        &[&EbiInputType::Object(
            EbiObjectType::BusinessProcessModelAndNotation,
        )],
    ],
    input_names: &["SLANG", "BPMN"],
    input_helps: &[
        "A finite stochastic language (log) to get the occurrences from.",
        "A business process model and notation model with the control flow.",
    ],
    execute: |mut inputs, _| {
        let language = inputs
            .remove(0)
            .to_type::<dyn EbiTraitFiniteStochasticLanguage>()?;
        let bpmn = inputs
            .remove(0)
            .to_type::<BusinessProcessModelAndNotation>()?;
        Ok(EbiOutput::Object(
            EbiObject::StochasticBusinessProcessModelAndNotation(
                bpmn.mine_occurrences_stochastic_bpmn(language)?,
            ),
        ))
    },
    output_type: &EbiOutputType::ObjectType(
        EbiObjectType::StochasticBusinessProcessModelAndNotation,
    ),
};

pub const EBI_DISCOVER_OCCURRENCE_SLPN: EbiCommand = EbiCommand::Command {
    name_short: "slpn",
    name_long: Some("stochastic-labelled-Petri-net"),
    explanation_short: "Give each transition a weight that matches the occurrences of its label; silent transitions get a weight of 1.",
    explanation_long: None,
    latex_link: Some("~\\cite{DBLP:conf/icpm/BurkeLW20}"),
    cli_command: None,
    exact_arithmetic: true,
    input_types: &[
        &[&EbiInputType::Trait(EbiTrait::FiniteStochasticLanguage)],
        &[&EbiInputType::Object(EbiObjectType::LabelledPetriNet)],
    ],
    input_names: &["SLANG", "LPN"],
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
            lpn.mine_occurrences_stochastic_lpn(language),
        )))
    },
    output_type: &EbiOutputType::ObjectType(EbiObjectType::StochasticLabelledPetriNet),
};

pub const EBI_DISCOVER_OCCURRENCE_SPTREE: EbiCommand = EbiCommand::Command {
    name_short: "sptree",
    name_long: Some("stochastic-process-tree"),
    explanation_short: "Give each leaf a weight that matches the occurrences of its label; silent leaves get a weight of 1.",
    explanation_long: None,
    latex_link: Some("~\\cite{DBLP:conf/icpm/BurkeLW20}"),
    cli_command: None,
    exact_arithmetic: true,
    input_types: &[
        &[&EbiInputType::Trait(EbiTrait::FiniteStochasticLanguage)],
        &[&EbiInputType::Object(EbiObjectType::ProcessTree)],
    ],
    input_names: &["SLANG", "TREE"],
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

pub const EBI_DISCOVER_RANDOM: EbiCommand = EbiCommand::Group {
    name_short: "rnd",
    name_long: Some("random"),
    explanation_short: "Give each transition a random weight between 0 (exclusive) and 1 (inclusive).",
    explanation_long: None,
    children: &[
        &EBI_DISCOVER_RANDOM_SBPMN,
        &EBI_DISCOVER_RANDOM_SLPN,
        &EBI_DISCOVER_RANDOM_SPTREE,
    ],
};

pub const EBI_DISCOVER_RANDOM_SBPMN: EbiCommand = EbiCommand::Command {
    name_short: "sbpmn",
    name_long: Some("stochastic-business-process-model-and-notation"),
    explanation_short: "Give each sequence flow a random weight between 0 (exclusive) and 1 (inclusive).",
    explanation_long: None,
    latex_link: None,
    cli_command: None,
    exact_arithmetic: true,
    input_types: &[&[&EbiInputType::Object(
        EbiObjectType::BusinessProcessModelAndNotation,
    )]],
    input_names: &["BPMN"],
    input_helps: &["A business process model and notation model with the control flow."],
    execute: |mut inputs, _| {
        let bpmn = inputs
            .remove(0)
            .to_type::<BusinessProcessModelAndNotation>()?;
        Ok(EbiOutput::Object(
            EbiObject::StochasticBusinessProcessModelAndNotation(
                bpmn.mine_random_stochastic_business_process_model_and_notation(
                    Fraction::random_seed(),
                )?,
            ),
        ))
    },
    output_type: &EbiOutputType::ObjectType(
        EbiObjectType::StochasticBusinessProcessModelAndNotation,
    ),
};

pub const EBI_DISCOVER_RANDOM_SLPN: EbiCommand = EbiCommand::Command {
    name_short: "slpn",
    name_long: Some("stochastic-labelled-Petri-net"),
    explanation_short: "Give each transition a random weight between 0 (exclusive) and 1 (inclusive).",
    explanation_long: None,
    latex_link: None,
    cli_command: None,
    exact_arithmetic: true,
    input_types: &[&[&EbiInputType::Object(EbiObjectType::LabelledPetriNet)]],
    input_names: &["LPN"],
    input_helps: &["A labelled Petri net."],
    execute: |mut inputs, _| {
        let lpn = inputs.remove(0).to_type::<LabelledPetriNet>()?;
        Ok(EbiOutput::Object(EbiObject::StochasticLabelledPetriNet(
            lpn.mine_uniform_stochastic_lpn(),
        )))
    },
    output_type: &EbiOutputType::ObjectType(EbiObjectType::StochasticLabelledPetriNet),
};

pub const EBI_DISCOVER_RANDOM_SPTREE: EbiCommand = EbiCommand::Command {
    name_short: "sptree",
    name_long: Some("stochastic-process-tree"),
    explanation_short: "Give each leaf a random weight between 0 (exclusive) and 1 (inclusive).",
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
            tree.mine_random_stochastic_process_tree(Fraction::random_seed()),
        )))
    },
    output_type: &EbiOutputType::ObjectType(EbiObjectType::StochasticProcessTree),
};

pub const EBI_DISCOVER_UNIFORM: EbiCommand = EbiCommand::Group {
    name_short: "uni",
    name_long: Some("uniform"),
    explanation_short: "Give each transition a weight of 1.",
    explanation_long: None,
    children: &[
        &EBI_DISCOVER_UNIFORM_SBPMN,
        &EBI_DISCOVER_UNIFORM_SLPN,
        &EBI_DISCOVER_UNIFORM_SPTREE,
    ],
};

pub const EBI_DISCOVER_UNIFORM_SLPN: EbiCommand = EbiCommand::Command {
    name_short: "slpn",
    name_long: Some("stochastic-labelled-Petri-net"),
    explanation_short: "Give each transition a weight of 1.",
    explanation_long: None,
    latex_link: None,
    cli_command: None,
    exact_arithmetic: true,
    input_types: &[&[&EbiInputType::Object(EbiObjectType::LabelledPetriNet)]],
    input_names: &["LPN"],
    input_helps: &["A labelled Petri net."],
    execute: |mut inputs, _| {
        let lpn = inputs.remove(0).to_type::<LabelledPetriNet>()?;
        Ok(EbiOutput::Object(EbiObject::StochasticLabelledPetriNet(
            lpn.mine_uniform_stochastic_lpn(),
        )))
    },
    output_type: &EbiOutputType::ObjectType(EbiObjectType::StochasticLabelledPetriNet),
};

pub const EBI_DISCOVER_UNIFORM_SPTREE: EbiCommand = EbiCommand::Command {
    name_short: "sptree",
    name_long: Some("stochastic-process-tree"),
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

pub const EBI_DISCOVER_UNIFORM_SBPMN: EbiCommand = EbiCommand::Command {
    name_short: "sbpmn",
    name_long: Some("stochastic-business-process-model-and-notation"),
    explanation_short: "Give each sequence flow a weight of 1.",
    explanation_long: None,
    latex_link: None,
    cli_command: None,
    exact_arithmetic: true,
    input_types: &[&[&EbiInputType::Object(
        EbiObjectType::BusinessProcessModelAndNotation,
    )]],
    input_names: &["BPMN"],
    input_helps: &["A business-process-model-and-notation model."],
    execute: |mut inputs, _| {
        let bpmn = inputs
            .remove(0)
            .to_type::<BusinessProcessModelAndNotation>()?;
        Ok(EbiOutput::Object(
            EbiObject::StochasticBusinessProcessModelAndNotation(
                bpmn.mine_uniform_stochastic_business_process_model_and_notation()?,
            ),
        ))
    },
    output_type: &EbiOutputType::ObjectType(
        EbiObjectType::StochasticBusinessProcessModelAndNotation,
    ),
};
