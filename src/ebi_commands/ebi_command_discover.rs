use crate::{
    ebi_framework::{
        ebi_command::EbiCommand,
        ebi_input::EbiInputType,
        ebi_object::{EbiObject, EbiObjectType},
        ebi_output::{EbiOutput, EbiOutputType},
        ebi_trait::EbiTrait,
    },
    ebi_objects::{labelled_petri_net::LabelledPetriNet, process_tree::ProcessTree},
    ebi_traits::ebi_trait_finite_stochastic_language::EbiTraitFiniteStochasticLanguage,
    techniques::{
        alignment_stochastic_miner::AlignmentMiner,
        occurrences_stochastic_miner::OccurrencesStochasticMiner,
        uniform_stochastic_miner::{UniformStochasticMinerLPN, UniformStochasticMinerTree},
    },
};

pub const EBI_DISCOVER: EbiCommand = EbiCommand::Group {
    name_short: "disc",
    name_long: Some("discover"),
    explanation_short: "Discover a process model.",
    explanation_long: None,
    children: &[
        &EBI_DISCOVER_ALIGNMENTS,
        &EBI_DISCOVER_OCCURRENCE,
        &EBI_DISCOVER_UNIFORM,
    ],
};

pub const EBI_DISCOVER_ALIGNMENTS: EbiCommand = EbiCommand::Command {
    name_short: "ali",
    name_long: Some("alignments"),
    explanation_short: "Give each transition a weight that matches the aligned occurrences of its label. The model must be livelock-free.",
    explanation_long: None,
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

pub const EBI_DISCOVER_OCCURRENCE: EbiCommand = EbiCommand::Command {
    name_short: "occ",
    name_long: Some("occurrence"),
    explanation_short: "Give each transition a weight that matches the occurrences of its label; silent transitions get a weight of 1.",
    explanation_long: None,
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
            lpn.mine_occurrences_stochastic(language),
        )))
    },
    output_type: &EbiOutputType::ObjectType(EbiObjectType::StochasticLabelledPetriNet),
};

pub const EBI_DISCOVER_UNIFORM: EbiCommand = EbiCommand::Group {
    name_short: "uni",
    name_long: Some("uniform"),
    explanation_short: "Give each transition a weight of 1.",
    explanation_long: None,
    children: &[&EBI_DISCOVER_UNIFORM_LPN, &EBI_DISCOVER_UNIFORM_PTREE],
};

pub const EBI_DISCOVER_UNIFORM_LPN: EbiCommand = EbiCommand::Command {
    name_short: "lpn",
    name_long: Some("labelled-petri-net"),
    explanation_short: "Give each transition a weight of 1 in a labelled Petri net.",
    explanation_long: None,
    latex_link: None,
    cli_command: None,
    exact_arithmetic: true,
    input_types: &[&[&EbiInputType::Object(EbiObjectType::LabelledPetriNet)]],
    input_names: &["MODEL"],
    input_helps: &["A labelled Petri net."],
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
