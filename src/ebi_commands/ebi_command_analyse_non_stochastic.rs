use crate::{
    ebi_framework::{
        ebi_command::EbiCommand,
        ebi_input::{EbiInput, EbiInputType},
        ebi_output::{EbiOutput, EbiOutputType},
        ebi_trait::EbiTrait,
    },
    ebi_traits::{
        ebi_trait_activities::EbiTraitActivities, ebi_trait_event_log::EbiTraitEventLog,
        ebi_trait_finite_language::EbiTraitFiniteLanguage, ebi_trait_semantics::EbiTraitSemantics,
    },
    techniques::{
        any_traces::AnyTraces, bounded::Bounded, executions::FindExecutions,
        infinitely_many_traces::InfinitelyManyTraces, medoid_non_stochastic::MedoidNonStochastic,
    },
};
use anyhow::anyhow;
use ebi_objects::{EbiObject, EbiObjectType};
use std::io::Write;

pub const EBI_ANALYSE_NON_STOCHASTIC: EbiCommand = EbiCommand::Group {
    name_short: "anans",
    name_long: Some("analyse-non-stochastic"),
    explanation_short: "Analyse a language without considering its stochastic perspective.",
    explanation_long: None,
    children: &[
        &EBI_ANALYSE_NON_STOCHASTIC_ACTIVITIES,
        &EBI_ANALYSE_NON_STOCHASTIC_BOUNDED,
        &EBI_ANALYSE_NON_STOCHASTIC_CLUSTER,
        &EBI_ANALYSE_NON_STOCHASTIC_EXECUTIONS,
        &EBI_ANALYSE_NON_STOCHASTIC_ANY_TRACES,
        &EBI_ANALYSE_NON_STOCHASTIC_INFINITELY_MANY_TRACES,
        &EBI_ANALYSE_NON_STOCHASTIC_MEDOID,
    ],
};

pub const EBI_ANALYSE_NON_STOCHASTIC_ACTIVITIES: EbiCommand = EbiCommand::Command {
    name_short: "act",
    name_long: Some("activities"),
    library_name: "ebi_commands::ebi_command_analyse_non_stochastic::EBI_ANALYSE_NON_STOCHASTIC_ACTIVITIES",
    explanation_short: "Shows the activities that are declared in the object.",
    explanation_long: None,
    latex_link: None,
    cli_command: None,
    exact_arithmetic: true,
    input_types: &[&[&EbiInputType::Trait(EbiTrait::Activities)]],
    input_names: &["FILE"],
    input_helps: &["Any object with activities"],
    execute: |mut objects, _| {
        let activities = objects.remove(0).to_type::<dyn EbiTraitActivities>()?;
        let mut f = vec![];
        for label in &activities.activity_key().activity2name {
            writeln!(f, "{}", label)?;
        }
        Ok(EbiOutput::String(String::from_utf8(f)?))
    },
    output_type: &EbiOutputType::String,
};

pub const EBI_ANALYSE_NON_STOCHASTIC_BOUNDED: EbiCommand = EbiCommand::Command {
    name_short: "bnd",
    name_long: Some("bounded"),
    library_name: "ebi_commands::ebi_command_analyse_non_stochastic::EBI_ANALYSE_NON_STOCHASTIC_BOUNDED",
    explanation_short: "Compute whether the model has a bounded state space.",
    explanation_long: Some(
        "Compute whether the model has a bounded state space. 
        For Petri nets, a coverability graph is computed~\\cite{esparza2019petri}. 
        For other types of models, `true' is returned.",
    ),
    latex_link: None,
    cli_command: None,
    exact_arithmetic: true,
    input_types: &[&[
        &EbiInputType::Object(EbiObjectType::DirectlyFollowsGraph),
        &EbiInputType::Object(EbiObjectType::StochasticProcessTree),
        &EbiInputType::Object(EbiObjectType::ProcessTree),
        &EbiInputType::Object(EbiObjectType::StochasticDirectlyFollowsModel),
        &EbiInputType::Object(EbiObjectType::DirectlyFollowsModel),
        &EbiInputType::Object(EbiObjectType::EventLog),
        &EbiInputType::Object(EbiObjectType::FiniteStochasticLanguage),
        &EbiInputType::Object(EbiObjectType::FiniteLanguage),
        &EbiInputType::Object(EbiObjectType::StochasticDeterministicFiniteAutomaton),
        &EbiInputType::Object(EbiObjectType::DeterministicFiniteAutomaton),
        &EbiInputType::Object(EbiObjectType::StochasticLabelledPetriNet),
        &EbiInputType::Object(EbiObjectType::LabelledPetriNet),
    ]],
    input_names: &["MODEL"],
    input_helps: &["The model."],
    execute: |mut objects, _| {
        let model = objects.remove(0);
        let result = match model {
            EbiInput::Object(EbiObject::ProcessTree(tree), _) => tree.bounded()?,
            EbiInput::Object(EbiObject::StochasticProcessTree(tree), _) => tree.bounded()?,
            EbiInput::Object(EbiObject::LabelledPetriNet(lpn), _) => lpn.bounded()?,
            EbiInput::Object(EbiObject::StochasticLabelledPetriNet(slpn), _) => slpn.bounded()?,
            EbiInput::Object(EbiObject::DeterministicFiniteAutomaton(dfa), _) => dfa.bounded()?,
            EbiInput::Object(EbiObject::StochasticDeterministicFiniteAutomaton(sdfa), _) => {
                sdfa.bounded()?
            }
            EbiInput::Object(EbiObject::EventLog(object), _) => object.bounded()?,
            EbiInput::Object(EbiObject::FiniteLanguage(object), _) => object.bounded()?,
            EbiInput::Object(EbiObject::FiniteStochasticLanguage(object), _) => object.bounded()?,
            EbiInput::Object(EbiObject::DirectlyFollowsModel(object), _) => object.bounded()?,
            EbiInput::Object(EbiObject::StochasticDirectlyFollowsModel(object), _) => {
                object.bounded()?
            }
            EbiInput::Object(EbiObject::DirectlyFollowsGraph(object), _) => object.bounded()?,

            EbiInput::Trait(_, _) => {
                return Err(anyhow!("Cannot compute whether object is bounded."));
            }
            EbiInput::String(_, _) => {
                return Err(anyhow!("Cannot compute whether object is bounded."));
            }
            EbiInput::Usize(_, _) => {
                return Err(anyhow!("Cannot compute whether object is bounded."));
            }
            EbiInput::FileHandler(_) => {
                return Err(anyhow!("Cannot compute whether object is bounded."));
            }
            EbiInput::Fraction(_, _) => {
                return Err(anyhow!("Cannot compute whether object is bounded."));
            }
            EbiInput::Object(EbiObject::Executions(_), _) => {
                return Err(anyhow!("Cannot compute whether object is bounded."));
            }
            EbiInput::Object(EbiObject::LanguageOfAlignments(_), _) => {
                return Err(anyhow!("Cannot compute whether object is bounded."));
            }
            EbiInput::Object(EbiObject::StochasticLanguageOfAlignments(_), _) => {
                return Err(anyhow!("Cannot compute whether object is bounded."));
            }
            EbiInput::Object(EbiObject::ScalableVectorGraphics(_), _) => {
                return Err(anyhow!("Cannot compute whether object is bounded."));
            }
        };
        if result {
            log::debug!("The model has a bounded state space.");
        } else {
            log::debug!("The model has an unbounded state space.")
        }
        return Ok(EbiOutput::Bool(result));
    },
    output_type: &EbiOutputType::Bool,
};

pub const EBI_ANALYSE_NON_STOCHASTIC_CLUSTER: EbiCommand = EbiCommand::Command {
    name_short: "clus",
    name_long: Some("cluster"),
    library_name: "ebi_commands::ebi_command_analyse_non_stochastic::EBI_ANALYSE_NON_STOCHASTIC_CLUSTER",
    explanation_short: "Apply k-medoid clustering on a finite set of traces, without considering the stochastic perspective.",
    explanation_long: Some(
        "Apply k-medoid clustering: group the traces into a given number of clusters, such that the average distance of each trace to its closest medoid is minimal. The computation is random and does not take into account how often each trace occurs.",
    ),
    latex_link: Some("~\\cite{DBLP:journals/is/SchubertR21}"),
    cli_command: None,
    exact_arithmetic: true,
    input_types: &[
        &[&EbiInputType::Trait(EbiTrait::FiniteLanguage)],
        &[&EbiInputType::Usize(Some(1), None, None)],
    ],
    input_names: &["LANGUAGE", "NUMBER_OF_CLUSTERS"],
    input_helps: &["The finite stochastic language.", "The number of clusters."],
    execute: |mut objects, _| {
        let language = objects.remove(0).to_type::<dyn EbiTraitFiniteLanguage>()?;
        let number_of_clusters = objects.remove(0).to_type::<usize>()?;
        let result = language.k_medoids_clustering(*number_of_clusters)?;
        return Ok(EbiOutput::Object(EbiObject::FiniteLanguage(result)));
    },
    output_type: &EbiOutputType::ObjectType(EbiObjectType::FiniteLanguage),
};

pub const EBI_ANALYSE_NON_STOCHASTIC_EXECUTIONS: EbiCommand = EbiCommand::Command {
    name_short: "exe",
    name_long: Some("executions"),
    library_name: "ebi_commands::ebi_command_analyse_non_stochastic::EBI_ANALYSE_NON_STOCHASTIC_EXECUTIONS",
    explanation_short: "Compute the executions of each transition of the model in the log.",
    explanation_long: Some(
        "Compute executions.\nNB 1: the model must be able to terminate and its states must be bounded.\nNB 2: the search performed is not optimised. For Petri nets, the ProM implementation may be more efficient.",
    ),
    latex_link: None,
    cli_command: None,
    exact_arithmetic: true,
    input_types: &[
        &[&EbiInputType::Trait(EbiTrait::EventLog)],
        &[&EbiInputType::Trait(EbiTrait::Semantics)],
    ],
    input_names: &["LOG", "MODEL"],
    input_helps: &["The event log.", "The model."],
    execute: |mut objects, _| {
        let log = objects.remove(0).to_type::<dyn EbiTraitEventLog>()?;
        let mut model = objects.remove(0).to_type::<EbiTraitSemantics>()?;

        let result = model.find_executions(log)?;

        return Ok(EbiOutput::Object(EbiObject::Executions(result)));
    },
    output_type: &EbiOutputType::ObjectType(EbiObjectType::Executions),
};

pub const EBI_ANALYSE_NON_STOCHASTIC_ANY_TRACES: EbiCommand = EbiCommand::Command {
    name_short: "at",
    name_long: Some("any-traces"),
    library_name: "ebi_commands::ebi_command_analyse_non_stochastic::EBI_ANALYSE_NON_STOCHASTIC_ANY_TRACES",
    explanation_short: "Compute whether the model has any traces.",
    explanation_long: Some(
        "Compute whether the model has any traces.
        Reasons for a model not to have any traces could be if the initial state is part of a livelock, or if there is no initial state.
        'true' means that the model has traces, 'false' means that the model has no traces.
        The computation may not terminate if the model is unbounded.",
    ),
    latex_link: None,
    cli_command: None,
    exact_arithmetic: true,
    input_types: &[&[
        &EbiInputType::Object(EbiObjectType::DirectlyFollowsGraph),
        &EbiInputType::Object(EbiObjectType::StochasticProcessTree),
        &EbiInputType::Object(EbiObjectType::ProcessTree),
        &EbiInputType::Object(EbiObjectType::StochasticDirectlyFollowsModel),
        &EbiInputType::Object(EbiObjectType::DirectlyFollowsModel),
        &EbiInputType::Object(EbiObjectType::EventLog),
        &EbiInputType::Object(EbiObjectType::FiniteStochasticLanguage),
        &EbiInputType::Object(EbiObjectType::FiniteLanguage),
        &EbiInputType::Object(EbiObjectType::StochasticDeterministicFiniteAutomaton),
        &EbiInputType::Object(EbiObjectType::DeterministicFiniteAutomaton),
        &EbiInputType::Object(EbiObjectType::StochasticLabelledPetriNet),
        &EbiInputType::Object(EbiObjectType::LabelledPetriNet),
    ]],
    input_names: &["MODEL"],
    input_helps: &["The model."],
    execute: |mut objects, _| {
        let model = objects.remove(0);
        let result = match model {
            EbiInput::Object(EbiObject::ProcessTree(tree), _) => tree.any_traces()?,
            EbiInput::Object(EbiObject::StochasticProcessTree(tree), _) => tree.any_traces()?,
            EbiInput::Object(EbiObject::LabelledPetriNet(lpn), _) => lpn.any_traces()?,
            EbiInput::Object(EbiObject::StochasticLabelledPetriNet(slpn), _) => {
                slpn.any_traces()?
            }
            EbiInput::Object(EbiObject::DeterministicFiniteAutomaton(dfa), _) => {
                dfa.any_traces()?
            }
            EbiInput::Object(EbiObject::StochasticDeterministicFiniteAutomaton(sdfa), _) => {
                sdfa.any_traces()?
            }
            EbiInput::Object(EbiObject::EventLog(object), _) => object.any_traces()?,
            EbiInput::Object(EbiObject::FiniteLanguage(object), _) => object.any_traces()?,
            EbiInput::Object(EbiObject::FiniteStochasticLanguage(object), _) => {
                object.any_traces()?
            }
            EbiInput::Object(EbiObject::DirectlyFollowsModel(object), _) => object.any_traces()?,
            EbiInput::Object(EbiObject::StochasticDirectlyFollowsModel(object), _) => object.any_traces()?,
            EbiInput::Object(EbiObject::DirectlyFollowsGraph(object), _) => object.any_traces()?,
            EbiInput::Trait(_, _) => {
                return Err(anyhow!("Cannot compute whether object has traces."));
            }
            EbiInput::String(_, _) => {
                return Err(anyhow!("Cannot compute whether object has traces."));
            }
            EbiInput::Usize(_, _) => {
                return Err(anyhow!("Cannot compute whether object has traces."));
            }
            EbiInput::FileHandler(_) => {
                return Err(anyhow!("Cannot compute whether object has traces."));
            }
            EbiInput::Fraction(_, _) => {
                return Err(anyhow!("Cannot compute whether object has traces."));
            }
            EbiInput::Object(EbiObject::Executions(_), _) => {
                return Err(anyhow!("Cannot compute whether object has traces."));
            }
            EbiInput::Object(EbiObject::LanguageOfAlignments(_), _) => {
                return Err(anyhow!("Cannot compute whether object has traces."));
            }
            EbiInput::Object(EbiObject::StochasticLanguageOfAlignments(_), _) => {
                return Err(anyhow!("Cannot compute whether object has traces."));
            }
            EbiInput::Object(EbiObject::ScalableVectorGraphics(_), _) => {
                return Err(anyhow!("Cannot compute whether object has traces."));
            }
        };
        if result {
            log::debug!("The model cannot terminate and has an empty language.");
        } else {
            log::debug!("The model can terminate and has traces.")
        }
        return Ok(EbiOutput::Bool(result));
    },
    output_type: &EbiOutputType::Bool,
};

pub const EBI_ANALYSE_NON_STOCHASTIC_INFINITELY_MANY_TRACES: EbiCommand = EbiCommand::Command {
    name_short: "inft",
    name_long: Some("infinitely-many-traces"),
    library_name: "ebi_commands::ebi_command_analyse_non_stochastic::EBI_ANALYSE_NON_STOCHASTIC_INFINITELY_MANY_TRACES",
    explanation_short: "Compute whether the model has infinitely many traces.",
    explanation_long: Some(
        "Compute whether the model has infinitely many traces. The computation may not terminate if the model is unbounded.",
    ),
    latex_link: None,
    cli_command: None,
    exact_arithmetic: true,
    input_types: &[&[
        &EbiInputType::Object(EbiObjectType::EventLog),
        &EbiInputType::Object(EbiObjectType::FiniteStochasticLanguage),
        &EbiInputType::Object(EbiObjectType::FiniteLanguage),
        &EbiInputType::Object(EbiObjectType::DirectlyFollowsGraph),
        &EbiInputType::Object(EbiObjectType::StochasticDirectlyFollowsModel),
        &EbiInputType::Object(EbiObjectType::DirectlyFollowsModel),
        &EbiInputType::Object(EbiObjectType::StochasticProcessTree),
        &EbiInputType::Object(EbiObjectType::ProcessTree),
        &EbiInputType::Object(EbiObjectType::StochasticDeterministicFiniteAutomaton),
        &EbiInputType::Object(EbiObjectType::DeterministicFiniteAutomaton),
        &EbiInputType::Object(EbiObjectType::StochasticLabelledPetriNet),
        &EbiInputType::Object(EbiObjectType::LabelledPetriNet),
    ]],
    input_names: &["MODEL"],
    input_helps: &["The model."],
    execute: |mut objects, _| {
        let model = objects.remove(0);
        let result = match model {
            EbiInput::Object(EbiObject::EventLog(object), _) => object.infinitely_many_traces()?,
            EbiInput::Object(EbiObject::FiniteLanguage(object), _) => {
                object.infinitely_many_traces()?
            }
            EbiInput::Object(EbiObject::FiniteStochasticLanguage(object), _) => {
                object.infinitely_many_traces()?
            }
            EbiInput::Object(EbiObject::StochasticProcessTree(object), _) => {
                object.infinitely_many_traces()?
            }
            EbiInput::Object(EbiObject::ProcessTree(object), _) => {
                object.infinitely_many_traces()?
            }
            EbiInput::Object(EbiObject::DeterministicFiniteAutomaton(object), _) => {
                object.infinitely_many_traces()?
            }
            EbiInput::Object(EbiObject::StochasticDeterministicFiniteAutomaton(object), _) => {
                object.infinitely_many_traces()?
            }
            EbiInput::Object(EbiObject::LabelledPetriNet(object), _) => {
                object.infinitely_many_traces()?
            }
            EbiInput::Object(EbiObject::StochasticLabelledPetriNet(object), _) => {
                object.infinitely_many_traces()?
            }
            EbiInput::Object(EbiObject::DirectlyFollowsGraph(object), _) => {
                object.infinitely_many_traces()?
            }
            EbiInput::Object(EbiObject::DirectlyFollowsModel(object), _) => {
                object.infinitely_many_traces()?
            }
            EbiInput::Object(EbiObject::StochasticDirectlyFollowsModel(object), _) => {
                object.infinitely_many_traces()?
            }
            _ => unreachable!(),
        };
        if result {
            log::debug!("The language of the model has infinitely many traces.");
        } else {
            log::debug!("The language of the model has finitely many traces.")
        }
        return Ok(EbiOutput::Bool(result));
    },
    output_type: &EbiOutputType::Bool,
};

pub const EBI_ANALYSE_NON_STOCHASTIC_MEDOID: EbiCommand = EbiCommand::Command {
    name_short: "med",
    name_long: Some("medoid"),
    library_name: "ebi_commands::ebi_command_analyse_non_stochastic::EBI_ANALYSE_NON_STOCHASTIC_MEDOID",
    explanation_short: "Find the traces with the least distance to the other traces, without considering the stochastic perspective.",
    explanation_long: Some(
        "Find the traces with the lowest average normalised Levenshtein distance to the other traces; ties are resolved arbritrarily. The computation is random and does not take into account how often each trace occurs.",
    ),
    latex_link: None,
    cli_command: None,
    exact_arithmetic: true,
    input_types: &[
        &[&EbiInputType::Trait(EbiTrait::FiniteLanguage)],
        &[&EbiInputType::Usize(None, None, Some(1))],
    ],
    input_names: &["FILE", "NUMBER_OF_TRACES"],
    input_helps: &[
        "The finite stochastic language.",
        "The number of traces that should be extracted.",
    ],
    execute: |mut objects, _| {
        let language = objects.remove(0).to_type::<dyn EbiTraitFiniteLanguage>()?;
        let number_of_traces = objects.remove(0).to_type::<usize>()?;
        let language: Box<dyn EbiTraitFiniteLanguage> = language;
        let result = language.medoid(*number_of_traces)?;
        return Ok(EbiOutput::Object(EbiObject::FiniteLanguage(result)));
    },
    output_type: &EbiOutputType::ObjectType(EbiObjectType::FiniteLanguage),
};
