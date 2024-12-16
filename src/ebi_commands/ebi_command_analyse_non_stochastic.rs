use crate::{ebi_framework::{ebi_command::EbiCommand, ebi_input::EbiInputType, ebi_object::{EbiObject, EbiObjectType}, ebi_output::{EbiOutput, EbiOutputType}, ebi_trait::EbiTrait}, ebi_traits::{ebi_trait_event_log::EbiTraitEventLog, ebi_trait_finite_language::EbiTraitFiniteLanguage, ebi_trait_finite_stochastic_language::EbiTraitFiniteStochasticLanguage, ebi_trait_semantics::EbiTraitSemantics}, techniques::{align::Align, executions::FindExecutions, medoid_non_stochastic::MedoidNonStochastic}};


pub const EBI_ANALYSE_NON_STOCHASTIC: EbiCommand = EbiCommand::Group {
    name_short: "anans",
    name_long: Some("analyse-non-stochastic"),
    explanation_short: "Analyse a language without considering its stochastic perspective.",
    explanation_long: None,
    children: &[
        &EBI_ANALYSE_NON_STOCHASTIC_CLUSTER,
        &EBI_ANALYSE_NON_STOCHASTIC_MEDOID,
        &EBI_ANALYSE_NON_STOCHASTIC_ALIGNMENT,
        &EBI_ANALYSE_NON_STOCHASTIC_EXECUTIONS,
    ],
};

pub const EBI_ANALYSE_NON_STOCHASTIC_MEDOID: EbiCommand = EbiCommand::Command { 
    name_short: "med", 
    name_long: Some("medoid"),
    explanation_short: "Find the traces with the least distance to the other traces, without considering the stochastic perspective.", 
    explanation_long: Some("Find the traces with the lowest average normalised Levenshtein distance to the other traces; ties are resolved arbritrarily. The computation is random and does not take into account how often each trace occurs."), 
    latex_link: None, 
    cli_command: None, 
    exact_arithmetic: true, 
    input_types: &[ 
        &[ &EbiInputType::Trait(EbiTrait::FiniteLanguage)], 
        &[ &EbiInputType::Usize] 
    ],
    input_names: &[ "FILE", "NUMBER_OF_TRACES"],
    input_helps: &[ "The finite stochastic language.", "The number of traces that should be extracted."],
    execute: |mut objects, _| {
        let language = objects.remove(0).to_type::<dyn EbiTraitFiniteLanguage>()?;
        let number_of_traces = objects.remove(0).to_type::<usize>()?;
        let language: Box<dyn EbiTraitFiniteLanguage> = language;
        let result = language.medoid(*number_of_traces)?;
        return Ok(EbiOutput::Object(EbiObject::FiniteLanguage(result)));
    }, 
    output_type: &EbiOutputType::ObjectType(EbiObjectType::FiniteLanguage)
};

pub const EBI_ANALYSE_NON_STOCHASTIC_CLUSTER: EbiCommand = EbiCommand::Command {
    name_short: "clus", 
    name_long: Some("cluster"),
    explanation_short: "Apply k-medoid clustering on a finite set of traces, without considering the stochastic perspective.", 
    explanation_long: Some("Apply k-medoid clustering: group the traces into a given number of clusters, such that the average distance of each trace to its closest medoid is minimal. The computation is random and does not take into account how often each trace occurs."), 
    latex_link: Some("~\\cite{DBLP:journals/is/SchubertR21}"), 
    cli_command: None, 
    exact_arithmetic: true, 
    input_types: &[ 
        &[ &EbiInputType::Trait(EbiTrait::FiniteLanguage)], 
        &[ &EbiInputType::Usize ] 
    ],
    input_names: &[ "LANGUAGE", "NUMBER_OF_CLUSTERS"],
    input_helps: &[ "The finite stochastic language.", "The number of clusters."],
    execute: |mut objects, _| {
        let language = objects.remove(0).to_type::<dyn EbiTraitFiniteLanguage>()?;
        let number_of_clusters = objects.remove(0).to_type::<usize>()?;
        let result = language.k_medoids_clustering(*number_of_clusters)?;
        return Ok(EbiOutput::Object(EbiObject::FiniteLanguage(result)));
    }, 
    output_type: &EbiOutputType::ObjectType(EbiObjectType::FiniteLanguage)
};

pub const EBI_ANALYSE_NON_STOCHASTIC_ALIGNMENT: EbiCommand = EbiCommand::Command {
    name_short: "ali", 
    name_long: Some("alignment"),
    explanation_short: "Compute alignments.", 
    explanation_long: Some("Compute alignments.\nNB 1: the model must be able to terminate and its states must be bounded.\nNB 2: the search performed is not optimised. For Petri nets, the ProM implementation may be more efficient."), 
    latex_link: Some("~\\cite{DBLP:conf/edoc/AdriansyahDA11}"), 
    cli_command: None, 
    exact_arithmetic: true, 
    input_types: &[ 
        &[ &EbiInputType::Trait(EbiTrait::FiniteStochasticLanguage)],
        &[ &EbiInputType::Trait(EbiTrait::Semantics)]
    ],
    input_names: &[ "FILE_1", "FILE_2"],
    input_helps: &[ "The finite language.", "The model."],
    execute: |mut objects, _| {
        let log = objects.remove(0).to_type::<dyn EbiTraitFiniteStochasticLanguage>()?;
        let mut model = objects.remove(0).to_type::<EbiTraitSemantics>()?;
        
        let result = model.align_stochastic_language(log)?;
        
        return Ok(EbiOutput::Object(EbiObject::StochasticLanguageOfAlignments(result)));
    }, 
    output_type: &EbiOutputType::ObjectType(EbiObjectType::StochasticLanguageOfAlignments)
};

pub const EBI_ANALYSE_NON_STOCHASTIC_EXECUTIONS: EbiCommand = EbiCommand::Command {
    name_short: "exe", 
    name_long: Some("executions"),
    explanation_short: "Compute the executions of each transition of the model in the log.", 
    explanation_long: Some("Compute executions.\nNB 1: the model must be able to terminate and its states must be bounded.\nNB 2: the search performed is not optimised. For Petri nets, the ProM implementation may be more efficient."), 
    latex_link: None, 
    cli_command: None, 
    exact_arithmetic: true, 
    input_types: &[ 
        &[ &EbiInputType::Trait(EbiTrait::EventLog)],
        &[ &EbiInputType::Trait(EbiTrait::Semantics)]
    ],
    input_names: &[ "LOG", "MODEL"],
    input_helps: &[ "The event log.", "The model."],
    execute: |mut objects, _| {
        let log = objects.remove(0).to_type::<dyn EbiTraitEventLog>()?;
        let model = objects.remove(0).to_type::<EbiTraitSemantics>()?;
        
        let result = model.find_executions(log)?;
        
        return Ok(EbiOutput::Object(EbiObject::Executions(result)));
    }, 
    output_type: &EbiOutputType::ObjectType(EbiObjectType::Executions)
};