use anyhow::Context;
use ebi_arithmetic::{Fraction, Zero};
use ebi_objects::{DirectlyFollowsGraph, EbiObject, EbiObjectType};

use crate::{ebi_framework::{ebi_command::EbiCommand, ebi_input::{EbiInput, EbiInputType}, ebi_output::{EbiOutput, EbiOutputType}, ebi_trait::EbiTrait, ebi_trait_object::EbiTraitObject}, ebi_traits::{ebi_trait_event_log::EbiTraitEventLog, ebi_trait_finite_stochastic_language::EbiTraitFiniteStochasticLanguage, ebi_trait_stochastic_deterministic_semantics::EbiTraitStochasticDeterministicSemantics}, math::constant_fraction::ConstFraction, techniques::{completeness::Completeness, edge_difference::EdgeDifference, medoid, probability_queries::ProbabilityQueries, process_variety::ProcessVariety}};

pub const EBI_ANALYSE: EbiCommand = EbiCommand::Group {
    name_short: "ana",
    name_long: Some("analyse"),
    explanation_short: "Analyse a stochastic language.",
    explanation_long: None,
    children: &[
        &EBI_ANALYSE_ALL,
        &EBI_ANALYSE_COMPLETENESS,
        &EBI_ANALYSE_COVERAGE,
        &EBI_ANALYSE_DIRECTLY_FOLLOWS_EDGE_DIFFERENCE,
        &EBI_ANALYSE_MEDOID,
        &EBI_ANALYSE_MINPROB,
        &EBI_ANALYSE_MODE,
        &EBI_ANALYSE_MOSTLIKELY,
        &EBI_ANALYSE_VARIETY,
    ],
};

pub const EBI_ANALYSE_ALL: EbiCommand = EbiCommand::Command {
    name_short: "all", 
    name_long: Some("all-traces"),
    library_name: "ebi_commands::ebi_command_analyse::EBI_ANALYSE_ALL", 
    explanation_short: "Find all traces of a stochastic model.", 
    explanation_long: Some("List all traces of a stohastic model. Models containing loops and unbounded models are not supported and the computation will run forever."),
    cli_command: None, 
    latex_link: None,
    exact_arithmetic: true,
    input_types: &[ 
        &[ &EbiInputType::Trait(EbiTrait::FiniteStochasticLanguage), &EbiInputType::Trait(EbiTrait::StochasticDeterministicSemantics) ], 
    ],
    input_names: &[ "FILE" ],
    input_helps: &[ "Any object with deterministic stochastic semantics."],
    execute: |mut objects, _| {

        let object = objects.remove(0);
        let result = match object {
            EbiInput::Trait(EbiTraitObject::FiniteStochasticLanguage(slang), _) => {
                slang.as_ref().to_finite_stochastic_language()
            },
            EbiInput::Trait(EbiTraitObject::StochasticDeterministicSemantics(semantics), _) => {
                semantics.analyse_minimum_probability(&Fraction::zero()).with_context(|| "could not analyse language")?
            },
            _ => unreachable!()
        };
        return Ok(EbiOutput::Object(EbiObject::FiniteStochasticLanguage(result)));
    }, 
    output_type: &EbiOutputType::ObjectType(EbiObjectType::FiniteStochasticLanguage)
};


pub const EBI_ANALYSE_COMPLETENESS: EbiCommand = EbiCommand::Command {
    name_short: "comp", 
    name_long: Some("completeness"), 
    library_name: "ebi_commands::ebi_command_analyse::EBI_ANALYSE_COMPLETENESS",
    explanation_short: "Estimate the completeness of an event log using species discovery.", 
    explanation_long: None,
    cli_command: None, 
    latex_link: Some("~\\cite{DBLP:conf/icpm/KabierskiRW23}"),
    exact_arithmetic: true,
    input_types: &[ &[ &EbiInputType::Trait(EbiTrait::EventLog) ] ],
    input_names: &[ "FILE" ],
    input_helps: &[ "An event log."],
    execute: |mut objects, _| {
        let log = objects.remove(0).to_type::<dyn EbiTraitEventLog>()?;
        
        let result = log.to_multiset().estimate_completeness();
        
        return Ok(EbiOutput::Fraction(result));
    }, 
    output_type: &EbiOutputType::Fraction
};

pub const EBI_ANALYSE_COVERAGE: EbiCommand = EbiCommand::Command {
    name_short: "cov", 
    name_long: Some("coverage"), 
    library_name: "ebi_commands::ebi_command_analyse::EBI_ANALYSE_COVERAGE",
    explanation_short: "Find the most-likely traces that together cover a minimum probability.", 
    explanation_long: Some("Find the most-likely traces that together cover the given minimum probability.
Will return a finite stochastic language with the extracted traces.
The computation may not terminate if the model has non-decreasing livelocks, or if the model is unbounded and this unboundedness can be triggered using silent transitions, or if the model has livelocks."),
    cli_command: None, 
    latex_link: None,
    exact_arithmetic: true,
    input_types: &[ 
        &[ &EbiInputType::Trait(EbiTrait::FiniteStochasticLanguage), &EbiInputType::Trait(EbiTrait::StochasticDeterministicSemantics) ], 
        &[ &EbiInputType::Fraction(Some(ConstFraction::zero()), Some(ConstFraction::one()), None)]
    ],
    input_names: &[ "FILE", "MINIMUM_COVERAGE"],
    input_helps: &[ "Any object with deterministic stochastic semantics.", "The minimum probability that a trace should have to be included."],
    execute: |mut objects, _| {
        let model = objects.remove(0);
        let coverage = objects.remove(0).to_type::<Fraction>()?;

        let result = match model {
            EbiInput::Trait(EbiTraitObject::FiniteStochasticLanguage(slang), _) => {
                slang.analyse_probability_coverage(&coverage).context("Analysing language.")?
            },
            EbiInput::Trait(EbiTraitObject::StochasticDeterministicSemantics(semantics), _) => {
                semantics.analyse_probability_coverage(&coverage).context("Analysing language.")?
            },
            _ => unreachable!()
        };
        return Ok(EbiOutput::Object(EbiObject::FiniteStochasticLanguage(result)));
    }, 
    output_type: &EbiOutputType::ObjectType(EbiObjectType::FiniteStochasticLanguage)
};

pub const EBI_ANALYSE_DIRECTLY_FOLLOWS_EDGE_DIFFERENCE: EbiCommand = EbiCommand::Command {
    name_short: "dfgedi", 
    name_long: Some("directly-follows-edge-difference"), 
    library_name: "ebi_commands::ebi_command_analyse::EBI_ANALYSE_DIRECTLY_FOLLOWS_EDGE_DIFFERENCE",
    explanation_short: "The number of edges that differ between two directly follows graphs.", 
    explanation_long: None,
    cli_command: None, 
    latex_link: None,
    exact_arithmetic: true,
    input_types: &[ 
        &[ &EbiInputType::Object(EbiObjectType::DirectlyFollowsGraph) ], 
        &[ &EbiInputType::Object(EbiObjectType::DirectlyFollowsGraph)]
    ],
    input_names: &[ "DFG_1", "DFG_2"],
    input_helps: &[ "A directly follows graph.", "A directly follows graph."],
    execute: |mut objects, _| {
        let mut dfg1 = objects.remove(0).to_type::<DirectlyFollowsGraph>()?;
        let mut dfg2 = objects.remove(0).to_type::<DirectlyFollowsGraph>()?;
        
        let difference = dfg1.edge_difference(&mut dfg2);

        return Ok(EbiOutput::Fraction(difference));
    }, 
    output_type: &EbiOutputType::Fraction
};

pub const EBI_ANALYSE_MINPROB: EbiCommand = EbiCommand::Command {
    name_short: "minprob", 
    name_long: Some("minimum-probability-traces"), 
    library_name: "ebi_commands::ebi_command_analyse::EBI_ANALYSE_MINPROB",
    explanation_short: "Find all traces that have a given minimum probability.", 
    explanation_long: Some("Find all traces that have a given minimum probability.
Will return a finate stochastic language with the extracted traces.
Will return an error if there are no such traces.
The computation may not terminate if the model is unbounded and this unboundedness can be triggered using silent transitions."),
    cli_command: None, 
    latex_link: None,
    exact_arithmetic: true,
    input_types: &[ 
        &[ &EbiInputType::Trait(EbiTrait::StochasticDeterministicSemantics) ], 
        &[ &EbiInputType::Fraction(Some(ConstFraction::zero()), Some(ConstFraction::one()), None)]
    ],
    input_names: &[ "FILE", "MINIMUM_PROBABILITY"],
    input_helps: &[ "Any object with deterministic stochastic semantics.", "The minimum probability that a trace should have to be included."],
    execute: |mut objects, _| {
        let semantics = objects.remove(0).to_type::<EbiTraitStochasticDeterministicSemantics>()?;
        let at_least = objects.remove(0).to_type::<Fraction>()?;
        let result = semantics.analyse_minimum_probability(&at_least).context("could not analyse")?;
        return Ok(EbiOutput::Object(EbiObject::FiniteStochasticLanguage(result)));
    }, 
    output_type: &EbiOutputType::ObjectType(EbiObjectType::FiniteStochasticLanguage)
};

pub const EBI_ANALYSE_MOSTLIKELY: EbiCommand = EbiCommand::Command {
    name_short: "mostlikely", 
    name_long: Some("most-likely-traces"), 
    library_name: "ebi_commands::ebi_command_analyse::EBI_ANALYSE_MOSTLIKELY",
    explanation_short: "Find the traces with the highest probabilities.", 
    explanation_long: Some("Find the given number of traces with the highest probabilities.
If there are more than one trace with the same probability, an arbitrary choice is made which one to return.
The computation may run forever if the model is unbounded.
Computation is more efficient for an object with a finite stochastic language."),
    cli_command: None, 
    latex_link: None,
    exact_arithmetic: true,
    input_types: &[ 
        &[ &EbiInputType::Trait(EbiTrait::FiniteStochasticLanguage), &EbiInputType::Trait(EbiTrait::StochasticDeterministicSemantics)], 
        &[ &EbiInputType::Usize(Some(1), None, None)] 
    ],
    input_names: &[ "FILE", "NUMBER_OF_TRACES"],
    input_helps: &[ "Any object with deterministic stochastic semantics.", "The number of traces that should be extracted."],
    execute: |mut objects, _| {
        let object = objects.remove(0);
        let number_of_traces = objects.remove(0).to_type::<usize>()?;
        let result = match object {
            EbiInput::Trait(EbiTraitObject::FiniteStochasticLanguage(slang), _) => {
                slang.analyse_most_likely_traces(&number_of_traces).context("Analysing language.")?
            },
            EbiInput::Trait(EbiTraitObject::StochasticDeterministicSemantics(semantics), _) => {
                semantics.analyse_most_likely_traces(&number_of_traces).context("Analysing language.")?
            },
            _ => unreachable!()
        };
        return Ok(EbiOutput::Object(EbiObject::FiniteStochasticLanguage(result)));
    }, 
    output_type: &EbiOutputType::ObjectType(EbiObjectType::FiniteStochasticLanguage)
};

pub const EBI_ANALYSE_MODE: EbiCommand = EbiCommand::Command { 
    name_short: "mode", 
    name_long: None,
    library_name: "ebi_commands::ebi_command_analyse::EBI_ANALYSE_MODE",
    explanation_short: "Find a trace with the highest probability in a stochastic model.", 
    explanation_long: Some("Find the trace with the highest probability.
If there is more than one trace with the highest probability, an arbitrary choice is made which one to return.
The computation may run forever if the model is unbounded.
Equivalent to `Ebi evaluate mostlikely 1`.
Computation is more efficient for a model with a finite stochastic language."),
    latex_link: None, 
    cli_command: None, 
    exact_arithmetic: true, 
    input_types: &[ 
        &[ &EbiInputType::Trait(EbiTrait::FiniteStochasticLanguage), &EbiInputType::Trait(EbiTrait::StochasticDeterministicSemantics)],  
    ],
    input_names: &[ "FILE" ],
    input_helps: &[ "Any object with deterministic stochastic semantics." ],
    execute: |mut objects, _| {
        let number_of_traces = 1;
        let result = match objects.remove(0) {
            EbiInput::Trait(EbiTraitObject::FiniteStochasticLanguage(slang), _) => {
                slang.analyse_most_likely_traces(&number_of_traces).context("Analysing language.")?
            },
            EbiInput::Trait(EbiTraitObject::StochasticDeterministicSemantics(semantics), _) => {
                semantics.analyse_most_likely_traces(&number_of_traces).context("Analysing language.")?
            },
            _ => unreachable!()
        };
        return Ok(EbiOutput::Object(EbiObject::FiniteStochasticLanguage(result)));
    }, 
    output_type: &EbiOutputType::ObjectType(EbiObjectType::FiniteStochasticLanguage)
};

pub const EBI_ANALYSE_MEDOID: EbiCommand = EbiCommand::Command { 
    name_short: "med", 
    name_long: Some("medoid"),
    library_name: "ebi_commands::ebi_command_analyse::EBI_ANALYSE_MEDOID",
    explanation_short: "Find the traces with the least distance to the other traces.", 
    explanation_long: Some("Find the traces with the lowest average normalised Levenshtein distance to the other traces.
If there are more than one such trace, an arbitrary one is returned."), 
    latex_link: None, 
    cli_command: None, 
    exact_arithmetic: true, 
    input_types: &[ 
        &[ &EbiInputType::Trait(EbiTrait::FiniteStochasticLanguage)], 
        &[ &EbiInputType::Usize(None, None, Some(1))],
    ],
    input_names: &[ "FILE", "NUMBER_OF_TRACES"],
    input_helps: &[ "Any object with a finite stochastic language.", "The number of traces that should be extracted."],
    execute: |mut objects, _| {
        let language = objects.remove(0).to_type::<dyn EbiTraitFiniteStochasticLanguage>()?;
        let number_of_traces = objects.remove(0).to_type::<usize>()?;
        let result = medoid::medoid(language.as_ref(), &number_of_traces)?;
        return Ok(EbiOutput::Object(EbiObject::FiniteLanguage(result)));
    }, 
    output_type: &EbiOutputType::ObjectType(EbiObjectType::FiniteLanguage)
};

pub const EBI_ANALYSE_VARIETY: EbiCommand = EbiCommand::Command {
    name_short: "var", 
    name_long: Some("variety"), 
    library_name: "ebi_commands::ebi_command_analyse::EBI_ANALYSE_VARIETY",
    explanation_short: "Compute the variety of a stochastic language. That is, the average distance between two arbitrary traces in the language.", 
    explanation_long: None,
    cli_command: None, 
    latex_link: None,
    exact_arithmetic: true,
    input_types: &[ &[ &EbiInputType::Trait(EbiTrait::FiniteStochasticLanguage) ] ],
    input_names: &[ "FILE" ],
    input_helps: &[ "An event log."],
    execute: |mut objects, _| {
        let log = objects.remove(0).to_type::<dyn EbiTraitFiniteStochasticLanguage>()?;
        
        let result = log.rao_stirling_diversity();
        
        return Ok(EbiOutput::Fraction(result));
    }, 
    output_type: &EbiOutputType::Fraction
};