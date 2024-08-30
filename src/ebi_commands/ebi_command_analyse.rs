use std::{io::{self, IsTerminal}, path::PathBuf};

use clap::{value_parser, Arg, ArgAction, ArgMatches, Command};
use anyhow::{anyhow, Context, Result};
use process_mining::event_log::stream_xes::Mode;
use crate::{analysis::FiniteStochasticLanguageAnalyser, completeness, ebi_input_output::{EbiInput, EbiInputType}, ebi_objects::ebi_object::{EbiObject, EbiObjectType, EbiTraitObject}, ebi_traits::{ebi_trait::EbiTrait, ebi_trait_event_log::EbiTraitEventLog, ebi_trait_finite_language::EbiTraitFiniteLanguage, ebi_trait_finite_stochastic_language::EbiTraitFiniteStochasticLanguage, ebi_trait_stochastic_deterministic_semantics::EbiTraitStochasticDeterministicSemantics}, export::{EbiOutput, EbiOutputType}, math::fraction::{Fraction, FractionNotParsedYet}, medoid};

use super::ebi_command::EbiCommand;

pub const EBI_ANALYSE: EbiCommand = EbiCommand::Group {
    name_short: "ana",
    name_long: Some("analyse"),
    explanation_short: "Analyse a stochastic language.",
    explanation_long: None,
    children: &[
        &EBI_ANALYSE_ALL,
        &EBI_ANALYSE_COMPLETENESS,
        &EBI_ANALYSE_MEDOID,
        &EBI_ANALYSE_MINPROB,
        &EBI_ANALYSE_MODE,
        &EBI_ANALYSE_MOSTLIKELY,
    ],
};

pub const EBI_ANALYSE_ALL: EbiCommand = EbiCommand::Command {
    name_short: "all", 
    name_long: Some("all-traces"), 
    explanation_short: "Find all traces.", 
    explanation_long: Some("Find all traces.\nModels containing loops are not supported."),
    cli_command: None, 
    latex_link: None,
    exact_arithmetic: true,
    input_types: &[ 
        &[ &EbiInputType::Trait(EbiTrait::FiniteStochasticLanguage), &EbiInputType::Trait(EbiTrait::StochasticDeterministicSemantics) ], 
    ],
    input_names: &[ "FILE" ],
    input_helps: &[ "The file with an object that has deterministic stochastic semantics."],
    execute: |mut objects, _| {

        let object = objects.remove(0);
        let result = match object {
            EbiInput::Trait(EbiTraitObject::FiniteStochasticLanguage(slang), _) => {
                slang.as_ref().to_finite_stochastic_language()
            },
            EbiInput::Trait(EbiTraitObject::StochasticDeterministicSemantics(semantics), _) => {
                semantics.analyse_minimum_probability(&Fraction::zero()).context("Analysing language.")?
            },
            _ => unreachable!()
        };
        return Ok(EbiOutput::Object(EbiObject::FiniteStochasticLanguage(result)));

        let semantics = objects.remove(0).to_type::<EbiTraitStochasticDeterministicSemantics>()?;
        let at_least = objects.remove(0).to_type::<Fraction>()?;
        let result = semantics.analyse_minimum_probability(&at_least).context("could not analyse")?;
        return Ok(EbiOutput::Object(EbiObject::FiniteStochasticLanguage(result)));
    }, 
    output: &EbiOutputType::ObjectType(EbiObjectType::FiniteStochasticLanguage)
};


pub const EBI_ANALYSE_COMPLETENESS: EbiCommand = EbiCommand::Command {
    name_short: "comp", 
    name_long: Some("completeness"), 
    explanation_short: "Estimate the completeness of a finite language.", 
    explanation_long: None,
    cli_command: None, 
    latex_link: Some("~\\cite{DBLP:conf/icpm/KabierskiRW23}"),
    exact_arithmetic: true,
    input_types: &[ &[ &EbiInputType::Trait(EbiTrait::EventLog) ] ],
    input_names: &[ "FILE" ],
    input_helps: &[ "The event log."],
    execute: |mut objects, _| {
        let mut log = objects.remove(0).to_type::<dyn EbiTraitEventLog>()?;
        
        let result = completeness::estimate_completeness(&log.to_multiset());
        
        return Ok(EbiOutput::Fraction(result));
    }, 
    output: &EbiOutputType::Fraction
};

pub const EBI_ANALYSE_MINPROB: EbiCommand = EbiCommand::Command {
    name_short: "minprob", 
    name_long: Some("minimum-probability-traces"), 
    explanation_short: "Find all traces that have a given minimum probability.", 
    explanation_long: Some("Find all traces that have a given minimum probability.\nPlease be aware of models containing livelocks: these may cause the computation to never finish.\nWill return an error if there are no such traces."),
    cli_command: None, 
    latex_link: None,
    exact_arithmetic: true,
    input_types: &[ 
        &[ &EbiInputType::Trait(EbiTrait::StochasticDeterministicSemantics) ], 
        &[ &EbiInputType::Fraction]
    ],
    input_names: &[ "FILE", "MINIMUM_PROBABILITY"],
    input_helps: &[ "The file with an object that has deterministic stochastic semantics.", "The minimum probability that a trace should have to be included."],
    execute: |mut objects, _| {
        let semantics = objects.remove(0).to_type::<EbiTraitStochasticDeterministicSemantics>()?;
        let at_least = objects.remove(0).to_type::<Fraction>()?;
        let result = semantics.analyse_minimum_probability(&at_least).context("could not analyse")?;
        return Ok(EbiOutput::Object(EbiObject::FiniteStochasticLanguage(result)));
    }, 
    output: &EbiOutputType::ObjectType(EbiObjectType::FiniteStochasticLanguage)
};

pub const EBI_ANALYSE_MOSTLIKELY: EbiCommand = EbiCommand::Command {
    name_short: "mostlikely", 
    name_long: Some("most-likely-traces"), 
    explanation_short: "Find the traces with the highest probabilities.", 
    explanation_long: Some("Find the traces with the highest probabilities; ties are resolved arbritrarily.\nPlease be aware of models containing livelocks: these may cause the computation to never finish."),
    cli_command: None, 
    latex_link: None,
    exact_arithmetic: true,
    input_types: &[ 
        &[ &EbiInputType::Trait(EbiTrait::FiniteStochasticLanguage), &EbiInputType::Trait(EbiTrait::StochasticDeterministicSemantics)], 
        &[ &EbiInputType::Usize] 
    ],
    input_names: &[ "FILE", "NUMBER_OF_TRACES"],
    input_helps: &[ "The file with an object that has deterministic stochastic semantics.", "The number of traces that should be extracted."],
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
    output: &EbiOutputType::ObjectType(EbiObjectType::FiniteStochasticLanguage)
};

pub const EBI_ANALYSE_MODE: EbiCommand = EbiCommand::Command { 
    name_short: "mode", 
    name_long: None,
    explanation_short: "Find the trace with the highest probability.", 
    explanation_long: Some("Find the trace with the highest probability; ties are resolved arbritrarily. Equivalent to `Ebi evaluate mostlikely 1`."), 
    latex_link: None, 
    cli_command: None, 
    exact_arithmetic: true, 
    input_types: &[ 
        &[ &EbiInputType::Trait(EbiTrait::FiniteStochasticLanguage), &EbiInputType::Trait(EbiTrait::StochasticDeterministicSemantics)],  
    ],
    input_names: &[ "FILE" ],
    input_helps: &[ "The file with an object that has deterministic stochastic semantics." ],
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
    output: &EbiOutputType::ObjectType(EbiObjectType::FiniteStochasticLanguage)
};

pub const EBI_ANALYSE_MEDOID: EbiCommand = EbiCommand::Command { 
    name_short: "med", 
    name_long: Some("medoid"),
    explanation_short: "Find the traces with the least distance to the other traces.", 
    explanation_long: Some("Find the traces with the lowest average normalised Levenshtein distance to the other traces; ties are resolved arbritrarily."), 
    latex_link: None, 
    cli_command: None, 
    exact_arithmetic: true, 
    input_types: &[ 
        &[ &EbiInputType::Trait(EbiTrait::FiniteStochasticLanguage)], 
        &[ &EbiInputType::Usize] 
    ],
    input_names: &[ "FILE", "NUMBER_OF_TRACES"],
    input_helps: &[ "The finite stochastic language.", "The number of traces that should be extracted."],
    execute: |mut objects, _| {
        let language = objects.remove(0).to_type::<dyn EbiTraitFiniteStochasticLanguage>()?;
        let number_of_traces = objects.remove(0).to_type::<usize>()?;
        let result = medoid::medoid(language.as_ref(), &number_of_traces)?;
        return Ok(EbiOutput::Object(EbiObject::FiniteLanguage(result)));
    }, 
    output: &EbiOutputType::ObjectType(EbiObjectType::FiniteLanguage)
};

