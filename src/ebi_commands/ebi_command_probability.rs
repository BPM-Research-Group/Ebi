use std::{path::PathBuf, io::{self, IsTerminal}};
use anyhow::{Result, Ok, Context, anyhow};
use clap::{arg, value_parser, Arg, ArgAction, ArgMatches, Command};
use fraction::{One, Zero};

use crate::{ebi_input_output::EbiInputType, ebi_objects::ebi_object::{EbiObject, EbiObjectType}, ebi_traits::{ebi_trait::EbiTrait, ebi_trait_finite_language::EbiTraitFiniteLanguage, ebi_trait_finite_stochastic_language::EbiTraitFiniteStochasticLanguage, ebi_trait_queriable_stochastic_language::EbiTraitQueriableStochasticLanguage}, export::{self, EbiOutput, EbiOutputType}, follower_semantics::FollowerSemantics, import, math::fraction::Fraction};

use super::ebi_command::EbiCommand;

pub const EBI_PROBABILITY: EbiCommand = EbiCommand::Group { 
    name_short: "prob",
    name_long: Some("probability"),
    explanation_short: "Compute the probability of a trace or specification on the model.", 
    explanation_long: None, 
    children: &[
        &EBI_PROBABILITY_MODEL,
        &EBI_PROBABILITY_TRACE
    ]
};

pub const EBI_PROBABILITY_MODEL: EbiCommand = EbiCommand::Command { 
    name_short: "mod", 
    name_long: Some("model"), 
    explanation_short: "Compute the probability that a queriable stochastic language (stochastic model) produces any trace of the model.", 
    explanation_long: None, 
    latex_link: Some("~\\cite{DBLP:journals/is/LeemansMM24}"), 
    cli_command: None, 
    exact_arithmetic: true, 
    input_types: &[ 
        &[ &EbiInputType::Trait(EbiTrait::QueriableStochasticLanguage) ], 
        &[ &EbiInputType::Trait(EbiTrait::FiniteLanguage) ] 
    ], 
    input_names: &[ "FILE_1", "FILE_2" ], 
    input_helps: &[ "The queriable stochastic language (model).", "The finite language (log)." ], 
    execute: |mut inputs, cli_matches| {
        let mut model: Box<dyn EbiTraitQueriableStochasticLanguage> = inputs.remove(0).to_type::<dyn EbiTraitQueriableStochasticLanguage>()?;
        let mut log = inputs.remove(0).to_type::<dyn EbiTraitFiniteLanguage>()?;
        
        let mut sum = Fraction::zero();
        for trace in log.iter() {
            sum += model.get_probability(&FollowerSemantics::Trace(&trace)).with_context(|| format!("cannot compute probability of trace {:?}", trace))?;
        }
        return Ok(EbiOutput::Fraction(sum));
    }, 
    output: &EbiOutputType::Fraction,
};

pub const EBI_PROBABILITY_TRACE: EbiCommand = EbiCommand::Command { 
    name_short: "trac", 
    name_long: Some("trace"), 
    explanation_short: "Compute the probability of a trace in a queriable stochastic language (model).", 
    explanation_long: None, 
    latex_link: Some("~\\cite{DBLP:journals/is/LeemansMM24}"), 
    cli_command: Some(|command| {
        command.arg(Arg::new("trace")
            .action(ArgAction::Set)
            .value_name("TRACE")
            .help("The trace.")
            .required(true)
            .value_parser(value_parser!(String))
            .num_args(0..))
    }), 
    exact_arithmetic: true, 
    input_types: &[ 
        &[ &EbiInputType::Trait(EbiTrait::QueriableStochasticLanguage) ] 
    ], 
    input_names: &[ "FILE" ], 
    input_helps: &[ "The queriable stochastic language (model)." ], 
    execute: |mut inputs, cli_matches| {
        let mut model = inputs.remove(0).to_type::<dyn EbiTraitQueriableStochasticLanguage>()?;
        if let Some(x) = cli_matches.get_many::<String>("trace") {
            let t: Vec<&String> = x.collect();
            let trace = t.into_iter().map(|activity| activity.as_str()).collect::<Vec<_>>();
            let trace = model.get_activity_key_mut().process_trace_ref(&trace);

            log::trace!("compute probability of trace {:?}", trace);
        
            let result = model.get_probability(&FollowerSemantics::Trace(&trace)).with_context(|| format!("cannot compute probability of trace {:?}", trace))?;
            return Ok(EbiOutput::Fraction(result));
        } else {
            return Err(anyhow!("no trace given"));
        }
    }, 
    output: &EbiOutputType::Fraction,
};