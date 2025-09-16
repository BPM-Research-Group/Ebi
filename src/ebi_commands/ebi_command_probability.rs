use anyhow::{Context, anyhow};
use clap::{Arg, ArgAction, value_parser};
use ebi_arithmetic::Fraction;
use ebi_objects::{EbiObject, EbiObjectType, HasActivityKey};

use crate::{
    ebi_framework::{
        ebi_command::EbiCommand,
        ebi_input::EbiInputType,
        ebi_output::{EbiOutput, EbiOutputType},
        ebi_trait::EbiTrait,
    },
    ebi_traits::{
        ebi_trait_finite_language::EbiTraitFiniteLanguage,
        ebi_trait_queriable_stochastic_language::EbiTraitQueriableStochasticLanguage,
        ebi_trait_stochastic_semantics::EbiTraitStochasticSemantics,
    },
    follower_semantics::FollowerSemantics,
    math::constant_fraction::ConstFraction,
    techniques::explain_trace::ExplainTrace,
};

pub const EBI_PROBABILITY: EbiCommand = EbiCommand::Group {
    name_short: "prob",
    name_long: Some("probability"),
    explanation_short: "Compute the probability of a trace or specification on the model.",
    explanation_long: None,
    children: &[
        &EBI_PROBABILITY_LOG,
        &EBI_PROBABILITY_TRACE,
        &EBI_PROBABILITY_EXPLAIN_TRACE,
    ],
};

pub const EBI_PROBABILITY_LOG: EbiCommand = EbiCommand::Command {
    name_short: "log",
    name_long: None,
    library_name: "ebi_commands::ebi_command_probability::EBI_PROBABILITY_LOG",
    explanation_short: "Compute the probability that a stochastic model produces any trace of a log.",
    explanation_long: None,
    latex_link: Some("~\\cite{DBLP:journals/is/LeemansMM24}"),
    cli_command: None,
    exact_arithmetic: true,
    input_types: &[
        &[&EbiInputType::Trait(EbiTrait::QueriableStochasticLanguage)],
        &[&EbiInputType::Trait(EbiTrait::FiniteLanguage)],
    ],
    input_names: &["FILE_1", "FILE_2"],
    input_helps: &[
        "The queriable stochastic language (model).",
        "The finite language (log).",
    ],
    execute: |mut inputs, _| {
        let mut model: Box<dyn EbiTraitQueriableStochasticLanguage> =
            inputs
                .remove(0)
                .to_type::<dyn EbiTraitQueriableStochasticLanguage>()?;
        let log = inputs.remove(0).to_type::<dyn EbiTraitFiniteLanguage>()?;

        let sum = model
            .get_probability_language(log)
            .with_context(|| "cannot compute probability")?;

        return Ok(EbiOutput::Fraction(sum));
    },
    output_type: &EbiOutputType::Fraction,
};

pub const EBI_PROBABILITY_TRACE: EbiCommand = EbiCommand::Command {
    name_short: "trac",
    name_long: Some("trace"),
    library_name: "ebi_commands::ebi_command_probability::EBI_PROBABILITY_TRACE",
    explanation_short: "Compute the probability of a trace in a stochastic model.",
    explanation_long: None,
    latex_link: Some("~\\cite{DBLP:journals/is/LeemansMM24}"),
    cli_command: Some(|command| {
        command.arg(
            Arg::new("trace")
                .action(ArgAction::Set)
                .value_name("TRACE")
                .help("The trace.")
                .required(true)
                .value_parser(value_parser!(String))
                .num_args(0..),
        )
    }),
    exact_arithmetic: true,
    input_types: &[&[&EbiInputType::Trait(EbiTrait::QueriableStochasticLanguage)]],
    input_names: &["FILE"],
    input_helps: &["The queriable stochastic language (model)."],
    execute: |mut inputs, cli_matches| {
        let mut model = inputs
            .remove(0)
            .to_type::<dyn EbiTraitQueriableStochasticLanguage>()?;
        if let Some(x) = cli_matches.unwrap().get_many::<String>("trace") {
            let t: Vec<&String> = x.collect();
            let trace = t
                .into_iter()
                .map(|activity| activity.as_str())
                .collect::<Vec<_>>();
            let trace = model.activity_key_mut().process_trace_ref(&trace);

            log::trace!("compute probability of trace {:?}", trace);

            let result = model
                .get_probability(&FollowerSemantics::Trace(&trace))
                .with_context(|| format!("cannot compute probability of trace {:?}", trace))?;
            return Ok(EbiOutput::Fraction(result));
        } else {
            return Err(anyhow!("no trace given"));
        }
    },
    output_type: &EbiOutputType::Fraction,
};

pub const EBI_PROBABILITY_EXPLAIN_TRACE: EbiCommand = EbiCommand::Command {
    name_short: "exptra",
    name_long: Some("explain-trace"),
    library_name: "ebi_commands::ebi_command_probability::EBI_PROBABILITY_EXPLAIN_TRACE",
    explanation_short: "Compute the most likely explanation of a trace given the stochastic model.",
    explanation_long: None,
    latex_link: None,
    cli_command: Some(|command| {
        command.arg(
            Arg::new("trace")
                .action(ArgAction::Set)
                .value_name("TRACE")
                .help("The trace.")
                .required(true)
                .value_parser(value_parser!(String))
                .num_args(0..),
        )
    }),
    exact_arithmetic: true,
    input_types: &[
        &[&EbiInputType::Trait(EbiTrait::StochasticSemantics)],
        &[&EbiInputType::Fraction(
            Some(ConstFraction::zero()),
            Some(ConstFraction::one()),
            None,
        )],
    ],
    input_names: &["FILE", "VALUE"],
    input_helps: &[
        "The model.",
        "Balance between 0 (=only consider deviations) to 1 (=only consider weight in the model)",
    ],
    execute: |mut inputs, cli_matches| {
        let mut semantics = inputs.remove(0).to_type::<EbiTraitStochasticSemantics>()?;
        let balance = inputs.remove(0).to_type::<Fraction>()?;
        if let Some(x) = cli_matches.unwrap().get_many::<String>("trace") {
            let t: Vec<&String> = x.collect();
            let trace = t
                .into_iter()
                .map(|activity| activity.as_str())
                .collect::<Vec<_>>();
            let trace = semantics.activity_key_mut().process_trace_ref(&trace);

            log::trace!("explain the trace {:?} given the model", trace);

            let result = semantics
                .explain_trace(&trace, &balance)
                .with_context(|| format!("cannot explain the trace {:?}", trace))?;
            return Ok(EbiOutput::Object(EbiObject::LanguageOfAlignments(result)));
        } else {
            return Err(anyhow!("no trace given"));
        }
    },
    output_type: &EbiOutputType::ObjectType(EbiObjectType::LanguageOfAlignments),
};
