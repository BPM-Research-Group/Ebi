use std::io::Write;
use clap::{value_parser, Arg, ArgAction, Command};
use anyhow::Context;

use crate::{ebi_framework::{ebi_command::EbiCommand, ebi_input::EbiInputType, ebi_output::{EbiOutput, EbiOutputType}, ebi_trait::EbiTrait}, ebi_info, ebi_traits::ebi_trait_event_log::EbiTraitEventLog, techniques::association::Associations};

macro_rules! number_of_samples {
    () => {500};
}
pub(crate) use number_of_samples;

pub const EBI_ASSOCIATION: EbiCommand = EbiCommand::Group {
    name_short: "asso",
    name_long: Some("association"), 
    explanation_short: "Compute associations between a process and other aspects.", 
    explanation_long: None, 
    children: &[
        &ASSOCIATION_ATTRIBUTE,
        &ASSOCIATION_ATTRIBUTES
    ]
};

pub const ASSOCIATION_ATTRIBUTE: EbiCommand = EbiCommand::Command { 
    name_short: "att", 
    name_long: Some("trace-attribute"),
    explanation_short: "Compute the association between the process and a trace attribute.", 
    explanation_long: Some(concat!("Compute the association between the process and a given trace attribute; ", number_of_samples!(), " samples are taken.")), 
    latex_link: Some("\\cite{DBLP:journals/tkde/LeemansMPH23}"), 
    cli_command: Some(|command| cli_number_of_samples(command)), 
    exact_arithmetic: true, 
    input_types: &[ 
        &[&EbiInputType::Trait(EbiTrait::EventLog)], 
        &[&EbiInputType::String] 
    ], 
    input_names: &[ "FILE", "ATTRIBUTE" ], 
    input_helps: &[ "The event log for which association is to be computed.", concat!(concat!("The trace attribute for which association is to be computed. The trace attributes of a log can be found using `Ebi ", ebi_info!()), "`.")], 
    execute: |mut inputs, cli_matches| {
        let mut event_log = inputs.remove(0).to_type::<dyn EbiTraitEventLog>()?;
        let attribute = inputs.remove(0).to_type::<String>()?;
        let number_of_samples = cli_matches.unwrap().get_one::<usize>("samples").unwrap();

        let ass = event_log.association(*number_of_samples, &attribute).with_context(|| format!("attribute {}", attribute))?;

        Ok(EbiOutput::ContainsRoot(ass))
    }, 
    output_type: &EbiOutputType::ContainsRoot
};

pub const ASSOCIATION_ATTRIBUTES: EbiCommand = EbiCommand::Command { 
    name_short: "atts", 
    name_long: Some("all-trace-attributes"),
    explanation_short: "Compute the association between the process and trace attributes.", 
    explanation_long: Some(concat!("Compute the association between the process and trace attributes; ", number_of_samples!(), " samples are taken.")), 
    latex_link: Some("\\cite{DBLP:journals/tkde/LeemansMPH23}"), 
    cli_command: Some(|command| cli_number_of_samples(command)), 
    exact_arithmetic: true, 
    input_types: &[ 
        &[ &EbiInputType::Trait(EbiTrait::EventLog) ], 
     ], 
    input_names: &[ "FILE" ], 
    input_helps: &[ "The event log for which association is to be computed." ], 
    execute: |mut inputs, cli_matches| {
        let mut event_log = inputs.remove(0).to_type::<dyn EbiTraitEventLog>()?;
        let number_of_samples = cli_matches.unwrap().get_one::<usize>("samples").unwrap();

        let result = event_log.associations(*number_of_samples);

        let mut f = vec![];
        for (x, y) in result {
            let y = y?;
            writeln!(f, "Trace atribute `{}` has an association of approximately {}. Exact value: {:?}", x, &y.approximate(), &y)?;
        }
        Ok(EbiOutput::String(String::from_utf8(f).unwrap()))
    }, 
    output_type: &EbiOutputType::String
};

pub fn cli_number_of_samples(command: Command) -> Command {
    command.arg(
        Arg::new("samples")
        .action(ArgAction::Set)
        .value_name("NUMBER")
        .short('s')
        .long("number-of-samples")
        .help("Take a number of samples.")
        .default_value(number_of_samples!().to_string())
        .value_parser(value_parser!(usize))
        .required(false)
    )
}