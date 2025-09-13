use anyhow::Context;
use std::io::Write;

use crate::{
    ebi_framework::{
        ebi_command::EbiCommand,
        ebi_input::EbiInputType,
        ebi_output::{EbiOutput, EbiOutputType},
        ebi_trait::EbiTrait,
    },
    ebi_info,
    ebi_traits::ebi_trait_event_log::EbiTraitEventLog,
    techniques::association::Associations,
};

pub const DEFAULT_NUMBER_OF_SAMPLES: usize = 500;

pub const EBI_ASSOCIATION: EbiCommand = EbiCommand::Group {
    name_short: "asso",
    name_long: Some("association"),
    explanation_short: "Compute associations between a process and other aspects.",
    explanation_long: None,
    children: &[&ASSOCIATION_ATTRIBUTE, &ASSOCIATION_ATTRIBUTES],
};

pub const ASSOCIATION_ATTRIBUTE: EbiCommand = EbiCommand::Command {
    name_short: "att",
    name_long: Some("trace-attribute"),
    library_name: "ebi_commands::ebi_command_association::ASSOCIATION_ATTRIBUTE",
    explanation_short: "Compute the association between the process and a trace attribute.",
    explanation_long: None,
    latex_link: Some("\\cite{DBLP:journals/tkde/LeemansMPH23}"),
    cli_command: None,
    exact_arithmetic: true,
    input_types: &[
        &[&EbiInputType::Trait(EbiTrait::EventLog)],
        &[&EbiInputType::String(None, None)],
        &[&EbiInputType::Usize(
            Some(1),
            None,
            Some(DEFAULT_NUMBER_OF_SAMPLES),
        )],
    ],
    input_names: &["FILE", "ATTRIBUTE", "SAMPLES"],
    input_helps: &[
        "The event log for which association is to be computed.",
        concat!(
            concat!(
                "The trace attribute for which association is to be computed. The trace attributes of a log can be found using `Ebi ",
                ebi_info!()
            ),
            "`."
        ),
        "The number of samples.",
    ],
    execute: |mut inputs, _| {
        let event_log = inputs.remove(0).to_type::<dyn EbiTraitEventLog>()?;
        let attribute = inputs.remove(0).to_type::<String>()?;
        let number_of_samples = inputs.remove(0).to_type::<usize>()?;

        let ass = event_log
            .association(*number_of_samples, &attribute)
            .with_context(|| format!("attribute {}", attribute))?;

        Ok(EbiOutput::ContainsRoot(ass))
    },
    output_type: &EbiOutputType::ContainsRoot,
};

pub const ASSOCIATION_ATTRIBUTES: EbiCommand = EbiCommand::Command {
    name_short: "atts",
    name_long: Some("all-trace-attributes"),
    library_name: "ebi_commands::ebi_command_association::ASSOCIATION_ATTRIBUTES",
    explanation_short: "Compute the association between the process and trace attributes.",
    explanation_long: None,
    latex_link: Some("\\cite{DBLP:journals/tkde/LeemansMPH23}"),
    cli_command: None,
    exact_arithmetic: true,
    input_types: &[
        &[&EbiInputType::Trait(EbiTrait::EventLog)],
        &[&EbiInputType::Usize(
            Some(1),
            None,
            Some(DEFAULT_NUMBER_OF_SAMPLES),
        )],
    ],
    input_names: &["FILE", "SAMPLES"],
    input_helps: &[
        "The event log for which association is to be computed.",
        "The number of samples taken.",
    ],
    execute: |mut inputs, _| {
        let event_log = inputs.remove(0).to_type::<dyn EbiTraitEventLog>()?;
        let number_of_samples = inputs.remove(0).to_type::<usize>()?;

        let result = event_log.associations(*number_of_samples);

        let mut f = vec![];
        for (x, y) in result {
            let y = y?;
            writeln!(
                f,
                "Trace atribute `{}` has an association of approximately {}. Exact value: {:?}",
                x,
                &y.approximate(),
                &y
            )?;
        }
        Ok(EbiOutput::String(String::from_utf8(f).unwrap()))
    },
    output_type: &EbiOutputType::String,
};
