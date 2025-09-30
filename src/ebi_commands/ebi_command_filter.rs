use crate::{
    EbiInputTypeEnum,
    ebi_framework::{
        ebi_command::EbiCommand,
        ebi_input::EbiInputType,
        ebi_output::{EbiOutput, EbiOutputType},
    },
    techniques::filter::{EventSelector, Operator},
};
use ebi_objects::{EbiObject, EbiObjectType, EventLog, HasActivityKey};
use strum::VariantNames;

pub const EBI_FILTER: EbiCommand = EbiCommand::Group {
    name_short: "fil",
    name_long: Some("filter"),
    explanation_short: "Filter an event log.",
    explanation_long: None,
    children: &[&EBI_FILTER_TRACES],
};

pub const EBI_FILTER_TRACES: EbiCommand = EbiCommand::Group {
    name_short: "tr",
    name_long: Some("traces"),
    explanation_short: "Remove traces that do not satisfy the filter.",
    explanation_long: None,
    children: &[
        &EBI_FILTER_TRACES_EMPTY,
        &EBI_FILTER_TRACES_LENGTH,
        &EBI_FILTER_TRACES_WITH_EVENT,
    ],
};

pub const EBI_FILTER_TRACES_LENGTH: EbiCommand = EbiCommand::Command {
    name_short: "len",
    name_long: Some("length"),
    explanation_short: "Remove traces that do not have a given length.",
    explanation_long: None,
    latex_link: None,
    cli_command: None,
    exact_arithmetic: true,
    input_types: &[
        &[&EbiInputType::Object(EbiObjectType::EventLog)],
        &[&EbiInputTypeEnum!(Operator)],
        &[&EbiInputType::Usize(Some(0), None, None)],
    ],
    input_names: &["operator", "value", "event log"],
    input_helps: &["the operator", "the value", "the log"],
    execute: |mut inputs, _| {
        let mut log = inputs.remove(0).to_type::<EventLog>()?;
        let operator = inputs.remove(0).to_type::<Operator>()?;
        let value = inputs.remove(0).to_type::<usize>()?;

        log.retain_traces_mut(&mut |(trace, _)| operator.apply(trace.len(), *value));

        Ok(EbiOutput::Object(EbiObject::EventLog(*log)))
    },
    output_type: &EbiOutputType::ObjectType(EbiObjectType::EventLog),
};

pub const EBI_FILTER_TRACES_EMPTY: EbiCommand = EbiCommand::Command {
    name_short: "empty",
    name_long: None,
    explanation_short: "Remove empty traces.",
    explanation_long: None,
    latex_link: None,
    cli_command: None,
    exact_arithmetic: true,
    input_types: &[&[&EbiInputType::Object(EbiObjectType::EventLog)]],
    input_names: &["event log"],
    input_helps: &["the log"],
    execute: |mut inputs, _| {
        let mut log = inputs.remove(0).to_type::<EventLog>()?;

        log.retain_traces_mut(&mut |(trace, _)| trace.len() != 0);

        Ok(EbiOutput::Object(EbiObject::EventLog(*log)))
    },
    output_type: &EbiOutputType::ObjectType(EbiObjectType::EventLog),
};

pub const EBI_FILTER_TRACES_WITH_EVENT: EbiCommand = EbiCommand::Group {
    name_short: "event",
    name_long: None,
    explanation_short: "Remove traces that do not have event(s) as specified.",
    explanation_long: None,
    children: &[&EBI_FILTER_TRACES_EVENT_ACTIVITY],
};

pub const EBI_FILTER_TRACES_EVENT_ACTIVITY: EbiCommand = EbiCommand::Command {
    name_short: "act",
    name_long: Some("activity"),
    explanation_short: "Remove all traces that do not have event(s) as specified.",
    explanation_long: None,
    latex_link: None,
    cli_command: None,
    exact_arithmetic: true,
    input_types: &[
        &[&EbiInputType::Object(EbiObjectType::EventLog)],
        &[&EbiInputTypeEnum!(EventSelector)],
        &[&EbiInputType::String(None, None)],
    ],
    input_names: &["event log", "event selector", "activity"],
    input_helps: &[
        "the log",
        "event selector",
        "the activity the filter targets",
    ],
    execute: |mut inputs, _| {
        let activity_label = *inputs.remove(0).to_type::<String>()?;
        let event_selector = inputs.remove(0).to_type::<EventSelector>()?;
        let mut log = inputs.remove(0).to_type::<EventLog>()?;

        let activity = log.activity_key_mut().process_activity(&activity_label);

        log.retain_traces_mut(&mut |(trace, _)| {
            event_selector.apply(trace, |act| act == &activity)
        });

        Ok(EbiOutput::Object(EbiObject::EventLog(*log)))
    },
    output_type: &EbiOutputType::ObjectType(EbiObjectType::EventLog),
};
