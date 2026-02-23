use crate::{
    EbiInputTypeEnum,
    ebi_framework::{
        ebi_command::EbiCommand,
        ebi_input::EbiInputType,
        ebi_output::{EbiOutput, EbiOutputType},
    },
    techniques::filter::{EventSelector, Filter, Operator},
};
use ebi_objects::{EbiObject, EbiObjectType, EventLog, EventLogXes, HasActivityKey};
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
    explanation_short: "Remove traces that have a given length.",
    explanation_long: None,
    latex_link: None,
    cli_command: None,
    exact_arithmetic: true,
    input_types: &[
        &[&EbiInputType::Object(EbiObjectType::EventLogXes)],
        &[&EbiInputTypeEnum!(Operator)],
        &[&EbiInputType::Usize(Some(0), None, None)],
    ],
    input_names: &["event log", "operator", "value"],
    input_helps: &["the log", "the operator", "the value"],
    execute: |mut inputs, _| {
        let mut log = inputs.remove(0).to_type::<EventLogXes>()?;
        let operator = inputs.remove(0).to_type::<Operator>()?;
        let value = inputs.remove(0).to_type::<usize>()?;

        log.remove_traces_length(*operator, *value);

        Ok(EbiOutput::Object(EbiObject::EventLogXes(*log)))
    },
    output_type: &EbiOutputType::ObjectType(EbiObjectType::EventLogXes),
};

pub const EBI_FILTER_TRACES_EMPTY: EbiCommand = EbiCommand::Command {
    name_short: "empty",
    name_long: None,
    explanation_short: "Remove empty traces.",
    explanation_long: None,
    latex_link: None,
    cli_command: None,
    exact_arithmetic: true,
    input_types: &[&[&EbiInputType::Object(EbiObjectType::EventLogXes)]],
    input_names: &["event log"],
    input_helps: &["the log"],
    execute: |mut inputs, _| {
        let mut log = inputs.remove(0).to_type::<EventLog>()?;

        log.remove_traces_empty();

        Ok(EbiOutput::Object(EbiObject::EventLog(*log)))
    },
    output_type: &EbiOutputType::ObjectType(EbiObjectType::EventLogXes),
};

pub const EBI_FILTER_TRACES_WITH_EVENT: EbiCommand = EbiCommand::Group {
    name_short: "event",
    name_long: None,
    explanation_short: "Remove traces that have event(s) as specified.",
    explanation_long: None,
    children: &[&EBI_FILTER_TRACES_EVENT_ACTIVITY],
};

pub const EBI_FILTER_TRACES_EVENT_ACTIVITY: EbiCommand = EbiCommand::Command {
    name_short: "act",
    name_long: Some("activity"),
    explanation_short: "Remove traces that have event(s) as specified.",
    explanation_long: None,
    latex_link: None,
    cli_command: None,
    exact_arithmetic: true,
    input_types: &[
        &[&EbiInputType::Object(EbiObjectType::EventLogXes)],
        &[&EbiInputTypeEnum!(EventSelector)],
        &[&EbiInputType::String(None, None)],
    ],
    input_names: &["event log", "event selector", "activity"],
    input_helps: &[
        "the log",
        "which event(s) in the trace should be the activity",
        "the activity the filter targets",
    ],
    execute: |mut inputs, _| {
        let mut log = inputs.remove(0).to_type::<EventLog>()?;
        let activity_label = *inputs.remove(0).to_type::<String>()?;
        let event_selector = inputs.remove(0).to_type::<EventSelector>()?;

        let activity = log.activity_key_mut().process_activity(&activity_label);

        log.remove_traces_event_activity(*event_selector, activity);

        Ok(EbiOutput::Object(EbiObject::EventLog(*log)))
    },
    output_type: &EbiOutputType::ObjectType(EbiObjectType::EventLogXes),
};
