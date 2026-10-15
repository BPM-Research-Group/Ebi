use crate::{
    EbiInputTypeEnum,
    ebi_framework::{
        ebi_command::EbiCommand,
        ebi_input::EbiInputType,
        ebi_output::{EbiOutput, EbiOutputType},
        ebi_trait::EbiTrait,
    },
    ebi_traits::{
        ebi_trait_finite_language::EbiTraitFiniteLanguage,
        ebi_trait_finite_stochastic_language::EbiTraitFiniteStochasticLanguage,
    },
    techniques::{
        filter_stochastic_trace::FilterStochasticTraces,
        filter_trace::{EventSelector, FilterTrace, Operator},
    },
};
use ebi_objects::{EbiObject, EbiObjectType, EventLogXes, HasActivityKey};
use strum::VariantNames;

pub const EBI_FILTER: EbiCommand = EbiCommand::Group {
    name_short: "fil",
    name_long: Some("filter"),
    explanation_short: "Filter.",
    explanation_long: None,
    children: &[&EBI_FILTER_LANG, &EBI_FILTER_LOG, &EBI_FILTER_SLANG],
};

pub const EBI_FILTER_LANG: EbiCommand = EbiCommand::Group {
    name_short: "lang",
    name_long: Some("language"),
    explanation_short: "Filter a language.",
    explanation_long: None,
    children: &[&EBI_FILTER_LANG_TRACES],
};

pub const EBI_FILTER_LANG_TRACES: EbiCommand = EbiCommand::Group {
    name_short: "tr",
    name_long: Some("traces"),
    explanation_short: "Remove traces that do not satisfy the filter.",
    explanation_long: None,
    children: &[
        &EBI_FILTER_LANG_TRACES_EMPTY,
        &EBI_FILTER_LANG_TRACES_LENGTH,
        &EBI_FILTER_LANG_TRACES_WITH_EVENT,
    ],
};

pub const EBI_FILTER_LANG_TRACES_EMPTY: EbiCommand = EbiCommand::Command {
    name_short: "empty",
    name_long: None,
    explanation_short: "Remove empty traces.",
    explanation_long: None,
    latex_link: None,
    cli_command: None,
    exact_arithmetic: true,
    input_types: &[&[&EbiInputType::Trait(EbiTrait::FiniteLanguage)]],
    input_names: &["LANG"],
    input_helps: &["the language"],
    execute: |mut inputs, _| {
        let mut log = inputs.remove(0).to_type::<dyn EbiTraitFiniteLanguage>()?;

        log.remove_traces_empty();

        Ok(EbiOutput::Object(EbiObject::FiniteLanguage(
            log.to_finite_language(),
        )))
    },
    output_type: &EbiOutputType::ObjectType(EbiObjectType::FiniteLanguage),
};

pub const EBI_FILTER_LANG_TRACES_LENGTH: EbiCommand = EbiCommand::Command {
    name_short: "len",
    name_long: Some("length"),
    explanation_short: "Remove traces that have a given length.",
    explanation_long: None,
    latex_link: None,
    cli_command: None,
    exact_arithmetic: true,
    input_types: &[
        &[&EbiInputType::Trait(EbiTrait::FiniteLanguage)],
        &[&EbiInputTypeEnum!(Operator)],
        &[&EbiInputType::Usize(Some(0), None, None)],
    ],
    input_names: &["LANG", "OPERATOR", "THRESHOLD"],
    input_helps: &["the language", "the operator", "the threshold"],
    execute: |mut inputs, _| {
        let mut log = inputs.remove(0).to_type::<dyn EbiTraitFiniteLanguage>()?;
        let operator = inputs.remove(0).to_type::<Operator>()?;
        let value = inputs.remove(0).to_type::<usize>()?;

        log.remove_traces_length(*operator, *value);

        Ok(EbiOutput::Object(EbiObject::FiniteLanguage(
            log.to_finite_language(),
        )))
    },
    output_type: &EbiOutputType::ObjectType(EbiObjectType::FiniteLanguage),
};

pub const EBI_FILTER_LANG_TRACES_WITH_EVENT: EbiCommand = EbiCommand::Group {
    name_short: "event",
    name_long: None,
    explanation_short: "Remove traces that have event(s) as specified.",
    explanation_long: None,
    children: &[&EBI_FILTER_LANG_TRACES_EVENT_ACTIVITY],
};

pub const EBI_FILTER_LANG_TRACES_EVENT_ACTIVITY: EbiCommand = EbiCommand::Command {
    name_short: "act",
    name_long: Some("activity"),
    explanation_short: "Remove traces that have event(s) as specified.",
    explanation_long: None,
    latex_link: None,
    cli_command: None,
    exact_arithmetic: true,
    input_types: &[
        &[&EbiInputType::Trait(EbiTrait::FiniteLanguage)],
        &[&EbiInputTypeEnum!(EventSelector)],
        &[&EbiInputType::String(None, None)],
    ],
    input_names: &["LANG", "SELECTOR", "ACTIVITY"],
    input_helps: &[
        "the log",
        "which event(s) in the trace should be the activity",
        "the name of the activity that the filter targets",
    ],
    execute: |mut inputs, _| {
        let mut log = inputs.remove(0).to_type::<dyn EbiTraitFiniteLanguage>()?;
        let event_selector = inputs.remove(0).to_type::<EventSelector>()?;
        let activity_label = *inputs.remove(0).to_type::<String>()?;

        let activity = log.activity_key_mut().process_activity(&activity_label);

        log.remove_traces_event_activity(*event_selector, activity);

        Ok(EbiOutput::Object(EbiObject::FiniteLanguage(
            log.to_finite_language(),
        )))
    },
    output_type: &EbiOutputType::ObjectType(EbiObjectType::FiniteLanguage),
};

pub const EBI_FILTER_LOG: EbiCommand = EbiCommand::Group {
    name_short: "log",
    name_long: None,
    explanation_short: "Filter an event log.",
    explanation_long: None,
    children: &[&EBI_FILTER_LOG_TRACES],
};

pub const EBI_FILTER_LOG_TRACES: EbiCommand = EbiCommand::Group {
    name_short: "tr",
    name_long: Some("traces"),
    explanation_short: "Remove traces that do not satisfy the filter.",
    explanation_long: None,
    children: &[
        &EBI_FILTER_LOG_TRACES_EMPTY,
        &EBI_FILTER_LOG_TRACES_LENGTH,
        &EBI_FILTER_LOG_TRACES_WITH_EVENT,
    ],
};

pub const EBI_FILTER_LOG_TRACES_EMPTY: EbiCommand = EbiCommand::Command {
    name_short: "empty",
    name_long: None,
    explanation_short: "Remove empty traces.",
    explanation_long: None,
    latex_link: None,
    cli_command: None,
    exact_arithmetic: true,
    input_types: &[&[&EbiInputType::Object(EbiObjectType::EventLogXes)]],
    input_names: &["LOG"],
    input_helps: &["the log"],
    execute: |mut inputs, _| {
        let mut log = inputs.remove(0).to_type::<EventLogXes>()?;

        log.remove_traces_empty();

        Ok(EbiOutput::Object(EbiObject::EventLogXes(*log)))
    },
    output_type: &EbiOutputType::ObjectType(EbiObjectType::EventLogXes),
};

pub const EBI_FILTER_LOG_TRACES_LENGTH: EbiCommand = EbiCommand::Command {
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
    input_names: &["LOG", "OPERATOR", "THRESHOLD"],
    input_helps: &["the log", "the operator", "the threshold"],
    execute: |mut inputs, _| {
        let mut log = inputs.remove(0).to_type::<EventLogXes>()?;
        let operator = inputs.remove(0).to_type::<Operator>()?;
        let value = inputs.remove(0).to_type::<usize>()?;

        log.remove_traces_length(*operator, *value);

        Ok(EbiOutput::Object(EbiObject::EventLogXes(*log)))
    },
    output_type: &EbiOutputType::ObjectType(EbiObjectType::EventLogXes),
};

pub const EBI_FILTER_LOG_TRACES_WITH_EVENT: EbiCommand = EbiCommand::Group {
    name_short: "event",
    name_long: None,
    explanation_short: "Remove traces that have event(s) as specified.",
    explanation_long: None,
    children: &[&EBI_FILTER_LOG_TRACES_EVENT_ACTIVITY],
};

pub const EBI_FILTER_LOG_TRACES_EVENT_ACTIVITY: EbiCommand = EbiCommand::Command {
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
    input_names: &["LOG", "SELECTOR", "ACTIVITY"],
    input_helps: &[
        "the log",
        "which event(s) in the trace should be the activity",
        "the name of the activity that the filter targets",
    ],
    execute: |mut inputs, _| {
        let mut log = inputs.remove(0).to_type::<EventLogXes>()?;
        let event_selector = inputs.remove(0).to_type::<EventSelector>()?;
        let activity_label = *inputs.remove(0).to_type::<String>()?;

        let activity = log.activity_key_mut().process_activity(&activity_label);

        log.remove_traces_event_activity(*event_selector, activity);

        Ok(EbiOutput::Object(EbiObject::EventLogXes(*log)))
    },
    output_type: &EbiOutputType::ObjectType(EbiObjectType::EventLogXes),
};

pub const EBI_FILTER_SLANG: EbiCommand = EbiCommand::Group {
    name_short: "slang",
    name_long: Some("stochastic-language"),
    explanation_short: "Filter a stochastic language.",
    explanation_long: None,
    children: &[&EBI_FILTER_SLANG_TRACES],
};

pub const EBI_FILTER_SLANG_TRACES: EbiCommand = EbiCommand::Group {
    name_short: "tr",
    name_long: Some("traces"),
    explanation_short: "Remove traces that do not satisfy the filter.",
    explanation_long: None,
    children: &[
        &EBI_FILTER_SLANG_TRACES_EMPTY,
        &EBI_FILTER_SLANG_TRACES_LEAST_LIKELY,
        &EBI_FILTER_SLANG_TRACES_LENGTH,
        &EBI_FILTER_SLANG_TRACES_MOST_LIKELY,
        &EBI_FILTER_SLANG_TRACES_WITH_EVENT,
    ],
};

pub const EBI_FILTER_SLANG_TRACES_EMPTY: EbiCommand = EbiCommand::Command {
    name_short: "empty",
    name_long: None,
    explanation_short: "Remove empty traces.",
    explanation_long: None,
    latex_link: None,
    cli_command: None,
    exact_arithmetic: true,
    input_types: &[&[&EbiInputType::Trait(EbiTrait::FiniteStochasticLanguage)]],
    input_names: &["SLANG"],
    input_helps: &["the finite stochastic language"],
    execute: |mut inputs, _| {
        let mut log = inputs
            .remove(0)
            .to_type::<dyn EbiTraitFiniteStochasticLanguage>()?;

        log.remove_traces_empty();

        Ok(EbiOutput::Object(EbiObject::FiniteStochasticLanguage(
            log.to_finite_stochastic_language(),
        )))
    },
    output_type: &EbiOutputType::ObjectType(EbiObjectType::FiniteStochasticLanguage),
};

pub const EBI_FILTER_SLANG_TRACES_LEAST_LIKELY: EbiCommand = EbiCommand::Command {
    name_short: "ll",
    name_long: Some("least-likely"),
    explanation_short: "Remove the least-likely traces, leaving the most likely.",
    explanation_long: None,
    latex_link: None,
    cli_command: None,
    exact_arithmetic: true,
    input_types: &[
        &[&EbiInputType::Trait(EbiTrait::FiniteStochasticLanguage)],
        &[&EbiInputType::Usize(Some(0), None, None)],
    ],
    input_names: &["SLANG", "KEEP"],
    input_helps: &["the stochastic language", "the number of traces to keep"],
    execute: |mut inputs, _| {
        let mut log = inputs
            .remove(0)
            .to_type::<dyn EbiTraitFiniteStochasticLanguage>()?;
        let number_of_traces_to_keep = inputs.remove(0).to_type::<usize>()?;

        log.remove_least_likely_traces(*number_of_traces_to_keep);

        Ok(EbiOutput::Object(EbiObject::FiniteStochasticLanguage(
            log.to_finite_stochastic_language(),
        )))
    },
    output_type: &EbiOutputType::ObjectType(EbiObjectType::FiniteStochasticLanguage),
};

pub const EBI_FILTER_SLANG_TRACES_LENGTH: EbiCommand = EbiCommand::Command {
    name_short: "len",
    name_long: Some("length"),
    explanation_short: "Remove traces that have a given length.",
    explanation_long: None,
    latex_link: None,
    cli_command: None,
    exact_arithmetic: true,
    input_types: &[
        &[&EbiInputType::Trait(EbiTrait::FiniteStochasticLanguage)],
        &[&EbiInputTypeEnum!(Operator)],
        &[&EbiInputType::Usize(Some(0), None, None)],
    ],
    input_names: &["SLANG", "OPERATOR", "THRESHOLD"],
    input_helps: &["the stochastic language", "the operator", "the threshold"],
    execute: |mut inputs, _| {
        let mut log = inputs
            .remove(0)
            .to_type::<dyn EbiTraitFiniteStochasticLanguage>()?;
        let operator = inputs.remove(0).to_type::<Operator>()?;
        let value = inputs.remove(0).to_type::<usize>()?;

        log.remove_traces_length(*operator, *value);

        Ok(EbiOutput::Object(EbiObject::FiniteStochasticLanguage(
            log.to_finite_stochastic_language(),
        )))
    },
    output_type: &EbiOutputType::ObjectType(EbiObjectType::FiniteStochasticLanguage),
};

pub const EBI_FILTER_SLANG_TRACES_MOST_LIKELY: EbiCommand = EbiCommand::Command {
    name_short: "ml",
    name_long: Some("most-likely"),
    explanation_short: "Remove the most-likely traces, leaving the least likely.",
    explanation_long: None,
    latex_link: None,
    cli_command: None,
    exact_arithmetic: true,
    input_types: &[
        &[&EbiInputType::Trait(EbiTrait::FiniteStochasticLanguage)],
        &[&EbiInputType::Usize(Some(0), None, None)],
    ],
    input_names: &["SLANG", "KEEP"],
    input_helps: &["the stochastic language", "the number of traces to keep"],
    execute: |mut inputs, _| {
        let mut log = inputs
            .remove(0)
            .to_type::<dyn EbiTraitFiniteStochasticLanguage>()?;
        let number_of_traces_to_keep = inputs.remove(0).to_type::<usize>()?;

        log.remove_most_likely_traces(*number_of_traces_to_keep);

        Ok(EbiOutput::Object(EbiObject::FiniteStochasticLanguage(
            log.to_finite_stochastic_language(),
        )))
    },
    output_type: &EbiOutputType::ObjectType(EbiObjectType::FiniteStochasticLanguage),
};

pub const EBI_FILTER_SLANG_TRACES_WITH_EVENT: EbiCommand = EbiCommand::Group {
    name_short: "event",
    name_long: None,
    explanation_short: "Remove traces that have event(s) as specified.",
    explanation_long: None,
    children: &[&EBI_FILTER_SLANG_TRACES_EVENT_ACTIVITY],
};

pub const EBI_FILTER_SLANG_TRACES_EVENT_ACTIVITY: EbiCommand = EbiCommand::Command {
    name_short: "act",
    name_long: Some("activity"),
    explanation_short: "Remove traces that have event(s) as specified.",
    explanation_long: None,
    latex_link: None,
    cli_command: None,
    exact_arithmetic: true,
    input_types: &[
        &[&EbiInputType::Trait(EbiTrait::FiniteStochasticLanguage)],
        &[&EbiInputTypeEnum!(EventSelector)],
        &[&EbiInputType::String(None, None)],
    ],
    input_names: &["SLANG", "SELECTOR", "ACTIVITY"],
    input_helps: &[
        "the stochastic language",
        "which event(s) in the trace should be the activity",
        "the name of the activity that the filter targets",
    ],
    execute: |mut inputs, _| {
        let mut log = inputs
            .remove(0)
            .to_type::<dyn EbiTraitFiniteStochasticLanguage>()?;
        let event_selector = inputs.remove(0).to_type::<EventSelector>()?;
        let activity_label = *inputs.remove(0).to_type::<String>()?;

        let activity = log.activity_key_mut().process_activity(&activity_label);

        log.remove_traces_event_activity(*event_selector, activity);

        Ok(EbiOutput::Object(EbiObject::FiniteStochasticLanguage(
            log.to_finite_stochastic_language(),
        )))
    },
    output_type: &EbiOutputType::ObjectType(EbiObjectType::FiniteStochasticLanguage),
};
