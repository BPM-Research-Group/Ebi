use crate::{
    ebi_framework::{
        ebi_command::EbiCommand,
        ebi_input::EbiInputType,
        ebi_output::{EbiOutput, EbiOutputType},
        ebi_trait::EbiTrait,
    },
    ebi_traits::ebi_trait_event_log_trace_attributes::EbiTraitEventLogTraceAttributes,
    techniques::cohort_analysis::cohort_analysis,
    tests::test_ebi_command,
};

pub const EBI_COHORT_ANALYSIS: EbiCommand = EbiCommand::Command {
    name_short: "coh",
    name_long: Some("cohort-analysis"),
    explanation_short: "Discover which trace attributes drive the largest behavioural differences.",
    explanation_long: Some(
        "Scans every categorical trace attribute in the event log. \
        For each attribute-value pair, the log is partitioned into a target cohort \
        (traces that carry that specific value) and a rest cohort (all other traces). \
        Both sub-logs are normalised into proper stochastic languages and their \
        behavioural distance is measured with the Earth Mover's Stochastic Conformance \
        (EMSC) using normalised Levenshtein as the ground distance. \
        The output is a ranked leaderboard: the attribute-value pairs at the top cause \
        the greatest shift in process behaviour and are therefore the best candidates \
        for further investigation.",
    ),
    latex_link: None,
    cli_command: None,
    exact_arithmetic: true,
    input_types: &[&[&EbiInputType::Trait(EbiTrait::EventLogTraceAttributes)]],
    input_names: &["FILE"],
    input_helps: &["An event log with trace attributes (XES or OCEL format)."],
    execute: |mut inputs, _| {
        let log = inputs
            .remove(0)
            .to_type::<dyn EbiTraitEventLogTraceAttributes>()?;
        let result = cohort_analysis(log.as_ref())?;
        Ok(EbiOutput::String(result))
    },
    output_type: &EbiOutputType::String,
};
test_ebi_command!(EBI_COHORT_ANALYSIS);
