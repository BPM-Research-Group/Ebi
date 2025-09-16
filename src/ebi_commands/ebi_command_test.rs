use anyhow::Context;
use ebi_arithmetic::Fraction;
use std::io::Write;

use crate::{
    ebi_commands::ebi_command_association::DEFAULT_NUMBER_OF_SAMPLES,
    ebi_framework::{
        ebi_command::EbiCommand,
        ebi_input::EbiInputType,
        ebi_output::{EbiOutput, EbiOutputType},
        ebi_trait::EbiTrait,
    },
    ebi_info,
    ebi_traits::{
        ebi_trait_event_log::EbiTraitEventLog,
        ebi_trait_finite_stochastic_language::EbiTraitFiniteStochasticLanguage,
    },
    math::constant_fraction::ConstFraction,
    techniques::{
        bootstrap_test::{BootstrapTest, StatisticalTestsLogCategoricalAttribute},
        permutation_test::PermutationTest,
    },
};

pub const DEFAULT_P_VALUE: ConstFraction = ConstFraction::of(1, 20);

pub const EBI_TEST: EbiCommand = EbiCommand::Group {
    name_short: "tst",
    name_long: Some("test"),
    explanation_short: "Test a hypothesis.",
    explanation_long: None,
    children: &[&EBI_BOOTSTRAP_TEST, &EBI_TEST_LOG_ATTRIBUTE],
};

pub const EBI_TEST_LOG_ATTRIBUTE: EbiCommand = EbiCommand::Command {
    name_short: "lcat",
    name_long: Some("log-categorical-attribute"),
    library_name: "ebi_commands::ebi_command_test::EBI_TEST_LOG_ATTRIBUTE",
    explanation_short: "Test the hypothesis that the sub-logs defined by the categorical attribute are derived from identical processes.",
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
        &[&EbiInputType::Fraction(
            Some(ConstFraction::zero()),
            Some(ConstFraction::one()),
            Some(DEFAULT_P_VALUE),
        )],
    ],
    input_names: &["FILE", "ATTRIBUTE", "SAMPLES", "P-VALUE"],
    input_helps: &[
        "The event log for which the test is to be performed.",
        concat!(
            concat!(
                "The trace attribute for which the test is to be performed. The trace attributes of a log can be found using `Ebi ",
                ebi_info!()
            ),
            "`."
        ),
        "The number of samples taken.",
        "The threshold p-value.",
    ],
    execute: |mut inputs, _| {
        let event_log = inputs.remove(0).to_type::<dyn EbiTraitEventLog>()?;
        let attribute = inputs.remove(0).to_type::<String>()?;
        let number_of_samples = inputs.remove(0).to_type::<usize>()?;
        let p_value = inputs.remove(0).to_type::<Fraction>()?;

        let (value, sustained) = event_log
            .log_categorical_attribute(*number_of_samples, &attribute, &p_value)
            .with_context(|| format!("attribute {}", attribute))?;

        let mut f = vec![];
        writeln!(f, "p-value \t {}", value)?;
        writeln!(
            f,
            "Null-hypothesis: the sub-logs defined by trace attribute `{}` all follow identical processes.",
            attribute
        )?;

        if sustained {
            writeln!(
                f,
                "The data does not provide enough evidence to make a claim on this hypothesis.\nDo not reject the null-hypothesis."
            )?;
        } else {
            writeln!(
                f,
                "The data provides enough evidence to conclude that at least one value of the categorical attribute `{}` associates with a difference in process.\nReject the null-hypothesis.",
                attribute
            )?;
        };

        Ok(EbiOutput::String(String::from_utf8(f).unwrap()))
    },
    output_type: &EbiOutputType::String,
};

pub const EBI_BOOTSTRAP_TEST: EbiCommand = EbiCommand::Command {
    name_short: "btst",
    name_long: Some("bootstrap-test"),
    library_name: "ebi_commands::ebi_command_test::EBI_BOOTSTRAP_TEST",
    explanation_short: "Test the hypothesis that the logs are derived from identical processes.",
    explanation_long: None,
    latex_link: Some("\\cite{DBLP:journals/tkde/LeemansMPH23}"),
    cli_command: None,
    exact_arithmetic: true,
    input_types: &[
        &[&EbiInputType::Trait(EbiTrait::FiniteStochasticLanguage)],
        &[&EbiInputType::Trait(EbiTrait::FiniteStochasticLanguage)],
        &[&EbiInputType::Usize(
            Some(1),
            None,
            Some(DEFAULT_NUMBER_OF_SAMPLES),
        )],
        &[&EbiInputType::Fraction(
            Some(ConstFraction::zero()),
            Some(ConstFraction::one()),
            Some(DEFAULT_P_VALUE),
        )],
    ],
    input_names: &["LANG_1", "LANG_2", "SAMPLES", "P-VALUE"],
    input_helps: &[
        "The first event log for which the test is to be performed.",
        "The second event log for which the test is to be performed.",
        "The number of samples taken.",
        "The threshold p-value",
    ],
    execute: |mut inputs, _| {
        let mut log1 = inputs
            .remove(0)
            .to_type::<dyn EbiTraitFiniteStochasticLanguage>()?;
        let mut log2: Box<dyn EbiTraitFiniteStochasticLanguage + 'static> =
            inputs
                .remove(0)
                .to_type::<dyn EbiTraitFiniteStochasticLanguage>()?;
        let number_of_samples = inputs.remove(0).to_type::<usize>()?;
        let p_value = inputs.remove(0).to_type::<Fraction>()?;

        log1.translate_using_activity_key(log2.activity_key_mut());

        let (value, sustained) = log1
            .bootstrap_test(log2.as_mut(), *number_of_samples, &p_value)
            .with_context(|| format!("performing test"))?;

        let mut f = vec![];
        writeln!(f, "p-value \t {}", value)?;
        writeln!(f, "Null-hypothesis: the logs follow identical processes.")?;

        if sustained {
            writeln!(
                f,
                "The data does not provide enough evidence to make a claim on this hypothesis.\nDo not reject the null-hypothesis."
            )?;
        } else {
            writeln!(
                f,
                "The data provides enough evidence to conclude that the logs are derived from different processes.\nReject the null-hypothesis.",
            )?;
        };

        Ok(EbiOutput::String(String::from_utf8(f).unwrap()))
    },
    output_type: &EbiOutputType::String,
};

pub const EBI_PERMUTATION_TEST: EbiCommand = EbiCommand::Command {
    name_short: "perm",
    name_long: Some("permutation-test"),
    library_name: "ebi_commands::ebi_command_test::EBI_PERMUTATION_TEST",
    explanation_short: "Test the hypothesis that the logs are derived from identical processes.",
    explanation_long: None,
    latex_link: None,
    cli_command: None,
    exact_arithmetic: true,
    input_types: &[
        &[&EbiInputType::Trait(EbiTrait::FiniteStochasticLanguage)],
        &[&EbiInputType::Trait(EbiTrait::FiniteStochasticLanguage)],
        &[&EbiInputType::Usize(
            Some(1),
            None,
            Some(DEFAULT_NUMBER_OF_SAMPLES),
        )],
        &[&EbiInputType::Fraction(
            Some(ConstFraction::zero()),
            Some(ConstFraction::one()),
            Some(DEFAULT_P_VALUE),
        )],
    ],
    input_names: &["LANG_1", "LANG_2", "SAMPLES", "P-VALUE"],
    input_helps: &[
        "The first event log for which the test is to be performed.",
        "The first event log for which the test is to be performed.",
        "The number of samples taken.",
        "The threshold p-value",
    ],
    execute: |mut inputs, _| {
        let mut log1 = inputs
            .remove(0)
            .to_type::<dyn EbiTraitFiniteStochasticLanguage>()?;
        let mut log2: Box<dyn EbiTraitFiniteStochasticLanguage + 'static> =
            inputs
                .remove(0)
                .to_type::<dyn EbiTraitFiniteStochasticLanguage>()?;
        let number_of_samples = inputs.remove(0).to_type::<usize>()?;
        let p_value = inputs.remove(0).to_type::<Fraction>()?;

        let (value, sustained) = log1
            .permutation_test(log2.as_mut(), *number_of_samples, &p_value)
            .with_context(|| format!("performing test"))?;

        let mut f = vec![];
        writeln!(f, "p-value \t {}", value)?;
        writeln!(f, "Null-hypothesis: the logs follow identical processes.")?;

        if sustained {
            writeln!(
                f,
                "The data does not provide enough evidence to make a claim on this hypothesis.\nDo not reject the null-hypothesis."
            )?;
        } else {
            writeln!(
                f,
                "The data provides enough evidence to conclude that the logs are derived from different processes.\nReject the null-hypothesis.",
            )?;
        };

        Ok(EbiOutput::String(String::from_utf8(f).unwrap()))
    },
    output_type: &EbiOutputType::String,
};
