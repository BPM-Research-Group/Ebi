use anyhow::{Context, Result, anyhow};
use ebi_objects::{EbiObject, EbiObjectType, EventLog, FiniteStochasticLanguage};

use crate::{
    ebi_framework::{
        ebi_command::EbiCommand,
        ebi_input::{EbiInput, EbiInputType},
        ebi_output::{EbiOutput, EbiOutputType},
        ebi_trait::EbiTrait,
        ebi_trait_object::EbiTraitObject,
    },
    techniques::{sample::Sampler, sample_folds::FoldsSampler},
};

pub const SAMPLED_OBJECT_INPUTS: &[&EbiInputType] = &[
    &EbiInputType::Trait(EbiTrait::FiniteStochasticLanguage),
    &EbiInputType::Trait(EbiTrait::StochasticSemantics),
];

pub fn get_sampled_object(
    object: EbiInput,
    number_of_traces: usize,
) -> Result<FiniteStochasticLanguage> {
    match object {
        EbiInput::Trait(EbiTraitObject::FiniteStochasticLanguage(slang), _) => slang
            .sample(number_of_traces)
            .context("Sample finite stochastic language."),
        EbiInput::Trait(EbiTraitObject::StochasticSemantics(semantics), _) => semantics
            .sample(number_of_traces)
            .context("Sample semantics."),
        _ => unreachable!(),
    }
}

pub const EBI_SAMPLE: EbiCommand = EbiCommand::Group {
    name_short: "sam",
    name_long: Some("sample"),
    explanation_short: "Draw traces randomly from a model or an event log.",
    explanation_long: None,
    children: &[&EBI_SAMPLE_FOLDS, &EBI_SAMPLE_TRACES],
};

pub const EBI_SAMPLE_TRACES: EbiCommand = EbiCommand::Command {
    name_short: "tra",
    name_long: Some("traces"),
    library_name: "ebi_commands::ebi_command_sample::EBI_SAMPLE_TRACES",
    explanation_short: "Draw traces randomly from a model.",
    explanation_long: Some(
        "Sample traces randomly. Please note that this may run forever if the model contains a livelock.",
    ),
    latex_link: None,
    cli_command: None,
    exact_arithmetic: true,
    input_types: &[
        SAMPLED_OBJECT_INPUTS,
        &[&EbiInputType::Usize(Some(1), None, None)],
    ],
    input_names: &["FILE", "NUMBER_OF_TRACES"],
    input_helps: &[
        "The stochastic semantics (model).",
        "The number of traces to be sampled.",
    ],
    execute: |mut inputs, _| {
        let object = inputs.remove(0);
        let number_of_traces = inputs.remove(0).to_type::<usize>()?;
        let result: FiniteStochasticLanguage = get_sampled_object(object, *number_of_traces)?;
        return Ok(EbiOutput::Object(EbiObject::FiniteStochasticLanguage(
            result,
        )));
    },
    output_type: &EbiOutputType::ObjectType(EbiObjectType::FiniteStochasticLanguage),
};

pub const EBI_SAMPLE_FOLDS: EbiCommand = EbiCommand::Command {
    name_short: "folds",
    name_long: None,
    library_name: "ebi_commands::ebi_command_sample::EBI_SAMPLE_FOLDS",
    explanation_short: "Randomly split a log into a given number of sub-logs, and return a specific one of these sub-logs.",
    explanation_long: Some(
        "Randomly but reproducibly split a log into a given number of sub-logs. Each trace has a likelihood of 1/folds to end up in any of the folds. Giving the same random seed yields the same split, as long as the same build number of Ebi is used. 
        \n\n For instance, one can perform k-fold cross validation: one would repeatedly apply the folds command with the same seed and the same number of folds, but vary the returned sub-logs.",
    ),
    latex_link: None,
    cli_command: None,
    exact_arithmetic: true,
    input_types: &[
        &[&EbiInputType::Object(EbiObjectType::EventLog)],
        &[&EbiInputType::Usize(Some(1), None, None)],
        &[&EbiInputType::Usize(None, None, None)],
        &[&EbiInputType::Usize(None, None, None)],
    ],
    input_names: &["LOG", "NUMBER_OF_FOLDS", "SEED", "RETURN_FOLD"],
    input_helps: &[
        "The event log.",
        "The number of folds.",
        "The random seed.",
        "The fold to be returned.",
    ],
    execute: |mut inputs, _| {
        let mut log = inputs.remove(0).to_type::<EventLog>()?;
        let number_of_folds = *inputs.remove(0).to_type::<usize>()?;
        let seed = *inputs.remove(0).to_type::<usize>()?;
        let return_fold = *inputs.remove(0).to_type::<usize>()?;

        if return_fold >= number_of_folds {
            return Err(anyhow!(
                "fold {} was requested, but there are only {} folds.",
                return_fold,
                number_of_folds
            ));
        }

        let number_of_folds: u32 = number_of_folds.try_into()?;
        let seed: u64 = seed.try_into()?;
        let return_fold: u32 = return_fold.try_into()?;

        log.sample_folds(number_of_folds, seed, return_fold);
        Ok(EbiOutput::Object(EbiObject::EventLog(*log)))
    },
    output_type: &EbiOutputType::ObjectType(EbiObjectType::EventLog),
};
