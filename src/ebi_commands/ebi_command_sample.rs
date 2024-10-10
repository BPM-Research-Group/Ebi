use anyhow::{Context, Result};

use crate::{ebi_framework::{ebi_command::EbiCommand, ebi_input::{EbiInput, EbiInputType}, ebi_object::{EbiObject, EbiObjectType, EbiTraitObject}, ebi_output::{EbiOutput, EbiOutputType}, ebi_trait::EbiTrait}, ebi_objects::finite_stochastic_language::FiniteStochasticLanguage, techniques::sample::Sampler};


pub const SAMPLED_OBJECT_INPUTS: &[&EbiInputType] = &[ &EbiInputType::Trait(EbiTrait::FiniteStochasticLanguage), &EbiInputType::Trait(EbiTrait::StochasticSemantics) ];

pub fn get_sampled_object(object: EbiInput, number_of_traces: usize) -> Result<FiniteStochasticLanguage> {
    match object {
        EbiInput::Trait(EbiTraitObject::FiniteStochasticLanguage(slang), _) => {
            slang.sample(number_of_traces).context("Sample finite stochastic language.")
        },
        EbiInput::Trait(EbiTraitObject::StochasticSemantics(semantics), _) => {
            semantics.sample(number_of_traces).context("Sample semantics.")
        },
        _ => unreachable!()
    }
}

pub const EBI_SAMPLE: EbiCommand = EbiCommand::Command {
    name_short: "sam",
    name_long: Some("sample"),
    explanation_short: "Sample traces randomly.",
    explanation_long: Some("Sample traces randomly. Please note that this may run forever if the model contains a livelock."),
    latex_link: None,
    cli_command: None,
    exact_arithmetic: true,
    input_types: &[ 
        SAMPLED_OBJECT_INPUTS,
        &[ &EbiInputType::Usize] 
    ],
    input_names: &[ "FILE", "NUMBER_OF_TRACES"],
    input_helps: &[ "The stochastic semantics (model).", "The number of traces to be sampled."],
    execute: |mut inputs, _| {
        let object = inputs.remove(0);
        let number_of_traces = inputs.remove(0).to_type::<usize>()?;
        let result: FiniteStochasticLanguage = get_sampled_object(object, *number_of_traces)?;
        return Ok(EbiOutput::Object(EbiObject::FiniteStochasticLanguage(result)));
    },
    output_type: &EbiOutputType::ObjectType(EbiObjectType::FiniteStochasticLanguage),
};