use ebi_objects::{EbiObject, EbiObjectType, FiniteStochasticLanguage, LabelledPetriNet, StochasticDeterministicFiniteAutomaton};

use crate::ebi_framework::{
    ebi_command::EbiCommand,
    ebi_input::EbiInputType,
    ebi_output::{EbiOutput, EbiOutputType},
};

pub const EBI_CONVERT: EbiCommand = EbiCommand::Group {
    name_short: "conv",
    name_long: Some("convert"),
    explanation_short: "Convert an object into something else.",
    explanation_long: None,
    children: &[&EBI_CONVERT_LPN, &EBI_CONVERT_SLANG, &EBI_CONVERT_SDFA],
};

pub const EBI_CONVERT_LPN: EbiCommand = EbiCommand::Command {
    name_short: "lpn",
    name_long: Some("labelled-petri-net"),
    library_name: "ebi_commands::ebi_command_convert::EBI_CONVERT_LPN",
    explanation_short: "Convert an object to a labelled Petri net.",
    explanation_long: None,
    latex_link: None,
    cli_command: None,
    exact_arithmetic: true,
    input_types: &[&[
        &EbiInputType::Object(EbiObjectType::LabelledPetriNet), //every object that can be imported as an LPN will be supported by the framework
    ]],
    input_names: &["FILE"],
    input_helps: &["Any file supported by Ebi that can be converted."],
    execute: |mut inputs, _| {
        let lpn = inputs.remove(0).to_type::<LabelledPetriNet>()?;
        Ok(EbiOutput::Object(EbiObject::LabelledPetriNet(*lpn)))
    },
    output_type: &EbiOutputType::ObjectType(EbiObjectType::LabelledPetriNet),
};

pub const EBI_CONVERT_SLANG: EbiCommand = EbiCommand::Command {
    name_short: "slang",
    name_long: Some("finite-stochastic-language"),
    library_name: "ebi_commands::ebi_command_convert::EBI_CONVERT_SLANG",
    explanation_short: "Convert an object to a finite stochastic language.",
    explanation_long: None,
    latex_link: None,
    cli_command: None,
    exact_arithmetic: true,
    input_types: &[&[
        &EbiInputType::Object(EbiObjectType::FiniteStochasticLanguage), //every object that can be imported as an FSL will be supported by the framework
    ]],
    input_names: &["FILE"],
    input_helps: &["Any file supported by Ebi that can be converted."],
    execute: |mut inputs, _| {
        let slang = inputs.remove(0).to_type::<FiniteStochasticLanguage>()?;
        Ok(EbiOutput::Object(EbiObject::FiniteStochasticLanguage(
            *slang,
        )))
    },
    output_type: &EbiOutputType::ObjectType(EbiObjectType::FiniteStochasticLanguage),
};

pub const EBI_CONVERT_SDFA: EbiCommand = EbiCommand::Command {
    name_short: "sdfa",
    name_long: Some("stochastic-finite-deterministic-automaton"),
    library_name: "ebi_commands::ebi_command_convert::EBI_CONVERT_SDFA",
    explanation_short: "Convert an object to a stochastic deterministic finite automaton.",
    explanation_long: None,
    latex_link: None,
    cli_command: None,
    exact_arithmetic: true,
    input_types: &[&[
        &EbiInputType::Object(EbiObjectType::StochasticDeterministicFiniteAutomaton), //every object that can be imported as an SDFA will be supported by the framework
    ]],
    input_names: &["FILE"],
    input_helps: &["Any file supported by Ebi that can be converted."],
    execute: |mut inputs, _| {
        let sdfa = inputs
            .remove(0)
            .to_type::<StochasticDeterministicFiniteAutomaton>()?;
        Ok(EbiOutput::Object(
            EbiObject::StochasticDeterministicFiniteAutomaton(*sdfa),
        ))
    },
    output_type: &EbiOutputType::ObjectType(EbiObjectType::StochasticDeterministicFiniteAutomaton),
};
