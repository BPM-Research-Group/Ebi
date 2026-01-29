use ebi_objects::{
    EbiObject, EbiObjectType, EventLog, FiniteLanguage, FiniteStochasticLanguage, LabelledPetriNet,
    StochasticDeterministicFiniteAutomaton, StochasticLabelledPetriNet,
};

use crate::{
    ebi_framework::{
        ebi_command::EbiCommand,
        ebi_input::{EbiInput, EbiInputType},
        ebi_output::{EbiOutput, EbiOutputType},
    },
    techniques::stochastic_markovian_abstraction::build_embedded_snfa,
};

pub const EBI_CONVERT: EbiCommand = EbiCommand::Group {
    name_short: "conv",
    name_long: Some("convert"),
    explanation_short: "Convert an object into something else.",
    explanation_long: None,
    children: &[
        &EBI_CONVERT_LANG,
        &EBI_CONVERT_LOG,
        &EBI_CONVERT_LPN,
        &EBI_CONVERT_SLANG,
        &EBI_CONVERT_SDFA,
        &EBI_CONVERT_SLPN,
        &EBI_CONVERT_SNFA,
    ],
};

pub const EBI_CONVERT_LOG: EbiCommand = EbiCommand::Command {
    name_short: "log",
    name_long: None,
    explanation_short: "Convert an object to an event log, considering only its traces.",
    explanation_long: None,
    latex_link: None,
    cli_command: None,
    exact_arithmetic: true,
    input_types: &[&[
        &EbiInputType::Object(EbiObjectType::EventLog), //every object that can be imported as an event log will be supported by the framework
    ]],
    input_names: &["FILE"],
    input_helps: &["Any file supported by Ebi that can be converted."],
    execute: |mut inputs, _| {
        let lpn = inputs.remove(0).to_type::<EventLog>()?;
        Ok(EbiOutput::Object(EbiObject::EventLog(*lpn)))
    },
    output_type: &EbiOutputType::ObjectType(EbiObjectType::EventLog),
};

pub const EBI_CONVERT_LPN: EbiCommand = EbiCommand::Command {
    name_short: "lpn",
    name_long: Some("labelled-petri-net"),
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

pub const EBI_CONVERT_LANG: EbiCommand = EbiCommand::Command {
    name_short: "lang",
    name_long: Some("finite-language"),
    explanation_short: "Convert an object to a finite language.",
    explanation_long: None,
    latex_link: None,
    cli_command: None,
    exact_arithmetic: true,
    input_types: &[&[
        &EbiInputType::Object(EbiObjectType::FiniteLanguage), //every object that can be imported will be supported by the framework
    ]],
    input_names: &["FILE"],
    input_helps: &["Any file supported by Ebi that can be converted."],
    execute: |mut inputs, _| {
        let slang = inputs.remove(0).to_type::<FiniteLanguage>()?;
        Ok(EbiOutput::Object(EbiObject::FiniteLanguage(*slang)))
    },
    output_type: &EbiOutputType::ObjectType(EbiObjectType::FiniteLanguage),
};

pub const EBI_CONVERT_SLANG: EbiCommand = EbiCommand::Command {
    name_short: "slang",
    name_long: Some("finite-stochastic-language"),
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
    name_long: Some("stochastic-deterministic-finite-automaton"),
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

pub const EBI_CONVERT_SLPN: EbiCommand = EbiCommand::Command {
    name_short: "slpn",
    name_long: Some("stochastic-labelled-petri-net"),
    explanation_short: "Convert an object to a stochastic labelled Petri net.",
    explanation_long: None,
    latex_link: None,
    cli_command: None,
    exact_arithmetic: true,
    input_types: &[&[
        &EbiInputType::Object(EbiObjectType::StochasticLabelledPetriNet), //every object that can be imported will be supported by the framework
    ]],
    input_names: &["FILE"],
    input_helps: &["Any file supported by Ebi that can be converted."],
    execute: |mut inputs, _| {
        let lpn = inputs.remove(0).to_type::<StochasticLabelledPetriNet>()?;
        Ok(EbiOutput::Object(EbiObject::StochasticLabelledPetriNet(
            *lpn,
        )))
    },
    output_type: &EbiOutputType::ObjectType(EbiObjectType::StochasticLabelledPetriNet),
};

pub const EBI_CONVERT_SNFA: EbiCommand = EbiCommand::Command {
    name_short: "snfa",
    name_long: Some("stochastic-nondeterministic-finite-automaton"),
    explanation_short: "Convert an object to a stochastic nondeterministic finite automaton.",
    explanation_long: None,
    latex_link: None,
    cli_command: None,
    exact_arithmetic: true,
    input_types: &[&[
        &EbiInputType::Object(EbiObjectType::StochasticNondeterministicFiniteAutomaton), //every object that can be imported as an SDFA will be supported by the framework
        &EbiInputType::Object(EbiObjectType::StochasticLabelledPetriNet),
    ]],
    input_names: &["FILE"],
    input_helps: &[
        "Any file supported by Ebi that can be converted. An SLPN must be livelock-free and bounded.",
    ],
    execute: |mut inputs, _| {
        let snfa = match inputs.remove(0) {
            EbiInput::Object(EbiObject::StochasticNondeterministicFiniteAutomaton(snfa), _) => snfa,
            EbiInput::Object(EbiObject::StochasticLabelledPetriNet(slpn), _) => {
                build_embedded_snfa(&slpn)?
            }
            _ => unreachable!(),
        };
        Ok(EbiOutput::Object(
            EbiObject::StochasticNondeterministicFiniteAutomaton(snfa),
        ))
    },
    output_type: &EbiOutputType::ObjectType(
        EbiObjectType::StochasticNondeterministicFiniteAutomaton,
    ),
};
