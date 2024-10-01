

use crate::ebi_framework::{ebi_command::EbiCommand, ebi_input::{EbiInput, EbiInputType}, ebi_object::{EbiObject, EbiObjectType}, ebi_output::{EbiOutput, EbiOutputType}};

pub const EBI_CONVERT: EbiCommand = EbiCommand::Group { 
    name_short: "conv",
    name_long: Some("convert"), 
    explanation_short: "Convert an object into something else.", 
    explanation_long: None, 
    children: &[ 
        &EBI_CONVERT_LPN,
        &EBI_CONVERT_SLANG,
        &EBI_CONVERT_SDFA
    ]
};

pub const EBI_CONVERT_LPN: EbiCommand = EbiCommand::Command { 
    name_short: "lpn", 
    name_long: Some("labelled-Petri-net"),
    explanation_short: "Convert an object to a labelled Petri net.", 
    explanation_long: None, 
    latex_link: None, 
    cli_command: None, 
    exact_arithmetic: true, 
    input_types: &[ 
        &[
            &EbiInputType::Object(EbiObjectType::StochasticLabelledPetriNet),
            &EbiInputType::Object(EbiObjectType::StochasticDeterministicFiniteAutomaton),
            &EbiInputType::Object(EbiObjectType::LabelledPetriNet),
            &EbiInputType::Object(EbiObjectType::DirectlyFollowsModel),
        ] ], 
    input_names: &[ "FILE" ], 
    input_helps: &[ "Any file supported by Ebi that can be converted." ], 
    execute: |mut inputs, _| {
        let lpn = match inputs.remove(0) {
            EbiInput::Object(EbiObject::DirectlyFollowsModel(dfm), _) => dfm.get_labelled_petri_net(),
            EbiInput::Object(EbiObject::StochasticLabelledPetriNet(slpn), _) => slpn.into(),
            EbiInput::Object(EbiObject::LabelledPetriNet(lpn), _) => lpn,
            EbiInput::Object(EbiObject::StochasticDeterministicFiniteAutomaton(sdfa), _) => sdfa.get_stochastic_labelled_petri_net().into(),
            _ => unreachable!()
        };
        Ok(EbiOutput::Object(EbiObject::LabelledPetriNet(lpn)))
    }, 
    output_type: &EbiOutputType::ObjectType(EbiObjectType::LabelledPetriNet) 
};

pub const EBI_CONVERT_SLANG: EbiCommand = EbiCommand::Command { 
    name_short: "slang", 
    name_long: Some("finite-stochastic-language"), 
    explanation_short: "Convert an object to a finite stochastic language.", 
    explanation_long: None, 
    latex_link: None, 
    cli_command: None, 
    exact_arithmetic: true, 
    input_types: &[ 
        &[
            &EbiInputType::Object(EbiObjectType::EventLog),
            &EbiInputType::Object(EbiObjectType::FiniteStochasticLanguage),
        ] ], 
    input_names: &[ "FILE" ], 
    input_helps: &[ "Any file supported by Ebi that can be converted." ], 
    execute: |mut inputs, _| {
        let slang = match inputs.remove(0) {
            EbiInput::Object(EbiObject::FiniteStochasticLanguage(slang), _) => slang,
            EbiInput::Object(EbiObject::EventLog(log), _) => log.get_finite_stochastic_language(),
            _ => unreachable!()
        };
        Ok(EbiOutput::Object(EbiObject::FiniteStochasticLanguage(slang)))
    }, 
    output_type: &EbiOutputType::ObjectType(EbiObjectType::FiniteStochasticLanguage) 
};

pub const EBI_CONVERT_SDFA: EbiCommand = EbiCommand::Command { 
    name_short: "sdfa", 
    name_long: Some("stochastic-finite-deterministic-automaton"), 
    explanation_short: "Convert an object to a finite stochastic language.", 
    explanation_long: None, 
    latex_link: None, 
    cli_command: None, 
    exact_arithmetic: true, 
    input_types: &[ 
        &[
            &EbiInputType::Object(EbiObjectType::EventLog),
            &EbiInputType::Object(EbiObjectType::FiniteStochasticLanguage),
            &EbiInputType::Object(EbiObjectType::StochasticDeterministicFiniteAutomaton),
        ] ], 
    input_names: &[ "FILE" ], 
    input_helps: &[ "Any file supported by Ebi that can be converted." ], 
    execute: |mut inputs, _| {
        let sdfa = match inputs.remove(0) {
            EbiInput::Object(EbiObject::FiniteStochasticLanguage(slang), _) => slang.get_stochastic_deterministic_finite_automaton(),
            EbiInput::Object(EbiObject::StochasticDeterministicFiniteAutomaton(sdfa), _) => sdfa,
            EbiInput::Object(EbiObject::EventLog(log), _) => log.to_stochastic_deterministic_finite_automaton(),
            _ => unreachable!()
        };
        Ok(EbiOutput::Object(EbiObject::StochasticDeterministicFiniteAutomaton(sdfa)))
    }, 
    output_type: &EbiOutputType::ObjectType(EbiObjectType::StochasticDeterministicFiniteAutomaton)
};