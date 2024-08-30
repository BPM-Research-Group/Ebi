use std::{io::{self, IsTerminal}, path::PathBuf};
use anyhow::{anyhow, Context, Result};
use clap::{value_parser, Arg, ArgAction, ArgMatches, Command};

use crate::{ebi_input_output::{EbiInput, EbiInputType}, ebi_objects::{directly_follows_model::DirectlyFollowsModel, ebi_object::{EbiObject, EbiObjectType, EbiTraitObject}, finite_stochastic_language::FiniteStochasticLanguage, labelled_petri_net::LabelledPetriNet, stochastic_deterministic_finite_automaton::StochasticDeterministicFiniteAutomaton}, ebi_traits::ebi_trait::EbiTrait, export::{self, EbiOutput, EbiOutputType}, import::{self, MultipleReader}};
use super::{ebi_command::EbiCommand, ebi_command_analyse::EBI_ANALYSE, ebi_command_discover::EBI_DISCOVER};

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
            &EbiInputType::ObjectType(EbiObjectType::StochasticLabelledPetriNet),
            &EbiInputType::ObjectType(EbiObjectType::StochasticDeterministicFiniteAutomaton),
            &EbiInputType::ObjectType(EbiObjectType::LabelledPetriNet),
            &EbiInputType::ObjectType(EbiObjectType::DirectlyFollowsModel),
        ] ], 
    input_names: &[ "FILE" ], 
    input_helps: &[ "Any file supported by Ebi that can be converted." ], 
    execute: |mut inputs, _| {
        let lpn = match inputs.remove(0) {
            EbiInput::Object(EbiObject::DirectlyFollowsModel(dfm), _) => dfm.to_labelled_petri_net(),
            EbiInput::Object(EbiObject::StochasticLabelledPetriNet(slpn), _) => slpn.to_labelled_petri_net(),
            EbiInput::Object(EbiObject::LabelledPetriNet(lpn), _) => lpn,
            EbiInput::Object(EbiObject::StochasticDeterministicFiniteAutomaton(sdfa), _) => sdfa.to_stochastic_labelled_petri_net().to_labelled_petri_net(),
            _ => unreachable!()
        };
        Ok(EbiOutput::Object(EbiObject::LabelledPetriNet(lpn)))
    }, 
    output: &EbiOutputType::ObjectType(EbiObjectType::LabelledPetriNet) 
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
            &EbiInputType::ObjectType(EbiObjectType::EventLog),
            &EbiInputType::ObjectType(EbiObjectType::FiniteStochasticLanguage),
        ] ], 
    input_names: &[ "FILE" ], 
    input_helps: &[ "Any file supported by Ebi that can be converted." ], 
    execute: |mut inputs, cli_matches| {
        let slang = match inputs.remove(0) {
            EbiInput::Object(EbiObject::FiniteStochasticLanguage(slang), _) => slang,
            EbiInput::Object(EbiObject::EventLog(log), _) => log.to_finite_stochastic_language(),
            _ => unreachable!()
        };
        Ok(EbiOutput::Object(EbiObject::FiniteStochasticLanguage(slang)))
    }, 
    output: &EbiOutputType::ObjectType(EbiObjectType::FiniteStochasticLanguage) 
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
            &EbiInputType::ObjectType(EbiObjectType::EventLog),
            &EbiInputType::ObjectType(EbiObjectType::FiniteStochasticLanguage),
            &EbiInputType::ObjectType(EbiObjectType::StochasticDeterministicFiniteAutomaton),
        ] ], 
    input_names: &[ "FILE" ], 
    input_helps: &[ "Any file supported by Ebi that can be converted." ], 
    execute: |mut inputs, _| {
        let sdfa = match inputs.remove(0) {
            EbiInput::Object(EbiObject::FiniteStochasticLanguage(mut slang), _) => slang.to_stochastic_deterministic_finite_automaton(),
            EbiInput::Object(EbiObject::StochasticDeterministicFiniteAutomaton(sdfa), _) => sdfa,
            EbiInput::Object(EbiObject::EventLog(mut log), _) => log.to_stochastic_deterministic_finite_automaton(),
            _ => unreachable!()
        };
        Ok(EbiOutput::Object(EbiObject::StochasticDeterministicFiniteAutomaton(sdfa)))
    }, 
    output: &EbiOutputType::ObjectType(EbiObjectType::StochasticDeterministicFiniteAutomaton)
};