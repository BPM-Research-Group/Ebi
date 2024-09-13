use std::{path::PathBuf, io::{self, IsTerminal}};

use crate::{dottable::Dottable, ebi_input_output::{EbiInput, EbiInputType}, ebi_objects::ebi_object::{EbiObject, EbiObjectType, EbiTraitObject}, ebi_traits::ebi_trait::EbiTrait, export::{self, EbiOutput, EbiOutputType}, import};
use anyhow::{Result, Context, anyhow};
use clap::{ArgMatches, Command, value_parser, ArgAction, Arg};
use layout::backends::svg::SVGWriter;

use super::ebi_command::EbiCommand;

pub const EBI_VISUALISE: EbiCommand = EbiCommand::Group { 
    name_short: "vis", 
    name_long: Some("visualise"),
    explanation_short: "Visualse an object.", 
    explanation_long: None,
    children: &[
        &EBI_VISUALISE_SVG,
        &EBI_VISUALISE_TEXT
    ]
};

pub const EBI_VISUALISE_TEXT: EbiCommand = EbiCommand::Command { 
    name_short: "txt", 
    name_long: Some("text"),
    explanation_short: "Visualise an object as text.",
    explanation_long: None, 
    latex_link: None, 
    cli_command: None, 
    exact_arithmetic: false, 
    input_types: &[ &[&EbiInputType::AnyObject] ], 
    input_names: &[ "FILE" ], 
    input_helps: &[ "Any file that can be visualised textually." ], 
    execute: |mut inputs, _| {
        let result = match inputs.remove(0) {
                EbiInput::Object(EbiObject::StochasticLabelledPetriNet(slpn), _) => slpn.to_string(),
                EbiInput::Object(EbiObject::LabelledPetriNet(lpn), _) => lpn.to_string(),
                EbiInput::Object(EbiObject::FiniteStochasticLanguage(lang), _) => lang.to_string(),
                EbiInput::Object(EbiObject::StochasticDeterministicFiniteAutomaton(sdfa), _) => sdfa.to_string(),
                EbiInput::Object(EbiObject::EventLog(log), _) => log.to_string(),
                EbiInput::Object(EbiObject::FiniteLanguage(language), _) => language.to_string(),
                EbiInput::Object(EbiObject::DirectlyFollowsModel(d), _) => d.to_string(),
                EbiInput::Object(EbiObject::Alignments(a), _) => a.to_string(),
                EbiInput::FileHandler(_) => unreachable!(),
                EbiInput::Trait(_, _) => unreachable!(),
                EbiInput::String(_) => unreachable!(),
                EbiInput::Usize(_) => unreachable!(),
                EbiInput::Fraction(_) => unreachable!(),
        };
        Ok(EbiOutput::String(result))
    }, 
    output: &EbiOutputType::String
};

pub const EBI_VISUALISE_SVG: EbiCommand = EbiCommand::Command { 
    name_short: "svg", 
    name_long: None, 
    explanation_short: "Visualise an object as scalable vector graphics.",
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
    input_helps: &[ "Any file that can be visualised as a graph." ], 
    execute: |mut inputs, _| {
        let mut result = match inputs.remove(0) {
            EbiInput::Object(EbiObject::LabelledPetriNet(lpn), _) => lpn.to_dot(),
            EbiInput::Object(EbiObject::StochasticLabelledPetriNet(slpn), _) => slpn.to_dot(),
            EbiInput::Object(EbiObject::StochasticDeterministicFiniteAutomaton(sdfa), _) => sdfa.to_dot(),
            EbiInput::Object(EbiObject::DirectlyFollowsModel(dfm), _) => dfm.to_dot(),
            EbiInput::Object(EbiObject::EventLog(_), _) => unreachable!(),
            EbiInput::Object(EbiObject::FiniteLanguage(_), _) => unreachable!(),
            EbiInput::Object(EbiObject::FiniteStochasticLanguage(_), _) => unreachable!(),
            EbiInput::Object(EbiObject::Alignments(_), _) => unreachable!(),
            EbiInput::FileHandler(_) => unreachable!(),
            EbiInput::Trait(_, _) => unreachable!(),
            EbiInput::String(_) => unreachable!(),
            EbiInput::Usize(_) => unreachable!(),
            EbiInput::Fraction(_) => unreachable!(),
        };

        let mut svg = SVGWriter::new();
        result.do_it(false, false, false, &mut svg);

        return Ok(EbiOutput::String(svg.finalize()));
    
    }, 
    output: &EbiOutputType::String
};