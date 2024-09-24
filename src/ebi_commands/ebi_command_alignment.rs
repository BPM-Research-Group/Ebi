// use std::{path::PathBuf, io::{self, IsTerminal}};
// use clap::{Command, ArgMatches, value_parser, Arg, ArgAction};
// use anyhow::{Result, Context, anyhow};

// use crate::{align, ebi_alignments::{self, stochastic_astar}, ebi_input_output::{EbiInput, EbiInputType}, ebi_objects::{ebi_object::{EbiObject, EbiObjectType, EbiTraitObject}, labelled_petri_net::LPNMarking}, ebi_traits::{ebi_trait::EbiTrait, ebi_trait_finite_language::EbiTraitFiniteLanguage, ebi_trait_finite_stochastic_language::EbiTraitFiniteStochasticLanguage, ebi_trait_queriable_stochastic_language::EbiTraitQueriableStochasticLanguage, ebi_trait_semantics::{EbiTraitSemantics, Semantics}, ebi_trait_stochastic_semantics::{EbiTraitStochasticSemantics, StochasticSemantics}}, entropic_relevance, export::{self, EbiOutput, EbiOutputType}, import, jenson_shannon_stochastic_conformance, math::{fraction::Fraction, log_div::LogDiv, root_log_div::RootLogDiv}, sample::{self, Sampler}, stochastic_labelled_petri_net_semantics::StochasticLabelledPetriNetSemantics, unit_earth_movers_stochastic_conformance};
// use fraction::{One, Zero};
// use super::{ebi_command::EbiCommand, ebi_command_sample::{self, SAMPLED_OBJECT_INPUTS}};


// pub const EBI_ALIGNMENT: EbiCommand = EbiCommand::Group { 
//     name_short: "sa",
//     name_long: Some("stochastic alignment"), 
//     explanation_short: "Check the stochastic alignments of an event log and a stochastic model.", 
//     explanation_long: None, 
//     children: &[
//         &STOCHASTIC_ALIGNMENT,
//     ]
// };

// pub const STOCHASTIC_ALIGNMENT: EbiCommand = EbiCommand::Command { 
//     name_short: "stochastic alignment", 
//     name_long: Some("stochastic alignment"), 
//     explanation_short: "Compute stochastic alignment.", 
//     explanation_long: None, 
//     latex_link: Some(""), 
//     cli_command: None, 
//     exact_arithmetic: true, 
//     input_types: &[ 
//         &[ &EbiInputType::Trait(EbiTrait::Semantics) ], 
//         &[ &EbiInputType::Trait(EbiTrait::FiniteLanguage) ] 
//     ], 
//     input_names: &[ "FILE_1", "FILE_2" ], 
//     input_helps: &[ "The queriable stochastic language (model).", "The finite language (log)." ], 
//     execute: |mut inputs, cli_matches| {
//         let mut model: Box<dyn StochasticSemantics<State=LPNMarking>> = inputs.remove(0).to_type::<dyn StochasticSemantics<>>()?;
//         let mut event_log = inputs.remove(0).to_type::<dyn EbiTraitFiniteLanguage>()?;

//         let alignment = model.align_log(event_log)?;
//         return Ok(alignment);
//     }, 
//     output: &EbiOutputType::Fraction,
// };