use std::{collections::BTreeSet, fmt::{Debug, Display}, hash::Hash, path::PathBuf};
use clap::{value_parser, Arg, ArgAction, ArgMatches, Command};
use anyhow::{anyhow, Context, Result};
use indicatif::{ProgressBar, ProgressStyle};

use crate::{ebi_commands::{ebi_command_analyse, ebi_command_analyse_non_stochastic, ebi_command_association, ebi_command_conformance, ebi_command_convert, ebi_command_discover, ebi_command_info, ebi_command_itself, ebi_command_probability, ebi_command_sample, ebi_command_test, ebi_command_validate, ebi_command_visualise}, ebi_framework::ebi_output, math::fraction::{Fraction, FractionNotParsedYet}};

use super::{ebi_file_handler::EbiFileHandler, ebi_input::{self, EbiInput, EbiInputType}, ebi_output::{EbiExporter, EbiOutput, EbiOutputType}};

pub const EBI_COMMANDS: EbiCommand = EbiCommand::Group {
    name_short: "Ebi",
    name_long: None,
    explanation_short: "Ebi: a tool for stochastic process mining.",
    explanation_long: None, 
    children: &[
        &ebi_command_analyse::EBI_ANALYSE,
        &ebi_command_analyse_non_stochastic::EBI_ANALYSE_NON_STOCHASTIC,
        &ebi_command_association::EBI_ASSOCIATION,
        &ebi_command_conformance::EBI_CONFORMANCE,
        &ebi_command_convert::EBI_CONVERT,
        &ebi_command_discover::EBI_DISCOVER,
        &ebi_command_itself::EBI_ITSELF,
        &ebi_command_info::EBI_INFO,
        &ebi_command_probability::EBI_PROBABILITY,
        &ebi_command_sample::EBI_SAMPLE,
        &ebi_command_test::EBI_TEST,
        &ebi_command_validate::EBI_VALIDATE,
        &ebi_command_visualise::EBI_VISUALISE,
    ] 
};

pub const ARG_SHORT_OUTPUT: char = 'o';
pub const ARG_SHORT_APPROX: char = 'a';
pub const ARG_ID_OUTPUT: &str = "output";

pub enum EbiCommand {
    Group{
        name_short: &'static str,
        name_long: Option<&'static str>,
        explanation_short: &'static str,
        explanation_long: Option<&'static str>,
        children: &'static [&'static EbiCommand]
    },
    Command{
        name_short: &'static str,
        name_long: Option<&'static str>,
        explanation_short: &'static str,
        explanation_long: Option<&'static str>,
        latex_link: Option<&'static str>, //insert a link to a Latex target
        cli_command: Option<fn(command: Command) -> Command>, //create the cli command. An output -o argument is always added

        exact_arithmetic: bool, //if set to true, then Ebi will use exact arithmetic by default, but allow the user to disable it
        
        input_types: &'static [&'static [&'static EbiInputType]], //for each fixed-position input parameter, the ebi traits that are accepted. If accept_source_file is true, that is the first parameter
        input_names: &'static [&'static str],
        input_helps: &'static [&'static str],

        execute: fn(inputs: Vec<EbiInput>, cli_matches: Option<&ArgMatches>) -> Result<EbiOutput>, //the cli_matches are provided only when cli_command is set to Some(_).
        output_type: &'static EbiOutputType
    }
}

impl EbiCommand {
    pub fn build_cli(&self) -> Command {
        let mut command;
        match self {
            EbiCommand::Group { name_short, name_long, explanation_short, explanation_long, children } => {
                let name = if let Some(x) = name_long {x} else {name_short};
                command = Command::new(name)
                    .about(explanation_short)
                    .subcommand_required(true)
                    .allow_external_subcommands(false);

                if name_long.is_some() {
                    command = command.alias(name_short);
                }

                if let Some(l) = explanation_long {
                    command = command.long_about(l);
                }
                
                for child in children.iter() {
                    let subcommand = child.build_cli();
                    command = command.subcommand(subcommand);
                }
            },
            EbiCommand::Command { name_short, name_long, explanation_short, explanation_long, cli_command, exact_arithmetic, input_types, input_helps: input_help, input_names, ..} => {
                let name = if let Some(x) = name_long {x} else {name_short};
                command = Command::new(name)
                    .about(explanation_short);

                if name_long.is_some() {
                    command = command.alias(name_short);
                }

                if let Some(l) = explanation_long {
                    command = command.long_about(l);
                }

                for (i, (input_name, (input_type, input_help))) in input_names.iter().zip(input_types.iter().zip(input_help.iter())).enumerate() {
                    let arg = Arg::new(format!("{}x{}", input_name, i))
                    .action(ArgAction::Set)
                    .value_name(input_name)
                    .help(input_help)
                    .required(true)
                    .value_parser(EbiInputType::get_parser_of_list(input_type))
                    .long_help(EbiInputType::possible_inputs_as_strings_with_articles(input_type, " and "));

                    command = command.arg(arg);
                }

                if let Some(f) = cli_command {
                    command = (f)(command);   
                }

                command = command.arg(
                    Arg::new(ARG_ID_OUTPUT)
                    .short(ARG_SHORT_OUTPUT)
                    .long(ARG_ID_OUTPUT)
                    .action(ArgAction::Set)
                    .value_name("FILE")
                    .help("Saves the result to a file.")
                    .required(false)
                    .value_parser(value_parser!(PathBuf))
                );

                if *exact_arithmetic {
                    command = command.arg(
                        Arg::new("approx")
                        .short(ARG_SHORT_APPROX)
                        .long("approximate")
                        .action(ArgAction::SetTrue)
                        .num_args(0)
                        .help("Use approximate arithmetic instead of exact arithmetic.")
                        .required(false)
                        .value_parser(value_parser!(bool))
                    )
                }
            },
        };
        return command;
    }

    pub fn short_name(&self) -> &str {
        match self {
            EbiCommand::Group { name_short, .. } => name_short,
            EbiCommand::Command { name_short, .. } => name_short,
        }
    }

    pub fn long_name(&self) -> &str {
        match self {
            EbiCommand::Group { name_short, name_long, .. } => match name_long {
                Some(x) => x,
                None => &name_short,
            },
            EbiCommand::Command { name_short, name_long, .. } => match name_long {
                Some(x) => x,
                None => &name_short,
            },
        }
    }

    pub fn explanation_long(&self) -> &str {
        match self {
            EbiCommand::Group { explanation_short, explanation_long, .. } => match explanation_long {
                Some(x) => x,
                None => &explanation_short,
            },
            EbiCommand::Command { explanation_short, explanation_long, .. } => match explanation_long {
                Some(x) => x,
                None => &explanation_short,
            },
        }
    }

    pub fn get_progress_bar(total_ticks: usize) -> ProgressBar {
        let pb = ProgressBar::new(total_ticks.try_into().unwrap());
        pb.set_style(ProgressStyle::with_template("[{wide_bar:.cyan/blue}] {pos:>7}/{len:7}")
            .unwrap()
            .progress_chars("#>-"));
        pb.set_position(0);
        return pb
    }

    pub fn execute(&self, cli_matches: &ArgMatches) -> Result<()> {
        match self {
            EbiCommand::Group { children, ..} => {
                for child in children.iter() {
                    if let Some(sub_matches) = cli_matches.subcommand_matches(child.long_name()) {
                        return child.execute(sub_matches);
                    }
                }
            },
            EbiCommand::Command { exact_arithmetic, input_types: input_typess, execute, output_type, input_names, .. } => {
                //set exact arithmetic
                if !exact_arithmetic || cli_matches.get_flag("approx") {
                    log::info!("Use approximate arithmetic");
                    Fraction::set_exact_globally(false);
                }

                //read the inputs
                let mut inputs = vec![];
                for (i, (input_types, input_name)) in input_typess.iter().zip(input_names.iter()).enumerate() {
                    let cli_id = format!("{}x{}", input_name, i);

                    //read input
                    log::info!("Reading {}", input_name);
                    let input = Self::attempt_parse(input_types, cli_matches, &cli_id).with_context(|| format!("Reading parameter {}.", input_name))?;
                    inputs.push(input);
                }

                log::info!("Starting {}", self.long_name());

                let result = (execute)(inputs, Some(cli_matches))?;

                if &&result.get_type() != output_type {
                    return Err(anyhow!("Output type {} does not match the declared output of {}.", result.get_type(), output_type))
                }

                if let Some(to_file) = cli_matches.get_one::<PathBuf>(ARG_ID_OUTPUT) {
                    //write result to file
                    let exporter = Self::select_exporter(output_type, Some(to_file));
                    log::info!("Writing result to {:?} as {} {}", to_file, exporter.get_article(), exporter);
                    ebi_output::export_object(to_file, result, exporter)?;
                } else {
                    //write result to STDOUT
                    let exporter = Self::select_exporter(output_type, None);
                    log::info!("Writing result as {} {}", exporter.get_article(), exporter);
                    println!("{}", ebi_output::export_to_string(result, exporter)?);
                }

                return Ok(());
            },
        }
        Err(anyhow!("command not recognised"))
    }

    pub fn select_exporter(output_type: &EbiOutputType, to_file: Option<&PathBuf>) -> EbiExporter {
        let exporters = output_type.get_exporters();
        
        if exporters.len() == 1 || to_file.is_none() {
            return exporters.into_iter().next().unwrap();
        }

        //strategy: take the exporter with the longest extension first (to export .xes.gz before .xes)
        {
            let mut exporters = exporters.clone();
            exporters.sort_by(|a, b| {
                if let EbiExporter::Object(_, file_handler_a) = a {
                    if let EbiExporter::Object(_, file_handler_b) = b {
                        file_handler_b.file_extension.len().cmp(&file_handler_a.file_extension.len())
                    } else {
                        unreachable!()
                    }
                } else {
                    unreachable!()
                }
            });

            for exporter in exporters {
                if let EbiExporter::Object(_, file_handler) = exporter {
                    if to_file.unwrap().display().to_string().ends_with(&(".".to_string() + file_handler.file_extension)) {
                        return exporter;
                    }
                } else {
                    unreachable!()
                }
            }
        }

        //otherwise, take the first one that was mentioned
        return exporters.into_iter().next().unwrap();
    }

    /**
     * Attempt to parse an input as any of the given input types. Returns the last error if unsuccessful.
     */
    pub fn attempt_parse(input_types: &[&EbiInputType], cli_matches: &ArgMatches, cli_id: &str) -> Result<EbiInput> {
        //an input may be of several types; go through each of them
        let mut error = None;
        for input_type in input_types.iter() {
            //try to parse the input as this type
            match input_type {
                EbiInputType::Trait(etrait) => {
                    
                    //try to parse a trait
                    match ebi_input::get_reader(cli_matches, cli_id).context("Getting reader.") {
                        Ok(mut reader) => {
                            match ebi_input::read_as_trait(etrait, &mut reader).with_context(|| format!("Parsing as the trait `{}`.", etrait)) {
                                Ok((object, file_handler)) => return Ok(EbiInput::Trait(object, file_handler)),
                                Err(e) => error = Some(e)
                            }
                        },
                        Err(e) => error = Some(e)
                    }
                },
                EbiInputType::Object(etype) => {
                    
                    //try to parse a specific object
                    match ebi_input::get_reader(cli_matches, cli_id).context("Getting reader.") {
                        Ok(mut reader) => {
                            match ebi_input::read_as_object(etype, &mut reader).with_context(|| format!("Parsing as the object type `{}`.", etype)) {
                                Ok((object, file_handler)) => return Ok(EbiInput::Object(object, file_handler)),
                                Err(e) => error = Some(e)
                            }
                        },
                        Err(e) => error = Some(e)
                    }
                },
                EbiInputType::AnyObject => {
                    match ebi_input::get_reader(cli_matches, cli_id).context("Getting reader.") {
                        Ok(mut reader) => {
                            match ebi_input::read_as_any_object(&mut reader).context("Parsing as any object.") {
                                Ok((object, file_handler)) => return Ok(EbiInput::Object(object, file_handler)),
                                Err(e) => error = Some(e)
                            }
                        },
                        Err(e) => error = Some(e)
                    }
                },
                EbiInputType::FileHandler => {
                    if let Some(value) = cli_matches.get_one::<EbiFileHandler>(&cli_id) {
                        return Ok(EbiInput::FileHandler(value.clone()));
                    }
                },
                EbiInputType::String => {
                    if let Some(value) = cli_matches.get_one::<String>(&cli_id) {
                        return Ok(EbiInput::String(value.clone()));
                    }
                },
                EbiInputType::Usize => {
                    if let Some(value) = cli_matches.get_one::<usize>(&cli_id) {
                        return Ok(EbiInput::Usize(value.clone()));
                    }
                },
                EbiInputType::Fraction => {
                    if let Some(value) = cli_matches.get_one::<FractionNotParsedYet>(&cli_id) {
                        return Ok(EbiInput::Fraction(value.try_into()?));
                    }
                },
            }
        }

        match error {
            Some(e) => Err(e),
            None => Err(anyhow!("argument was not given")),
        }
    } 

    pub fn path_to_string(path: &Vec<&EbiCommand>) -> String {
        let result: Vec<&str> = path.iter().map(|command| command.long_name()).collect();
        result.join(" ")
    }

    pub fn path_to_short_string(path: &Vec<&EbiCommand>) -> String {
        let result: Vec<&str> = path.iter().map(|command| command.short_name()).collect();
        result.join(" ")
    }

    pub fn find_command_with_string(&self, name: &String) -> Option<Vec<&EbiCommand>> {
        for path in self.get_command_paths() {
            if Self::path_to_string(&path) == *name {
                return Some(path)
            }
        }
        None
    }

    pub fn get_command_paths(&self) -> BTreeSet<Vec<&'static EbiCommand>> {
        let mut result = BTreeSet::new();
        self.get_paths_recursive(&EBI_COMMANDS, &mut result, vec![]);
        result
    }

    fn get_paths_recursive(&self, command: &'static EbiCommand, result: &mut BTreeSet<Vec<&'static EbiCommand>>, prefix: Vec<&'static EbiCommand>) {
        match command {
            EbiCommand::Group { children, .. } => {
                for child in children.iter() {
                    let mut prefix = prefix.clone();
                    prefix.push(command);
                    self.get_paths_recursive(child, result, prefix);
                }
            },
            EbiCommand::Command { .. } => {
                let mut prefix = prefix.clone();
                prefix.push(command);
                result.insert(prefix);
            },
        }   
    }
}

impl Ord for EbiCommand {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.long_name().cmp(other.long_name())
    }
}

impl PartialOrd for EbiCommand {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        self.long_name().partial_cmp(other.long_name())
    }
}

impl Display for EbiCommand {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.long_name())
    }
}

impl Debug for EbiCommand {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Group { name_short, name_long, .. } => f.debug_struct("Group").field("name_short", name_short).field("name_long", name_long).finish(),
            Self::Command { name_short, name_long, .. } => f.debug_struct("Command").field("name_short", name_short).field("name_long", name_long).finish(),
        }
    }
}

impl Eq for EbiCommand {}

impl PartialEq for EbiCommand {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Group { name_short: l_name_short, name_long: l_name_long, explanation_short: l_explanation_short, explanation_long: l_explanation_long, children: l_children }, Self::Group { name_short: r_name_short, name_long: r_name_long, explanation_short: r_explanation_short, explanation_long: r_explanation_long, children: r_children }) => l_name_short == r_name_short && l_name_long == r_name_long && l_explanation_short == r_explanation_short && l_explanation_long == r_explanation_long && l_children == r_children,
            (Self::Command { name_short: l_name_short, name_long: l_name_long, explanation_short: l_explanation_short, explanation_long: l_explanation_long, latex_link: l_latex_link, cli_command: l_cli_command, exact_arithmetic: l_exact_arithmetic, input_types: l_input_types, input_names: l_input_names, input_helps: l_input_helps, execute: l_execute, output_type: l_output }, Self::Command { name_short: r_name_short, name_long: r_name_long, explanation_short: r_explanation_short, explanation_long: r_explanation_long, latex_link: r_latex_link, cli_command: r_cli_command, exact_arithmetic: r_exact_arithmetic, input_types: r_input_types, input_names: r_input_names, input_helps: r_input_helps, execute: r_execute, output_type: r_output }) => l_name_short == r_name_short && l_name_long == r_name_long && l_explanation_short == r_explanation_short && l_explanation_long == r_explanation_long && l_latex_link == r_latex_link && l_cli_command == r_cli_command && l_exact_arithmetic == r_exact_arithmetic && l_input_types == r_input_types && l_input_names == r_input_names && l_input_helps == r_input_helps && l_execute == r_execute && l_output == r_output,
            _ => false,
        }
    }
}

impl Hash for EbiCommand {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.long_name().hash(state)
    }
}