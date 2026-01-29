use super::{
    ebi_file_handler::EbiFileHandler,
    ebi_input::{self, EbiInput, EbiInputType},
    ebi_output::{EbiExporter, EbiOutput, EbiOutputType},
};
use crate::{
    ebi_commands::{
        ebi_command_analyse, ebi_command_analyse_non_stochastic, ebi_command_association,
        ebi_command_conformance, ebi_command_conformance_non_stochastic, ebi_command_convert,
        ebi_command_discover, ebi_command_discover_non_stochastic, ebi_command_filter,
        ebi_command_info,
        ebi_command_itself::{self},
        ebi_command_probability, ebi_command_sample, ebi_command_test, ebi_command_validate,
        ebi_command_visualise,
    },
    ebi_framework::{ebi_importer_parameters, ebi_output},
    prom::java_object_handler::get_possible_inputs_with_java,
    text::Joiner,
};
use anyhow::{Context, Result, anyhow};
use clap::{Arg, ArgAction, ArgMatches, Command, value_parser};
use ebi_objects::{
    EbiObjectType,
    ebi_arithmetic::{Fraction, exact::set_exact_globally, parsing::FractionNotParsedYet},
};
use indicatif::{ProgressBar, ProgressStyle};
use itertools::Itertools;
use log::Level;
use logging_timer::timer;
use std::{
    collections::BTreeSet,
    fmt::{Debug, Display},
    hash::Hash,
    io::Write,
    path::PathBuf,
    time::Duration,
};

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
        &ebi_command_conformance_non_stochastic::EBI_CONFORMANCE_NON_STOCHASTIC,
        &ebi_command_convert::EBI_CONVERT,
        &ebi_command_discover::EBI_DISCOVER,
        &ebi_command_discover_non_stochastic::EBI_DISCOVER_NON_STOCHASTIC,
        &ebi_command_filter::EBI_FILTER,
        &ebi_command_itself::EBI_ITSELF,
        &ebi_command_info::EBI_INFO,
        &ebi_command_probability::EBI_PROBABILITY,
        &ebi_command_sample::EBI_SAMPLE,
        &ebi_command_test::EBI_TEST,
        &ebi_command_validate::EBI_VALIDATE,
        &ebi_command_visualise::EBI_VISUALISE,
    ],
};

pub const ARG_SHORT_OUTPUT: char = 'o';
pub const ARG_SHORT_OUTPUT_TYPE: char = 't';
pub const ARG_SHORT_APPROX: char = 'a';
pub const ARG_ID_OUTPUT: &str = "output";
pub const ARG_ID_OUTPUT_TYPE: &str = "output_type";

pub enum EbiCommand {
    Group {
        name_short: &'static str,
        name_long: Option<&'static str>,
        explanation_short: &'static str,
        explanation_long: Option<&'static str>,
        children: &'static [&'static EbiCommand],
    },
    Command {
        /// The short name of the command. Do not use macros in this field, due to Python command-hunting.
        name_short: &'static str,
        name_long: Option<&'static str>,
        explanation_short: &'static str,
        explanation_long: Option<&'static str>,
        latex_link: Option<&'static str>, //insert a link to a Latex target
        cli_command: Option<fn(command: Command) -> Command>, //create the cli command. An output -o argument is always added

        exact_arithmetic: bool, //if set to true, then Ebi will use exact arithmetic by default, but allow the user to disable it. If false, the command will only be available in approximate mode.

        input_types: &'static [&'static [&'static EbiInputType]], //for each fixed-position input parameter, the ebi traits that are accepted. If accept_source_file is true, that is the first parameter
        input_names: &'static [&'static str],
        input_helps: &'static [&'static str],

        execute: fn(inputs: Vec<EbiInput>, cli_matches: Option<&ArgMatches>) -> Result<EbiOutput>, //the cli_matches are provided only when cli_command is set to Some(_).
        output_type: &'static EbiOutputType,
    },
}

impl EbiCommand {
    pub fn build_cli(&self) -> Command {
        let mut command;
        match self {
            EbiCommand::Group {
                name_short,
                name_long,
                explanation_short,
                explanation_long,
                children,
            } => {
                let name = if let Some(x) = name_long {
                    x
                } else {
                    name_short
                };
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
            }
            EbiCommand::Command {
                name_short,
                name_long,
                explanation_short,
                explanation_long,
                cli_command,
                exact_arithmetic,
                input_types,
                input_helps: input_help,
                input_names,
                output_type,
                ..
            } => {
                let name = if let Some(x) = name_long {
                    x
                } else {
                    name_short
                };
                command = Command::new(name).about(explanation_short);

                if name_long.is_some() {
                    command = command.alias(name_short);
                }

                if let Some(l) = explanation_long {
                    command = command.long_about(l);
                }

                for (i, (input_name, (input_type, input_help))) in input_names
                    .iter()
                    .zip(input_types.iter().zip(input_help.iter()))
                    .enumerate()
                {
                    let mut arg = Arg::new(format!("{}x{}", input_name, i))
                        .action(ArgAction::Set)
                        .value_name(input_name)
                        .help(input_help)
                        .value_parser(EbiInputType::get_parser_of_list(input_type))
                        .long_help(EbiInputType::possible_inputs_as_strings_with_articles(
                            input_type, " and ",
                        ));

                    if let Some(default) = ebi_input::default(input_type) {
                        arg = arg.required(false).default_value(default);
                    } else {
                        arg = arg.required(true);
                    }

                    command = command.arg(arg);
                }

                if let Some(f) = cli_command {
                    command = (f)(command);
                }

                //output type flag
                if output_type.get_exporters().len() > 1 {
                    let output_extensions = output_type
                        .get_exporters()
                        .iter()
                        .map(|exporter| exporter.get_extension())
                        .collect::<Vec<_>>()
                        .join_with(", ", " and ");
                    command = command.arg(
                        Arg::new(ARG_ID_OUTPUT_TYPE)
                            .short(ARG_SHORT_OUTPUT_TYPE)
                            .long(ARG_ID_OUTPUT_TYPE)
                            .action(ArgAction::Set)
                            .value_name("OUTPUT_TYPE")
                            .help("Specify the output type.")
                            .long_help(format!("Specify the output file extension (without period). The default is {}. Possible values are {}.", output_type.get_default_exporter().get_extension(), output_extensions))
                            .value_parser(value_parser!(String)),
                        );
                };

                //output file flag
                command = command.arg(
                    { let mut arg = Arg::new(ARG_ID_OUTPUT)
                        .short(ARG_SHORT_OUTPUT)
                        .long(ARG_ID_OUTPUT)
                        .action(ArgAction::Set)
                        .value_name("FILE")
                        .help("Saves the result to a file.")
                        .required(false)
                        .value_parser(value_parser!(PathBuf));

                        if output_type.get_exporters().len() > 1 {
                            arg = arg.long_help(format!("Saves the results to a file. The file type is determined by its extension, unless the -{} parameter is also given.", ARG_SHORT_OUTPUT_TYPE))
                        }

                        arg
                    }
                );

                //exact arithmetic flag
                if *exact_arithmetic {
                    command = command.arg(
                        Arg::new("approx")
                            .short(ARG_SHORT_APPROX)
                            .long("approximate")
                            .action(ArgAction::SetTrue)
                            .num_args(0)
                            .help("Use approximate arithmetic instead of exact arithmetic.")
                            .required(false)
                            .value_parser(value_parser!(bool)),
                    )
                }

                //importer flags
                for (input_index, inputs) in input_types.iter().enumerate() {
                    let merged_importer_parameters =
                        ebi_importer_parameters::merge_importer_parameters(inputs);
                    command = ebi_importer_parameters::build_cli(
                        command,
                        merged_importer_parameters,
                        input_index,
                    );
                }
            }
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
            EbiCommand::Group {
                name_short,
                name_long,
                ..
            } => match name_long {
                Some(x) => x,
                None => &name_short,
            },
            EbiCommand::Command {
                name_short,
                name_long,
                ..
            } => match name_long {
                Some(x) => x,
                None => &name_short,
            },
        }
    }

    pub fn explanation_short(&self) -> &str {
        match self {
            EbiCommand::Group {
                explanation_short, ..
            } => &explanation_short,

            EbiCommand::Command {
                explanation_short, ..
            } => &explanation_short,
        }
    }

    pub fn explanation_long(&self) -> &str {
        match self {
            EbiCommand::Group {
                explanation_short,
                explanation_long,
                ..
            } => match explanation_long {
                Some(x) => x,
                None => &explanation_short,
            },
            EbiCommand::Command {
                explanation_short,
                explanation_long,
                ..
            } => match explanation_long {
                Some(x) => x,
                None => &explanation_short,
            },
        }
    }

    pub fn get_progress_bar_ticks(total_ticks: usize) -> ProgressBar {
        let pb = ProgressBar::new(total_ticks.try_into().unwrap());
        pb.set_style(
            ProgressStyle::with_template(&("[{wide_bar:.cyan/blue}] {pos:>7}/{len:7}".to_owned()))
                .unwrap()
                .progress_chars("#>-"),
        );
        pb.set_position(0);
        pb
    }

    pub fn get_progress_bar_message(message: String) -> ProgressBar {
        let pb = ProgressBar::new_spinner();
        pb.set_style(ProgressStyle::with_template(&("{spinner} {wide_msg}")).unwrap());
        pb.enable_steady_tick(Duration::from_millis(100));
        pb.set_message(message);
        pb.tick();
        pb
    }

    pub fn execute(&self, cli_matches: &ArgMatches) -> Result<()> {
        match self {
            EbiCommand::Group { children, .. } => {
                for child in children.iter() {
                    if let Some(sub_matches) = cli_matches.subcommand_matches(child.long_name()) {
                        return child.execute(sub_matches);
                    }
                }
            }
            EbiCommand::Command {
                exact_arithmetic,
                input_types: input_typess,
                execute,
                output_type,
                input_names,
                ..
            } => {
                //set exact arithmetic
                if !exact_arithmetic || cli_matches.get_flag("approx") {
                    log::info!("Use approximate arithmetic");
                    set_exact_globally(false);
                }

                //read the inputs
                let mut inputs = vec![];
                for (i, (input_types, input_name)) in
                    input_typess.iter().zip(input_names.iter()).enumerate()
                {
                    let cli_id = format!("{}x{}", input_name, i);

                    //read input
                    log::info!("Reading {}", input_name);
                    let input = Self::attempt_parse(input_types, cli_matches, &cli_id)
                        .with_context(|| format!("Reading parameter {}.", input_name))?;
                    inputs.push(input);
                }

                log::info!("Starting {} command", self.long_name());
                let result = {
                    let _tmr = timer!(Level::Info; self.long_name());

                    (execute)(inputs, Some(cli_matches))?
                };

                if &&result.get_type() != output_type {
                    return Err(anyhow!(
                        "Output type {} does not match the declared output of {}.",
                        result.get_type(),
                        output_type
                    ));
                }

                if let Some(to_file) = cli_matches.get_one::<PathBuf>(ARG_ID_OUTPUT) {
                    //write result to file
                    let exporter =
                        Self::select_exporter(output_type, Some(to_file), Some(cli_matches))?;
                    log::info!(
                        "Writing result to {:?} as {} {}",
                        to_file,
                        exporter.get_article(),
                        exporter
                    );
                    ebi_output::export_object(to_file, result, exporter)?;
                } else {
                    //write result to STDOUT
                    let exporter = Self::select_exporter(output_type, None, Some(cli_matches))?;
                    log::info!("Writing result as {} {}", exporter.get_article(), exporter);
                    if exporter.is_binary() {
                        let mut out = std::io::stdout();
                        out.write_all(&ebi_output::export_to_bytes(result, exporter)?)?;
                        out.flush()?;
                    } else {
                        println!("{}", ebi_output::export_to_string(result, exporter)?);
                    }
                }

                return Ok(());
            }
        }
        Err(anyhow!("command not recognised"))
    }

    pub fn select_exporter(
        output_type: &EbiOutputType,
        to_file: Option<&PathBuf>,
        cli_matches: Option<&ArgMatches>,
    ) -> Result<EbiExporter> {
        let exporters = output_type.get_exporters();

        if let Some(cli_matches) = cli_matches
            && exporters.len() > 1
        {
            //attempt to read the cli parameter
            if let Some(extension) = cli_matches.get_one::<String>(ARG_ID_OUTPUT_TYPE) {
                for exporter in exporters {
                    if exporter.get_extension() == extension {
                        //found the requested exporter
                        return Ok(exporter);
                    }
                }
                return Err(anyhow!(
                    "the requested output file type {} is not available for this command",
                    extension
                ));
            }
        }

        match to_file {
            Some(file) => {
                //strategy: take the exporter with the longest extension first (to export .xes.gz before .xes)
                {
                    let mut exporters = exporters.clone();
                    exporters.sort_by(|a, b| a.get_extension().len().cmp(&b.get_extension().len()));

                    //see whether an output extension matches the requested file
                    for exporter in exporters {
                        if let EbiExporter::Object(_, file_handler) = exporter {
                            if file
                                .display()
                                .to_string()
                                .ends_with(&(".".to_string() + file_handler.file_extension))
                            {
                                return Ok(exporter);
                            }
                        }
                    }
                }

                //otherwise, take the default one
                return Ok(output_type.get_default_exporter());
            }
            None => {
                return Ok(output_type.get_default_exporter());
            }
        }
    }

    /**
     * Attempt to parse an input as any of the given input types. Returns the last error if unsuccessful.
     */
    pub fn attempt_parse(
        input_types: &[&'static EbiInputType],
        cli_matches: &ArgMatches,
        cli_id: &str,
    ) -> Result<EbiInput> {
        //an input may be of several types; go through each of them
        let mut error = None;
        let mut reader = match ebi_input::get_reader(cli_matches, cli_id).context("Getting reader.")
        {
            Ok(x) => Some(x),
            Err(e) => {
                error = Some(e);
                None
            }
        };

        for (input_index, input_type) in input_types.iter().enumerate() {
            //try to parse the input as this type
            match input_type {
                EbiInputType::Trait(etrait) => {
                    //try to parse a trait
                    if let Some(ref mut reader) = reader {
                        match ebi_input::read_as_trait(
                            etrait,
                            reader,
                            Some(cli_matches),
                            input_index,
                        )
                        .with_context(|| format!("Parsing as the trait `{}`.", etrait))
                        {
                            Ok((object, file_handler)) => {
                                return Ok(EbiInput::Trait(object, file_handler));
                            }
                            Err(e) => error = Some(e),
                        }
                    }
                }
                EbiInputType::Object(etype) => {
                    //try to parse a specific object
                    if let Some(ref mut reader) = reader {
                        match ebi_input::read_as_object(
                            etype,
                            reader,
                            Some(cli_matches),
                            input_index,
                        )
                        .with_context(|| format!("Parsing as the object type `{}`.", etype))
                        {
                            Ok((object, file_handler)) => {
                                return Ok(EbiInput::Object(object, file_handler));
                            }
                            Err(e) => error = Some(e),
                        }
                    }
                }
                EbiInputType::AnyObject => {
                    if let Some(ref mut reader) = reader {
                        match ebi_input::read_as_any_object(reader, Some(cli_matches), input_index)
                            .context("Parsing as any object.")
                        {
                            Ok((object, file_handler)) => {
                                return Ok(EbiInput::Object(object, file_handler));
                            }
                            Err(e) => error = Some(e),
                        }
                    }
                }
                EbiInputType::FileHandler => {
                    if let Some(value) = cli_matches.get_one::<EbiFileHandler>(&cli_id) {
                        return Ok(EbiInput::FileHandler(value.clone()));
                    }
                }
                EbiInputType::String(None, _) => {
                    if let Some(value) = cli_matches.get_one::<String>(&cli_id) {
                        return Ok(EbiInput::String(value.clone(), &input_type));
                    }
                }
                EbiInputType::String(Some(allowed_values), _) => {
                    if let Some(value) = cli_matches.get_one::<String>(&cli_id) {
                        if allowed_values.contains(&value.as_str()) {
                            return Ok(EbiInput::String(value.clone(), &input_type));
                        } else {
                            error = Some(anyhow!("value should be one of {:?}", allowed_values));
                        }
                    }
                }
                EbiInputType::Usize(min, max, _) => {
                    if let Some(value) = cli_matches.get_one::<usize>(&cli_id) {
                        match (min, max) {
                            (Some(min), Some(max)) => {
                                if value < min || value > max {
                                    error =
                                        Some(anyhow!("Value must be between {} and {}.", min, max))
                                } else {
                                    return Ok(EbiInput::Usize(value.clone(), input_type));
                                }
                            }
                            (Some(min), None) => {
                                if value < min {
                                    error = Some(anyhow!("Value must be below {}.", min))
                                } else {
                                    return Ok(EbiInput::Usize(value.clone(), input_type));
                                }
                            }
                            (None, Some(max)) => {
                                if value > max {
                                    error = Some(anyhow!("Value must be above {}.", max))
                                } else {
                                    return Ok(EbiInput::Usize(value.clone(), input_type));
                                }
                            }
                            (None, None) => {
                                return Ok(EbiInput::Usize(value.clone(), input_type));
                            }
                        }
                    }
                }
                EbiInputType::Fraction(min, max, _) => {
                    if let Some(value) = cli_matches.get_one::<FractionNotParsedYet>(&cli_id) {
                        let value: Fraction = value.try_into()?;

                        match (min, max) {
                            (Some(min), Some(max)) => {
                                if min > &value || max < &value {
                                    error =
                                        Some(anyhow!("Value must be between {} and {}.", min, max))
                                } else {
                                    return Ok(EbiInput::Fraction(value, input_type));
                                }
                            }
                            (Some(min), None) => {
                                if min > &value {
                                    error = Some(anyhow!("Value must be below {}.", min))
                                } else {
                                    return Ok(EbiInput::Fraction(value, input_type));
                                }
                            }
                            (None, Some(max)) => {
                                if max > &value {
                                    error = Some(anyhow!("Value must be above {}.", max))
                                } else {
                                    return Ok(EbiInput::Fraction(value, input_type));
                                }
                            }
                            (None, None) => {
                                return Ok(EbiInput::Fraction(value, input_type));
                            }
                        }
                    }
                }
            }
        }

        match error {
            Some(e) => Err(e),
            None => Err(anyhow!("argument was not given")),
        }
    }

    pub fn path_to_string(path: &[&EbiCommand]) -> String {
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
                return Some(path);
            }
        }
        None
    }

    pub fn get_command_paths(&self) -> BTreeSet<Vec<&'static EbiCommand>> {
        let mut result = BTreeSet::new();
        self.get_paths_recursive(&EBI_COMMANDS, &mut result, vec![]);
        result
    }

    fn get_paths_recursive(
        &self,
        command: &'static EbiCommand,
        result: &mut BTreeSet<Vec<&'static EbiCommand>>,
        prefix: Vec<&'static EbiCommand>,
    ) {
        match command {
            EbiCommand::Group { children, .. } => {
                for child in children.iter() {
                    let mut prefix = prefix.clone();
                    prefix.push(command);
                    self.get_paths_recursive(child, result, prefix);
                }
            }
            EbiCommand::Command { .. } => {
                let mut prefix = prefix.clone();
                prefix.push(command);
                result.insert(prefix);
            }
        }
    }

    pub fn is_in_java(&self) -> bool {
        if let EbiCommand::Command {
            cli_command,
            output_type,
            input_types: input_typess,
            ..
        } = &self
        {
            if cli_command.is_some() {
                return false;
            }
            for exporter in output_type.get_exporters() {
                for output_java_object_handler in exporter.get_java_object_handlers() {
                    if let Some(_) = output_java_object_handler.translator_ebi_to_java {
                        let input_typesss = input_typess
                            .iter()
                            .map(|arr| get_possible_inputs_with_java(arr))
                            .collect::<Vec<_>>();
                        for _ in input_typesss.iter().multi_cartesian_product() {
                            return true;
                        }
                    }
                }
            }
            return false;
        } else {
            false
        }
    }

    pub fn is_in_python(&self) -> bool {
        if let EbiCommand::Command { cli_command, .. } = &self {
            return cli_command.is_none();
        } else {
            false
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
            Self::Group {
                name_short,
                name_long,
                ..
            } => f
                .debug_struct("Group")
                .field("name_short", name_short)
                .field("name_long", name_long)
                .finish(),
            Self::Command {
                name_short,
                name_long,
                ..
            } => f
                .debug_struct("Command")
                .field("name_short", name_short)
                .field("name_long", name_long)
                .finish(),
        }
    }
}

impl Eq for EbiCommand {}

impl PartialEq for EbiCommand {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (
                Self::Group {
                    name_short: l_name_short,
                    name_long: l_name_long,
                    explanation_short: l_explanation_short,
                    explanation_long: l_explanation_long,
                    children: l_children,
                },
                Self::Group {
                    name_short: r_name_short,
                    name_long: r_name_long,
                    explanation_short: r_explanation_short,
                    explanation_long: r_explanation_long,
                    children: r_children,
                },
            ) => {
                l_name_short == r_name_short
                    && l_name_long == r_name_long
                    && l_explanation_short == r_explanation_short
                    && l_explanation_long == r_explanation_long
                    && l_children == r_children
            }
            (
                Self::Command {
                    name_short: l_name_short,
                    name_long: l_name_long,
                    explanation_short: l_explanation_short,
                    explanation_long: l_explanation_long,
                    latex_link: l_latex_link,
                    cli_command: _,
                    exact_arithmetic: l_exact_arithmetic,
                    input_types: l_input_types,
                    input_names: l_input_names,
                    input_helps: l_input_helps,
                    execute: _,
                    output_type: l_output,
                },
                Self::Command {
                    name_short: r_name_short,
                    name_long: r_name_long,
                    explanation_short: r_explanation_short,
                    explanation_long: r_explanation_long,
                    latex_link: r_latex_link,
                    cli_command: _,
                    exact_arithmetic: r_exact_arithmetic,
                    input_types: r_input_types,
                    input_names: r_input_names,
                    input_helps: r_input_helps,
                    execute: _,
                    output_type: r_output,
                },
            ) => {
                l_name_short == r_name_short
                    && l_name_long == r_name_long
                    && l_explanation_short == r_explanation_short
                    && l_explanation_long == r_explanation_long
                    && l_latex_link == r_latex_link
                    && l_exact_arithmetic == r_exact_arithmetic
                    && l_input_types == r_input_types
                    && l_input_names == r_input_names
                    && l_input_helps == r_input_helps
                    && l_output == r_output
            }
            _ => false,
        }
    }
}

impl Hash for EbiCommand {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.long_name().hash(state)
    }
}

pub fn get_applicable_commands(object_type: &EbiObjectType) -> BTreeSet<Vec<&'static EbiCommand>> {
    let mut result = EBI_COMMANDS.get_command_paths();
    result.retain(|path| {
        if let EbiCommand::Command { input_types, .. } = path[path.len() - 1] {
            for input_typess in input_types.iter() {
                for input_typesss in input_typess.iter() {
                    if input_typesss == &&EbiInputType::AnyObject
                        || input_typesss == &&EbiInputType::Object(object_type.clone())
                    {
                        return true;
                    }
                }
            }
        }
        false
    });
    result
}

#[cfg(test)]
mod tests {
    use super::{EBI_COMMANDS, EbiCommand};
    use crate::{
        ebi_framework::{
            ebi_file_handler::{EBI_FILE_HANDLERS, EbiFileHandler},
            ebi_input::{self, EbiInput, EbiInputType},
            ebi_trait::EbiTrait,
        },
        multiple_reader::MultipleReader,
    };
    use ebi_objects::{EbiObject, ebi_arithmetic::Fraction};
    use itertools::Itertools;
    use ntest::timeout;
    use std::{
        collections::HashSet,
        fmt::Debug,
        fs::{self, File},
        path::PathBuf,
    };

    #[test]
    fn build_cli() {
        EBI_COMMANDS.build_cli();
    }

    #[test]
    fn basic_calls() {
        for command in EBI_COMMANDS.get_command_paths() {
            command.first().unwrap().short_name();
            command.first().unwrap().explanation_long();
            command.last().unwrap().short_name();
            command.last().unwrap().explanation_long();
            EbiCommand::path_to_short_string(&command);
            command.last().unwrap().is_in_java();
            command.first().unwrap().is_in_java();
            command.last().unwrap().to_string();
            let _ = format!("{:?}", command.first().unwrap());
            let _ = format!("{:?}", command.last().unwrap());
            let _ = command
                .first()
                .unwrap()
                .partial_cmp(&command.last().unwrap());
            let _ = command.first().unwrap().eq(command.last().unwrap());
            let _ = command.first().unwrap().eq(command.first().unwrap());
            let _ = command.last().unwrap().eq(command.last().unwrap());
            let mut hash = HashSet::new();
            hash.insert(command.last().unwrap());
        }
    }

    #[test]
    #[timeout(500000)]
    fn call_all_non_cli_commands() {
        for path in EBI_COMMANDS.get_command_paths() {
            if let EbiCommand::Command {
                input_types,
                execute,
                cli_command,
                exact_arithmetic,
                output_type,
                ..
            } = path.last().unwrap()
            {
                if cli_command.is_none() // only test commands that do not use the cli directly
                    && (*exact_arithmetic // do not test approximate commands in exact mode
                        || cfg!(all(not(feature = "eexactarithmetic"), feature = "eapproximatearithmetic")))
                {
                    println!("command {}", EbiCommand::path_to_string(&path));

                    //for each input type, find all input combinations
                    let inputss = find_inputs(input_types);
                    if inputss.is_empty() && input_types.len() > 0 {
                        panic!("Could not find input to call command.");
                    }

                    //apply the command to all input combinations
                    for inputs in inputss {
                        eprintln!("\t\t{:?}", inputs);
                        //we do not know whether a command should succeed (giving an error is fine in general), but no command should panic
                        let output = (execute)(transform(inputs), None);

                        //verify that the output is of the correct type
                        if let Ok(output) = output {
                            assert_eq!(output.get_type(), output_type.to_owned().to_owned());
                        }
                    }
                }
            }
        }
    }

    fn find_inputs(input_typess: &[&[&'static EbiInputType]]) -> Vec<Vec<TestInput>> {
        let mut it = input_typess.iter();
        let mut result = if let Some(input_types) = it.next() {
            find_inputs_for_position(input_types)
                .into_iter()
                .map(|x| vec![x])
                .collect()
        } else {
            vec![]
        };
        while let Some(input_types) = it.next() {
            let add = find_inputs_for_position(input_types);
            result = result
                .into_iter()
                .cartesian_product(add.into_iter())
                .map(|(mut xs, x)| {
                    xs.push(x);
                    xs
                })
                .collect();
        }
        println!("\tinput combinations {}", result.len());
        return result;
    }

    fn transform(inputs: Vec<TestInput>) -> Vec<EbiInput> {
        inputs
            .into_iter()
            .map(|x| x.to_ebi_input())
            .collect::<Vec<_>>()
    }

    #[derive(Clone)]
    enum TestInput {
        Trait(EbiTrait, PathBuf), //a trait cannot be cloned, thus we must parse it every time in the cartesian product
        Object(EbiObject, &'static EbiFileHandler, PathBuf),
        String(String, &'static EbiInputType),
        Usize(usize, &'static EbiInputType),
        FileHandler(EbiFileHandler),
        Fraction(Fraction, &'static EbiInputType),
    }

    impl TestInput {
        fn to_ebi_input(self) -> EbiInput {
            match self {
                TestInput::Trait(etrait, file) => {
                    let mut reader = MultipleReader::from_file(File::open(file).unwrap());
                    match ebi_input::read_as_trait(&etrait, &mut reader, None, 0) {
                        Ok((object, file_handler)) => EbiInput::Trait(object, file_handler),
                        Err(_) => unreachable!(),
                    }
                }
                TestInput::Object(o, fh, _) => EbiInput::Object(o, fh),
                TestInput::String(s, input_type) => EbiInput::String(s, input_type),
                TestInput::Usize(u, input_type) => EbiInput::Usize(u, input_type),
                TestInput::FileHandler(fh) => EbiInput::FileHandler(fh),
                TestInput::Fraction(f, input_type) => EbiInput::Fraction(f, input_type),
            }
        }
    }

    impl Debug for TestInput {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            match self {
                Self::Trait(arg0, arg1) => f.debug_tuple("Trait").field(arg0).field(arg1).finish(),
                Self::Object(_, _, arg2) => f.debug_tuple("Object").field(arg2).finish(),
                Self::String(arg0, _) => f.debug_tuple("String").field(arg0).finish(),
                Self::Usize(arg0, _) => f.debug_tuple("Usize").field(arg0).finish(),
                Self::FileHandler(arg0) => f.debug_tuple("FileHandler").field(&arg0.name).finish(),
                Self::Fraction(arg0, _) => f.debug_tuple("Fraction").field(arg0).finish(),
            }
        }
    }

    fn find_inputs_for_position(input_types: &[&'static EbiInputType]) -> Vec<TestInput> {
        let mut result = vec![];
        for input_type in input_types {
            match input_type {
                EbiInputType::FileHandler => {
                    result.append(
                        &mut EBI_FILE_HANDLERS
                            .iter()
                            .map(|f| TestInput::FileHandler(f.clone()))
                            .collect::<Vec<_>>(),
                    );
                }
                EbiInputType::Fraction(_, _, Some(default)) => {
                    result.push(TestInput::Fraction(default.to_fraction(), &input_type));
                }
                EbiInputType::Fraction(Some(min), _, _) => {
                    result.push(TestInput::Fraction(min.to_fraction(), &input_type));
                }
                EbiInputType::Fraction(_, Some(max), _) => {
                    result.push(TestInput::Fraction(max.to_fraction(), &input_type));
                }
                EbiInputType::Fraction(_, _, _) => {
                    result.push(TestInput::Fraction(Fraction::from((1, 2)), &input_type));
                }
                EbiInputType::String(_, Some(default)) => {
                    result.push(TestInput::String(default.to_string(), &input_type));
                }
                EbiInputType::String(Some(allowed_values), None) => {
                    result.push(TestInput::String(
                        allowed_values[0].to_string(),
                        &input_type,
                    ));
                }
                EbiInputType::String(None, None) => {
                    result.push(TestInput::String("some string".to_string(), &input_type));
                }
                EbiInputType::Usize(_, _, Some(default)) => {
                    result.push(TestInput::Usize(*default, &input_type));
                }
                EbiInputType::Usize(Some(min), _, _) => {
                    result.push(TestInput::Usize(*min, &input_type));
                }
                EbiInputType::Usize(_, Some(max), _) => {
                    result.push(TestInput::Usize(*max, &input_type));
                }
                EbiInputType::Usize(_, _, _) => {
                    result.push(TestInput::Usize(10, &input_type));
                }
                _ => {
                    //look through all files
                    let files = fs::read_dir("./testfiles").unwrap();
                    for path in files {
                        let file = path.unwrap();

                        //for now, we skip files with loops and livelocks, as the techniques cannot handle them yet
                        if !file.file_name().into_string().unwrap().contains("livelock")
                            && !file.file_name().into_string().unwrap().contains("empty")
                            && !file.file_name().into_string().unwrap().contains("infinite")
                            && !file.file_name().into_string().unwrap().contains("loop")
                            && !file.file_name().into_string().unwrap().contains("pdc")
                            && !file
                                .file_name()
                                .into_string()
                                .unwrap()
                                .contains("irregular")
                        {
                            let mut reader =
                                MultipleReader::from_file(File::open(file.path()).unwrap());

                            match input_type {
                                EbiInputType::Trait(etrait) => {
                                    //try to parse a trait

                                    match ebi_input::read_as_trait(etrait, &mut reader, None, 0) {
                                        Ok((_, _)) => {
                                            result.push(TestInput::Trait(*etrait, file.path()));
                                        }
                                        Err(_) => {}
                                    }
                                }
                                EbiInputType::Object(etype) => {
                                    //try to parse a specific object
                                    match ebi_input::read_as_object(etype, &mut reader, None, 0) {
                                        Ok((object, file_handler)) => {
                                            result.push(TestInput::Object(
                                                object,
                                                file_handler,
                                                file.path(),
                                            ));
                                        }
                                        Err(_) => {}
                                    }
                                }
                                EbiInputType::AnyObject => {
                                    match ebi_input::read_as_any_object(&mut reader, None, 0) {
                                        Ok((object, file_handler)) => {
                                            result.push(TestInput::Object(
                                                object,
                                                file_handler,
                                                file.path(),
                                            ));
                                        }
                                        Err(_) => {}
                                    }
                                }
                                _ => {}
                            }
                        }
                    }
                }
            }
        }

        result
    }
}
