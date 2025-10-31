use crate::text::Joiner;
use anyhow::{Result, anyhow};
use clap::{Arg, ArgAction, ArgMatches, Command, builder::ValueParser, value_parser};
use ebi_arithmetic::{Fraction, parsing::FractionNotParsedYet};
use ebi_objects::traits::importable::{
    ImporterParameter, ImporterParameterValue, ImporterParameterValues,
};
use std::collections::BTreeSet;

pub fn build_gui(
    mut command: Command,
    parameters: BTreeSet<&'static ImporterParameter>,
    input_index: usize,
) -> Command {
    for parameter in parameters {
        let gui_id = format!("input{}_{}", input_index, parameter.name());
        let mut arg = Arg::new(&gui_id)
            .long(gui_id)
            .help(parameter.explanation())
            .required(false);
        arg = match parameter {
            ImporterParameter::Flag { .. } => arg.action(ArgAction::SetTrue),
            ImporterParameter::String {
                explanation,
                allowed_values,
                default_value,
                ..
            } => {
                let parser: ValueParser = value_parser!(String).into();

                if let Some(allowed) = allowed_values {
                    let allowed = allowed
                        .iter()
                        .map(|s| s.to_string())
                        .collect::<Vec<_>>()
                        .join_with(", ", " and ");
                    arg = arg.long_help(format!(
                        "{}. Allowed values: {}. Default value: {}",
                        explanation, allowed, default_value
                    ));
                }

                arg.action(ArgAction::Set).value_parser(parser)
            }
            ImporterParameter::Usize {
                explanation,
                minimum_value,
                maximum_value,
                default_value,
                ..
            } => {
                let parser: ValueParser = value_parser!(usize).into();

                let allowed = match (minimum_value, maximum_value) {
                    (None, None) => "integer".to_string(),
                    (None, Some(max)) => format!("integer below or equal to {}", max),
                    (Some(min), None) => format!("integer above or equal to {}", min),
                    (Some(min), Some(max)) => format!("integer between {} and {}", min, max),
                };
                arg = arg.long_help(format!(
                    "{}. Value: {}. Default: {}",
                    explanation, allowed, default_value
                ));

                arg.action(ArgAction::Set).value_parser(parser)
            }
            ImporterParameter::Fraction {
                explanation,
                minimum_value,
                maximum_value,
                default_value,
                ..
            } => {
                let parser: ValueParser = value_parser!(FractionNotParsedYet).into();

                let allowed = match (minimum_value, maximum_value) {
                    (None, None) => "fraction".to_string(),
                    (None, Some(max)) => format!("fraction below or equal to {}", max),
                    (Some(min), None) => format!("fraction above or equal to {}", min),
                    (Some(min), Some(max)) => format!("fraction between {} and {}", min, max),
                };
                arg = arg.long_help(format!(
                    "{}. Value: {}. Default: {}",
                    explanation, allowed, default_value
                ));

                arg.action(ArgAction::Set).value_parser(parser)
            }
        };
        command = command.arg(arg)
    }
    command
}

/// Get parameters from CLI, appended with defaults.
pub fn extract_parameter_values(
    cli_matches: Option<&ArgMatches>,
    parameters: &'static [ImporterParameter],
    input_index: usize,
) -> Result<ImporterParameterValues> {
    let mut result = ImporterParameterValues::new();

    for parameter in parameters {
        result.insert(
            *parameter,
            match parameter {
                ImporterParameter::Flag { name, .. } => {
                    if let Some(cli_matches) = cli_matches
                        && cli_matches.contains_id(&name_to_id(name, input_index))
                    {
                        ImporterParameterValue::Boolean(true)
                    } else {
                        ImporterParameterValue::Boolean(false)
                    }
                }
                ImporterParameter::String {
                    name,
                    allowed_values,
                    default_value,
                    ..
                } => {
                    if let Some(cli_matches) = cli_matches {
                        if let Some(value) =
                            cli_matches.get_one::<String>(&name_to_id(name, input_index))
                        {
                            if let Some(allowed_values) = allowed_values {
                                if allowed_values.contains(&value.as_str()) {
                                    ImporterParameterValue::String(value.to_string())
                                } else {
                                    return Err(anyhow!(
                                        "value should be one of {:?}",
                                        allowed_values
                                    ));
                                }
                            } else {
                                ImporterParameterValue::String(value.to_string())
                            }
                        } else {
                            ImporterParameterValue::String(default_value.to_string())
                        }
                    } else {
                        ImporterParameterValue::String(default_value.to_string())
                    }
                }
                ImporterParameter::Usize {
                    name,
                    minimum_value,
                    maximum_value,
                    default_value,
                    ..
                } => {
                    if let Some(cli_matches) = cli_matches {
                        if let Some(value) =
                            cli_matches.get_one::<usize>(&name_to_id(name, input_index))
                        {
                            match (minimum_value, maximum_value) {
                                (Some(min), Some(max)) => {
                                    if value < min || value > max {
                                        return Err(anyhow!(
                                            "Value must be between {} and {}.",
                                            min,
                                            max
                                        ));
                                    } else {
                                        ImporterParameterValue::Usize(*value)
                                    }
                                }
                                (Some(min), None) => {
                                    if value < min {
                                        return Err(anyhow!("Value must be below {}.", min));
                                    } else {
                                        ImporterParameterValue::Usize(*value)
                                    }
                                }
                                (None, Some(max)) => {
                                    if value > max {
                                        return Err(anyhow!("Value must be above {}.", max));
                                    } else {
                                        ImporterParameterValue::Usize(*value)
                                    }
                                }
                                (None, None) => ImporterParameterValue::Usize(*value),
                            }
                        } else {
                            ImporterParameterValue::Usize(*default_value)
                        }
                    } else {
                        ImporterParameterValue::Usize(*default_value)
                    }
                }
                ImporterParameter::Fraction {
                    name,
                    minimum_value,
                    maximum_value,
                    default_value,
                    ..
                } => {
                    if let Some(cli_matches) = cli_matches {
                        if let Some(value) = cli_matches
                            .get_one::<FractionNotParsedYet>(&name_to_id(name, input_index))
                        {
                            let value: Fraction = value.try_into()?;
                            match (minimum_value, maximum_value) {
                                (Some(min), Some(max)) => {
                                    if min > &value || max < &value {
                                        return Err(anyhow!(
                                            "Value must be between {} and {}.",
                                            min,
                                            max
                                        ));
                                    } else {
                                        ImporterParameterValue::Fraction(value)
                                    }
                                }
                                (Some(min), None) => {
                                    if min > &value {
                                        return Err(anyhow!("Value must be below {}.", min));
                                    } else {
                                        ImporterParameterValue::Fraction(value)
                                    }
                                }
                                (None, Some(max)) => {
                                    if max > &value {
                                        return Err(anyhow!("Value must be above {}.", max));
                                    } else {
                                        ImporterParameterValue::Fraction(value)
                                    }
                                }
                                (None, None) => ImporterParameterValue::Fraction(value),
                            }
                        } else {
                            ImporterParameterValue::Fraction(default_value.to_fraction())
                        }
                    } else {
                        ImporterParameterValue::Fraction(default_value.to_fraction())
                    }
                }
            },
        );
    }

    Ok(result)
}

pub fn name_to_id(name: &str, input_index: usize) -> String {
    format!("input{}_{}", input_index, name)
}
