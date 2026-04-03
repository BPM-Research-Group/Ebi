use crate::{
    ebi_framework::{
        ebi_command::{EBI_COMMANDS, EbiCommand},
        ebi_input::{self, EbiInputType},
        ebi_output::{EbiExporter, EbiOutput, EbiOutputType},
    },
    text::HTMLEscaper,
};
use clap::Command;
use ebi_optimisation::anyhow::Result;
use inflector::Inflector;
use std::io::Write;

pub fn page_start(f: &mut Vec<u8>) -> Result<()> {
    // #016764, #005958, #014848, #00312F, #001E1E
    writeln!(
        f,
        "<!DOCTYPE html>
        <html>
        <head>
            <title>Ebi - a stochastic process mining tool</title>
            <link rel=\"shortcut icon\" href=\"https://bpm.rwth-aachen.de/favicon.png\">
            <style>
            body {{
                background-color: #001E1E;
                color: #019a94;
            }}
            td {{
                vertical-align: top;
            }}
            a {{
                color: #019a94;
            }}
            table tr:not(:first-child) td {{
                padding-top: 20px;
            }}
            .selectable {{
                display: none;
            }}
            .selectable:target {{
                display: block;
            }}
            .parameter {{
                font-family: monospace;
            }}
            </style>
        </head>
        <body>"
    )?;
    Ok(())
}

fn page_end(f: &mut Vec<u8>) -> Result<()> {
    writeln!(
        f,
        "</body>
    </html>"
    )?;
    Ok(())
}

pub fn documentation_filehandlers() -> Result<EbiOutput> {
    let mut f = vec![];
    page_start(&mut f)?;
    writeln!(f, "file handlers")?;
    page_end(&mut f)?;
    Ok(EbiOutput::String(String::from_utf8(f).unwrap()))
}

pub fn documentation_commands() -> Result<EbiOutput> {
    let mut f = vec![];
    page_start(&mut f)?;
    menu_commands(&mut f)?;
    commands(&mut f)?;
    page_end(&mut f)?;
    Ok(EbiOutput::String(String::from_utf8(f).unwrap()))
}

fn menu_commands(f: &mut Vec<u8>) -> Result<()> {
    writeln!(f, "<div class=\"menu\">")?;
    for path in EBI_COMMANDS.get_command_paths() {
        writeln!(
            f,
            "<a href=\"#{}\">{}</a>",
            EbiCommand::path_to_short_string(&path),
            EbiCommand::path_to_string(&[path.last().unwrap()])
        )?;
    }

    writeln!(f, "</div>")?;
    Ok(())
}

fn commands(f: &mut Vec<u8>) -> Result<()> {
    writeln!(f, "<div class=\"commands\">")?;
    for path in EBI_COMMANDS.get_command_paths() {
        if let EbiCommand::Command {
            name_long,
            explanation_short,
            explanation_long,
            latex_link: _ll,
            cli_command,
            exact_arithmetic,
            input_types: input_typess,
            input_names,
            input_helps,
            output_type,
            ..
        } = path[path.len() - 1]
        {
            writeln!(
                f,
                "<div class=\"selectable command\" id=\"{}\">",
                EbiCommand::path_to_short_string(&path)
            )?;

            writeln!(f, "<h1>{}</h1>", EbiCommand::path_to_string(&path))?;

            //alias
            if let Some(_) = name_long {
                writeln!(
                    f,
                    "<div>Short alias: {}.</div>",
                    EbiCommand::path_to_short_string(&path)
                )?;
            }

            //explanation
            writeln!(
                f,
                "<div>{}</div>",
                if let Some(l) = explanation_long {
                    l
                } else {
                    explanation_short
                }
            )?;

            //parameters
            writeln!(f, "<div>Parameters:<br><table>")?;

            command_standard_parameters(f, input_typess, input_names, input_helps)?;
            command_custom_parameters(f, cli_command)?;
            command_output_type(f, output_type)?;
            command_output(f, output_type)?;
            command_exact_arithmetic(f, *exact_arithmetic)?;

            writeln!(f, "</table></div>")?;

            writeln!(f, "</div>")?;
        }
    }
    writeln!(f, "</div>")?;
    Ok(())
}

fn command_standard_parameters(
    f: &mut Vec<u8>,
    input_types: &'static [&'static [&'static EbiInputType]],
    input_names: &'static [&'static str],
    input_helps: &'static [&'static str],
) -> Result<()> {
    for (input_name, (input_types, input_help)) in input_names
        .iter()
        .zip(input_types.iter().zip(input_helps.iter()))
    {
        writeln!(f, "<tr>")?;
        writeln!(f, "<td>&lt;{}&gt;</td>", input_name.escape_html())?;
        writeln!(f, "<td>")?;
        writeln!(f, "{}.", input_help.to_sentence_case())?;

        //accepted values
        writeln!(
            f,
            "<br>Accepted values:<ul><li>{}</li></ul>",
            EbiInputType::get_possible_inputs_with_html(input_types).join("</li><li>")
        )?;

        //mandatoryness
        if let Some(default) = ebi_input::default(input_types) {
            writeln!(
                f,
                "Mandatory: no. If no value is provided, a default of {} will be used. It can also be provided on STDIN by giving a `-' on the command line.",
                default
            )?;
        } else {
            writeln!(
                f,
                "Mandatory: yes, though it can be given on STDIN by giving a `-' on the command line."
            )?;
        }

        writeln!(f, "</td></tr>")?;
    }
    Ok(())
}

fn command_custom_parameters(
    f: &mut Vec<u8>,
    cli_command: &Option<fn(command: Command) -> Command>,
) -> Result<()> {
    if let Some(fu) = cli_command {
        for arg in (fu)(Command::new("")).get_arguments() {
            writeln!(f, "<tr>")?;

            //name
            writeln!(
                f,
                "<td>{}</td>",
                if let Some(short) = arg.get_short() {
                    if let Some(long) = arg.get_long() {
                        format!(
                            "<span class=\"parameter\">-{}</span> or <span class=\"parameter\">--{}</span>",
                            short, long
                        )
                    } else {
                        format!("<span class=\"parameter\">-{}</span>", short.to_string())
                    }
                } else {
                    if let Some(long) = arg.get_long() {
                        format!("<span class=\"parameter\">--{}</span>", long)
                    } else {
                        format!("&lt;{}&gt;", arg.get_value_names().unwrap()[0])
                    }
                }
            )?;

            writeln!(
                f,
                "<td>{}",
                if let Some(h) = arg.get_long_help() {
                    h.to_string()
                } else {
                    arg.get_help().unwrap().to_string()
                }
            )?;

            if arg.get_short().is_none() && arg.get_long().is_none() {
                //custom argument
                if let Some(default) = arg.get_default_values().iter().next() {
                    writeln!(
                        f,
                        "<br>Mandatory: {}.",
                        if arg.is_required_set() {
                            "yes".to_owned()
                        } else {
                            format!(
                                "no; if not provided, a default value of {} will be used",
                                default.to_string_lossy()
                            )
                        }
                    )?;
                } else {
                    writeln!(
                        f,
                        "<br>Mandatory: {}.",
                        if arg.is_required_set() { "yes" } else { "no" }
                    )?;
                }
            } else {
                writeln!(f, "<br>Mandatory: no.")?;
            }
            writeln!(f, "</td></tr>")?;
        }
    }
    Ok(())
}

fn command_output_type(f: &mut Vec<u8>, output_type: &EbiOutputType) -> Result<()> {
    if output_type.get_exporters().len() > 1 {
        let output_extensions = output_type
            .get_exporters()
            .iter()
            .map(|exporter| {
                format!(
                    "<span class=\"parameter\">{}</span> ({})",
                    exporter.get_extension(),
                    exporter.get_name()
                )
            })
            .collect::<Vec<_>>()
            .join("</li><li>");
        writeln!(
            f,
            "<tr><td><span class=\"parameter\">-t</span> or<br><span class=\"parameter\">--output_type</span> &lt;OUTPUT_TYPE&gt;</td>"
        )?;
        writeln!(
            f,
            "<td>The file format as which Ebi should write the result. Given as the file extension without period. Possible values are: <ul><li>{}</li></ul>",
            output_extensions
        )?;
        writeln!(
            f,
            "Mandatory: no. If no value is provided, the default of <span class=\"parameter\">{}</span> ({}) will be used.",
            output_type.get_default_exporter().get_extension(),
            output_type.get_default_exporter().get_name()
        )?;
        writeln!(f, "</td></tr>")?;
    }
    Ok(())
}

fn command_output(f: &mut Vec<u8>, output_type: &EbiOutputType) -> Result<()> {
    //output
    writeln!(
        f,
        "<tr><td><span class=\"parameter\">-o</span> or<br> <span class=\"parameter\">--output</span> &lt;FILE&gt;</td>"
    )?;
    if output_type.get_exporters().len() == 1 {
        let exporter = output_type.get_exporters().remove(0);
        writeln!(
            f,
            "<td>The {} file to which the result must be written.<br>",
            exporter
        )?;
    } else {
        writeln!(
            f,
            "<td>The file to which the results must be written. Ebi will use the file extension to determine the output file format, which may be <ul><li>{}</li></ul>",
            output_types(output_type)
        )?;
    }
    let default = if output_type.get_exporters().len() > 1 {
        format!(
            "in the file format specified by <span class=\"parameter\">-t</span>, which by default is {} {} (<a href=\"file_handlers.html#{}\">.{}</a>)",
            output_type.get_default_exporter().get_extension(),
            output_type.get_default_exporter().get_name(),
            output_type.get_default_exporter().get_extension(),
            output_type.get_default_exporter().get_extension()
        )
    } else {
        format!(
            "in the file format of {} (<a href=\"file_handlers.html#{}\">.{}</a>)",
            output_type.get_default_exporter().get_name(),
            output_type.get_default_exporter().get_extension(),
            output_type.get_default_exporter().get_extension()
        )
    };
    writeln!(
        f,
        "Mandatory: no. If no value is provided, the result will be written to STDOUT {}.",
        default
    )?;
    writeln!(f, "<td></tr>")?;
    Ok(())
}

pub fn output_types(output_type: &EbiOutputType) -> String {
    let mut list = output_type
        .get_exporters()
        .into_iter()
        .map(|exp| match exp {
            EbiExporter::Object(_, file_handler) => format!(
                "{} (<a href=\"file_handlers.html#{}\">.{}</a>)",
                file_handler.name, file_handler.file_extension, file_handler.file_extension
            ),
            _ => exp.to_string(),
        })
        .collect::<Vec<_>>();
    if list.len() == 1 {
        return list.remove(0);
    }
    list.join("</li><li>")
}

fn command_exact_arithmetic(f: &mut Vec<u8>, exact_arithmetic: bool) -> Result<()> {
    //exact arithmetic flag
    if exact_arithmetic {
        writeln!(
            f,
            "<tr>
                <td><span class=\"parameter\">-a</span> or <span class=\"parameter\">--approximate</span></td>
                <td>Use approximate arithmetic instead of exact arithmetic.
                <br>Mandatory: no. If not provided, exact arithmetic will be used.</td></tr>"
        )?;
    }
    Ok(())
}
