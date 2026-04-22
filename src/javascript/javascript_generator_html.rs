use crate::ebi_framework::{
    ebi_command::{EBI_COMMANDS, EbiCommand},
    ebi_input::EbiInputType,
};
use ebi_objects::anyhow::anyhow;
use ebi_optimisation::anyhow::Result;
use itertools::Itertools;
use std::io::Write;

pub fn javascript_function_name(path: &Vec<&EbiCommand>) -> String {
    let raw_name = EbiCommand::path_to_string(path);
    raw_name
        .strip_prefix("Ebi ")
        .unwrap_or(&raw_name)
        .to_lowercase()
        .chars()
        .map(|c| if c == ' ' || c == '-' { '_' } else { c })
        .collect()
}

pub(crate) fn javascript_html_header() -> String {
    let mut result = String::new();
    for path in EBI_COMMANDS.get_command_paths() {
        if let EbiCommand::Command {
            input_types: input_typess,
            ..
        } = path.last().unwrap()
            && path.last().unwrap().is_in_javascript()
        {
            let function_name = javascript_function_name(&path);
            result.push_str(&format!(
                "function {function_name}_changed_text(event) {{
                    document.getElementById(\"{function_name}_output\").parentElement.style.display = \"none\";
                    document.getElementById(\"{function_name}_error\").parentElement.style.display = \"none\";"));

            result.push_str(&format!(
                "

                    file = document.getElementById(\"{function_name}_input_0\").files[0];
                    var fr = new FileReader();
                    fr.onload = function(e) {{
                        console.log(\"file loaded\");
                        window.{function_name}_input_0 = e.target.result;
                        document.getElementById(\"{function_name}_button_run\").disabled = false;
                    }};
                    fr.readAsText(file);
                }}

                function {function_name}_run() {{
                    console.log(\"run Ebi {function_name}\");
                    window.{function_name}([window.{function_name}_input_0])
                }}"
            ));
        }
    }
    result
}

pub(crate) fn javascript_html_form(f: &mut Vec<u8>, path: &Vec<&EbiCommand>) -> Result<()> {
    if let EbiCommand::Command {
        input_types: input_typess,
        input_names,
        input_helps,
        ..
    } = path.last().unwrap()
        && path.last().unwrap().is_in_javascript()
    {
        let function_name = javascript_function_name(path);
        writeln!(f, "<div class=\"online-command\">")?;
        writeln!(f, "<table>")?;

        for (input_i, (input_name, (input_types, input_help))) in input_names
            .iter()
            .zip(input_typess.iter().zip(input_helps.iter()))
            .enumerate()
        {
            writeln!(f, "<tr><td>&lt;{input_name}&gt;</td><td>")?;

            match input_types
                .iter()
                .next()
                .ok_or_else(|| anyhow!("empty input types"))?
            {
                EbiInputType::Trait(_) | EbiInputType::Object(_) | EbiInputType::AnyObject => {
                    //file
                    writeln!(
                        f,
                        "<input type=\"file\" id=\"{function_name}_input_{input_i}\" onchange=\"input_changed_file(event, '{function_name}');\" autocomplete=\"off\"/>"
                    )?;
                }
                EbiInputType::FileHandler => {
                    writeln!(f, "TODO")?;
                }
                EbiInputType::String(None, None) => {
                    //free text
                    writeln!(
                        f,
                        "<input type=\"text\" id=\"{function_name}_input_{input_i}\" onchange=\"input_changed_text(event, '{function_name}');\" autocomplete=\"off\"/>"
                    )?;
                }
                EbiInputType::String(None, Some(default)) => {
                    //free text with default
                    writeln!(
                        f,
                        "<input type=\"text\" id=\"{function_name}_input_{input_i}\" onchange=\"input_changed_text(event, '{function_name}');\" autocomplete=\"off\" value=\"{default}\"/>"
                    )?;
                }
                EbiInputType::String(Some(items), default) => {
                    //list of allowed items
                    writeln!(
                        f,
                        "<select id=\"{function_name}_input_{input_i}\" onchange=\"input_changed_combobox(event, '{function_name}');\">{}</select>",
                        items
                            .iter()
                            .map(|s| {
                                if let Some(def) = default
                                    && def == s
                                {
                                    format!("<option selected=\"selected\">{s}</option>")
                                } else {
                                    format!("<option>{s}</option>")
                                }
                            })
                            .join("")
                    )?;
                }
                EbiInputType::Usize(min, max, default) => {
                    //free number
                    writeln!(
                        f,
                        "<input type=\"number\" id=\"{function_name}_input_{input_i}\" onchange=\"input_changed_number(event, '{function_name}');\" autocomplete=\"off\" {} {} {}/>",
                        if let Some(min) = min {
                            format!("min = \"{min}\"")
                        } else {
                            String::new()
                        },
                        if let Some(max) = max {
                            format!("min = \"{max}\"")
                        } else {
                            String::new()
                        },
                        if let Some(default) = default {
                            format!("min = \"{default}\"")
                        } else {
                            String::new()
                        },
                    )?;
                }
                EbiInputType::Fraction(const_fraction, const_fraction1, const_fraction2) => {
                    writeln!(f, "TODO")?;
                }
            }

            writeln!(f, "</td></tr>")?;
        }

        writeln!(
            f,
            "
            <tr><td></td><td>
                <button id=\"{function_name}_button_run\" onclick=\"{function_name}_run(event);\" 
                    disabled
                    autocomplete = \"off\">
                    Run command
                </button>
            </td></tr>
        </table>
        <div class=\"error\">Error:<br><pre id=\"{function_name}_error\"></pre></div>
        <div class=\"output\">Result:<br><pre id=\"{function_name}_output\"></pre></div>
        </div>"
        )?;
    }
    Ok(())
}
