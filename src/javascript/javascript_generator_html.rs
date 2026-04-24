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
    format!("
            <script type=\"text/javascript\">
            function input_changed_fraction(event, function_name, input_i, number_of_inputs) {{
                value = document.getElementById(function_name + \"_input_\" + input_i).value;
                if (value == \"\") {{
                    input_changed(event, function_name, input_i, number_of_inputs, undefined);
                }} else {{
                    input_changed(event, function_name, input_i, number_of_inputs, value);
                }}
            }}

            function input_changed_number(event, function_name, input_i, number_of_inputs) {{
                value = document.getElementById(function_name + \"_input_\" + input_i).value;
                if (value == \"\") {{
                    input_changed(event, function_name, input_i, number_of_inputs, undefined);
                }} else {{
                    input_changed(event, function_name, input_i, number_of_inputs, value);
                }}
            }}

            function input_changed_text(event, function_name, input_i, number_of_inputs) {{
                value = document.getElementById(function_name + \"_input_\" + input_i).value;
                input_changed(event, function_name, input_i, number_of_inputs, value);
            }}

            function input_changed_file(event, function_name, input_i, number_of_inputs) {{
                //disable run button
                document.getElementById(function_name + \"_button_run\").disabled = true;
                window[function_name + \"_input_\" + input_i] = undefined;

                //read file asynchronously
                file = document.getElementById(function_name + \"_input_\" + input_i).files[0];
                var fr = new FileReader();
                fr.onload = function(e) {{
                    console.log(\"file loaded\");
                    input_changed(event, function_name, input_i, number_of_inputs, e.target.result)
                }};
                fr.readAsText(file);
            }}

            function input_changed(event, function_name, input_i, number_of_inputs, value) {{
                document.getElementById(function_name + \"_output\").parentElement.style.display = \"none\";
                document.getElementById(function_name + \"_error\").parentElement.style.display = \"none\";
                URL.revokeObjectURL(window[function_name + '_output_url']);
                window[function_name + '_input_' + input_i + '_value'] = value;
                for (let i = 0; i < number_of_inputs; i++) {{
                    if (typeof window[function_name + '_input_' + i + '_value'] == \"undefined\" || window[function_name + '_input_' + i + '_value'] == \"\") {{
                        //input not defined, disable submission
                        document.getElementById(function_name + \"_button_run\").disabled = true;
                        return;
                    }}
                }}
                //enable button
                document.getElementById(function_name + \"_button_run\").disabled = false;
            }}
            
            function run_ebi(event, function_name, number_of_inputs) {{
                console.log('run Ebi ' + function_name);
                document.getElementById(function_name + \"_button_run\").disabled = true;
                //gather inputs
                inputs = new Array();
                for (let i = 0; i < number_of_inputs; i++) {{
                    if (typeof window[function_name + '_input_' + i + '_value'] == \"undefined\" || window[function_name + '_input_' + i + '_value'] == \"\") {{
                        //input not defined, disable submission
                        document.getElementById(function_name + \"_button_run\").disabled = true;
                        return;
                    }}
                    inputs.push(window[function_name + '_input_' + i + '_value']);
                }}
                window[function_name](inputs);
            }}
        </script>
        
        <script type=\"module\">
            import init, {{ {function_list} }} from \"./pkg/ebi.js\"; 
            {lifted_functions}

            window.ebi_error = function ebi_error(error, command_name) {{
                console.log(\"Error: \" + error);
                document.getElementById(command_name + \"_error\").innerHTML = error;
                document.getElementById(command_name + \"_error\").parentElement.style.display = \"block\";
            }};

            window.ebi_output = function ebi_output(output, command_name, file_extension) {{
                document.getElementById(command_name + \"_output\").innerHTML = output;
                document.getElementById(command_name + \"_output\").parentElement.style.display = \"block\";
                
                const blob = new Blob([output], {{ type: \"text/plain\" }});
                window[command_name + '_output_url'] = URL.createObjectURL(blob);
                let a = document.getElementById(command_name + \"_download\");
                a.href = window[command_name + '_output_url'];
                a.download = command_name + \".\" + file_extension;
            }};

            window.ebi_log = function ebi_log(message, command_name) {{
                console.log(message);
            }};

            init();
        </script>", 
        function_list = EBI_COMMANDS.get_command_paths()
            .iter()
            .filter_map(|path| 
                    if path.last().unwrap().is_in_javascript() {
                        Some(javascript_function_name(&path)) 
                    } else {
                        None
                    })
            .join(", "),
        lifted_functions = EBI_COMMANDS.get_command_paths()
            .iter()
            .filter_map(|path| 
                    if path.last().unwrap().is_in_javascript() {
                        Some(format!("window.{function_name} = {function_name};", function_name = javascript_function_name(&path))) 
                    } else {
                        None
                    }
                ).join(""))
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
                        "<input type=\"file\" id=\"{function_name}_input_{input_i}\" onchange=\"input_changed_file(event, '{function_name}', {input_i}, {});\" autocomplete=\"off\"/>",
                        input_typess.len()
                    )?;
                }
                EbiInputType::FileHandler => {
                    writeln!(f, "TODO")?;
                }
                EbiInputType::String(None, None) => {
                    //free text
                    writeln!(
                        f,
                        "<input type=\"text\" id=\"{function_name}_input_{input_i}\" oninput=\"input_changed_text(event, '{function_name}', {input_i}, {});\" autocomplete=\"off\"/>",
                        input_typess.len()
                    )?;
                }
                EbiInputType::String(None, Some(default)) => {
                    //free text with default
                    writeln!(
                        f,
                        "<input type=\"text\" id=\"{function_name}_input_{input_i}\" oninput=\"input_changed_text(event, '{function_name}', {input_i}, {});\" autocomplete=\"off\" value=\"{default}\"/>",
                        input_typess.len()
                    )?;
                }
                EbiInputType::String(Some(items), default) => {
                    //list of allowed items
                    writeln!(
                        f,
                        "<select id=\"{function_name}_input_{input_i}\" oninput=\"input_changed_combobox(event, '{function_name}, {input_i}, {}');\">{}</select>",
                        input_typess.len(),
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
                            .join(""),
                    )?;
                }
                EbiInputType::Usize(min, max, default) => {
                    //free number
                    writeln!(
                        f,
                        "<input type=\"number\" id=\"{function_name}_input_{input_i}\" oninput=\"input_changed_number(event, '{function_name}', {input_i}, {});\" autocomplete=\"off\" {} {} {}/>",
                        input_typess.len(),
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
                EbiInputType::Fraction(_, _, def) => {
                    writeln!(
                        f,
                        "<input type=\"text\" id=\"{function_name}_input_{input_i}\" oninput=\"input_changed_fraction(event, '{function_name}', {input_i}, {});\" autocomplete=\"off\" {}>",
                        input_typess.len(),
                        if let Some(default) = def {
                            format!("value = \"{}\"", default)
                        } else {
                            String::new()
                        }
                    )?;
                }
            }

            writeln!(f, "</td></tr>")?;
        }

        writeln!(
            f,
            "
            <tr><td></td><td>
                <button id=\"{function_name}_button_run\" onclick=\"run_ebi(event, '{function_name}', {});\" 
                    disabled
                    autocomplete = \"off\">
                    Run command
                </button>
            </td></tr>
        </table>
        <div class=\"error\">Error:<br><pre id=\"{function_name}_error\"></pre></div>
        <div class=\"output\">Result: (<a href=\"\" id=\"{function_name}_download\">download</a>)<br><pre id=\"{function_name}_output\"></pre></div>
        </div>",
            input_typess.len()
        )?;
    }
    Ok(())
}
