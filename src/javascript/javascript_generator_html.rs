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

            function input_changed_combobox(event, function_name, input_i, number_of_inputs) {{
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
                window[function_name + '_input_' + input_i + '_value'] = value;
                inputs_complete(function_name, number_of_inputs);
            }}

            function inputs_complete(function_name, number_of_inputs) {{
                document.getElementById(function_name + \"_output\").parentElement.style.display = \"none\";
                document.getElementById(function_name + \"_error\").parentElement.style.display = \"none\";
                URL.revokeObjectURL(window[function_name + '_output_url']);
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

            function output_type_changed(event, function_name, number_of_inputs) {{
                inputs_complete(function_name, number_of_inputs);
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
                //gather output
                let output = document.getElementById(function_name + \"_output_type\").value;
                window[function_name](inputs, output);
            }}
        </script>
        
        <script type=\"module\">
            import init, {{ {function_list} }} from \"./javascript/ebi.js\"; 
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
        output_type,
        ..
    } = path.last().unwrap()
        && path.last().unwrap().is_in_javascript()
    {
        let mut default_init = String::new();
        let function_name = javascript_function_name(path);
        writeln!(f, "<div class=\"online-command\">")?;
        writeln!(f, "<table>")?;
        let number_of_inputs = input_names.len();

        for (input_i, (input_name, input_types)) in input_names
            .iter()
            .zip(input_typess.iter())
            .enumerate()
        {
            writeln!(f, "<tr><td>&lt;{input_name}&gt;</td><td>")?;

            let input_type = input_types
                .iter()
                .next()
                .ok_or_else(|| anyhow!("empty input types"))?;
            match input_type
            {
                EbiInputType::Trait(_) | EbiInputType::Object(_) | EbiInputType::AnyObject => {
                    //file
                    writeln!(
                        f,
                        "<input 
                            type=\"file\" 
                            id=\"{function_name}_input_{input_i}\" 
                            onchange=\"input_changed_file(event, '{function_name}', {input_i}, {number_of_inputs});\" 
                            autocomplete=\"off\"
                            accept=\"{accept}\"/>
                        </td><td>{inputs_explanation}",
                        inputs_explanation = EbiInputType::get_possible_inputs_with_html_short(input_types).join(", "),
                        accept = EbiInputType::get_possible_input_extensions(input_types).into_iter().map(|s| format!(".{s}")).join(",")
                    )?;
                }
                EbiInputType::FileHandler => {
                    writeln!(f, "TODO")?;
                }
                EbiInputType::String(None, default) => {
                    //free text
                    writeln!(
                        f,
                        "<input 
                            type=\"text\" 
                            id=\"{function_name}_input_{input_i}\" 
                            oninput=\"input_changed_text(event, '{function_name}', {input_i}, {number_of_inputs});\" 
                            {}
                            autocomplete=\"off\"/>",
                        if let Some(default) = default {
                            default_init.push_str(&format!("<script type=\"text/javascript\">input_changed_text(event, '{function_name}', {input_i}, {number_of_inputs});</script>"));
                            format!("value=\"{default}\"")
                        } else {
                            String::new()
                        }
                    )?;
                }
                EbiInputType::String(Some(items), default) => {
                    //list of allowed items
                    writeln!(
                        f,
                        "<select 
                            id=\"{function_name}_input_{input_i}\" 
                            oninput=\"input_changed_combobox(event, '{function_name}', '{input_i}', '{}');\">
                            {}
                        </select>",
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
                    default_init.push_str(&format!("<script type=\"text/javascript\">input_changed_combobox(event, '{function_name}', '{input_i}', '{}');</script>", input_typess.len()));
                }
                EbiInputType::Usize(min, max, default) => {
                    //free number
                    let mut has_default = false;
                    writeln!(
                        f,
                        "<input 
                            type=\"number\" 
                            id=\"{function_name}_input_{input_i}\" 
                            oninput=\"input_changed_number(event, '{function_name}', {input_i}, {number_of_inputs});\" 
                            autocomplete=\"off\" 
                            {min} 
                            {max} 
                            {default} 
                            placeholder=\"integer\"/></td><td>{input_type}",
                        min = if let Some(min) = min {
                            format!("min = \"{min}\"")
                        } else {
                            String::new()
                        },
                        max = if let Some(max) = max {
                            format!("max = \"{max}\"")
                        } else {
                            String::new()
                        },
                        default = if let Some(default) = default {
                            has_default = true;
                            format!("value = \"{default}\"")
                        } else if let Some(min) = min {
                            has_default = true;
                            format!("value = \"{min}\"")
                        } else if let Some(max) = max {
                            has_default = true;
                            format!("value = \"{max}\"")
                        } else {
                            String::new()
                        },
                    )?;
                    if has_default {
                        default_init.push_str(&format!("<script type=\"text/javascript\">input_changed_number(event, '{function_name}', {input_i}, {});</script>", input_typess.len()));
                    }
                }
                EbiInputType::Fraction(_, _, def) => {
                    let mut has_default = false;
                    writeln!(
                        f,
                        "<input 
                            type=\"text\" 
                            id=\"{function_name}_input_{input_i}\" 
                            oninput=\"input_changed_fraction(event, '{function_name}', {input_i}, {});\" 
                            autocomplete=\"off\" 
                            {} 
                            placeholder=\"a/b or float\">
                        </td><td>float or {input_type}.",
                        input_typess.len(),
                        if let Some(default) = def {
                            has_default = true;
                            format!("value = \"{}\"", default)
                        } else {
                            String::new()
                        },
                    )?;
                    if has_default {
                        default_init.push_str(&format!("<script type=\"text/javascript\">input_changed_fraction(event, '{function_name}', {input_i}, {});</script>", input_typess.len()));
                    }
                }
            }

            writeln!(f, "</td></tr>")?;
        }

        //output type selector
        let default_exporter = output_type.get_default_exporter();
        if output_type.get_exporters().len() > 1 {
            writeln!(f, "<tr><td>&lt;OUTPUT&gt;</td><td colspan=\"2\"><select id=\"{function_name}_output_type\" onchange=\"output_type_changed(event, '{function_name}', {number_of_inputs})\">{outputs}</select></td></tr>",
                number_of_inputs = input_names.len(),
                outputs = output_type.get_exporters().iter().map(|exporter| {
                    if exporter == &default_exporter {
                        format!("<option value={} selected=\"selected\">{exporter}</option>", exporter.get_extension())
                    } else {
                        format!("<option value={}>{exporter}</option>", exporter.get_extension())
                    }
                }).join("")
            )?;
        } else {
            writeln!(f, "<input type=\"hidden\" id=\"{function_name}_output_type\" value=\"{extension}\"/>",
                extension = output_type.get_default_exporter().get_extension()
            )?;
        }

        //submit button
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
        {default_init}
        </div>",
            input_typess.len()
        )?;
    }
    Ok(())
}
