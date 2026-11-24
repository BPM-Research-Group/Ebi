use crate::{
    ebi_framework::{
        ebi_command::{search_command_in_source_files, EbiCommand, EBI_COMMANDS},
        ebi_output::EbiOutput,
    },
    surfaces::javascript::javascript_link::javascript_function_name,
};
use ebi_objects::anyhow::anyhow;
use ebi_optimisation::anyhow::Result;

pub fn generate_javascript_rust() -> Result<EbiOutput> {
    let mut result = String::new();
    for path in EBI_COMMANDS.get_command_paths() {
        if path.last().unwrap().is_in_javascript() {
            result.push_str("\n\n");
            result.push_str(&ebi_command_to_javascript_function(&path)?);
        }
    }

    Ok(EbiOutput::String(format!(
            "// This file has been automatically generated. Manual changes will be overridden.

use crate::{{
    ebi_framework::ebi_command::EbiCommand, surfaces::javascript::javascript_runner::{{execute_javascript_command, JavascriptInput}}
}};
use wasm_bindgen::prelude::*;

    {result}"
        )))
}

pub fn ebi_command_to_javascript_function(path: &Vec<&EbiCommand>) -> Result<String> {
    let function_name = javascript_function_name(path);

    // Extract input_types
    let exact_arithmetic = if let EbiCommand::Command {
        exact_arithmetic, ..
    } = path.last().unwrap()
    {
        *exact_arithmetic
    } else {
        return Err(anyhow!("not a command but a group"));
    };

    //function
    let result = format!(
        "#[wasm_bindgen]
pub fn {fun_nam}(javascript_inputs: Vec<JavascriptInput>, exporter_file_extension: &str) {{
    ebi_objects::ebi_arithmetic::exact::set_exact_globally({exa_ari});
    let command: &&EbiCommand = 
        &&{lib_nam};
    execute_javascript_command(command, javascript_inputs, \"{fun_nam}\", exporter_file_extension);
}}",
        fun_nam = function_name,
        exa_ari = exact_arithmetic.to_string(),
        lib_nam = search_command_in_source_files(path)?
    );
    Ok(result)
}
