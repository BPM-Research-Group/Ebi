use crate::{
    ebi_framework::{
        ebi_command::{EBI_COMMANDS, EbiCommand, search_command_in_source_files},
        ebi_output::EbiOutput,
    },
    javascript::javascript_generator_html::javascript_function_name,
};
use ebi_objects::anyhow::anyhow;
use ebi_optimisation::anyhow::Result;

pub fn generate_javascript_rust() -> Result<EbiOutput> {
    let mut result = format!(
        "// This file has been automatically generated. Manual changes will be overridden.

use crate::{{
    ebi_framework::ebi_command::EbiCommand, javascript::javascript_link::execute_javascript_command,
}};
use wasm_bindgen::prelude::*;"
    );

    for path in EBI_COMMANDS.get_command_paths() {
        if path.last().unwrap().is_in_javascript() {
            result.push_str("\n\n");
            result.push_str(&ebi_command_to_javascript_function(&path)?);
        }
    }

    Ok(EbiOutput::String(result))
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
pub fn {fun_nam}(string_inputs: Vec<String>) {{
    ebi_objects::ebi_arithmetic::exact::set_exact_globally({exa_ari});
    let command: &&EbiCommand = 
        &&{lib_nam};
    execute_javascript_command(command, string_inputs, \"{fun_nam}\");
}}",
        fun_nam = function_name,
        exa_ari = exact_arithmetic.to_string(),
        lib_nam = search_command_in_source_files(path)?
    );
    Ok(result)
}
