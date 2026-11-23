use crate::{
    ebi_framework::{
        ebi_command::{EbiCommand, EBI_COMMANDS},
        ebi_output::EbiOutput,
    },
    surfaces::javascript::javascript_link::javascript_function_name,
    tests::{fallible_test::is_fallible, test_input::TestInput},
};
use ebi_objects::anyhow::Result;

pub fn generate_javascript_test() -> Result<EbiOutput> {
    let mut tests = String::new();
    for path in EBI_COMMANDS.get_command_paths() {
        if path.last().unwrap().is_in_javascript() {
            tests += &generate_javascript_rust_tests(path.last().unwrap(), &path)?;
        }
    }

    Ok(EbiOutput::String(format!(
        "// This file has been automatically generated. Manual changes will be overridden.

use crate::{{
    ebi_framework::ebi_command::EbiCommand, surfaces::javascript::javascript_link::{{execute_javascript_command, JavascriptInput}}
}};
use wasm_bindgen::prelude::*;

#[cfg(test)]
mod tests {{
    use crate::surfaces::javascript::javascript_link::JavascriptInput;
    
    {tests}}}
"
    )))
}

fn generate_javascript_rust_tests(command: &EbiCommand, path: &Vec<&EbiCommand>) -> Result<String> {
    if let EbiCommand::Command {
        input_types,
        exact_arithmetic,
        ..
    } = command
        && *exact_arithmetic
    {
        //for each input type, find all input combinations
        let inputss = crate::tests::test_input::TestInput::find_inputs(input_types);
        if inputss.is_empty() && input_types.len() > 0 {
            panic!("Could not find input to call command.");
        }

        let mut result = String::new();
        //apply the command to all input combinations
        for (input_i, inputs) in inputss.into_iter().take(3).enumerate() {
            result += &format!(
                "\t#[test]
\t{fallible}pub fn {function_name}_test_{input_i}() {{
        let inputs = {inputs_string};
        crate::surfaces::javascript::javascript_autogen::{function_name}(inputs, \".xes\");
    }}\n\n",
                fallible = if is_fallible(path, &inputs) {
                    "#[should_panic]\n\t"
                } else {
                    ""
                },
                function_name = javascript_function_name(path),
                inputs_string = input_to_javascript_test(&inputs)
            );
        }

        Ok(result)
    } else {
        Ok(String::new())
    }
}

fn input_to_javascript_test(inputs: &Vec<TestInput>) -> String {
    format!(
        "vec![\n\t\t\t{}\n\t\t]",
        inputs
            .into_iter()
            .map(|x| format!(
                "JavascriptInput::from({})\n\t\t\t// {}\n",
                match x {
                    TestInput::Trait(_, path_buf) | TestInput::Object(_, _, path_buf) => format!(
                        "std::fs::read_to_string(\"{}\").unwrap()",
                        path_buf.to_str().unwrap()
                    ),
                    TestInput::String(s, _) => format!("\"{s}\".to_string()"),
                    TestInput::Usize(u, _) => format!("\"{u}\".to_string()"),
                    TestInput::FileHandler(f) => format!("\"{f}\".to_string()"),
                    TestInput::Fraction(f, _) => format!("\"{f}\".to_string()"),
                    TestInput::StringList(_, _) => unreachable!(
                        "a command with a StringList input is not offered in JavaScript (see is_in_javascript)"
                    ),
                },
                x.to_unique_string()
            ))
            .collect::<Vec<_>>()
            .join("\t\t\t,\n\t\t\t")
    )
}
