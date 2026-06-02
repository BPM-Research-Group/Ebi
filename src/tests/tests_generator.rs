use crate::{
    ebi_framework::ebi_command::{EBI_COMMANDS, EbiCommand, search_command_in_source_files},
    ebi_objects::anyhow::{Result, anyhow},
    tests::{fallible_test::is_fallible, test_input::TestInput},
};
use itertools::Itertools;

pub const NUMBER_OF_TESTS_PER_COMMAND: usize = 14;

pub(crate) fn generate_tests() -> Result<String> {
    let path = EBI_COMMANDS;
    Ok(format!(
        "// This file has been automatically generated. Manual changes will be overridden.

mod tests{{
\tuse crate::ebi_framework::{{
\t\tebi_command::EbiCommand,
\t\tebi_input::{{self, EbiInput, read_as_object_with_file_handler, TEST_INPUT_TYPE_STRING, TEST_INPUT_TYPE_USIZE, TEST_INPUT_TYPE_FRACTION}}
\t}};
\tuse ebi_objects::ebi_activity_key::TestActivityKey;
\tuse crate::multiple_reader::MultipleReader;
\tuse ntest::timeout;
\tuse std::fs::File;

{result}}}",
        result = generate_tests_for_command(&EBI_COMMANDS, &vec![&path])?
    ))
}

pub(crate) fn generate_tests_for_command(
    command: &EbiCommand,
    path: &Vec<&EbiCommand>,
) -> Result<String> {
    if let EbiCommand::Group {
        name_short,
        children,
        ..
    } = command
    {
        let mut result = format!("\n\t// ==== group {name_short} ====\n");
        for child in children.iter() {
            let mut child_path = path.clone();
            child_path.push(child);

            result = format!(
                "{result}\n{}",
                generate_tests_for_command(child, &child_path)?
            );
        }
        return Ok(result);
    }

    if let EbiCommand::Command {
        name_short,
        input_types,
        exact_arithmetic,
        cli_command,
        ..
    } = command
        && cli_command.is_none()
        && input_types.len() > 0
    {
        //for each input type, find all input combinations
        let inputss = crate::tests::test_input::TestInput::find_inputs(input_types);
        if inputss.is_empty() && input_types.len() > 0 {
            return Err(anyhow!(
                "Could not find input to call command {}.",
                EbiCommand::path_to_short_string(path)
            ));
        }

        let mut result = format!("\n\t// ==== command {name_short} ====\n");
        //apply the command to all input combinations
        for (input_i, inputs) in inputss
            .into_iter()
            .take(NUMBER_OF_TESTS_PER_COMMAND)
            .enumerate()
        {
            let fallible = is_fallible(path, &inputs);

            let exact_code = if !exact_arithmetic {
                format!(
                    "if ebi_objects::ebi_arithmetic::is_exact_globally() {{
\t\t\treturn;
\t\t}}"
                )
            } else {
                String::new()
            };

            let function_name = EbiCommand::path_to_short_string(path)
                .replace(" ", "_")
                .replace("-", "_")
                .to_ascii_lowercase();
            let inputs_code = test_inputs_to_import_code(&inputs);
            let execution_code = search_command_in_source_files(path)?;
            let (_, execution_code_short) = execution_code.rsplit_once(":").unwrap();
            let unique_input_strings = test_inputs_to_fallible_list_code(&inputs);

            let fallible_hint = if !fallible {
                format!("// to indicate that this test is expected to fail, add the following to src/tests/fallible_test_list.rs:
\t\t/* 
\t\t(
\t\t\t&{execution_code_short},
\t\t\t&[
\t\t\t\t{unique_input_strings}			
\t\t\t]
\t\t),
\t\t*/")
            } else {
                format!("// this test has been indicated as to be expected to fail")
            };

            let match_code = if !fallible {
                format!(
                    "if let EbiCommand::Command{{execute, output_type, ..}} = {execution_code} {{
\t\t\tmatch (execute)(inputs, None) {{
\t\t\t\tOk(output) => {{
\t\t\t\t\toutput.test_activity_key();
\t\t\t\t\tassert_eq!(&output.get_type(), output_type);
\t\t\t\t}}
\t\t\t\tErr(e) => Err(e).unwrap(),
\t\t\t}}
\t\t}}"
                )
            } else {
                format!(
                    "if let EbiCommand::Command{{execute, ..}} = {execution_code} {{
\t\t\tassert!(((execute)(inputs, None)).is_err())
\t\t}}"
                )
            };

            result += &format!(
                "\t#[test]
\t#[timeout(10000)]
\tpub fn {function_name}_test_{input_i}() {{
\t\t{fallible_hint}
\t\t{exact_code}
\t\t{inputs_code}
\t\t{match_code}
\t}}\n",
            );
        }

        Ok(result)
    } else {
        Ok(String::new())
    }
}

fn test_inputs_to_import_code(test_inputs: &Vec<TestInput>) -> String {
    format!("{}\n\t\tlet inputs = vec![{}];\n",
    test_inputs
        .into_iter()
        .enumerate()
        .map(|(input_i, x)| match x {
            TestInput::Trait(ebi_trait, path_buf) => format!(
                "let mut reader = MultipleReader::from_file(File::open(\"{}\").unwrap());
\t\tlet (object{input_i}, file_handler{input_i}) = ebi_input::read_as_trait(&(\"{ebi_trait}\".parse().unwrap()), &mut reader, None, 0).unwrap();
\t\tlet input{input_i} = EbiInput::Trait(object{input_i}, file_handler{input_i});",
                path_buf.to_str().unwrap()
            ),
            TestInput::Object(ebi_object, file_handler, path_buf) => {
                format!("let mut reader = MultipleReader::from_file(File::open(\"{}\").unwrap());
\t\tlet object{input_i} = read_as_object_with_file_handler(&\"{ebi_object_type}\".parse().unwrap(), &mut reader, None, 0, &mut None, \"{file_handler_name}\".parse().unwrap()).unwrap();
\t\tlet input{input_i} = EbiInput::Object(object{input_i}, \"{file_handler_name}\".parse().unwrap());",
                path_buf.to_str().unwrap(),
                ebi_object_type = ebi_object.get_type(),
                file_handler_name = file_handler.name
            )},
            TestInput::String(s, _) => format!(
                "let input{input_i} = EbiInput::String(\"{s}\".to_string(), &TEST_INPUT_TYPE_STRING);"
            ),
            TestInput::Usize(u, _) => format!(
                "let input{input_i} = EbiInput::Usize({u}, &TEST_INPUT_TYPE_USIZE);",
            ),
            TestInput::FileHandler(f) => format!("let input{input_i} = \"{f}\".to_string();"),
            TestInput::Fraction(f, _) => format!("let input{input_i} = EbiInput::Fraction(\"{f}\".parse().unwrap(), &TEST_INPUT_TYPE_FRACTION);"),
        })
        .collect::<Vec<_>>()
        .join("\n\t\t"),
        (0..test_inputs.len()).map(|i| format!("input{i}")).join(", ")
    )
}

fn test_inputs_to_fallible_list_code(test_inputs: &Vec<TestInput>) -> String {
    test_inputs
        .iter()
        .map(|test_input| format!("\"{}\"", test_input.to_unique_string()))
        .join(",\n\t\t\t\t")
}
