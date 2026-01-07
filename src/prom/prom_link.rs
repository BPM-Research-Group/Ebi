use crate::{
    ebi_framework::{
        ebi_command::{EBI_COMMANDS, EbiCommand},
        ebi_file_handler::EbiFileHandler,
        ebi_input::{self, EbiInput, EbiInputType},
        ebi_output::{self},
    },
    multiple_reader::MultipleReader,
};
use anyhow::{Context, Result, anyhow};
use ebi_objects::ebi_arithmetic::{Fraction, parsing::FractionNotParsedYet};
use jni::{
    JNIEnv,
    objects::{JClass, JObjectArray, JString},
    sys::jstring,
};
use std::path::PathBuf;

//command line to push Ebi to the Java project:
// cargo build --release ; cp target/release/libebi.so ~/eclipse-workspace/Ebi/ ; cargo run itself java -o ~/eclipse-workspace/Ebi/src/org/processmining/ebi/plugins/EbiPlugins.java

// This keeps Rust from "mangling" the name and making it unique for this crate.
#[unsafe(no_mangle)]
pub extern "system" fn Java_org_processmining_ebi_CallEbi_call_1ebi_1internal<'local>(
    mut env: JNIEnv<'local>,
    // This is the class that owns our static method. It's not going to be used,
    // but still must be present to match the expected signature of a static
    // native method.
    _class: JClass<'local>,
    command_name: JString<'local>,
    output_format: JString<'local>,
    inputs: JObjectArray<'local>,
) -> jstring {
    // First, we have to get the string out of Java. Check out the `strings`
    // module for more info on how this works.
    let command_name: String = unsafe {
        env.get_string_unchecked(&command_name)
            .expect("Could not get java string.")
            .into()
    };
    let output_format: String = unsafe {
        env.get_string_unchecked(&output_format)
            .expect("Could not get java string.")
            .into()
    };

    let number_of_inputs: i32 = env
        .get_array_length(&inputs)
        .expect("Could not get java array length")
        .into();
    let mut inputss = Vec::with_capacity(number_of_inputs.try_into().unwrap());
    for i in 0..number_of_inputs {
        let jstring = env
            .get_object_array_element(&inputs, i)
            .expect("Could not get Java array element.");
        let jstring = JString::from(jstring);
        let string: String = unsafe {
            env.get_string_unchecked(&jstring)
                .expect("Could not read Java string.")
                .into()
        };
        println!("=== Ebi received\n{}", string);
        inputss.push(string);
    }

    let output_ebi = match handle_prom_request(command_name, output_format, inputss) {
        Ok(result) => result,
        Err(err) => "Ebi: error: ".to_string() + err.to_string().as_str(),
    };
    println!("== Ebi sends\n{}", output_ebi);
    let output = env
        .new_string(output_ebi)
        .expect("Couldn't create java string!");

    // Finally, extract the raw pointer to return.
    output.into_raw()
}

pub fn handle_prom_request(
    command_name: String,
    output_format: String,
    string_inputs: Vec<String>,
) -> Result<String> {
    let binding = EBI_COMMANDS
        .find_command_with_string(&command_name)
        .ok_or(anyhow!("command not found"))?;
    let command = binding.last().ok_or(anyhow!("command not found"))?;

    if let EbiCommand::Command {
        execute,
        input_types: input_typess,
        input_names,
        output_type,
        ..
    } = command
    {
        //read the inputs
        let mut inputs = vec![];
        for ((input_types, input_name), string_input) in input_typess
            .iter()
            .zip(input_names.iter())
            .zip(string_inputs.into_iter())
        {
            //read input
            let input = attempt_parse(input_types, string_input)
                .with_context(|| format!("Reading parameter {}.", input_name))?;
            inputs.push(input);
        }

        //call the command
        let result = (execute)(inputs, None)?;

        if &&result.get_type() != output_type {
            return Err(anyhow!(
                "Output type {} does not match the declared output of {}.",
                result.get_type(),
                output_type
            ));
        }

        //write result to string
        let mut output_path = PathBuf::new();
        output_path.push(output_format);
        let exporter = EbiCommand::select_exporter(output_type, Some(&output_path), None)?;
        ebi_output::export_to_string(result, exporter)
    } else {
        Err(anyhow!("Command not found"))
    }
}

/**
 * Attempt to parse an input as any of the given input types. Returns the last error if unsuccessful.
 */
pub fn attempt_parse(input_types: &[&'static EbiInputType], value: String) -> Result<EbiInput> {
    //an input may be of several types; go through each of them
    let mut error = None;
    let mut reader = MultipleReader::String(value);
    for input_type in input_types.iter() {
        //try to parse the input as this type
        match input_type {
            EbiInputType::Trait(etrait) => {
                //try to parse a trait
                match ebi_input::read_as_trait(etrait, &mut reader, None, 0)
                    .with_context(|| format!("Parsing as the trait `{}`.", etrait))
                {
                    Ok((object, file_handler)) => return Ok(EbiInput::Trait(object, file_handler)),
                    Err(e) => error = Some(e),
                }
            }
            EbiInputType::Object(etype) => {
                //try to parse a specific object
                match ebi_input::read_as_object(etype, &mut reader, None, 0)
                    .with_context(|| format!("Parsing as the object type `{}`.", etype))
                {
                    Ok((object, file_handler)) => {
                        return Ok(EbiInput::Object(object, file_handler));
                    }
                    Err(e) => error = Some(e),
                }
            }
            EbiInputType::AnyObject => {
                match ebi_input::read_as_any_object(&mut reader, None, 0)
                    .context("Parsing as any object.")
                {
                    Ok((object, file_handler)) => {
                        return Ok(EbiInput::Object(object, file_handler));
                    }
                    Err(e) => error = Some(e),
                }
            }
            EbiInputType::FileHandler => {
                if let MultipleReader::String(string) = &reader {
                    if let Ok(value) = string.parse::<EbiFileHandler>() {
                        return Ok(EbiInput::FileHandler(value));
                    }
                };
            }
            EbiInputType::String(None, _) => {
                if let MultipleReader::String(string) = &reader {
                    return Ok(EbiInput::String(string.clone(), &input_type));
                } else {
                    unreachable!()
                }
            }
            EbiInputType::String(Some(allowed_values), _) => {
                if let MultipleReader::String(string) = &reader {
                    if allowed_values.contains(&string.as_str()) {
                        return Ok(EbiInput::String(string.clone(), &input_type));
                    } else {
                        error = Some(anyhow!("value should be one of {:?}", allowed_values));
                    }
                } else {
                    unreachable!()
                }
            }
            EbiInputType::Usize(min, max, _) => {
                if let MultipleReader::String(string) = &reader {
                    if let Ok(value) = string.parse::<usize>() {
                        match (min, max) {
                            (None, None) => {
                                return Ok(EbiInput::Usize(value.clone(), &input_type));
                            }
                            (None, Some(max)) => {
                                if max < &value {
                                    error = Some(anyhow!("value should be at most {}", max));
                                } else {
                                    return Ok(EbiInput::Usize(value.clone(), &input_type));
                                }
                            }
                            (Some(min), None) => {
                                if min > &value {
                                    error = Some(anyhow!("value should be at least {}", min));
                                } else {
                                    return Ok(EbiInput::Usize(value.clone(), &input_type));
                                }
                            }
                            (Some(min), Some(max)) => {
                                if min > &value || max < &value {
                                    error = Some(anyhow!(
                                        "value should be between {} and {}",
                                        min,
                                        max
                                    ));
                                } else {
                                    return Ok(EbiInput::Usize(value.clone(), &input_type));
                                }
                            }
                        }
                    }
                }
            }
            EbiInputType::Fraction(min, max, _) => {
                if let MultipleReader::String(string) = &reader {
                    if let Ok(value) = &string.parse::<FractionNotParsedYet>() {
                        let value: Fraction = value.try_into()?;

                        match (min, max) {
                            (None, None) => {
                                return Ok(EbiInput::Fraction(value.clone(), &input_type));
                            }
                            (None, Some(max)) => {
                                if max < &value {
                                    error = Some(anyhow!("value should be at most {}", max));
                                } else {
                                    return Ok(EbiInput::Fraction(value.clone(), &input_type));
                                }
                            }
                            (Some(min), None) => {
                                if min > &value {
                                    error = Some(anyhow!("value should be at least {}", min));
                                } else {
                                    return Ok(EbiInput::Fraction(value.clone(), &input_type));
                                }
                            }
                            (Some(min), Some(max)) => {
                                if min > &value || max < &value {
                                    error = Some(anyhow!(
                                        "value should be between {} and {}",
                                        min,
                                        max
                                    ));
                                } else {
                                    return Ok(EbiInput::Fraction(value.clone(), &input_type));
                                }
                            }
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

#[cfg(test)]
mod tests {
    use super::handle_prom_request;
    use crate::{
        ebi_framework::{
            ebi_file_handler::get_file_handlers,
            ebi_input::{EbiInput, TEST_INPUT_TYPE_STRING},
            ebi_trait::FromEbiTraitObject,
        },
        prom::{
            java_object_handler::{JavaObjectHandlerQueryExport, JavaObjectHandlerQueryImport},
            prom_plugin_generator::print_java_plugins,
        },
    };
    use ebi_objects::EbiObjectType;
    use std::fs;
    use strum::IntoEnumIterator;

    #[test]
    fn java() {
        let fin = fs::read_to_string("testfiles/a-b.xes").unwrap();
        assert!(
            handle_prom_request(
                "Ebi visualise text".to_string(),
                "txt".to_string(),
                vec![fin]
            )
            .is_ok()
        );

        let _ = print_java_plugins();
    }

    #[test]
    fn object_types() {
        for object_type in EbiObjectType::iter() {
            object_type.get_article();
            get_file_handlers(&object_type);
            object_type.get_java_object_handlers_that_can_export();
            object_type.get_java_object_handlers_that_can_import();
        }

        let _ =
            String::from_trait_object(EbiInput::String("xyz".to_string(), &TEST_INPUT_TYPE_STRING));
    }
}
