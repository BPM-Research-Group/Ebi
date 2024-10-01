use std::path::PathBuf;

use jni::JNIEnv;

// These objects are what you should use as arguments to your native
// function. They carry extra lifetime information to prevent them escaping
// this context and getting used after being GC'd.
use jni::objects::{JClass, JObjectArray, JString};

// This is just a pointer. We'll be returning it from our function. We
// can't return one of the objects with lifetime information because the
// lifetime checker won't let us.
use jni::sys::jstring;
use anyhow::{anyhow, Context, Result};

use crate::{math::fraction::FractionNotParsedYet, multiple_reader::MultipleReader};

use super::{ebi_command::{EbiCommand, EBI_COMMANDS}, ebi_file_handler::EbiFileHandler, ebi_input::{self, EbiInput, EbiInputType}, ebi_output::{self}};

// This keeps Rust from "mangling" the name and making it unique for this
// crate.
#[no_mangle]
pub extern "system" fn Java_org_processmining_ebi_plugins_EbiPlugin_call_1ebi<'local>(mut env: JNIEnv<'local>,
// This is the class that owns our static method. It's not going to be used,
// but still must be present to match the expected signature of a static
// native method.
    _class: JClass<'local>, command_name: JString<'local>, output_format: JString<'local>, inputs: JObjectArray<'local>) -> jstring {

    // First, we have to get the string out of Java. Check out the `strings`
    // module for more info on how this works.
    let command_name: String = unsafe { env.get_string_unchecked(&command_name).expect("Could not get java string.").into() };
    let output_format: String = unsafe { env.get_string_unchecked(&output_format).expect("Could not get java string.").into() };
    
    let number_of_inputs: i32 = env.get_array_length(&inputs).expect("Could not get java array length").into();
    let mut inputss = Vec::with_capacity(number_of_inputs.try_into().unwrap());
    for i in 0..number_of_inputs {
        let jstring = env.get_object_array_element(&inputs, i).expect("Could not get Java array element.");
        let jstring = JString::from(jstring);
        let string: String  = unsafe { env.get_string_unchecked(&jstring).expect("Could not read Java string.").into() };
        inputss.push(string);
    }
    
    let output = env.new_string(match handle_prom_request(command_name, output_format, inputss) {
        Ok(result) => result,
        Err(err) => "error ".to_string() + &err.to_string(),
    }).expect("Couldn't create java string!");

    // Finally, extract the raw pointer to return.
    output.into_raw()
}

pub fn handle_prom_request(command_name: String, output_format: String, string_inputs: Vec<String>) -> Result<String> {
    let binding = EBI_COMMANDS.find_command_with_string(&command_name).ok_or(anyhow!("command not found"))?;
    let command = binding.last().ok_or(anyhow!("command not found"))?;

    if let EbiCommand::Command { execute, input_types: input_typess, input_names, output_type, .. } = command {

        //read the inputs
        let mut inputs = vec![];
        for ((input_types, input_name), string_input) in input_typess.iter().zip(input_names.iter()).zip(string_inputs.into_iter()) {

            //read input
            log::info!("Reading {}", input_name);
            let input = attempt_parse(input_types, string_input).with_context(|| format!("Reading parameter {}.", input_name))?;
            inputs.push(input);
        }

        //call the command
        let result = (execute)(inputs, None)?;

        if &&result.get_type() != output_type {
            return Err(anyhow!("Output type {} does not match the declared output of {}.", result.get_type(), output_type))
        }

        //write result to string
        let mut output_path = PathBuf::new();
        output_path.push(output_format);
        let exporter = EbiCommand::select_exporter(output_type, Some(&output_path));
        ebi_output::export_to_string(result, exporter)
    } else {
        Err(anyhow!("Command not found"))
    }
}

/**
 * Attempt to parse an input as any of the given input types. Returns the last error if unsuccessful.
 */
pub fn attempt_parse(input_types: &[&EbiInputType], value: String) -> Result<EbiInput> {
    //an input may be of several types; go through each of them
    let mut error = None;
    let mut reader = MultipleReader::String(value);
    for input_type in input_types.iter() {
        //try to parse the input as this type
        match input_type {
            EbiInputType::Trait(etrait) => {
                //try to parse a trait                
                match ebi_input::read_as_trait(etrait, &mut reader).with_context(|| format!("Parsing as the trait `{}`.", etrait)) {
                    Ok((object, file_handler)) => return Ok(EbiInput::Trait(object, file_handler)),
                    Err(e) => error = Some(e)
                }
            },
            EbiInputType::Object(etype) => {
                //try to parse a specific object
                match ebi_input::read_as_object(etype, &mut reader).with_context(|| format!("Parsing as the object type `{}`.", etype)) {
                    Ok((object, file_handler)) => return Ok(EbiInput::Object(object, file_handler)),
                    Err(e) => error = Some(e)
                }
            },
            EbiInputType::AnyObject => {
                match ebi_input::read_as_any_object(&mut reader).context("Parsing as any object.") {
                    Ok((object, file_handler)) => return Ok(EbiInput::Object(object, file_handler)),
                    Err(e) => error = Some(e)
                }
            },
            EbiInputType::FileHandler => {
                if let MultipleReader::String(string) = &reader {
                    if let Ok(value) = string.parse::<EbiFileHandler>() {
                        return Ok(EbiInput::FileHandler(value));
                    }
                };
            },
            EbiInputType::String => {
                if let MultipleReader::String(string) = reader {
                    return Ok(EbiInput::String(string));
                } else {
                    unreachable!()
                }
            },
            EbiInputType::Usize => {
                if let MultipleReader::String(string) = &reader {
                    if let Ok(value) = string.parse::<usize>() {
                        return Ok(EbiInput::Usize(value.clone()));
                    }
                }
            },
            EbiInputType::Fraction => {
                if let MultipleReader::String(string) = &reader {
                    if let Ok(value) = &string.parse::<FractionNotParsedYet>() {
                        return Ok(EbiInput::Fraction(value.try_into()?));
                    }
                }
            },
        }
    }

    match error {
        Some(e) => Err(e),
        None => Err(anyhow!("argument was not given")),
    }
} 