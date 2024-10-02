use std::fmt::Display;
use std::{io::Write, path::PathBuf};
use itertools::Itertools;

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

use super::ebi_output::EbiOutput;
use super::{ebi_command::{EbiCommand, EBI_COMMANDS}, ebi_file_handler::EbiFileHandler, ebi_input::{self, EbiInput, EbiInputType}, ebi_output::{self}};

// This keeps Rust from "mangling" the name and making it unique for this
// crate.
#[no_mangle]
pub extern "system" fn Java_org_processmining_ebi_CallEbi_call_1ebi<'local>(mut env: JNIEnv<'local>,
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
        Err(err) => "Ebi: error: ".to_string() + &err.to_string(),
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

/**
 * Print the classes that are necessary to call Ebi from Java/ProM.
 */
pub fn print_classes() -> Result<EbiOutput> {
    let mut f = vec![];

    //header with imports
    writeln!(f, "package org.processmining.ebi.plugins;\n")?;
    writeln!(f, "import org.processmining.ebi.CallEbi;\n")?;
    writeln!(f, "\n/**\n * This file is automatically generated by Ebi. Do not edit it manually.\n * @author sander\n *\n */")?;
    writeln!(f, "public class EbiCommandPlugin {{\n")?;

    for path in EBI_COMMANDS.get_command_paths() {
        if let Some(EbiCommand::Command { name_short, name_long, explanation_short, explanation_long, latex_link, cli_command, exact_arithmetic, input_types: input_typess, input_names, input_helps, execute, output_type }) = path.last() {
            writeln!(f, "\n\n// == command {} == \n", EbiCommand::path_to_string(&path))?;

            if cli_command.is_some() {
                writeln!(f, "\t//command cannot be called from Java as it takes non-standard input from the command line\n")?;
                continue;
            }

            for exporter in  output_type.get_exporters() {

                //function name
                let java_function_name = escape(EbiCommand::path_to_string(&path));
                let java_exporter_name = escape(exporter.get_name().to_string());

                //output
                for output_java_object_handler in exporter.get_java_object_handlers() {

                    //inputs (create one function for each combination of inputs in the cartesian product)
                    let input_typesss = input_typess.iter().map(|arr| EbiInputType::get_possible_inputs_with_java(arr)).collect::<Vec<_>>();
                    for inputs_java_object_handler in input_typesss.iter().multi_cartesian_product()  {

                        let inputs_header = inputs_java_object_handler.iter().enumerate().map(|(i, input)| format!("{} input_{}", input.java_class, i)).join(", ");
                        let inputs = inputs_java_object_handler.iter().enumerate().map(|(i, input)| format!("{}(input_{})", input.translator_java_to_ebi.unwrap(), i)).join(", ");
                        let command = EbiCommand::path_to_string(&path);
                        let output_extension = exporter.get_extension();


                        writeln!(f, "\tpublic static {} {}__as__{}__to__{}({}) {{", output_java_object_handler.java_class, java_function_name, java_exporter_name, output_java_object_handler.name, inputs_header)?;

                        writeln!(f, "\t\tString result = CallEbi.call_ebi(\"{}\", \".{}\", new String[] {{{}}});", command, output_extension, inputs)?;
                        writeln!(f, "\t\treturn {}(result);", output_java_object_handler.translator_ebi_to_java)?;

                        writeln!(f, "\t}}\n")?;
                    }
                }
            }
        }
    }

    writeln!(f, "}}")?;

    Ok(EbiOutput::String(String::from_utf8(f).unwrap()))
}

pub fn escape(str: String) -> String {
    str.replace(' ', "_").replace("-", "_")
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct JavaObjectHandler {
    pub name: &'static str,
    pub translator_ebi_to_java: &'static str,
    pub translator_java_to_ebi: Option<&'static str>,
    pub java_class: &'static str,
}

impl Display for JavaObjectHandler {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "{}", self.name)
    }
}

pub const JAVA_OBJECT_HANDLERS_STRING: &[JavaObjectHandler] = &[
    JavaObjectHandler{ 
        name: "string_html", 
        translator_ebi_to_java: "org.processmining.ebi.objects.EbiString.fromEbiString",
        translator_java_to_ebi: Some("org.processmining.ebi.objects.EbiString.toEbiString"),
        java_class: "String" 
    },
];
pub const JAVA_OBJECT_HANDLERS_USIZE: &[JavaObjectHandler] = &[
    JavaObjectHandler{ 
        name: "integer", 
        translator_ebi_to_java: "org.processmining.ebi.objects.EbiInteger.fromEbiString", 
        translator_java_to_ebi: Some("org.processmining.ebi.objects.EbiInteger.toEbiString"),
        java_class: "int" 
    },
];
pub const JAVA_OBJECT_HANDLERS_FRACTION: &[JavaObjectHandler] = &[
    JavaObjectHandler{ 
        name: "fraction_html", 
        translator_ebi_to_java: "org.processmining.ebi.objects.EbiFraction.fromEbiString", 
        translator_java_to_ebi: Some("org.processmining.ebi.objects.EbiFraction.toEbiString"), 
        java_class: "org.processmining.framework.util.HTMLToString" 
    },
];
pub const JAVA_OBJECT_HANDLERS_SVG: &[JavaObjectHandler] = &[
    JavaObjectHandler{ 
        name: "svg", 
        translator_ebi_to_java: "org.processmining.ebi.objects.EbiSvg.fromEbiString",
        translator_java_to_ebi: None,
        java_class: "com.kitfox.svg.SVGDiagram" 
    },
];
pub const JAVA_OBJECT_HANDLERS_LOGDIV: &[JavaObjectHandler] = &[
    JavaObjectHandler{ 
        name: "logdiv_html", 
        translator_ebi_to_java: "org.processmining.ebi.objects.EbiLogDiv.fromEbiString", 
        translator_java_to_ebi: Some("org.processmining.ebi.objects.EbiLogDiv.toEbiString"), 
        java_class: "org.processmining.framework.util.HTMLToString" 
    },
];
pub const JAVA_OBJECT_HANDLERS_CONTAINSROOT: &[JavaObjectHandler] = &[
    JavaObjectHandler{ 
        name: "logdiv_html", 
        translator_ebi_to_java: "org.processmining.ebi.objects.EbiContainsRoot.fromEbiString",
        translator_java_to_ebi: Some("org.processmining.ebi.objects.EbiContainsRoot.toEbiString"),
        java_class: "org.processmining.framework.util.HTMLToString" 
    },
];
pub const JAVA_OBJECT_HANDLERS_ROOTLOGDIV: &[JavaObjectHandler] = &[
    JavaObjectHandler{ 
        name: "logdiv_html",
        translator_ebi_to_java: "org.processmining.ebi.objects.EbiRootLogDiv.fromEbiString", 
        translator_java_to_ebi: Some("org.processmining.ebi.objects.EbiRootLogDiv.toEbiString"), 
        java_class: "org.processmining.framework.util.HTMLToString" 
    },
];