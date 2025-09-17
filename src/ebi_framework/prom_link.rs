use ebi_arithmetic::Fraction;
use ebi_arithmetic::parsing::FractionNotParsedYet;
use ebi_objects::EbiObjectType;
use itertools::Itertools;
use std::collections::HashSet;
use std::fmt::Display;
use std::{io::Write, path::PathBuf};

use jni::JNIEnv;

// These objects are what you should use as arguments to your native
// function. They carry extra lifetime information to prevent them escaping
// this context and getting used after being GC'd.
use jni::objects::{JClass, JObjectArray, JString};

use crate::ebi_framework::ebi_output::EbiOutputType;
// This is just a pointer. We'll be returning it from our function. We
// can't return one of the objects with lifetime information because the
// lifetime checker won't let us.
use crate::multiple_reader::MultipleReader;
use crate::text::JavaEscaper;
use anyhow::{Context, Result, anyhow};
use jni::sys::jstring;
use strum::IntoEnumIterator;

use super::ebi_file_handler::EBI_FILE_HANDLERS;
use super::ebi_output::EbiOutput;
use super::{
    ebi_command::{EBI_COMMANDS, EbiCommand},
    ebi_file_handler::EbiFileHandler,
    ebi_input::{self, EbiInput, EbiInputType},
    ebi_output::{self},
};

//install the necessary toolcahins

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
        let exporter = EbiCommand::select_exporter(output_type, Some(&output_path));
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
                match ebi_input::read_as_trait(etrait, &mut reader)
                    .with_context(|| format!("Parsing as the trait `{}`.", etrait))
                {
                    Ok((object, file_handler)) => return Ok(EbiInput::Trait(object, file_handler)),
                    Err(e) => error = Some(e),
                }
            }
            EbiInputType::Object(etype) => {
                //try to parse a specific object
                match ebi_input::read_as_object(etype, &mut reader)
                    .with_context(|| format!("Parsing as the object type `{}`.", etype))
                {
                    Ok((object, file_handler)) => {
                        return Ok(EbiInput::Object(object, file_handler));
                    }
                    Err(e) => error = Some(e),
                }
            }
            EbiInputType::AnyObject => {
                match ebi_input::read_as_any_object(&mut reader).context("Parsing as any object.") {
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

/**
 * Print the classes that are necessary to call Ebi from Java/ProM.
 */
pub fn print_java_plugins() -> Result<EbiOutput> {
    let mut f = vec![];

    //header with imports
    writeln!(f, "package org.processmining.ebi.plugins;\n")?;
    writeln!(
        f,
        "import org.deckfour.uitopia.api.event.TaskListener.InteractionResult;"
    )?;
    writeln!(
        f,
        "import org.processmining.contexts.uitopia.UIPluginContext;"
    )?;
    writeln!(
        f,
        "import org.processmining.contexts.uitopia.annotations.UITopiaVariant;"
    )?;
    writeln!(f, "import org.processmining.ebi.CallEbi;")?;
    writeln!(
        f,
        "import org.processmining.framework.plugin.PluginContext;"
    )?;
    writeln!(
        f,
        "import org.processmining.framework.plugin.annotations.Plugin;"
    )?;
    writeln!(
        f,
        "import org.processmining.framework.plugin.annotations.PluginCategory;"
    )?;
    writeln!(
        f,
        "import org.processmining.framework.plugin.annotations.PluginLevel;"
    )?;
    writeln!(
        f,
        "import org.processmining.framework.plugin.annotations.PluginVariant;"
    )?;
    writeln!(
        f,
        "import org.processmining.plugins.InductiveMiner.plugins.dialogs.IMMiningDialog;\n"
    )?;
    writeln!(
        f,
        "\n/**\n * This file is automatically generated by Ebi. Do not edit it manually.\n * @author sander\n *\n */"
    )?;
    writeln!(f, "public class EbiPlugins {{\n")?;

    for path in EBI_COMMANDS.get_command_paths() {
        if let Some(EbiCommand::Command {
            explanation_short,
            cli_command,
            input_types: input_typess,
            output_type,
            input_helps,
            ..
        }) = path.last()
        {
            writeln!(
                f,
                "\n\n// == command {} == \n",
                EbiCommand::path_to_string(&path)
            )?;

            if cli_command.is_some() {
                writeln!(
                    f,
                    "\t//command cannot be called from Java as it takes non-standard input from the command line\n"
                )?;
                continue;
            }

            for exporter in output_type.get_exporters() {
                //function name
                let ebi_function_name = EbiCommand::path_to_string(&path).escape_java_code();
                let java_exporter_name = exporter.get_name().to_string().escape_java_code();

                //output
                for output_java_object_handler in exporter.get_java_object_handlers() {
                    if let Some(output_translator) =
                        output_java_object_handler.translator_ebi_to_java
                    {
                        //inputs (create one function for each combination of inputs in the cartesian product)
                        let input_typesss = input_typess
                            .iter()
                            .map(|arr| EbiInputType::get_possible_inputs_with_java(arr))
                            .collect::<Vec<_>>();
                        for inputs_java_object_handler in
                            input_typesss.iter().multi_cartesian_product()
                        {
                            let inputs_java_object_handler_without_gui = inputs_java_object_handler
                                .iter()
                                .filter(|java_object_handler| {
                                    java_object_handler.input_gui.is_none()
                                })
                                .collect::<Vec<_>>();
                            let inputs_java_object_handler_with_gui = inputs_java_object_handler
                                .iter()
                                .enumerate()
                                .filter_map(|(i, java_object_handler)| {
                                    if java_object_handler.input_gui.is_some() {
                                        Some((i, java_object_handler))
                                    } else {
                                        None
                                    }
                                })
                                .collect::<Vec<_>>();
                            // let java_plugin_inputs = inputs_java_object_handler.iter().enumerate().map(|(i, input)| format!(", {} input_{}", input.java_class, i)).join("");
                            let java_plugin_inputs_without_gui =
                                inputs_java_object_handler_without_gui
                                    .iter()
                                    .enumerate()
                                    .map(|(i, input)| format!(", {} input_{}", input.java_class, i))
                                    .join("");
                            let command = EbiCommand::path_to_string(&path);
                            let output_extension = exporter.get_extension();
                            let has_gui = inputs_java_object_handler
                                .iter()
                                .any(|java_object_handler| java_object_handler.input_gui.is_some());
                            let mut parameterlabels = inputs_java_object_handler_without_gui
                                .iter()
                                .map(|java_object_handler| {
                                    java_object_handler.name.escape_java_string()
                                });

                            //create function
                            let java_function_name = format!(
                                "{}__as__{}__to__{}",
                                ebi_function_name,
                                java_exporter_name,
                                output_java_object_handler.name
                            );
                            let java_function_inputs = inputs_java_object_handler
                                .iter()
                                .enumerate()
                                .map(|(i, input)| format!(", {} input_{}", input.java_class, i))
                                .join("");
                            writeln!(
                                f,
                                "\tpublic static {} {}(PluginContext context{}) throws Exception {{",
                                output_java_object_handler.java_class,
                                java_function_name,
                                java_function_inputs
                            )?;

                            //function body
                            let inputs_in_function = inputs_java_object_handler
                                .iter()
                                .enumerate()
                                .map(|(i, input)| {
                                    format!(
                                        "{}(context, input_{})",
                                        input.translator_java_to_ebi.unwrap(),
                                        i
                                    )
                                })
                                .join(", ");
                            writeln!(
                                f,
                                "\t\tString result = CallEbi.call_ebi(\"{}\", \".{}\", new String[] {{{}}});",
                                command, output_extension, inputs_in_function
                            )?;
                            writeln!(f, "\t\treturn {}(context, result);", output_translator)?;
                            writeln!(f, "\t}}\n")?;

                            //create ProM plug-in
                            writeln!(f, "\t@Plugin(")?;
                            let java_plugin_name = format!("prom_{}", java_function_name);
                            let abbreviated_inputs_without_gui =
                                inputs_java_object_handler_without_gui
                                    .iter()
                                    .map(|java_object_handler| java_object_handler.name)
                                    .collect::<Vec<_>>();
                            writeln!(
                                f,
                                "\t\tname = \"{} (input: {}; output: {})\",",
                                explanation_short.escape_java_string(),
                                abbreviated_inputs_without_gui
                                    .join(", ")
                                    .escape_java_string(),
                                output_java_object_handler.name.escape_java_string()
                            )?;
                            writeln!(f, "\t\tlevel = PluginLevel.PeerReviewed, ")?;
                            writeln!(
                                f,
                                "\t\treturnLabels = {{ \"{}\" }}, ",
                                output_java_object_handler.name.escape_java_string()
                            )?;
                            writeln!(
                                f,
                                "\t\treturnTypes = {{ {}.class }},",
                                output_java_object_handler.java_class
                            )?;
                            writeln!(
                                f,
                                "\t\tparameterLabels = {{ \"{}\" }},",
                                parameterlabels.join("\", \"")
                            )?;
                            writeln!(f, "\t\tuserAccessible = true,")?;
                            writeln!(
                                f,
                                "\t\tcategories = {{ PluginCategory.Discovery, PluginCategory.Analytics, PluginCategory.ConformanceChecking }},"
                            )?;
                            writeln!(
                                f,
                                "\t\thelp = \"{} (calls Ebi)\"",
                                path.last().unwrap().explanation_long().escape_java_string()
                            )?;
                            writeln!(f, "\t)")?;
                            writeln!(
                                f,
                                "\t@UITopiaVariant(affiliation = IMMiningDialog.affiliation, author = IMMiningDialog.author, email = IMMiningDialog.email)"
                            )?;
                            writeln!(
                                f,
                                "\t@PluginVariant(variantLabel = \"Call Ebi\", requiredParameterLabels = {{ {} }})",
                                (0..inputs_java_object_handler_without_gui.len()).join(", ")
                            )?;
                            if !has_gui {
                                //non-gui plug-in
                                writeln!(
                                    f,
                                    "\tpublic {} {}(PluginContext context{}) throws Exception {{",
                                    output_java_object_handler.java_class,
                                    java_plugin_name,
                                    java_plugin_inputs_without_gui
                                )?;
                            } else {
                                //gui plug-in
                                writeln!(
                                    f,
                                    "\tpublic {} {}(UIPluginContext context{}) throws Exception {{",
                                    output_java_object_handler.java_class,
                                    java_plugin_name,
                                    java_plugin_inputs_without_gui
                                )?;
                                writeln!(f, "\t\tEbiDialog dialog = new EbiDialog();")?;
                                for (i, java_object_handler) in &inputs_java_object_handler_with_gui
                                {
                                    writeln!(
                                        f,
                                        "\t\tdialog.add_input({}(\"{}\"));",
                                        java_object_handler.input_gui.unwrap(),
                                        input_helps[*i].escape_java_string()
                                    )?;
                                }
                                writeln!(
                                    f,
                                    "\t\tInteractionResult result = context.showWizard(\"{}\", true, true, dialog);\n",
                                    explanation_short.escape_java_string()
                                )?;
                                writeln!(f, "\t\tif (result != InteractionResult.FINISHED) {{")?;
                                writeln!(f, "\t\t\tcontext.getFutureResult(0).cancel(false);")?;
                                writeln!(f, "\t\t\treturn null;")?;
                                writeln!(f, "}}")?;

                                for (j, (i, java_object_handler)) in
                                    inputs_java_object_handler_with_gui.iter().enumerate()
                                {
                                    writeln!(
                                        f,
                                        "\t\t{} input_{} = dialog.get_parameter_{}({});",
                                        java_object_handler.java_class,
                                        i,
                                        java_object_handler
                                            .java_class
                                            .to_string()
                                            .escape_java_code(),
                                        j
                                    )?;
                                }
                            }

                            let inputs_to_call_function = (0..inputs_java_object_handler.len())
                                .map(|i| format!(", input_{}", i))
                                .join("");
                            writeln!(
                                f,
                                "\t\treturn {}(context{});",
                                java_function_name, inputs_to_call_function
                            )?;
                            writeln!(f, "\t}}\n")?;
                        }
                    }
                }
            }
        }
    }

    //It is important that every function declared in Rust is implemented in Java, even if it is not actually used (yet).
    //Therefore, we add a function that is never called but nevertheless has every java_object_handler interface in it.
    //Then, the Java compiler will complain, rather than the error showing during testing or at runtime.
    writeln!(f, "@SuppressWarnings(\"unused\")")?;
    writeln!(
        f,
        "\n\tprivate static void call_every_ebi_to_java_translator(PluginContext context) throws Exception {{"
    )?;
    for file_handler in EBI_FILE_HANDLERS {
        for java_object_handler in file_handler.java_object_handlers {
            java_compiler_trigger(&mut f, java_object_handler)?;
        }
    }
    for input_type in EbiInputType::iter() {
        for java_object_handler in input_type.get_java_object_handlers() {
            java_compiler_trigger(&mut f, java_object_handler)?;
        }
    }
    writeln!(f, "\t}}\n")?;

    writeln!(f, "}}")?;

    Ok(EbiOutput::String(String::from_utf8(f).unwrap()))
}

fn java_compiler_trigger(f: &mut Vec<u8>, java_object_handler: &JavaObjectHandler) -> Result<()> {
    writeln!(f, "\t\t{{")?;

    if let Some(translator_ebi_to_java) = java_object_handler.translator_ebi_to_java {
        writeln!(
            f,
            "\t\t\t{} input = {}(context, \"\");",
            java_object_handler.java_class, translator_ebi_to_java
        )?;
    } else {
        writeln!(f, "\t\t\t{} input = null;", java_object_handler.java_class)?;
    }

    if let Some(translator_java_to_ebi) = java_object_handler.translator_java_to_ebi {
        writeln!(f, "\t\t\t{}(context, input);", translator_java_to_ebi)?;
    }

    Ok(writeln!(f, "\t\t}}")?)
}

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub struct JavaObjectHandler {
    pub name: &'static str, //name to be used in java function names to indicate the Ebi object that is being handled. Must be unique. Must not contain spaces.
    pub java_class: &'static str, //The full path of the java class to/from which the Ebi object is to be translated
    pub translator_ebi_to_java: Option<&'static str>, //Full path of the java function that translates from a String returned by Ebi to the given java class
    pub translator_java_to_ebi: Option<&'static str>, //Full paht of the java function that translates from the java class to a String that can be read by Ebi
    pub input_gui: Option<&'static str>, //If not none, the given function will be called to create a gui for the user to input a value.
}

impl Display for JavaObjectHandler {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "{}", self.name)
    }
}

pub fn get_java_object_handlers_that_can_export(
    object_type: &EbiObjectType,
) -> HashSet<JavaObjectHandler> {
    EbiOutputType::ObjectType(object_type.clone()).get_java_object_handlers_that_can_export()
}

pub fn get_java_object_handlers_that_can_import(
    object_type: &EbiObjectType,
) -> HashSet<JavaObjectHandler> {
    EbiInputType::Object(object_type.clone()).get_java_object_handlers_that_can_import()
}

pub const JAVA_OBJECT_HANDLERS_STRING: &[JavaObjectHandler] = &[JavaObjectHandler {
    name: "string",
    java_class: "String",
    translator_ebi_to_java: Some("org.processmining.ebi.objects.EbiString.fromEbiString"),
    translator_java_to_ebi: Some("org.processmining.ebi.objects.EbiString.toEbiString"),
    input_gui: Some("org.processmining.ebi.objects.EbiString.create_input_panel"),
}];
pub const JAVA_OBJECT_HANDLERS_USIZE: &[JavaObjectHandler] = &[JavaObjectHandler {
    name: "usize",
    java_class: "Integer",
    translator_ebi_to_java: Some("org.processmining.ebi.objects.EbiInteger.fromEbiString"),
    translator_java_to_ebi: Some("org.processmining.ebi.objects.EbiInteger.toEbiString"),
    input_gui: Some("org.processmining.ebi.objects.EbiInteger.create_input_panel"),
}];
pub const JAVA_OBJECT_HANDLERS_FRACTION: &[JavaObjectHandler] = &[JavaObjectHandler {
    name: "fraction",
    java_class: "org.apache.commons.math3.fraction.BigFraction",
    translator_ebi_to_java: Some("org.processmining.ebi.objects.EbiFraction.fromEbiString"),
    translator_java_to_ebi: Some("org.processmining.ebi.objects.EbiFraction.toEbiString"),
    input_gui: Some("org.processmining.ebi.objects.EbiFraction.create_input_panel"),
}];
pub const JAVA_OBJECT_HANDLERS_LOGDIV: &[JavaObjectHandler] = &[JavaObjectHandler {
    name: "logdiv",
    java_class: "org.processmining.ebi.objects.EbiLogDiv",
    translator_ebi_to_java: Some("org.processmining.ebi.objects.EbiLogDiv.fromEbiString"),
    translator_java_to_ebi: None,
    input_gui: None,
}];
pub const JAVA_OBJECT_HANDLERS_CONTAINSROOT: &[JavaObjectHandler] = &[JavaObjectHandler {
    name: "containsroot_html",
    java_class: "org.processmining.framework.util.HTMLToString",
    translator_ebi_to_java: Some("org.processmining.ebi.objects.EbiContainsRoot.fromEbiString"),
    translator_java_to_ebi: None,
    input_gui: None,
}];
pub const JAVA_OBJECT_HANDLERS_ROOTLOGDIV: &[JavaObjectHandler] = &[JavaObjectHandler {
    name: "rootlogdiv",
    java_class: "org.processmining.framework.util.HTMLToString",
    translator_ebi_to_java: Some("org.processmining.ebi.objects.EbiRootLogDiv.fromEbiString"),
    translator_java_to_ebi: None,
    input_gui: None,
}];
pub const JAVA_OBJECT_HANDLERS_BOOL: &[JavaObjectHandler] = &[JavaObjectHandler {
    name: "boolean",
    java_class: "java.lang.Boolean",
    translator_ebi_to_java: Some("org.processmining.ebi.objects.EbiBoolean.fromEbiString"),
    translator_java_to_ebi: Some("org.processmining.ebi.objects.EbiBoolean.toEbiString"),
    input_gui: None,
}];

#[cfg(test)]
mod tests {
    use std::fs;

    use super::{handle_prom_request, print_java_plugins};

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
}
