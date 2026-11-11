use crate::{
    ebi_framework::{
        ebi_command::{search_command_in_source_files, EbiCommand, EBI_COMMANDS},
        ebi_output::EbiOutput,
    },
    surfaces::java::java_link::java_function_name,
};
use ebi_objects::anyhow::Result;
use itertools::Itertools;

pub const JAVA_PATH: &str = "Ebi";

pub fn generate_java_module_rust() -> Result<EbiOutput> {
    let imports = format!(
        "#![allow(unsafe_op_in_unsafe_fn)]
#![allow(unused_variables)]

// This file has been automatically generated. Manual changes will be overridden.

use crate::{{ebi_framework::ebi_command::EbiCommand, surfaces::java::{{java_export::ExportableToJava, java_link::import_or_load}}}};
use ebi_objects::anyhow::{{anyhow, Context}};
use jni::{{
    EnvUnowned, jni_mangle, objects::{{JClass, JString}},
}};"
    );
    let mut functions = String::new();

    for path in EBI_COMMANDS.get_command_paths() {
        if path.last().unwrap().is_in_java() {
            let body = ebi_command_to_java_function(&path)?;
            functions.push_str(&body);
            functions.push_str("\n");
        }
    }

    let result = format!("{}\n\n{}", imports, functions);
    Ok(EbiOutput::String(result))
}

/// return the tupe (string1, string2) where
/// string1 - function name
/// string2 - function body
fn ebi_command_to_java_function(path: &Vec<&EbiCommand>) -> Result<String> {
    // Derive raw name and fn name
    let fn_name = java_function_name(path);

    let library_name = search_command_in_source_files(path)?;

    // Extract input_types
    let (input_types, exact_arithmetic) = if let EbiCommand::Command {
        input_types,
        exact_arithmetic,
        ..
    } = path[path.len() - 1]
    {
        (input_types, *exact_arithmetic)
    } else {
        return Ok(String::new());
    };

    let args = (0..input_types.len())
        .map(|i| format!("arg{}: JString<'caller>", i))
        .collect::<Vec<_>>()
        .join(", ");

    let exact = if exact_arithmetic {
        "ebi_objects::ebi_arithmetic::exact::set_exact_globally(true);"
    } else {
        "ebi_objects::ebi_arithmetic::exact::set_exact_globally(false);"
    };

    let inputs = (0..input_types.len())
        .map(|idx| {
            format!(
                r###"let input{idx} = match import_or_load(arg{idx}, input_types[{idx}], {idx}).with_context(|| anyhow!("Could not import argument {idx}")) {{
                    Ok(x) => x,
                    Err(err) => {{
                        env.throw(err.to_string().as_str())?;
                        return Ok(JString::null())
                    }}
                }};
    "###
            )
        })
        .join("\n");

    let inputs_list = (0..input_types.len())
        .map(|i| format!("input{}", i))
        .collect::<Vec<_>>()
        .join(", ");

    // Start building function
    Ok(format!(
        r###"#[jni_mangle("{JAVA_PATH}")]
pub fn {fn_name}<'caller>(mut unowned_env: EnvUnowned<'caller>, _class: JClass<'caller>, {args}) -> JString<'caller> {{
    unowned_env.with_env(|env| -> Result<_, jni::errors::Error> {{
        {exact}
        let command: &&EbiCommand = &&{library_name};
        let input_types = match **command {{
            EbiCommand::Command {{ input_types, .. }} => input_types,
            _ => {{
                env.throw("Expected a command.")?;
                return Ok(JString::null())
            }},
        }};

        {inputs}

        let inputs = vec![{inputs_list}];

        // Execute the command.
        let output = match command.execute_with_inputs(inputs) {{
            Ok(result) => result,
            Err(err) => {{
                env.throw(format!("Command error: {{}}", err.to_string().as_str()))?;
                return Ok(JString::null());
            }}
        }};
        
        output.export_to_java(env)
    }}).resolve::<jni::errors::ThrowRuntimeExAndDefault>()
}}
"###
    ))
}
