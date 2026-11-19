use crate::{ebi_framework::ebi_command::EbiCommand, text::JavaEscaper};
#[cfg(feature = "java")]
use crate::{
    ebi_framework::ebi_input::{attempt_parse, EbiInput, EbiInputType},
    multiple_reader::MultipleReader,
    text::Joiner,
};
#[cfg(feature = "java")]
use ebi_objects::anyhow::{anyhow, Result};
#[cfg(feature = "java")]
use jni::{
    jni_mangle,
    objects::{JClass, JObjectArray, JString},
    EnvUnowned,
};

pub fn java_function_name(path: &Vec<&EbiCommand>) -> String {
    let raw_name = EbiCommand::path_to_string(path);
    raw_name
        .strip_prefix("Ebi ")
        .unwrap_or(&raw_name)
        .to_lowercase()
        .chars()
        .collect::<String>()
        .escape_java_code()
}

#[cfg(feature = "java")]
// helper: try import via Java importer, else if it's a str, import it directly
pub fn import_or_load<'caller>(
    to_import: JString<'caller>,
    input_types: &[&'static EbiInputType],
    index: usize,
) -> Result<EbiInput> {
    // 1) try all in‑memory importers

    // parse the content string
    attempt_parse(input_types, MultipleReader::String(to_import.to_string())).map_err(|e| {
            anyhow!("Failed to parse argument {}, which was given as a string literal. Last attempted parsing gave the error {}. Expected {}.", index, e, EbiInputType::get_possible_inputs(input_types).join_with(", ", " or "))
        })
}

#[cfg(feature = "java")]
// This keeps Rust from "mangling" the name and making it unique for this crate.
#[jni_mangle("org.ebitools.Ebi")]
pub fn call_ebi<'caller>(
    mut unowned_env: EnvUnowned<'caller>,
    _class: JClass<'caller>,
    command_name: JString<'caller>,
    output_format: JString<'caller>,
    inputs: JObjectArray<'caller, JString<'caller>>,
) -> JString<'caller> {
    // Before we can access JNI, jni-rs needs to know that the thread is
    // attached to the Java VM.
    //
    // Within a native method we can assume the JVM attaches the thread
    // before calling our implementation, and this is represented
    // by the EnvUnowned type.
    //
    // We use `with_env` to upgrade the EnvUnowned to a Env, which gives us
    // access to the full JNI API.
    //
    // IMPORTANT: Don't use `jni-rs` in any other way within a native method
    // until you call `unowned_env.with_env` or `AttachGuard::from_unowned`.
    //
    // Internally `with_env()` creates a hidden `AttachGuard`` to track the thread
    // attachment explicitly and this will also wrap the given closure with
    // `catch_unwind` to ensure that your code can't panic and unwind across
    // FFI boundaries.
    let outcome = unowned_env.with_env(|env| -> Result<_, jni::errors::Error> {
        // First, we have to get the string out of Java. Check out the `strings`
        // module for more info on how this works.
        let command_name = command_name.to_string();
        let output_format = output_format.to_string();

        let number_of_inputs = inputs.len(env)?;
        let mut inputss = Vec::with_capacity(number_of_inputs);
        for i in 0..number_of_inputs {
            let jstring = inputs.get_element(env, i)?;
            let string = jstring.to_string();
            println!("=== Ebi received\n{}", string);
            inputss.push(string);
        }

        // let output_ebi = match handle_prom_request(command_name, output_format, inputss) {
        //     Ok(result) => result,
        //     Err(err) => "Ebi: error: ".to_string() + err.to_string().as_str(),
        // };
        let output_ebi = match EbiCommand::execute_command_strings_string(
            &command_name,
            &output_format,
            inputss,
        ) {
            Ok(result) => result,
            Err(err) => {
                env.throw(format!("Ebi error: {}", err.to_string().as_str()))?;
                return Ok(JString::null());
            }
        };
        println!("== Ebi sends\n{}", output_ebi);

        JString::from_str(env, output_ebi)
    });

    // Finally, we have to resolve the `EnvOutcome` into a concrete return value.
    //
    // Our code above may have failed with a JNI error, or some other
    // application-specific error, or it may have panicked. None of these things
    // can pass over the FFI boundary back into the JVM.
    //
    // Mapping of errors and panics is done according to the selected
    // `ErrorPolicy` trait implementation which is able to use JNI for throwing
    // Java exceptions if necessary.
    //
    // This design lets you encapsulate your own approach to forwarding errors
    // and panics to Java code and then easily reuse it across multiple native
    // methods.
    //
    // In this case we use a built-in policy that throws a Java
    // `RuntimeException` with a message containing the error/panic details.
    outcome.resolve::<jni::errors::ThrowRuntimeExAndDefault>()
}

#[cfg(test)]
mod tests {
    use crate::{
        ebi_framework::{
            ebi_file_handler::get_file_handlers,
            ebi_input::{EbiInput, TEST_INPUT_TYPE_STRING},
            ebi_trait::FromEbiTraitObject,
        },
        surfaces::java::java_object_handler::{
            JavaObjectHandlerQueryExport, JavaObjectHandlerQueryImport,
        },
    };
    use ebi_objects::EbiObjectType;
    use strum::IntoEnumIterator;

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
