use crate::ebi_framework::{
    ebi_command::{EBI_COMMANDS, EbiCommand},
    ebi_output::EbiOutput,
};
use anyhow::Result;

pub const PM4PY_PACKAGE: &str = "ebi-pm";

pub fn pm4py_function_name(path: &Vec<&EbiCommand>) -> String {
    let raw_name = EbiCommand::path_to_string(path);
    raw_name
        .strip_prefix("Ebi ")
        .unwrap_or(&raw_name)
        .to_lowercase()
        .chars()
        .map(|c| if c == ' ' || c == '-' { '_' } else { c })
        .collect()
}

pub fn generate_pm4py_module() -> Result<EbiOutput> {
    let mut imports = format!(
        "#![allow(unsafe_op_in_unsafe_fn)]
#![allow(unused_variables)]

use pyo3::prelude::*;
use pyo3::types::PyAny;
use super::pm4py_link::{{import_or_load, ExportableToPM4Py}};
use crate::ebi_framework::ebi_command::EbiCommand;"
    );
    let mut functions = String::new();
    let mut module =
        format!("#[pymodule]\nfn ebi(_py: Python<'_>, m: &PyModule) -> PyResult<()> {{");

    for path in EBI_COMMANDS.get_command_paths() {
        if let EbiCommand::Command { library_name, .. } = path[path.len() - 1] {
            imports.push_str(&format!("\nuse crate::{};", library_name));
            let (fn_name, body) = ebi_command_to_pm4py_function(&path);
            functions.push_str(&body);
            module.push_str(&format!(
                "    m.add_function(wrap_pyfunction!({}, m)?)?;\n",
                fn_name
            ));
        }
    }

    module.push_str(
        "    Ok(())
}
",
    );

    let result = format!("{}\n\n{}\n\n{}", imports, functions, module);
    Ok(EbiOutput::String(result))
}

/// return the tupe (string1, string2) where
/// string1 - function name
/// string2 - function body
fn ebi_command_to_pm4py_function(path: &Vec<&EbiCommand>) -> (String, String) {
    // Derive raw name and fn name
    let fn_name = pm4py_function_name(path);

    // Extract input_types
    let (input_types, library_name, exact_arithmetic) = if let EbiCommand::Command {
        input_types,
        library_name,
        exact_arithmetic,
        ..
    } = path[path.len() - 1]
    {
        (input_types, library_name, *exact_arithmetic)
    } else {
        return (String::new(), String::new());
    };

    // Start building function
    let mut body = format!(
        r###"#[pyfunction]
fn {fname}(py: Python<'_>, {args}) -> PyResult<PyObject> {{
    {exact}
    let command: &&EbiCommand = &&{library_name};
    let input_types = match **command {{
        EbiCommand::Command {{ input_types, .. }} => input_types,
        _ => return Err(pyo3::exceptions::PyValueError::new_err("Expected a command.")),
    }};
"###,
        fname = fn_name,
        args = (0..input_types.len())
            .map(|i| format!("arg{}: &PyAny", i))
            .collect::<Vec<_>>()
            .join(", "),
        exact = if exact_arithmetic {
            "ebi_arithmetic::exact::set_exact_globally(true);"
        } else {
            "ebi_arithmetic::exact::set_exact_globally(false);"
        },
        library_name = library_name.split("::").last().unwrap()
    );

    // Import each argument
    for idx in 0..input_types.len() {
        body.push_str(&format!(r###"    let input{idx} = import_or_load(arg{idx}, input_types[{idx}], {idx})
            .map_err(|e| pyo3::exceptions::PyValueError::new_err(format!("Could not import argument {idx}: {{}}", e)))?;
    "###,
            idx = idx
        ));
    }

    // Collect inputs
    let inputs = (0..input_types.len())
        .map(|i| format!("input{}", i))
        .collect::<Vec<_>>()
        .join(", ");
    body.push_str(&format!(
        r###"    let inputs = vec![{}];

    // Execute the command.
    let result = command.execute_with_inputs(inputs)
        .map_err(|e| pyo3::exceptions::PyException::new_err(format!("Command error: {{}}", e)))?
        .export_to_pm4py(py)
        .map_err(|e| pyo3::exceptions::PyException::new_err(format!("Export error: {{}}", e)))?;

    Ok(result)
}}

"###,
        inputs
    ));

    (fn_name, body)
}
