use std::{fs, path::PathBuf};

use crate::{
    ebi_framework::{
        ebi_command::{EBI_COMMANDS, EbiCommand},
        ebi_output::EbiOutput,
    },
    python::python::pm4py_function_name,
};
use anyhow::{Result, anyhow};
use syn::Item;

pub fn generate_pm4py_module() -> Result<EbiOutput> {
    let imports = format!(
        "#![allow(unsafe_op_in_unsafe_fn)]
#![allow(unused_variables)]

// This file has been automatically generated. Manual changes will be overridden.

use pyo3::prelude::*;
use pyo3::types::PyAny;
use super::{{python_link::import_or_load, python_export::ExportableToPM4Py}};
use crate::ebi_framework::ebi_command::EbiCommand;"
    );
    let mut functions = String::new();
    let mut module = format!(
        "#[pymodule]\npub fn ebi(_py: Python<'_>, m: &Bound<'_, PyModule>) -> PyResult<()> {{"
    );

    for path in EBI_COMMANDS.get_command_paths() {
        if path.last().unwrap().is_in_python()
        {
            let (fn_name, body) = ebi_command_to_pm4py_function(&path)?;
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
fn ebi_command_to_pm4py_function(path: &Vec<&EbiCommand>) -> Result<(String, String)> {
    // Derive raw name and fn name
    let fn_name = pm4py_function_name(path);

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
        return Ok((String::new(), String::new()));
    };

    // Start building function
    let mut body = format!(
        r###"#[pyfunction]
fn {fname}(py: Python<'_>, {args}) -> PyResult<Py<PyAny>> {{
    {exact}
    let command: &&EbiCommand = &&{library_name};
    let input_types = match **command {{
        EbiCommand::Command {{ input_types, .. }} => input_types,
        _ => return Err(pyo3::exceptions::PyValueError::new_err("Expected a command.")),
    }};
"###,
        fname = fn_name,
        args = (0..input_types.len())
            .map(|i| format!("arg{}: &Bound<'_, PyAny>", i))
            .collect::<Vec<_>>()
            .join(", "),
        exact = if exact_arithmetic {
            "ebi_objects::ebi_arithmetic::exact::set_exact_globally(true);"
        } else {
            "ebi_objects::ebi_arithmetic::exact::set_exact_globally(false);"
        },
        library_name = library_name
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

    Ok((fn_name, body))
}

fn search_command_in_source_files(path: &Vec<&EbiCommand>) -> Result<String> {
    //start the search from the EBI_COMMANDS const
    let mut path = path.clone();
    path.remove(0);
    let (mut last_file, mut command_declaration) = root_command()?;

    //walk over the path of commands to find the file and const name that belongs to the lowest/actual command
    for child_command in path {
        (last_file, command_declaration) = search_child(&command_declaration, child_command)?;
    }

    let mut file_name = last_file.file_name().unwrap().to_str().unwrap().to_owned();
    file_name.pop();
    file_name.pop();
    file_name.pop();
    Ok(format!(
        "crate::ebi_commands::{}::{}",
        file_name, command_declaration.ident
    ))
}

fn root_command() -> Result<(PathBuf, syn::ItemConst)> {
    let file = "src/ebi_framework/ebi_command.rs";
    let contents = fs::read_to_string(file)?;
    let syn_file = syn::parse_file(&contents)?;
    for item in &syn_file.items {
        if let Item::Const(const_item) = item {
            if let syn::Type::Path(x) = &*const_item.ty {
                if let Some(j) = x.path.get_ident() {
                    if j.to_string() == "EbiCommand"
                        && const_item.ident.to_string() == "EBI_COMMANDS"
                    {
                        return Ok((PathBuf::from(file), const_item.clone()));
                    }
                }
            }
        }
    }
    return Err(anyhow!("source file not found"));
}

fn extract_children_names(const_item: &syn::ItemConst) -> Result<Vec<String>> {
    if let syn::Expr::Struct(x) = &*const_item.expr {
        for field in &x.fields {
            if let syn::Member::Named(field_name) = &field.member {
                if field_name.to_string() == "children" {
                    if let syn::Expr::Reference(refe) = &field.expr {
                        if let syn::Expr::Array(arr) = &*refe.expr {
                            let mut result = vec![];
                            for child in &arr.elems {
                                if let syn::Expr::Reference(child_ref) = child {
                                    if let syn::Expr::Path(command_name) = &*child_ref.expr {
                                        result.push(
                                            command_name
                                                .path
                                                .segments
                                                .last()
                                                .unwrap()
                                                .ident
                                                .to_string(),
                                        );
                                    } else {
                                        return Err(anyhow!("unexpected child found"));
                                    }
                                } else {
                                    return Err(anyhow!("unexpected child found"));
                                }
                            }
                            return Ok(result);
                        } else {
                            return Err(anyhow!("unexpected child found"));
                        }
                    } else {
                        return Err(anyhow!("unexpected child found"));
                    }
                }
            }
        }
    }
    Err(anyhow!("cannot extract children"))
}

fn search_child(
    parent_declaration: &syn::ItemConst,
    child_command: &EbiCommand,
) -> Result<(PathBuf, syn::ItemConst)> {
    let children_const_names = extract_children_names(parent_declaration)?;

    let child_short_name = child_command.short_name();

    let paths = fs::read_dir("src/ebi_commands")?;
    for entry in paths {
        let entry = entry?;
        let meta = entry.metadata()?;

        if meta.is_file() {
            let contents = fs::read_to_string(entry.path())?;
            let file = syn::parse_file(&contents)?;
            for item in &file.items {
                if let Item::Const(const_item) = item {
                    if children_const_names.contains(&const_item.ident.to_string()) {
                        if let syn::Type::Path(x) = &*const_item.ty {
                            if let Some(j) = x.path.get_ident() {
                                if j.to_string() == "EbiCommand" {
                                    //find the short name
                                    if let syn::Expr::Struct(x) = &*const_item.expr {
                                        for field in &x.fields {
                                            if let syn::Member::Named(field_name) = &field.member {
                                                if field_name.to_string() == "name_short" {
                                                    if let syn::Expr::Lit(lit) = &field.expr {
                                                        if let syn::Lit::Str(str) = &lit.lit {
                                                            if str.value() == child_short_name {
                                                                // println!(
                                                                //     "\tfound EbiCommand {} with short name {}",
                                                                //     const_item.ident,
                                                                //     child_short_name
                                                                // );
                                                                return Ok((
                                                                    entry.path(),
                                                                    const_item.clone(),
                                                                ));
                                                            }
                                                        }
                                                    }
                                                }
                                            } else {
                                                return Err(anyhow!("unexpected field"));
                                            }
                                        }
                                    } else {
                                        return Err(anyhow!("unexpected expr"));
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    }

    return Err(anyhow!("todo file not found {}", child_command));
}
