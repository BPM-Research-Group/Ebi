use crate::{
    ebi_framework::{
        ebi_command::EbiCommand,
        ebi_input::{EbiInput, EbiInputType},
        ebi_output::EbiOutput,
    },
    prom::prom_link::attempt_parse,
    python::python_import::PYTHON_IMPORTERS,
    text::Joiner,
};
use anyhow::{Result, anyhow};
use polars::prelude::*;
use pyo3::{exceptions::PyValueError, prelude::*, types::PyAny};
use std::io::Cursor;

// helper functions for petri nets

// fn get_collection_as_set<'a>(obj: &'a PyAny, attr: &'a str) -> PyResult<&'a PySet> {
//     Ok(obj.getattr(attr)?.downcast::<PySet>()?)
// }

// fn pyset_as_vec<'a>(set: &'a PySet) -> Vec<&'a PyAny> {
//     // Note: sets in Python are unordered.
//     set.iter().collect()
// }

// ================ Experimentation with passing Logs as DataFrame =================
/// internal function that creates a Polars DataFrame from IPC bytes
fn _extract_dataframe(ipc_bytes: &[u8]) -> Result<DataFrame, PolarsError> {
    let cursor = Cursor::new(ipc_bytes);
    // Use the IPC reader to deserialize the data into a DataFrame.
    polars::io::ipc::IpcReader::new(cursor).finish()
}

// ========= EbiCommand execution if inputs are given ==========

impl EbiCommand {
    /// Executes the command using the provided inputs without reading from CLI.
    /// Returns the resulting object.
    pub fn execute_with_inputs(&self, inputs: Vec<EbiInput>) -> Result<EbiOutput> {
        match self {
            EbiCommand::Command {
                execute,
                output_type,
                ..
            } => {
                // Call the command’s execute closure directly.
                // Passing None for ArgMatches since we don’t need any CLI options.
                let result = (execute)(inputs, None)?;
                if &&result.get_type() != output_type {
                    return Err(anyhow!(
                        "Output type {} does not match the declared output of {}.",
                        result.get_type(),
                        output_type
                    ));
                }
                Ok(result)
            }
            _ => Err(anyhow!("Not a command variant.")),
        }
    }
}

// helper: try import via PM4Py importer, else if it's a str, treat as file path
pub fn import_or_load(
    py_obj: &Bound<'_, PyAny>,
    input_types: &[&'static EbiInputType],
    index: usize,
) -> PyResult<EbiInput> {
    // 1) try all in‑memory importers
    let input = if let Some(inp) = PYTHON_IMPORTERS
        .iter()
        .find_map(|importer| importer(py_obj, input_types).ok())
    {
        inp
    } else if let Ok(content) = py_obj.extract::<String>() {
        // parse the content string
        attempt_parse(input_types, content).map_err(|e| {
            PyValueError::new_err(format!("Failed to parse argument {}, which was given as a string literal. Last attempted parsing gave the error {}. Expected {}.", index, e, EbiInputType::get_possible_inputs(input_types).join_with(", ", " or ")))
        })?
    } else {
        //did not manage to parse the input
        return Err(PyValueError::new_err(format!(
            "Could not import argument {}. Expected {}.",
            index,
            EbiInputType::get_possible_inputs(input_types).join_with(", ", " or ")
        )));
    };

    Ok(input)
}

#[cfg(test)]
mod tests {
    use crate::python::{python_export::natural_to_num_biguints, python_module_autogen::ebi};
    use ebi_objects::ebi_arithmetic::malachite::{
        Natural,
        base::num::basic::traits::{One, Zero},
    };
    use pyo3::{Python, types::PyAnyMethods};

    #[test]
    fn num_bigint_conversion() {
        let n_mal = Natural::ZERO;
        let n_num = natural_to_num_biguints(&n_mal);
        assert_eq!(n_num.to_string(), "0");

        let n_mal = Natural::ONE;
        let n_num = natural_to_num_biguints(&n_mal);
        assert_eq!(n_num.to_string(), "1");

        let n_str = "1234567897987987987";
        let n_mal = n_str.parse::<Natural>().unwrap();
        let n_num = natural_to_num_biguints(&n_mal);
        assert_eq!(n_num.to_string(), n_str);

        let n_str = "4564654564356546736574566574687756894756176468774647617687746947461746796474665474878934424516";
        let n_mal = n_str.parse::<Natural>().unwrap();
        let n_num = natural_to_num_biguints(&n_mal);
        assert_eq!(n_num.to_string(), n_str);
    }

    #[test]
    fn test_python() {
        pyo3::append_to_inittab!(ebi);
        Python::initialize();
        Python::attach(|py| {
            let py_module = py.import("ebi").unwrap();
            let _py_function = py_module.getattr("visualise_text").unwrap();
            // let result: PyResult<String> = match py_function.call1((1i32,)) {
            //     Ok(r) => r.extract(),
            //     Err(e) => Err(e),
            // };
            // let result = result.unwrap();
            // let expected_result = "1";
            // assert_eq!(result, expected_result);
        });
    }
}
