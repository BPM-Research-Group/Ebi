use crate::ebi_framework::ebi_command::EbiCommand;

pub const PYTHON_PACKAGE: &str = "ebi-pm";

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
