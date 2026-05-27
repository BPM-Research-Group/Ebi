use crate::{
    ebi_framework::ebi_command::EbiCommand,
    tests::{fallible_test_list::FALLIBLE_TESTS, test_input::TestInput},
};

pub(crate) fn is_fallible(path: &Vec<&EbiCommand>, inputs: &Vec<TestInput>) -> bool {
    //transform to strings
    let string_inputs = inputs
        .iter()
        .map(|input| input.to_unique_string())
        .collect::<Vec<_>>();
    let ref_string_inputs = string_inputs.iter().map(|s| s.as_str()).collect::<Vec<_>>();

    FALLIBLE_TESTS.contains(&(path.last().unwrap(), &ref_string_inputs))
}
