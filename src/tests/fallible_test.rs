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
    let needle_command = path.last().unwrap();

    'outer: for (command, test_inputs) in FALLIBLE_TESTS {
        if command != needle_command {
            continue 'outer;
        }

        'inner: for (string_input, test_input) in string_inputs.iter().zip(test_inputs.iter()) {
            if test_input == &"*" {
                continue 'inner;
            }

            if test_input != string_input {
                continue 'outer;
            }
        }

        return true;
    }

    return false;
}
