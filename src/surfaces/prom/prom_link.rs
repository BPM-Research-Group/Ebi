#[cfg(test)]
mod tests {
    use crate::ebi_framework::ebi_command::EbiCommand;
    use std::fs;

    #[test]
    fn java() {
        let fin = fs::read_to_string("testfiles/a-b.xes").unwrap();
        assert!(
            EbiCommand::execute_command_strings_string("Ebi visualise text", "txt", vec![fin])
                .is_ok()
        );
    }
}
