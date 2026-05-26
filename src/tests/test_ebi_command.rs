macro_rules! test_ebi_command {
    ($name:ident) => {
        #[cfg(test)]
        pastey::paste! {
            #[test]
            #[ntest::timeout(1000000)]
            #[allow(non_snake_case)]
            fn [<test_ebi_command_ $name>]() {
                crate::ebi_framework::ebi_command::tests::ebi_command_test(&$name);
            }
        }
    };
}
pub(crate) use test_ebi_command;