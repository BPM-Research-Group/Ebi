//! The one-line summary of a `cargo test` log.

#[derive(Debug, PartialEq, Eq)]
pub struct Summary {
    pub text: String,
    pub failed_tests: Vec<String>,
}

/// The numbers of passed and failed tests of a line such as
/// `test result: ok. 3 passed; 0 failed; 0 ignored; 0 measured; 0 filtered out; finished in 0.01s`.
fn counts(line: &str) -> Option<(u64, u64)> {
    let rest = line.strip_prefix("test result: ")?;
    let (_, rest) = rest.split_once(". ")?;
    let (mut passed, mut failed) = (None, None);
    for part in rest.split("; ") {
        let mut words = part.split_whitespace();
        if let (Some(number), Some(label)) = (words.next(), words.next())
            && let Ok(number) = number.parse::<u64>()
        {
            match label {
                "passed" => passed = Some(number),
                "failed" => failed = Some(number),
                _ => {}
            }
        }
    }
    Some((passed?, failed?))
}

/// The name in a line such as `test some::path ... FAILED`.
fn failed_test(line: &str) -> Option<&str> {
    line.strip_prefix("test ")?.strip_suffix(" ... FAILED")
}

pub fn summarise(log: &str) -> Summary {
    let (mut passed, mut failed, mut results) = (0, 0, 0);
    let mut failed_tests = vec![];
    for line in log.lines() {
        if let Some((p, f)) = counts(line) {
            passed += p;
            failed += f;
            results += 1;
        } else if let Some(name) = failed_test(line) {
            failed_tests.push(name.to_string());
        }
    }
    let text = if results == 0 {
        "see log".to_string()
    } else {
        format!("{passed} passed, {failed} failed")
    };
    Summary { text, failed_tests }
}

#[cfg(test)]
mod tests {
    use super::{Summary, summarise};

    #[test]
    fn counts_are_summed_over_every_test_binary() {
        let log = "\
running 3 tests
test result: ok. 3 passed; 0 failed; 0 ignored; 0 measured; 0 filtered out; finished in 0.01s

running 5 tests
test result: FAILED. 2 passed; 3 failed; 0 ignored; 0 measured; 1 filtered out; finished in 0.40s
";
        assert_eq!(summarise(log).text, "5 passed, 3 failed");
    }

    #[test]
    fn failing_tests_are_named() {
        let log = "\
test ebi_framework::a::tests::fine ... ok
test ebi_framework::a::tests::broken ... FAILED
test ebi_framework::b::tests::also_broken ... FAILED
test result: FAILED. 1 passed; 2 failed; 0 ignored; 0 measured; 0 filtered out; finished in 0.01s
";
        assert_eq!(
            summarise(log).failed_tests,
            vec![
                "ebi_framework::a::tests::broken".to_string(),
                "ebi_framework::b::tests::also_broken".to_string(),
            ]
        );
    }

    #[test]
    fn windows_line_endings_are_understood() {
        let log = "test x ... FAILED\r\ntest result: FAILED. 0 passed; 1 failed; 0 ignored; 0 measured; 0 filtered out; finished in 0.01s\r\n";
        assert_eq!(
            summarise(log),
            Summary {
                text: "0 passed, 1 failed".to_string(),
                failed_tests: vec!["x".to_string()],
            }
        );
    }

    #[test]
    fn a_log_without_results_points_to_the_log() {
        let log = "error: could not compile `ebi` (lib) due to 1 previous error\n";
        assert_eq!(summarise(log).text, "see log");
    }
}
