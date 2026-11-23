//! The command line of `ebi prob trac` is public: these tests pin it as it is.
use std::process::{Command, Output};

fn run(trace: &[&str]) -> Output {
    Command::new(env!("CARGO_BIN_EXE_ebi"))
        .args(["prob", "trac", "testfiles/aa-ab-ba.slang"])
        .args(trace)
        .output()
        .unwrap()
}

/// What the command prints for a fraction. With only approximate arithmetic there is no exact value to show.
fn fraction_output(approximately: &str, exact: &str) -> String {
    if cfg!(all(
        feature = "approximatearithmetic",
        not(feature = "exactarithmetic")
    )) {
        format!("Approximately {approximately}\n\n")
    } else {
        format!("Approximately {approximately}\n{exact}\n\n")
    }
}

fn stdout_of(trace: &[&str]) -> String {
    let output = run(trace);
    assert!(
        output.status.success(),
        "{}",
        String::from_utf8_lossy(&output.stderr)
    );
    String::from_utf8(output.stdout).unwrap()
}

#[test]
fn trace_is_given_as_separate_arguments() {
    assert_eq!(stdout_of(&["b", "a"]), fraction_output("0.6", "3/5"));
}

#[test]
fn trace_that_is_not_in_the_language_has_probability_zero() {
    assert_eq!(stdout_of(&["b", "b"]), fraction_output("0", "0"));
}

#[test]
fn trace_with_an_unknown_activity_has_probability_zero() {
    assert_eq!(stdout_of(&["a", "unknown"]), fraction_output("0", "0"));
}

#[test]
fn a_missing_trace_is_a_usage_error() {
    assert!(!run(&[]).status.success());
}

#[test]
fn explain_trace_takes_the_balance_and_then_the_trace_as_separate_arguments() {
    let output = Command::new(env!("CARGO_BIN_EXE_ebi"))
        .args(["prob", "exptra", "testfiles/aa-ab-ba_ali.slpn", "1/2", "a", "a"])
        .output()
        .unwrap();
    assert!(output.status.success());
    assert_eq!(
        String::from_utf8(output.stdout).unwrap(),
        "language of alignments\n# number of alignments\n1\n# alignment 0\n# number of moves\n3\n\
         # move 0\nmodel move\nlabel b\n# transition\n1\n\
         # move 1\nsynchronous move\nlabel a\n# transition\n4\n\
         # move 2\nlog move\nlabel a\n\n"
    );
}
