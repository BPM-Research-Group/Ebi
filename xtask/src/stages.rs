//! Which commands each stage of `cargo xtask test` runs.

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Step {
    pub label: String,
    pub command: Vec<String>,
}

/// Slow functional tests. They stay in the default `cargo test`; only the unit stage skips them.
pub const FUNCTIONAL_TESTS: [&str; 2] = [
    "ebi_framework::ebi_output::tests::all_exporters",
    "ebi_framework::ebi_output::tests::ebi_output",
];

/// Stages that run cargo.
pub const RUST_STAGES: [&str; 5] = ["unit", "generated", "functional", "rust", "bindings"];

/// Stages that the Python script runs.
pub const PYTHON_STAGES: [&str; 3] = ["python", "wheel", "wheel-release"];

/// What a bare `cargo xtask test` runs.
pub const DEFAULT_STAGES: [&str; 3] = ["rust", "bindings", "python"];

fn step(label: &str, command: &[&str]) -> Step {
    Step {
        label: label.to_string(),
        command: command.iter().map(|word| word.to_string()).collect(),
    }
}

/// `cargo test` for the given features, with optional arguments for the test harness.
fn cargo_test(label: &str, features: &str, lib: bool, harness_arguments: &[&str]) -> Step {
    let mut command = vec!["cargo", "test", "--locked", "--no-fail-fast"];
    if lib {
        command.push("--lib");
    }
    if !features.is_empty() {
        command.extend(["--features", features]);
    }
    if !harness_arguments.is_empty() {
        command.push("--");
        command.extend(harness_arguments);
    }
    step(label, &command)
}

fn rust_stage(name: &str) -> Vec<Step> {
    match name {
        "unit" => {
            let mut skipped = vec!["--skip", "tests_autogen"];
            for test in FUNCTIONAL_TESTS {
                skipped.extend(["--skip", test]);
            }
            vec![cargo_test("unit", "", true, &skipped)]
        }
        "generated" => vec![cargo_test("generated", "", true, &["tests_autogen"])],
        "functional" => {
            let heavy = FUNCTIONAL_TESTS.to_vec();
            vec![
                cargo_test("functional-heavy", "", true, &heavy),
                step(
                    "functional-cli",
                    &[
                        "cargo",
                        "test",
                        "--locked",
                        "--no-fail-fast",
                        "--test",
                        "prob_trace_cli",
                        "--test",
                        "earth_movers_test",
                    ],
                ),
            ]
        }
        "rust" => vec![
            cargo_test("rust-default", "", false, &[]),
            cargo_test("rust-exact", "exactarithmetic", false, &[]),
            cargo_test("rust-approximate", "approximatearithmetic", false, &[]),
            cargo_test(
                "rust-both",
                "exactarithmetic,approximatearithmetic",
                false,
                &[],
            ),
        ],
        "bindings" => vec![cargo_test("bindings", "python", false, &[])],
        _ => vec![],
    }
}

/// The steps of a stage, or the one step with that name.
pub fn steps_named(name: &str) -> Vec<Step> {
    let stage = rust_stage(name);
    if !stage.is_empty() {
        return stage;
    }
    RUST_STAGES
        .iter()
        .flat_map(|stage| rust_stage(stage))
        .filter(|step| step.label == name)
        .collect()
}

/// The command that regenerates a generated source file.
pub fn generate_step(what: &str) -> Option<Step> {
    match what {
        "python" => Some(step(
            "generate-python",
            &[
                "cargo",
                "run",
                "--locked",
                "--features",
                "python",
                "--",
                "itself",
                "python",
                "-o",
                "src/python/python_module_autogen.rs",
            ],
        )),
        _ => None,
    }
}

#[cfg(test)]
mod tests {
    use super::{FUNCTIONAL_TESTS, Step, generate_step, steps_named};

    fn command(step: &Step) -> String {
        step.command.join(" ")
    }

    #[test]
    fn the_unit_stage_skips_the_generated_and_the_slow_tests() {
        let steps = steps_named("unit");
        assert_eq!(steps.len(), 1);
        assert_eq!(steps[0].label, "unit");
        assert_eq!(
            command(&steps[0]),
            format!(
                "cargo test --locked --no-fail-fast --lib -- --skip tests_autogen --skip {} --skip {}",
                FUNCTIONAL_TESTS[0], FUNCTIONAL_TESTS[1]
            )
        );
    }

    #[test]
    fn the_rust_stage_runs_the_four_arithmetic_feature_sets() {
        let steps = steps_named("rust");
        let labels: Vec<&str> = steps.iter().map(|s| s.label.as_str()).collect();
        assert_eq!(
            labels,
            vec!["rust-default", "rust-exact", "rust-approximate", "rust-both"]
        );
        assert_eq!(
            command(&steps[3]),
            "cargo test --locked --no-fail-fast --features exactarithmetic,approximatearithmetic"
        );
    }

    #[test]
    fn the_bindings_stage_builds_with_the_python_feature() {
        assert_eq!(
            command(&steps_named("bindings")[0]),
            "cargo test --locked --no-fail-fast --features python"
        );
    }

    #[test]
    fn the_generated_stage_runs_only_the_blanket_tests() {
        let steps = steps_named("generated");
        assert_eq!(steps.len(), 1);
        assert_eq!(
            command(&steps[0]),
            "cargo test --locked --no-fail-fast --lib -- tests_autogen"
        );
    }

    #[test]
    fn the_functional_stage_runs_the_slow_tests_and_the_command_line_tests() {
        let steps = steps_named("functional");
        assert_eq!(steps.len(), 2);
        assert_eq!(steps[0].label, "functional-heavy");
        assert_eq!(
            command(&steps[1]),
            "cargo test --locked --no-fail-fast --test prob_trace_cli --test earth_movers_test"
        );
    }

    #[test]
    fn a_single_step_can_be_named() {
        let steps = steps_named("rust-exact");
        assert_eq!(steps.len(), 1);
        assert_eq!(
            command(&steps[0]),
            "cargo test --locked --no-fail-fast --features exactarithmetic"
        );
    }

    #[test]
    fn an_unknown_name_gives_no_steps() {
        assert!(steps_named("nonsense").is_empty());
    }

    #[test]
    fn the_python_module_is_generated_into_the_checked_in_file() {
        let step = generate_step("python").unwrap();
        assert_eq!(step.label, "generate-python");
        assert_eq!(
            command(&step),
            "cargo run --locked --features python -- itself python -o src/python/python_module_autogen.rs"
        );
    }

    #[test]
    fn there_is_nothing_else_to_generate() {
        assert!(generate_step("javascript").is_none());
    }
}
