//! Developer tasks for Ebi, run as `cargo xtask`. The Rust stages live here; the stages that are
//! Python tooling (the environment, the wheel, the Python tests) are in `xtask/python`.

mod stages;
mod summary;

use stages::{DEFAULT_STAGES, PYTHON_STAGES, Step, generate_step, steps_named};
use std::{
    env,
    ffi::OsString,
    fs::{self, File},
    path::{Path, PathBuf},
    process::{Command, ExitCode, Stdio},
    time::Instant,
};

const USAGE: &str = "\
Developer tasks for Ebi. Run from the repository root.

    cargo xtask test [stage|step ...]   run test stages, logging each step to var/test-<step>.log
    cargo xtask generate python         regenerate src/python/python_module_autogen.rs
    cargo xtask python env lock         resolve the dependencies of pyproject.toml, rewrite xtask/python/requirements.txt
    cargo xtask python env rebuild      recreate the ebi-pm environment from xtask/python/requirements.txt

Test stages (a bare `test` runs rust, bindings and python):
    unit            the hand-written unit tests only
    generated       the generated blanket tests (tests_autogen)
    functional      the slow exporter tests and the command line tests
    rust            cargo test for the four arithmetic feature sets
    bindings        cargo test with the python feature (which includes java)
    python          maturin develop into the ebi-pm environment, then the Python tests
    wheel           build a wheel, install it into a fresh environment and test it there
    wheel-release   the same with a release build
A step such as rust-exact runs on its own.
";

fn root() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .expect("xtask lives in the repository")
        .to_path_buf()
}

fn venv_python(root: &Path) -> PathBuf {
    let venv = root.join("ebi-pm");
    if cfg!(windows) {
        venv.join("Scripts").join("python.exe")
    } else {
        venv.join("bin").join("python")
    }
}

/// The variables that put the ebi-pm environment first, or nothing if it does not exist.
fn tool_environment(root: &Path) -> Vec<(OsString, OsString)> {
    let python = venv_python(root);
    if !python.exists() {
        return vec![];
    }
    let directory = python.parent().expect("python is in a directory").to_path_buf();
    let old = env::var_os("PATH").unwrap_or_default();
    let path = env::join_paths(std::iter::once(directory).chain(env::split_paths(&old)))
        .expect("the directories can be joined");
    vec![
        ("VIRTUAL_ENV".into(), root.join("ebi-pm").into_os_string()),
        ("PATH".into(), path),
        ("PYO3_PYTHON".into(), python.into_os_string()),
    ]
}

/// Run one step with its output in `log`, and print how it went.
fn run_step(step: &Step, log: &str, root: &Path, environment: &[(OsString, OsString)]) -> bool {
    let log_path = root.join(log);
    let files = fs::create_dir_all(root.join("var"))
        .and_then(|_| File::create(&log_path))
        .and_then(|file| file.try_clone().map(|copy| (file, copy)));
    let Ok((out, err)) = files else {
        println!("{}: FAILED, cannot write {log}", step.label);
        return false;
    };
    println!("{}: running {}", step.label, step.command.join(" "));
    let started = Instant::now();
    let status = Command::new(&step.command[0])
        .args(&step.command[1..])
        .current_dir(root)
        .envs(environment.iter().map(|(name, value)| (name, value)))
        .stdout(Stdio::from(out))
        .stderr(Stdio::from(err))
        .status();
    let seconds = started.elapsed().as_secs();
    let text = fs::read(&log_path)
        .map(|bytes| String::from_utf8_lossy(&bytes).into_owned())
        .unwrap_or_default();
    let summary = summary::summarise(&text);
    let ok = matches!(&status, Ok(status) if status.success());
    if let Err(error) = &status {
        println!("{}: FAILED, {}: {error}", step.label, step.command[0]);
        return false;
    }
    let outcome = if ok { "ok" } else { "FAILED" };
    println!("{}: {outcome} after {seconds}s, {} ({log})", step.label, summary.text);
    for name in &summary.failed_tests {
        println!("      FAILED {name}");
    }
    ok
}

/// Run the Python script that owns the Python-only stages.
fn run_python(root: &Path, arguments: &[&str]) -> bool {
    let script = root.join("xtask").join("python").join("ebi_python.py");
    let status = Command::new("python")
        .arg(script)
        .args(arguments)
        .current_dir(root)
        .status();
    matches!(status, Ok(status) if status.success())
}

fn missing_environment() -> ExitCode {
    eprintln!("The ebi-pm environment is missing; run `cargo xtask python env rebuild` first.");
    ExitCode::FAILURE
}

fn test(root: &Path, names: &[String]) -> ExitCode {
    let names: Vec<String> = if names.is_empty() {
        DEFAULT_STAGES.iter().map(|name| name.to_string()).collect()
    } else {
        names.to_vec()
    };
    let unknown: Vec<&str> = names
        .iter()
        .map(String::as_str)
        .filter(|name| !PYTHON_STAGES.contains(name) && steps_named(name).is_empty())
        .collect();
    if !unknown.is_empty() {
        eprintln!("unknown stage or step: {}\n\n{USAGE}", unknown.join(", "));
        return ExitCode::from(2);
    }
    let environment = tool_environment(root);
    let needs_environment = names
        .iter()
        .any(|name| name == "bindings" || PYTHON_STAGES.contains(&name.as_str()));
    if needs_environment && environment.is_empty() {
        return missing_environment();
    }
    let mut outcomes = vec![];
    for name in &names {
        if PYTHON_STAGES.contains(&name.as_str()) {
            let command = if name == "python" { "test" } else { name.as_str() };
            outcomes.push(run_python(root, &[command]));
        } else {
            for step in steps_named(name) {
                let log = format!("var/test-{}.log", step.label);
                outcomes.push(run_step(&step, &log, root, &environment));
            }
        }
    }
    if outcomes.iter().all(|ok| *ok) {
        ExitCode::SUCCESS
    } else {
        ExitCode::FAILURE
    }
}

fn generate(root: &Path, arguments: &[String]) -> ExitCode {
    let Some(step) = arguments.first().and_then(|what| generate_step(what)) else {
        eprintln!("nothing to generate called {:?}\n\n{USAGE}", arguments);
        return ExitCode::from(2);
    };
    let environment = tool_environment(root);
    if environment.is_empty() {
        return missing_environment();
    }
    let log = format!("var/{}.log", step.label);
    if run_step(&step, &log, root, &environment) {
        println!("generated; `git diff` shows what changed");
        ExitCode::SUCCESS
    } else {
        ExitCode::FAILURE
    }
}

fn main() -> ExitCode {
    let arguments: Vec<String> = env::args().skip(1).collect();
    let root = root();
    match arguments.first().map(String::as_str) {
        Some("test") => test(&root, &arguments[1..]),
        Some("generate") => generate(&root, &arguments[1..]),
        Some("python") => {
            let rest: Vec<&str> = arguments[1..].iter().map(String::as_str).collect();
            if run_python(&root, &rest) {
                ExitCode::SUCCESS
            } else {
                ExitCode::FAILURE
            }
        }
        _ => {
            eprint!("{USAGE}");
            ExitCode::from(2)
        }
    }
}
