#!/bin/bash

# Generate all Rust code that Ebi can generate.
# Needs to be re-run on every change in command structure.

# Fail on first error
set -e

# Clear generated Rust code files
> src/tests/tests_autogen.rs
> src/surfaces/javascript/javascript_autogen.rs
> src/python/python_module_autogen.rs
> src/surfaces/java/java_autogen.rs

# Generate Rust files
cargo run --features test_generation itself tests -o src/tests/tests_autogen.rs
cargo run --features surface_generation -- itself surface javascript -o src/surfaces/javascript/javascript_autogen.rs
cargo run --features surface_generation -- itself surface python -o src/python/python_module_autogen.rs
cargo run --features surface_generation -- itself surface java rust -o src/surfaces/java/java_autogen.rs

# Check compilation
cargo build --features javascript
cargo build --features python
cargo build --features java

# Check default completely
cargo test --verbose

# Check commands with compile features
cargo test --package ebi --lib -- tests::tests_autogen::tests --nocapture --features exactarithmetic
cargo test --package ebi --lib -- tests::tests_autogen::tests --nocapture --features approximatearithmetic
cargo test --package ebi --lib -- tests::tests_autogen::tests --nocapture --features javascript

echo "Ebi passed the tests."