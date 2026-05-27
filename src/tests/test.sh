#!/bin/bash
set -e
set -x
cargo build --features java
cargo run --features test_generation itself tests -o src/tests/tests_autogen.rs
cargo test --verbose
cargo test --verbose --features exactarithmetic,approximatearithmetic
cargo test --verbose --features exactarithmetic
cargo test --verbose --features approximatearithmetic
cargo test --verbose --features javascript

echo "Ebi passed the tests."