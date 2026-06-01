#!/bin/bash
set -e
set -x
# generate test code
cargo run --features test_generation itself tests -o src/tests/tests_autogen.rs
cargo run --features "javascript test_generation" -- itself javascript -o src/javascript/javascript_autogen.rs
# perform compile checks
cargo build --features exactarithmetic,approximatearithmetic
cargo build --features exactarithmetic
cargo build --features approximatearithmetic
cargo build --features javascript
# cargo build --features python
cargo build --features java
# perform tests
cargo test --verbose
cargo test --verbose --features exactarithmetic,approximatearithmetic
cargo test --verbose --features exactarithmetic
cargo test --verbose --features approximatearithmetic
cargo test --verbose --features javascript

echo "Ebi passed the tests."