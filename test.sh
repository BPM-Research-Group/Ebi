#!/bin/bash
set -e
set -x
cargo build --features java
cargo test --verbose
cargo test --verbose --features exactarithmetic,approximatearithmetic
cargo test --verbose --features exactarithmetic
cargo test --verbose --features approximatearithmetic
cargo test --features javascript

echo "Ebi was successfully tested"