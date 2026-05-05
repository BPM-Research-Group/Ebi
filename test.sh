#!/bin/bash
set -e
cargo build --features java
cargo build --features javascript
cargo test --verbose
cargo test --verbose --features exactarithmetic,approximatearithmetic
cargo test --verbose --features exactarithmetic
cargo test --verbose --features approximatearithmetic

echo "Ebi was successfully tested"