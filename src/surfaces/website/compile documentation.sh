set -e
cargo run --features javascript,test_generation -- itself javascript -o src/javascript/javascript_autogen.rs
wasm-pack build -d ./documentation/javascript --target web --features javascript,test_generation
cargo run --features javascript,test_generation -- itself documentation commands -o ./documentation/commands.html
cargo run --features javascript,test_generation -- itself documentation file-handlers -o ./documentation/file_handlers.html
cargo run --features javascript,test_generation -- itself documentation home -o ./documentation/index.html
