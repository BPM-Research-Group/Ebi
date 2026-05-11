set -e
cargo run --features javascript itself javascript -o src/javascript/javascript_autogen.rs
#wasm-pack build -d ./documentation/javascript --target web --features javascript
cargo run itself documentation commands -o ./documentation/commands.html
cargo run itself documentation file-handlers -o ./documentation/file_handlers.html
cargo run itself documentation home -o ./documentation/index.html
