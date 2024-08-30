# How to use

Ebi is a command line tool that requires neither installation nor internet access.

More information on its use can be found in its [manual.pdf](https://git.rwth-aachen.de/rwth-bpm/rustlibrary/-/raw/main/build/nightly/manual.pdf?ref_type=heads&inline=true).

# Getting started with development

1. Install Rustup
    https://www.rust-lang.org/tools/install

1. Log out and in again

1. Install Visual Studio Code

1. Install extension 'rust-analyzer' in Visual Studio Code
    - https://code.visualstudio.com/docs/languages/rust

1. Add an SSH key to your RWTH Gitlab account

1. Test whether your computer can connect to the RWTH Gitlab server:
    - (Ubuntu)
        - `ssh -T git@git.rwth-aachen.de`
        - Add the fingerprint from the server when asked

1. Go to the project at https://git.rwth-aachen.de/rwth-bpm/rustlibrary choose "Clone" and "Visual Studio Code (SSH)".

1. Set a git email and name
    - (Ubuntu)
        - Go in a terminal to the cloned folder and execute
            - `git config user.email "your email"`
            - `git config user.name "your name"`

# Setting up cross-platform compilation (not necessary for development)

- (Ubuntu)
    - `sudo apt install mingw-w64`
    - `rustup target add x86_64-pc-windows-gnu`
    - `rustup target add x86_64-apple-darwin`
    - `cargo build --target x86_64-pc-windows-gnu --release`
    - `cargo build --target x86_64-apple-darwin --release`

    Note that Apple compilation from Linux is challenging, and therefore it has not been set up yet.