# Ebi - a process mining tool

Ebi is a stochastic process mining software suite, maintained by the BPM group of RWTH University, Aachen, Germany. 
It contains several algorithms and techniques that perform analyses on event logs and process models.
Ebi can be used as a command-line utility, as a Python package, as a Rust crate, or in ProM. 

More information on its use can be found in its [PDF manual](https://git.rwth-aachen.de/rwth-bpm/rustlibrary/-/raw/main/build/nightly/manual.pdf?ref_type=heads&inline=true).

## How to use from the command line

1. Download Ebi for [Windows](https://git.rwth-aachen.de/rwth-bpm/rustlibrary/-/raw/main/build/nightly/Ebi-x86_64-windows.exe?ref_type=heads&inline=false) or for [Linux](https://git.rwth-aachen.de/rwth-bpm/rustlibrary/-/raw/main/build/nightly/Ebi-x86_64-linux?ref_type=heads&inline=false).
1. In Linux, give the file execution permissions.
1. Open a command prompt and run the executable.
    Installation is not necessary, and Ebi does not require internet access.
1. For an overview of the commands, refer to the [commands page](https://git.rwth-aachen.de/rwth-bpm/rustlibrary/-/raw/main/build/nightly/commands.html?ref_type=heads&inline=true) or the [PDF manual](https://git.rwth-aachen.de/rwth-bpm/rustlibrary/-/raw/main/build/nightly/manual.pdf?ref_type=heads&inline=true).

Ebi runs on Mac OS X, however, due to Apple's restrictions, only by self-compilation.
Please see below.

## How to use from Python

1. Install the Ebi-pm package using pip: 

        pip install ebi-pm

1. Then, one can use Ebi as follows:

        import pm4py
        import ebi
        # Load a log using PM4Py
        log = pm4py.read_xes("myLog.xes", return_legacy_log_object=True)
        # Load a model as a string
        with open('mymodel.slpn', 'r') as file:
            model = file.read()
        # Call the Ebi function
        result = ebi.conformance_earth_movers_sample(
            log,
            model,
            1000
        )
        print(result)

The names of the Ebi functions can be found in the [PDF manual](https://git.rwth-aachen.de/rwth-bpm/rustlibrary/-/raw/main/build/nightly/manual.pdf?ref_type=heads&inline=true).

Please note that for fractional numbers, Ebi returns an array consisting of 1) a 4-decimal approximation, 2) the full numerator, and 3) the full denominator.

## How to use from ProM

Ebi is also available in the [ProM framework](\url{https://promtools.org}) on Windows and Linux.
In the ProM Package Manager, install Ebi. 
Then, several Ebi commands can be used like any other ProM plug-in. 
As Ebi is extremely flexible in its inputs and outputs, please ensure you choose the correct input and output plug-in (there are many). 

The [PDF manual](https://git.rwth-aachen.de/rwth-bpm/rustlibrary/-/raw/main/build/nightly/manual.pdf?ref_type=heads&inline=true) indicates which commands are available in ProM.

## Getting started with development

Ebi is hosted on [Github](https://github.com/BPM-Research-Group/Ebi), and we welcome pull requests there.

1. Install [Rustup](https://www.rust-lang.org/tools/install)
1. Log out and in again
1. Install Visual Studio Code
1. In Visual Studio Code, install the extension [rust-analyzer](https://code.visualstudio.com/docs/languages/rust)
1. With a browser, go to the project on [Github](https://github.com/BPM-Research-Group/Ebi), choose "Clone", "Visual Studio Code (SSH)", and clone it in Visual Studio Code.
1. To run Ebi, use the terminal of Visual Studio Code to give the command "cargo run --" instead of "Ebi". Everything else is equivalent to the commands mentioned in the manual.
1. To compile Ebi, give the command "cargo build --release". The binary is then placed in the project folder, in the "build/release" sub-folder.

