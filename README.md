# Ebi - a process mining tool

Ebi is a process mining software suite, maintained by the [BPM group](https://bpm.rwth-aachen.de) of RWTH Aachen University, Germany. 
It contains several algorithms and techniques that perform analyses on event logs and process models.
Ebi can be used as a command-line utility, as a Python package, as a Rust crate, or in ProM. 

Ebi provides process mining and stochastic process mining algorithms, and supports exact arithmetic for most of these algorithms.

More information on its use can be found in its [PDF manual](https://git.rwth-aachen.de/rwth-bpm/rustlibrary/-/raw/main/build/nightly/manual.pdf?ref_type=heads&inline=true).

## Use Ebi from the command line

1. Download Ebi for [Windows](https://git.rwth-aachen.de/rwth-bpm/rustlibrary/-/raw/main/build/nightly/Ebi-x86_64-windows.exe?ref_type=heads&inline=false) or for [Linux](https://git.rwth-aachen.de/rwth-bpm/rustlibrary/-/raw/main/build/nightly/Ebi-x86_64-linux?ref_type=heads&inline=false).
1. In Linux, give the file execution permissions.
1. Open a command prompt and run the executable.
    Installation is not necessary, and Ebi does not require internet access.
1. For an overview of the commands, refer to the [commands page](https://leemans.ch/ebi/commands.php) or the [PDF manual](https://git.rwth-aachen.de/rwth-bpm/rustlibrary/-/raw/main/build/nightly/manual.pdf?ref_type=heads&inline=true).

Ebi runs on Mac OS X and many other platforms by self-compilation; please see below.

A good way to get started is to try `ebi info` followed by a file name, which will parse the file and print some information about it.

## Use Ebi from Python

Ebi can be used as a Python package, which integrates with [PM4Py](https://pypi.org/project/pm4py/).

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

The names of the Ebi functions can be found on the [commands page](https://leemans.ch/ebi/commands.php) or in the [PDF manual](https://git.rwth-aachen.de/rwth-bpm/rustlibrary/-/raw/main/build/nightly/manual.pdf?ref_type=heads&inline=true).

Some PM4Py objects, such as event logs, are read directly by Ebi, but most are passed as strings.
If Ebi returns an exact fraction, it is returned as an array consisting of 1) a floating-point approximation, 2) the full numerator, and 3) the full denominator.

## Use Ebi from Rust

Ebi can be used as a Rust crate, available from [crates.io](https://crates.io/crates/ebi).

1. Add the following to the Cargo.toml file:

        [dependencies]
        ebi = "*"

1. Then, one can use Ebi as follows:

        use ebi::ebi_objects::FiniteStochasticLanguage;
        use ebi::{
            ebi_traits::ebi_trait_finite_stochastic_language::EbiTraitFiniteStochasticLanguage,
            techniques::earth_movers_stochastic_conformance::EarthMoversStochasticConformance,
        };
        use ebi_objects::ebi_arithmetic::{Fraction, One};
        use std::fs;
        // Read the first file and box it
        let fin1 = fs::read_to_string("testfiles/empty_trace.slang").unwrap();
        let slang1 = fin1.parse::<FiniteStochasticLanguage>().unwrap();
        let mut object1: Box<dyn EbiTraitFiniteStochasticLanguage> = Box::new(slang1);
        // Read the second file and box it
        let fin2 = fs::read_to_string("testfiles/empty_trace.slang").unwrap();
        let slang2 = fin2.parse::<FiniteStochasticLanguage>().unwrap();
        let mut object2: Box<dyn EbiTraitFiniteStochasticLanguage> = Box::new(slang2);
        // Compute EMSC
        let emsc = object1
            .earth_movers_stochastic_conformance(object2.as_mut())
            .unwrap();
        assert_eq!(emsc, Fraction::one());
        

## Use Ebi in ProM

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
1. To run Ebi, use the terminal of Visual Studio Code to give the command "cargo run --" instead of "ebi". Everything else is equivalent to the commands mentioned in the manual.
1. To compile Ebi, give the command "cargo build --release". The binary is then placed in the project folder, in the "build/release" sub-folder.

Information on the architecture of Ebi, including its sub-crates, can be found in the [PDF manual](https://git.rwth-aachen.de/rwth-bpm/rustlibrary/-/raw/main/build/nightly/manual.pdf?ref_type=heads&inline=true).