Ebi is a tool and library that focuses on stochastic process mining algorithms.
Ebi is available as a command-line utility, as a ProM plug-in and as a Python package.

More information on its use can be found in its [manual.pdf](https://git.rwth-aachen.de/rwth-bpm/rustlibrary/-/raw/main/build/nightly/manual.pdf?ref_type=heads&inline=true).

# How to use from the command line

Ebi can be used as a standalone tool from the command line.
Compiled versions are available on [Ebi's  website](https://ebitools.org).

# How to use from ProM

Ebi can be used as a plug-in of the [ProM framework](\url{https://promtools.org}) on Windows and Linux. 
ProM has limited support for Mac OS X.
		
To install, open the ProM Package Manager and install the \verb=Ebi= package.
Then, a selection of Ebi commands can be run, just as any other ProM plug-in.

The [manual.pdf](https://git.rwth-aachen.de/rwth-bpm/rustlibrary/-/raw/main/build/nightly/manual.pdf?ref_type=heads&inline=true) indicates which commands are available in ProM.

# How to use from Python

1. Install the Ebi-pm package using pip: 

        pip install ebi-pm

1. Then, one can use it as follows:
        
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

    The names of the Ebi functions can be found in the [manual.pdf](https://git.rwth-aachen.de/rwth-bpm/rustlibrary/-/raw/main/build/nightly/manual.pdf?ref_type=heads&inline=true).
    
    Please note that for fractional numbers, Ebi returns an array consisting of 1) a 4-decimal approximation, 2) the full numerator, and 3) the full denominator.

# Getting started with development

1. Install Rustup
    https://www.rust-lang.org/tools/install

1. Log out and in again

1. Install Visual Studio Code

1. Install extension 'rust-analyzer' in Visual Studio Code
    - https://code.visualstudio.com/docs/languages/rust

1. Go to the project at https://github.com/BPM-Research-Group/Ebi choose "Clone" and "Visual Studio Code (SSH)", and clone it in Visual Studio Code.

1. To run Ebi, use the terminal of Visual Studio Code to give the command "cargo run --" instead of "Ebi". Everything else is equivalent to the commands mentioned in the manual.

