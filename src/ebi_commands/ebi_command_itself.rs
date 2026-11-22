use crate::ebi_framework::{
    ebi_command::EbiCommand,
    ebi_output::{EbiOutput, EbiOutputType},
    manual::{graph, manual},
};
#[cfg(feature = "surface_generation")]
use crate::surfaces::{
    java::{
        java_module_generator_java::generate_java_module_java,
        java_module_generator_rust::generate_java_module_rust,
    },
    javascript::javascript_generator_rust::generate_javascript_rust,
    website::website_generation::{generate_commands, generate_file_handlers, generate_home_page},
};
use ebi_objects::{ebi_objects::scalable_vector_graphics::ToSVGMut, EbiObject, EbiObjectType};

pub const LOGO: &str = r"□ □ □ □ □ □ □ □ □ □ □ □ □ □ □
 □ □ □ □ □ □ □ □ □ □ □ □ □ □ 
□ □ □ □ □◦◦◦◦ □-◦◦◦◦□ □ □ □ □
 □ □ □ □ ◦◦◦◦◦ ◦◦◦◦◦ □ □ □ □ 
□ □ □ ◦◦□ ◦◦◦ □ ◦◦◦ □◦◦ □ □ □
 □ □ ◦◦◦◦# □ ◦◦◦◦□ =◦◦◦◦ □ □ 
□ □ □ ◦◦◦+□◦◦◦◦◦◦◦◦-◦◦◦ □ □ □
 □ □ □ □ ◦◦◦◦◦◦◦◦◦◦◦ □ □ □ □ 
□ □ □ □ =◦◦◦◦◦◦◦◦◦◦◦◦ □ □ □ □
 □ □ □ □ ◦◦◦◦◦◦◦◦◦◦◦◦□ □ □ □ 
□ □ □ □ □ #=□ □ □ □ □ □ □ □ □
 □ □ □   ___| |__ (_)  □ □ □ 
□ □ □ □ / _ \ '_ \| | □ □ □ □
 □ □ □ |  __/ |_) | |  □ □ □ 
□ □ □ □ \___|_.__/|_| □ □ □ □
 □ □ □ □ □ □ □ □ □ □ □ □ □ □ ";

pub const EBI_ITSELF: EbiCommand = EbiCommand::Group {
    name_short: "it",
    name_long: Some("itself"),
    explanation_short: "Print things on Ebi.",
    explanation_long: None,
    children: &[
        #[cfg(feature = "surface_generation")]
        &EBI_ITSELF_SURFACE,
        &EBI_ITSELF_GRAPH,
        &EBI_ITSELF_LOGO,
        &EBI_ITSELF_MANUAL,
        #[cfg(feature = "test_generation")]
        &EBI_ITSELF_TESTS,
    ],
};

#[cfg(feature = "surface_generation")]
pub const EBI_ITSELF_SURFACE: EbiCommand = EbiCommand::Group {
    name_short: "sf",
    name_long: Some("surface"),
    explanation_short: "Print files related to the surfaces of Ebi.",
    explanation_long: None,
    children: &[
        &EBI_ITSELF_SURFACE_JAVA,
        &EBI_ITSELF_SURFACE_JAVASCRIPT,
        &EBI_ITSELF_SURFACE_PYTHON,
        &EBI_ITSELF_SURFACE_WEBSITE,
    ],
};

#[cfg(feature = "surface_generation")]
pub const EBI_ITSELF_SURFACE_WEBSITE: EbiCommand = EbiCommand::Group {
    name_short: "ws",
    name_long: Some("website"),
    explanation_short: "Print the HTML website of Ebi.",
    explanation_long: None,
    children: &[
        &EBI_ITSELF_WEBSITE_COMMANDS,
        &EBI_ITSELF_WEBSITE_FILE_HANDLERS,
        &EBI_ITSELF_WEBSITE_HOME,
    ],
};

#[cfg(feature = "surface_generation")]
pub const EBI_ITSELF_WEBSITE_COMMANDS: EbiCommand = EbiCommand::Command {
    name_short: "comm",
    name_long: Some("commands"),
    explanation_short: "Print the HTML website of Ebi's commands.",
    explanation_long: None,
    latex_link: None,
    cli_command: None,
    exact_arithmetic: true,
    input_types: &[],
    input_names: &[],
    input_helps: &[],
    execute: |_, _| Ok(generate_commands()?),
    output_type: &EbiOutputType::String,
};

#[cfg(feature = "surface_generation")]
pub const EBI_ITSELF_WEBSITE_FILE_HANDLERS: EbiCommand = EbiCommand::Command {
    name_short: "fhs",
    name_long: Some("file-handlers"),
    explanation_short: "Print the HTML website of Ebi's file handlers.",
    explanation_long: None,
    latex_link: None,
    cli_command: None,
    exact_arithmetic: true,
    input_types: &[],
    input_names: &[],
    input_helps: &[],
    execute: |_, _| Ok(generate_file_handlers()?),
    output_type: &EbiOutputType::String,
};

#[cfg(feature = "surface_generation")]
pub const EBI_ITSELF_WEBSITE_HOME: EbiCommand = EbiCommand::Command {
    name_short: "home",
    name_long: None,
    explanation_short: "Print the HTML home of Ebi's website.",
    explanation_long: None,
    latex_link: None,
    cli_command: None,
    exact_arithmetic: true,
    input_types: &[],
    input_names: &[],
    input_helps: &[],
    execute: |_, _| Ok(generate_home_page()?),
    output_type: &EbiOutputType::String,
};

pub const EBI_ITSELF_LOGO: EbiCommand = EbiCommand::Command {
    name_short: "log",
    name_long: Some("logo"),
    explanation_short: "Print the logo of Ebi.",
    explanation_long: None,
    cli_command: None,
    latex_link: None,
    exact_arithmetic: true,
    input_types: &[],
    input_names: &[],
    input_helps: &[],
    execute: |_, _| Ok(EbiOutput::String(LOGO.to_string())),
    output_type: &EbiOutputType::String,
};

pub const EBI_ITSELF_MANUAL: EbiCommand = EbiCommand::Command {
    name_short: "man",
    name_long: Some("manual"),
    explanation_short:
        "Print the automatically generated parts of the manual of Ebi in Latex format.",
    explanation_long: None,
    cli_command: None,
    latex_link: None,
    exact_arithmetic: true,
    input_types: &[],
    input_names: &[],
    input_helps: &[],
    execute: |_, _| manual(),
    output_type: &EbiOutputType::String,
};

pub const EBI_ITSELF_GRAPH: EbiCommand = EbiCommand::Command {
    name_short: "graph",
    name_long: None,
    explanation_short: "Print the graph of Ebi.",
    explanation_long: None,
    cli_command: None,
    latex_link: None,
    exact_arithmetic: true,
    input_types: &[],
    input_names: &[],
    input_helps: &[],
    execute: |_, _| {
        let svg = graph()?.to_svg_mut()?;
        Ok(EbiOutput::Object(EbiObject::ScalableVectorGraphics(svg)))
    },
    output_type: &EbiOutputType::ObjectType(EbiObjectType::ScalableVectorGraphics),
};

#[cfg(feature = "surface_generation")]
pub const EBI_ITSELF_SURFACE_JAVA: EbiCommand = EbiCommand::Group {
    name_short: "java",
    name_long: None,
    explanation_short: "Print files for Java.",
    explanation_long: None,
    children: &[&EBI_ITSELF_JAVA_JAVA, &EBI_ITSELF_JAVA_RUST],
};

#[cfg(feature = "surface_generation")]
pub const EBI_ITSELF_JAVA_JAVA: EbiCommand = EbiCommand::Command {
    name_short: "java",
    name_long: None,
    explanation_short: "Print the Java classes for Java.",
    explanation_long: None,
    cli_command: None,
    latex_link: None,
    exact_arithmetic: true,
    input_types: &[],
    input_names: &[],
    input_helps: &[],
    execute: |_, _| Ok(generate_java_module_java()?),
    output_type: &EbiOutputType::String,
};

#[cfg(feature = "surface_generation")]
pub const EBI_ITSELF_JAVA_RUST: EbiCommand = EbiCommand::Command {
    name_short: "rust",
    name_long: None,
    explanation_short: "Print the Rust classes for Java.",
    explanation_long: None,
    cli_command: None,
    latex_link: None,
    exact_arithmetic: true,
    input_types: &[],
    input_names: &[],
    input_helps: &[],
    execute: |_, _| Ok(generate_java_module_rust()?),
    output_type: &EbiOutputType::String,
};

#[cfg(feature = "surface_generation")]
pub const EBI_ITSELF_SURFACE_JAVASCRIPT: EbiCommand = EbiCommand::Command {
    name_short: "js",
    name_long: Some("javascript"),
    explanation_short: "Print the rust code bindings for javascript.",
    explanation_long: None,
    cli_command: None,
    latex_link: None,
    exact_arithmetic: true,
    input_types: &[],
    input_names: &[],
    input_helps: &[],
    execute: |_, _| Ok(generate_javascript_rust()?),
    output_type: &EbiOutputType::String,
};

#[cfg(feature = "surface_generation")]
pub const EBI_ITSELF_SURFACE_PYTHON: EbiCommand = EbiCommand::Command {
    name_short: "python",
    name_long: None,
    explanation_short: "Generate the module exposed to Python with all functions.",
    explanation_long: None,
    cli_command: None,
    latex_link: None,
    exact_arithmetic: true,
    input_types: &[],
    input_names: &[],
    input_helps: &[],
    execute: |_, _| Ok(crate::python::python_module_generator::generate_pm4py_module()?),
    output_type: &EbiOutputType::String,
};

#[cfg(feature = "test_generation")]
pub const EBI_ITSELF_TESTS: EbiCommand = EbiCommand::Command {
    name_short: "tests",
    name_long: None,
    explanation_short: "Generate blanket tests for commands.",
    explanation_long: None,
    cli_command: None,
    latex_link: None,
    exact_arithmetic: true,
    input_types: &[],
    input_names: &[],
    input_helps: &[],
    execute: |_, _| {
        Ok(EbiOutput::String(
            crate::tests::tests_generator::generate_tests()?,
        ))
    },
    output_type: &EbiOutputType::String,
};
