use crate::ebi_framework::{
    ebi_command::EbiCommand,
    ebi_output::{EbiOutput, EbiOutputType},
    manual::{graph, html, manual},
};
#[cfg(feature = "java")]
use crate::prom::prom_plugin_generator::print_java_plugins;
use ebi_objects::{EbiObject, EbiObjectType, ebi_objects::scalable_vector_graphics::ToSVGMut};

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
        &EBI_ITSELF_GRAPH,
        &EBI_ITSELF_HTML,
        #[cfg(feature = "java")]
        &EBI_ITSELF_JAVA,
        &EBI_ITSELF_LOGO,
        &EBI_ITSELF_MANUAL,
        #[cfg(feature = "python")]
        &EBI_ITSELF_PYTHON,
    ],
};

pub const EBI_ITSELF_LOGO: EbiCommand = EbiCommand::Command {
    name_short: "log",
    name_long: Some("logo"),
    explanation_short: "Print the logo of Ebi.",
    explanation_long: None,
    cli_command: None,
    latex_link: None,
    exact_arithmetic: false,
    input_types: &[],
    input_names: &[],
    input_helps: &[],
    execute: |_, _| Ok(EbiOutput::String(LOGO.to_string())),
    output_type: &EbiOutputType::String,
};

pub const EBI_ITSELF_MANUAL: EbiCommand = EbiCommand::Command {
    name_short: "man",
    name_long: Some("manual"),
    explanation_short: "Print the automatically generated parts of the manual of Ebi in Latex format.",
    explanation_long: None,
    cli_command: None,
    latex_link: None,
    exact_arithmetic: false,
    input_types: &[],
    input_names: &[],
    input_helps: &[],
    execute: |_, _| Ok(manual()?),
    output_type: &EbiOutputType::String,
};

pub const EBI_ITSELF_GRAPH: EbiCommand = EbiCommand::Command {
    name_short: "graph",
    name_long: None,
    explanation_short: "Print the graph of Ebi.",
    explanation_long: None,
    cli_command: None,
    latex_link: None,
    exact_arithmetic: false,
    input_types: &[],
    input_names: &[],
    input_helps: &[],
    execute: |_, _| {
        let svg = graph()?.to_svg_mut()?;
        Ok(EbiOutput::Object(EbiObject::ScalableVectorGraphics(svg)))
    },
    output_type: &EbiOutputType::ObjectType(EbiObjectType::ScalableVectorGraphics),
};

#[cfg(feature = "java")]
pub const EBI_ITSELF_JAVA: EbiCommand = EbiCommand::Command {
    name_short: "java",
    name_long: None,
    explanation_short: "Print the classes for Java.",
    explanation_long: None,
    cli_command: None,
    latex_link: None,
    exact_arithmetic: false,
    input_types: &[],
    input_names: &[],
    input_helps: &[],
    execute: |_, _| Ok(print_java_plugins()?),
    output_type: &EbiOutputType::String,
};

pub const EBI_ITSELF_HTML: EbiCommand = EbiCommand::Command {
    name_short: "html",
    name_long: None,
    explanation_short: "Print parts of the website.",
    explanation_long: None,
    cli_command: None,
    latex_link: None,
    exact_arithmetic: false,
    input_types: &[],
    input_names: &[],
    input_helps: &[],
    execute: |_, _| Ok(EbiOutput::String(html())),
    output_type: &EbiOutputType::String,
};

#[cfg(feature = "python")]
pub const EBI_ITSELF_PYTHON: EbiCommand = EbiCommand::Command {
    name_short: "python",
    name_long: None,
    explanation_short: "Generate the module exposed to Python with all functions.",
    explanation_long: None,
    cli_command: None,
    latex_link: None,
    exact_arithmetic: false,
    input_types: &[],
    input_names: &[],
    input_helps: &[],
    execute: |_, _| Ok(crate::python::python_module_generator::generate_pm4py_module()?),
    output_type: &EbiOutputType::String,
};
