use layout::backends::svg::SVGWriter;
use anyhow::Result;
use svg2pdf::{ConversionOptions, PageOptions};

use crate::{ebi_framework::{ebi_command::EbiCommand, ebi_input::{EbiInput, EbiInputType}, ebi_object::EbiObject, ebi_output::{EbiOutput, EbiOutputType}, ebi_trait::EbiTrait}, ebi_traits::ebi_trait_graphable::EbiTraitGraphable};

pub const EBI_VISUALISE: EbiCommand = EbiCommand::Group { 
    name_short: "vis", 
    name_long: Some("visualise"),
    explanation_short: "Visualse an object.", 
    explanation_long: None,
    children: &[
        &EBI_VISUALISE_PDF,
        &EBI_VISUALISE_SVG,
        &EBI_VISUALISE_TEXT
    ]
};

pub const EBI_VISUALISE_TEXT: EbiCommand = EbiCommand::Command { 
    name_short: "txt", 
    name_long: Some("text"),
    explanation_short: "Visualise an object as text.",
    explanation_long: None, 
    latex_link: None, 
    cli_command: None, 
    exact_arithmetic: false, 
    input_types: &[ &[&EbiInputType::AnyObject] ], 
    input_names: &[ "FILE" ], 
    input_helps: &[ "Any file that can be visualised textually." ], 
    execute: |mut inputs, _| {
        let result = match inputs.remove(0) {
                EbiInput::Object(EbiObject::StochasticLabelledPetriNet(slpn), _) => slpn.to_string(),
                EbiInput::Object(EbiObject::LabelledPetriNet(lpn), _) => lpn.to_string(),
                EbiInput::Object(EbiObject::FiniteStochasticLanguage(lang), _) => lang.to_string(),
                EbiInput::Object(EbiObject::StochasticDeterministicFiniteAutomaton(sdfa), _) => sdfa.to_string(),
                EbiInput::Object(EbiObject::EventLog(log), _) => log.to_string(),
                EbiInput::Object(EbiObject::FiniteLanguage(language), _) => language.to_string(),
                EbiInput::Object(EbiObject::DirectlyFollowsModel(d), _) => d.to_string(),
                EbiInput::Object(EbiObject::LanguageOfAlignments(a), _) => a.to_string(),
                EbiInput::Object(EbiObject::StochasticLanguageOfAlignments(a), _) => a.to_string(),
                EbiInput::Object(EbiObject::DeterministicFiniteAutomaton(s), _) => s.to_string(),
                EbiInput::Object(EbiObject::ProcessTree(pt), _) => pt.to_string(),
                EbiInput::Object(EbiObject::Executions(s), _) => s.to_string(),
                EbiInput::FileHandler(_) => unreachable!(),
                EbiInput::Trait(_, _) => unreachable!(),
                EbiInput::String(_) => unreachable!(),
                EbiInput::Usize(_) => unreachable!(),
                EbiInput::Fraction(_) => unreachable!(),
        };
        Ok(EbiOutput::String(result))
    }, 
    output_type: &EbiOutputType::String
};

pub const EBI_VISUALISE_SVG: EbiCommand = EbiCommand::Command { 
    name_short: "svg", 
    name_long: None, 
    explanation_short: "Visualise an object as scalable vector graphics.",
    explanation_long: None, 
    latex_link: None, 
    cli_command: None, 
    exact_arithmetic: true, 
    input_types: &[ &[ &EbiInputType::Trait(EbiTrait::Graphable), ] ], 
    input_names: &[ "FILE" ], 
    input_helps: &[ "Any file that can be visualised as a graph." ], 
    execute: |mut inputs, _| {
        let result = inputs.remove(0).to_type::<dyn EbiTraitGraphable>()?;

        let mut svg = SVGWriter::new();
        result.to_dot().do_it(false, false, false, &mut svg);

        return Ok(EbiOutput::SVG(svg.finalize()));
    }, 
    output_type: &EbiOutputType::SVG
};

pub const EBI_VISUALISE_PDF: EbiCommand = EbiCommand::Command { 
    name_short: "pdf", 
    name_long: None, 
    explanation_short: "Visualise an object as portable document format.",
    explanation_long: None, 
    latex_link: None, 
    cli_command: None, 
    exact_arithmetic: true, 
    input_types: &[ &[ &EbiInputType::Trait(EbiTrait::Graphable), ] ], 
    input_names: &[ "FILE" ], 
    input_helps: &[ "Any file that can be visualised as a graph." ], 
    execute: |mut inputs, _| {
        let result = inputs.remove(0).to_type::<dyn EbiTraitGraphable>()?;

        let mut svg = SVGWriter::new();
        result.to_dot().do_it(false, false, false, &mut svg);
        let svg_string = svg.finalize();

        let pdf = svg_to_pdf(&svg_string)?;

        return Ok(EbiOutput::PDF(pdf));
    
    }, 
    output_type: &EbiOutputType::PDF
};

pub fn svg_to_pdf(svg: &str) -> Result<Vec<u8>> {
    let mut options = svg2pdf::usvg::Options::default();
    options.fontdb_mut().load_system_fonts();
    let tree = svg2pdf::usvg::Tree::from_str(svg, &options)?;
    Ok(svg2pdf::to_pdf(&tree, ConversionOptions::default(), PageOptions::default()).unwrap())
}