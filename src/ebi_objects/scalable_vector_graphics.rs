use crate::{
    ebi_framework::{
        ebi_file_handler::EbiFileHandler,
        ebi_object::EbiObject,
        ebi_output::{EbiExporter, EbiObjectExporter, EbiOutput},
    },
    ebi_traits::ebi_trait_graphable::EbiTraitGraphable,
};
use anyhow::{Result, anyhow};
use layout::backends::svg::SVGWriter;
use svg2pdf::{ConversionOptions, PageOptions};

pub const FORMAT_SPECIFICATION: &str = "Ebi does not support importing of SVG or PDF files. However, if you run a command that outputs something that can be made into a graph (Graphable), then providing the \\verb=-o= parameter with a .svg or .pdf extension will output a picture of the output.";

pub const EBI_SCALABLE_VECTOR_GRAPHICS: EbiFileHandler = EbiFileHandler {
    name: "scalable vector graphics",
    article: "a",
    file_extension: "svg",
    format_specification: &FORMAT_SPECIFICATION,
    validator: None,
    trait_importers: &[],
    object_importers: &[],
    object_exporters: &[
        EbiObjectExporter::DeterministicFiniteAutomaton(export_from_object_svg),
        EbiObjectExporter::DirectlyFollowsModel(export_from_object_svg),
        EbiObjectExporter::LabelledPetriNet(export_from_object_svg),
        EbiObjectExporter::ProcessTree(export_from_object_svg),
        EbiObjectExporter::StochasticDeterministicFiniteAutomaton(export_from_object_svg),
        EbiObjectExporter::StochasticLabelledPetriNet(export_from_object_svg),
        EbiObjectExporter::StochasticProcessTree(export_from_object_svg),
    ],
    java_object_handlers: &[],
};

pub const EBI_PORTABLE_DOCUMENT_FORMAT: EbiFileHandler = EbiFileHandler {
    name: "portable document format",
    article: "a",
    file_extension: "pdf",
    format_specification: &FORMAT_SPECIFICATION,
    validator: None,
    trait_importers: &[],
    object_importers: &[],
    object_exporters: &[
        EbiObjectExporter::DeterministicFiniteAutomaton(export_from_object_pdf),
        EbiObjectExporter::DirectlyFollowsModel(export_from_object_pdf),
        EbiObjectExporter::LabelledPetriNet(export_from_object_pdf),
        EbiObjectExporter::ProcessTree(export_from_object_pdf),
        EbiObjectExporter::StochasticDeterministicFiniteAutomaton(export_from_object_pdf),
        EbiObjectExporter::StochasticLabelledPetriNet(export_from_object_pdf),
        EbiObjectExporter::StochasticProcessTree(export_from_object_pdf),
    ],
    java_object_handlers: &[],
};

fn export_from_object_svg(object: EbiOutput, f: &mut dyn std::io::Write) -> Result<()> {
    match object {
        EbiOutput::Object(EbiObject::DeterministicFiniteAutomaton(object)) => {
            export_as_svg(&object, f)
        }
        EbiOutput::Object(EbiObject::DirectlyFollowsModel(object)) => export_as_svg(&object, f),
        EbiOutput::Object(EbiObject::EventLog(_)) => {
            Err(anyhow!("cannot transform event log into SVG"))
        }
        EbiOutput::Object(EbiObject::Executions(_)) => {
            Err(anyhow!("cannot transform executions into SVG"))
        }
        EbiOutput::Object(EbiObject::FiniteLanguage(_)) => {
            Err(anyhow!("cannot transform finite language into SVG"))
        }
        EbiOutput::Object(EbiObject::FiniteStochasticLanguage(_)) => {
            Err(anyhow!("cannot transform finite stochastic into SVG"))
        }
        EbiOutput::Object(EbiObject::LabelledPetriNet(object)) => export_as_svg(&object, f),
        EbiOutput::Object(EbiObject::LanguageOfAlignments(_)) => {
            Err(anyhow!("cannot transform language of alignments into SVG"))
        }
        EbiOutput::Object(EbiObject::ProcessTree(object)) => export_as_svg(&object, f),
        EbiOutput::Object(EbiObject::StochasticDeterministicFiniteAutomaton(object)) => {
            export_as_svg(&object, f)
        }
        EbiOutput::Object(EbiObject::StochasticLabelledPetriNet(object)) => {
            export_as_svg(&object, f)
        }
        EbiOutput::Object(EbiObject::StochasticLanguageOfAlignments(_)) => Err(anyhow!(
            "cannot transform stochastic language of alignments into SVG"
        )),
        EbiOutput::Object(EbiObject::StochasticProcessTree(object)) => export_as_svg(&object, f),
        EbiOutput::String(_) => Err(anyhow!("cannot transform string into SVG")),
        EbiOutput::SVG(string) => EbiExporter::SVG.export_from_object(EbiOutput::SVG(string), f),
        EbiOutput::PDF(_) => Err(anyhow!("cannot transform PDF into SVG")),
        EbiOutput::Usize(_) => Err(anyhow!("cannot transform usize into SVG")),
        EbiOutput::Fraction(_) => Err(anyhow!("cannot transform fraction into SVG")),
        EbiOutput::LogDiv(_) => Err(anyhow!("cannot transform lopdiv into SVG")),
        EbiOutput::ContainsRoot(_) => Err(anyhow!("cannot transform containsroot into SVG")),
        EbiOutput::RootLogDiv(_) => Err(anyhow!("cannot transform rootlogdiv into SVG")),
        EbiOutput::Bool(_) => Err(anyhow!("cannot transform boolean into SVG")),
    }
}

fn export_from_object_pdf(object: EbiOutput, f: &mut dyn std::io::Write) -> Result<()> {
    match object {
        EbiOutput::Object(EbiObject::DeterministicFiniteAutomaton(object)) => {
            export_as_pdf(&object, f)
        }
        EbiOutput::Object(EbiObject::DirectlyFollowsModel(object)) => export_as_pdf(&object, f),
        EbiOutput::Object(EbiObject::EventLog(_)) => {
            Err(anyhow!("cannot transform event log into PDF"))
        }
        EbiOutput::Object(EbiObject::Executions(_)) => {
            Err(anyhow!("cannot transform executions into PDF"))
        }
        EbiOutput::Object(EbiObject::FiniteLanguage(_)) => {
            Err(anyhow!("cannot transform finite language into PDF"))
        }
        EbiOutput::Object(EbiObject::FiniteStochasticLanguage(_)) => {
            Err(anyhow!("cannot transform finite stochastic into PDF"))
        }
        EbiOutput::Object(EbiObject::LabelledPetriNet(object)) => export_as_pdf(&object, f),
        EbiOutput::Object(EbiObject::LanguageOfAlignments(_)) => {
            Err(anyhow!("cannot transform language of alignments into PDF"))
        }
        EbiOutput::Object(EbiObject::ProcessTree(object)) => export_as_pdf(&object, f),
        EbiOutput::Object(EbiObject::StochasticDeterministicFiniteAutomaton(object)) => {
            export_as_pdf(&object, f)
        }
        EbiOutput::Object(EbiObject::StochasticLabelledPetriNet(object)) => {
            export_as_pdf(&object, f)
        }
        EbiOutput::Object(EbiObject::StochasticLanguageOfAlignments(_)) => Err(anyhow!(
            "cannot transform stochastic language of alignments into PDF"
        )),
        EbiOutput::Object(EbiObject::StochasticProcessTree(object)) => export_as_pdf(&object, f),
        EbiOutput::String(_) => Err(anyhow!("cannot transform string into PDF")),
        EbiOutput::SVG(string) => {
            EbiExporter::PDF.export_from_object(EbiOutput::PDF(svg_to_pdf(&string)?), f)
        }
        EbiOutput::PDF(pdf) => EbiExporter::PDF.export_from_object(EbiOutput::PDF(pdf), f),
        EbiOutput::Usize(_) => Err(anyhow!("cannot transform usize into PDF")),
        EbiOutput::Fraction(_) => Err(anyhow!("cannot transform fraction into PDF")),
        EbiOutput::LogDiv(_) => Err(anyhow!("cannot transform lopdiv into PDF")),
        EbiOutput::ContainsRoot(_) => Err(anyhow!("cannot transform containsroot into PDF")),
        EbiOutput::RootLogDiv(_) => Err(anyhow!("cannot transform rootlogdiv into PDF")),
        EbiOutput::Bool(_) => Err(anyhow!("cannot transform boolean into PDF")),
    }
}

pub fn empty() -> String {
    "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\"?><svg width=\"352.5\" height=\"141\" viewBox=\"0 0 352.5 141\" xmlns=\"http://www.w3.org/2000/svg\"></svg>".to_string()
}

pub fn export_as_svg<T>(object: &T, f: &mut dyn std::io::Write) -> Result<()>
where
    T: EbiTraitGraphable,
{
    let output = EbiOutput::SVG(to_svg_string(object)?);
    EbiExporter::SVG.export_from_object(output, f)
}

pub fn export_as_pdf<T>(object: &T, f: &mut dyn std::io::Write) -> Result<()>
where
    T: EbiTraitGraphable,
{
    let svg_string = to_svg_string(object)?;
    let pdf = svg_to_pdf(&svg_string)?;
    let output = EbiOutput::PDF(pdf);
    EbiExporter::PDF.export_from_object(output, f)
}

pub fn to_svg_string_box(object: Box<dyn EbiTraitGraphable>) -> Result<String>
{
    let mut svg = SVGWriter::new();
    let mut graph = object.to_dot()?;
    Ok(if graph.num_nodes() == 0 {
        empty()
    } else {
        graph.do_it(false, false, false, &mut svg);
        svg.finalize()
    })
}

pub fn to_svg_string<T>(object: &T) -> Result<String>
where
    T: EbiTraitGraphable,
{
    let mut svg = SVGWriter::new();
    let mut graph = object.to_dot()?;
    Ok(if graph.num_nodes() == 0 {
        empty()
    } else {
        graph.do_it(false, false, false, &mut svg);
        svg.finalize()
    })
}

pub fn svg_to_pdf(svg: &str) -> Result<Vec<u8>> {
    let mut options = svg2pdf::usvg::Options::default();
    options.fontdb_mut().load_system_fonts();
    let tree = svg2pdf::usvg::Tree::from_str(svg, &options)?;
    Ok(svg2pdf::to_pdf(&tree, ConversionOptions::default(), PageOptions::default()).unwrap())
}
