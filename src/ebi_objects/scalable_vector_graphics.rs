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

pub const FORMAT_SPECIFICATION: &str = "Ebi does not support importing of SVG files.";

pub const EBI_SCALABLE_VECTOR_GRAPHICS: EbiFileHandler = EbiFileHandler {
    name: "svg",
    article: "an",
    file_extension: "svg",
    format_specification: &FORMAT_SPECIFICATION,
    validator: None,
    trait_importers: &[],
    object_importers: &[],
    object_exporters: &[
        EbiObjectExporter::DeterministicFiniteAutomaton(export_from_object),
        EbiObjectExporter::DirectlyFollowsModel(export_from_object),
        EbiObjectExporter::LabelledPetriNet(export_from_object),
        EbiObjectExporter::ProcessTree(export_from_object),
        EbiObjectExporter::StochasticDeterministicFiniteAutomaton(export_from_object),
        EbiObjectExporter::StochasticLabelledPetriNet(export_from_object),
        EbiObjectExporter::StochasticProcessTree(export_from_object),
    ],
    java_object_handlers: &[],
};

fn export_from_object(object: EbiOutput, f: &mut dyn std::io::Write) -> Result<()> {
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

fn empty() -> String {
    "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\"?><svg width=\"352.5\" height=\"141\" viewBox=\"0 0 352.5 141\" xmlns=\"http://www.w3.org/2000/svg\"></svg>".to_string()
}

fn export_as_svg<T>(object: &T, f: &mut dyn std::io::Write) -> Result<()>
where
    T: EbiTraitGraphable,
{
    let mut svg = SVGWriter::new();
    let mut graph = object.to_dot()?;
    let output = EbiOutput::SVG(if graph.num_nodes() == 0 {
        empty()
    } else {
        graph.do_it(false, false, false, &mut svg);
        svg.finalize()
    });
    EbiExporter::SVG.export_from_object(output, f)
}
