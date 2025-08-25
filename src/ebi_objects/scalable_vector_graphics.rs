use std::fmt::Display;

use crate::{
    ebi_framework::{
        ebi_file_handler::EbiFileHandler,
        ebi_object::EbiObject,
        ebi_output::{EbiObjectExporter, EbiOutput},
        exportable::Exportable,
        infoable::Infoable,
    },
    ebi_traits::ebi_trait_graphable::EbiTraitGraphable,
};
use anyhow::{Result, anyhow};
use layout::{backends::svg::SVGWriter, topo::layout::VisualGraph};

pub const FORMAT_SPECIFICATION: &str = "Ebi does not support importing of SVG files.";

pub const EBI_SCALABLE_VECTOR_GRAPHICS: EbiFileHandler = EbiFileHandler {
    name: "scalable vector graphics",
    article: "a",
    file_extension: "svg",
    is_binary: false,
    format_specification: &FORMAT_SPECIFICATION,
    validator: None,
    trait_importers: &[],
    object_importers: &[],
    object_exporters: &[
        EbiObjectExporter::DeterministicFiniteAutomaton(ScalableVectorGraphics::export_from_object),
        EbiObjectExporter::DirectlyFollowsModel(ScalableVectorGraphics::export_from_object),
        EbiObjectExporter::DirectlyFollowsGraph(ScalableVectorGraphics::export_from_object),
        EbiObjectExporter::LabelledPetriNet(ScalableVectorGraphics::export_from_object),
        EbiObjectExporter::ProcessTree(ScalableVectorGraphics::export_from_object),
        EbiObjectExporter::StochasticDeterministicFiniteAutomaton(
            ScalableVectorGraphics::export_from_object,
        ),
        EbiObjectExporter::StochasticLabelledPetriNet(ScalableVectorGraphics::export_from_object),
        EbiObjectExporter::StochasticProcessTree(ScalableVectorGraphics::export_from_object),
        EbiObjectExporter::ScalableVectorGraphics(ScalableVectorGraphics::export_from_object),
    ],
    java_object_handlers: &[
    //     JavaObjectHandler {
    //     name: "svg",
    //     java_class: "com.kitfox.svg.SVGDiagram",
    //     translator_ebi_to_java: Some("org.processmining.ebi.objects.EbiSvg.fromEbiString"),
    //     translator_java_to_ebi: None,
    //     input_gui: None,
    // }
    ],
};

#[derive(Clone)]
pub struct ScalableVectorGraphics(pub(crate) String);

impl ScalableVectorGraphics {
    pub fn empty() -> Self {
        Self("<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\"?><svg width=\"352.5\" height=\"141\" viewBox=\"0 0 352.5 141\" xmlns=\"http://www.w3.org/2000/svg\"></svg>".to_string())
    }

    pub fn from_graphable(object: Box<dyn EbiTraitGraphable>) -> Result<Self> {
        let mut svg = SVGWriter::new();
        let mut graph = object.to_dot()?;
        Ok(if graph.num_nodes() == 0 {
            Self::empty()
        } else {
            graph.do_it(false, false, false, &mut svg);
            ScalableVectorGraphics(svg.finalize())
        })
    }
}

impl Display for ScalableVectorGraphics {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.0.fmt(f)
    }
}

impl Infoable for ScalableVectorGraphics {
    fn info(&self, f: &mut impl std::io::Write) -> Result<()> {
        self.0.info(f)
    }
}

impl Exportable for ScalableVectorGraphics {
    fn export_from_object(object: EbiOutput, f: &mut dyn std::io::Write) -> Result<()> {
        match object {
            EbiOutput::Object(EbiObject::DeterministicFiniteAutomaton(object)) => {
                object.to_svg()?.export(f)
            }
            EbiOutput::Object(EbiObject::DirectlyFollowsModel(object)) => {
                object.to_svg()?.export(f)
            }
            EbiOutput::Object(EbiObject::StochasticDirectlyFollowsModel(object)) => {
                object.to_svg()?.export(f)
            }
            EbiOutput::Object(EbiObject::EventLog(_)) => {
                Err(anyhow!("cannot export event log as SVG"))
            }
            EbiOutput::Object(EbiObject::Executions(_)) => {
                Err(anyhow!("cannot export executions as SVG"))
            }
            EbiOutput::Object(EbiObject::FiniteLanguage(_)) => {
                Err(anyhow!("cannot export finite language as SVG"))
            }
            EbiOutput::Object(EbiObject::FiniteStochasticLanguage(_)) => {
                Err(anyhow!("cannot export finite stochastic as SVG"))
            }
            EbiOutput::Object(EbiObject::LabelledPetriNet(object)) => object.to_svg()?.export(f),
            EbiOutput::Object(EbiObject::LanguageOfAlignments(_)) => {
                Err(anyhow!("cannot export language of alignments as SVG"))
            }
            EbiOutput::Object(EbiObject::ProcessTree(object)) => object.to_svg()?.export(f),
            EbiOutput::Object(EbiObject::StochasticDeterministicFiniteAutomaton(object)) => {
                object.to_svg()?.export(f)
            }
            EbiOutput::Object(EbiObject::StochasticLabelledPetriNet(object)) => {
                object.to_svg()?.export(f)
            }
            EbiOutput::Object(EbiObject::StochasticLanguageOfAlignments(_)) => Err(anyhow!(
                "cannot export stochastic language of alignments as SVG"
            )),
            EbiOutput::Object(EbiObject::StochasticProcessTree(object)) => {
                object.to_svg()?.export(f)
            }
            EbiOutput::Object(EbiObject::DirectlyFollowsGraph(object)) => {
                object.to_svg()?.export(f)
            }
            EbiOutput::Object(EbiObject::ScalableVectorGraphics(object)) => object.export(f),
            EbiOutput::String(_) => Err(anyhow!("cannot export string as SVG")),
            EbiOutput::Usize(_) => Err(anyhow!("cannot export usize as SVG")),
            EbiOutput::Fraction(_) => Err(anyhow!("cannot export fraction as SVG")),
            EbiOutput::LogDiv(_) => Err(anyhow!("cannot export lopdiv as SVG")),
            EbiOutput::ContainsRoot(_) => Err(anyhow!("cannot export containsroot as SVG")),
            EbiOutput::RootLogDiv(_) => Err(anyhow!("cannot export rootlogdiv as SVG")),
            EbiOutput::Bool(_) => Err(anyhow!("cannot export boolean as SVG")),
        }
    }

    fn export(&self, f: &mut dyn std::io::Write) -> Result<()> {
        Ok(write!(f, "{}", self)?)
    }
}

impl From<String> for ScalableVectorGraphics {
    fn from(value: String) -> Self {
        Self(value)
    }
}

pub trait ToSVG {
    fn to_svg(&self) -> Result<ScalableVectorGraphics>;
}

impl<T> ToSVG for T
where
    T: EbiTraitGraphable,
{
    fn to_svg(&self) -> Result<ScalableVectorGraphics> {
        let mut svg = SVGWriter::new();
        let mut graph = self.to_dot()?;
        Ok(if graph.num_nodes() == 0 {
            ScalableVectorGraphics::empty()
        } else {
            graph.do_it(false, false, false, &mut svg);
            ScalableVectorGraphics(svg.finalize())
        })
    }
}

pub trait ToSVGMut {
    fn to_svg_mut(&mut self) -> Result<ScalableVectorGraphics>;
}

impl ToSVGMut for VisualGraph {
    fn to_svg_mut(&mut self) -> Result<ScalableVectorGraphics> {
        let mut svg = SVGWriter::new();
        self.do_it(false, false, false, &mut svg);
        Ok(svg.finalize().into())
    }
}
