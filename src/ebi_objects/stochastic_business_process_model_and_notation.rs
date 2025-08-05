use anyhow::{Result, anyhow};
use quick_xml::Reader as XmlReader;
use quick_xml::events::Event;
use crate::math::fraction::Fraction;
use std::fmt::Display;

use crate::ebi_framework::{
    ebi_file_handler::EbiFileHandler,
    ebi_input::{self, EbiInput, EbiObjectImporter},
    ebi_object::EbiObject,
    ebi_output::{EbiObjectExporter, EbiOutput},
    ebi_trait::FromEbiTraitObject,
    exportable::Exportable,
    importable::Importable,
    infoable::Infoable,
};

pub const FORMAT_SPECIFICATION: &str =
    "A Business Process Modelling and Notation (BPMN) file follows the BPMN 2.0.2 XML format~\\cite{bpmn}.
    Please note that Ebi supports only a small subset of BPMN elements.
For instance:
    \\lstinputlisting[language=xml, style=boxed]{../testfiles/a.bpmn}";

pub const EBI_STOCHASTIC_BUSINESS_PROCESS_MODEL_AND_NOTATION: EbiFileHandler = EbiFileHandler {
    name: "stochastic business process model and notation",
    article: "a",
    file_extension: "sbpmn",
    is_binary: false,
    format_specification: &FORMAT_SPECIFICATION,
    validator: Some(ebi_input::validate::<StochasticBusinessProcessModelAndNotation>),
    trait_importers: &[],
    object_importers: &[
        EbiObjectImporter::StochasticBusinessProcessModelAndNotation(
            StochasticBusinessProcessModelAndNotation::import_as_object,
        ),
    ],
    object_exporters: &[
        EbiObjectExporter::StochasticBusinessProcessModelAndNotation(
            StochasticBusinessProcessModelAndNotation::export_from_object,
        ),
    ],
    java_object_handlers: &[],
};

#[derive(Debug, Clone)]
pub struct StochasticTask {
    pub id: String,
    pub weight: Fraction,
}

#[derive(Debug, Clone)]
pub struct StochasticSequenceFlow {
    pub source_id: String,
    pub target_id: String,
    pub weight: Fraction,
}

#[derive(Clone, Debug)]
pub struct StochasticBusinessProcessModelAndNotation {
    pub tasks: Vec<StochasticTask>,
    pub start_event: String,
    pub xor_gateways: Vec<String>,
    pub and_gateways: Vec<String>,
    pub or_gateways: Vec<String>,
    pub end_events: usize,
    pub sequence_flows: Vec<StochasticSequenceFlow>,
}

impl Importable for StochasticBusinessProcessModelAndNotation {
    fn import_as_object(reader: &mut dyn std::io::BufRead) -> anyhow::Result<EbiObject> {
        Ok(EbiObject::StochasticBusinessProcessModelAndNotation(
            Self::import(reader)?,
        ))
    }

    fn import(reader: &mut dyn std::io::BufRead) -> anyhow::Result<Self>
    where
        Self: Sized,
    {
        let mut xml_data = String::new();
        reader.read_to_string(&mut xml_data)?;
        let mut xml_reader = XmlReader::from_str(&xml_data);
        let mut buf = Vec::new();
        let mut tasks = Vec::new();
        let mut sequence_flows = Vec::new();
        let mut start_event = String::new();
        let mut xor_gateways = Vec::new();
        let mut and_gateways = Vec::new();
        let mut or_gateways = Vec::new();
        let mut end_events = 0;
        while let Ok(event) = xml_reader.read_event_into(&mut buf) {
            match event {
                Event::Start(ref e) | Event::Empty(ref e) => {
                    let tag = xml_reader.decoder().decode(e.name().into_inner()).unwrap();
                    let tag = tag.split(':').last().unwrap();
                    match tag {
                        "startEvent" => {
                            for attr in e.attributes().flatten() {
                                if attr.key.as_ref() == b"id" {
                                    start_event = attr.unescape_value().unwrap().to_string();
                                }
                            }
                        }
                        "endEvent" => {
                            end_events += 1;
                        }
                        "task" => {
                            let mut id = String::new();
                            let mut weight = Fraction::from(1);
                            for attr in e.attributes().flatten() {
                                match attr.key.as_ref() {
                                    b"id" => id = attr.unescape_value().unwrap().to_string(),
                                    b"weight" => {
                                        weight = attr.unescape_value().unwrap().parse().unwrap_or(Fraction::from(1))
                                    }
                                    _ => {}
                                }
                            }
                            tasks.push(StochasticTask { id, weight });
                        }
                        "exclusiveGateway" => {
                            for attr in e.attributes().flatten() {
                                if attr.key.as_ref() == b"id" {
                                    xor_gateways.push(attr.unescape_value().unwrap().to_string());
                                }
                            }
                        }
                        "parallelGateway" => {
                            for attr in e.attributes().flatten() {
                                if attr.key.as_ref() == b"id" {
                                    and_gateways.push(attr.unescape_value().unwrap().to_string());
                                }
                            }
                        }
                        "inclusiveGateway" => {
                            for attr in e.attributes().flatten() {
                                if attr.key.as_ref() == b"id" {
                                    or_gateways.push(attr.unescape_value().unwrap().to_string());
                                }
                            }
                        }
                        "sequenceFlow" => {
                            let mut source_id = String::new();
                            let mut target_id = String::new();
                            let mut weight = Fraction::from(1);
                            for attr in e.attributes().flatten() {
                                match attr.key.as_ref() {
                                    b"sourceRef" => source_id = attr.unescape_value().unwrap().to_string(),
                                    b"targetRef" => target_id = attr.unescape_value().unwrap().to_string(),
                                    b"weight" => {
                                        weight = attr.unescape_value().unwrap().parse().unwrap_or(Fraction::from(1));
                                    }
                                    _ => {}
                                }
                            }
                            sequence_flows.push(StochasticSequenceFlow {
                                source_id,
                                target_id,
                                weight,
                            });
                        }
                        _ => {}
                    }
                }
                Event::Eof => break,
                _ => {}
            }
            buf.clear();
        }
        Ok(StochasticBusinessProcessModelAndNotation {
            tasks,
            start_event,
            xor_gateways,
            and_gateways,
            or_gateways,
            end_events,
            sequence_flows,
        })
    }
}

impl FromEbiTraitObject for StochasticBusinessProcessModelAndNotation {
    fn from_trait_object(object: ebi_input::EbiInput) -> Result<Box<Self>> {
        match object {
            EbiInput::Object(EbiObject::StochasticBusinessProcessModelAndNotation(e), _) => {
                Ok(Box::new(e))
            }
            _ => Err(anyhow!(
                "cannot read {} {} as a stochastic business process model and notation",
                object.get_type().get_article(),
                object.get_type()
            )),
        }
    }
}

impl Exportable for StochasticBusinessProcessModelAndNotation {
    fn export_from_object(object: EbiOutput, f: &mut dyn std::io::Write) -> anyhow::Result<()> {
        match object {
            EbiOutput::Object(EbiObject::StochasticBusinessProcessModelAndNotation(bpmn)) => {
                bpmn.export(f)
            }
            _ => Err(anyhow!("Cannot export object as BPMN.")),
        }
    }

    fn export(&self, f: &mut dyn std::io::Write) -> anyhow::Result<()> {
        Ok(write!(f, "{}", self)?)
    }
}

impl Display for StochasticBusinessProcessModelAndNotation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "Stochastic Business Process Model and Notation")?;
        writeln!(f, "Start Event: {}", self.start_event)?;
        writeln!(f, "Tasks:")?;
        for task in &self.tasks {
            writeln!(f, "  - {} (weight: {})", task.id, task.weight)?;
        }
        writeln!(f, "XOR Gateways: {}", self.xor_gateways.len())?;
        writeln!(f, "AND Gateways: {}", self.and_gateways.len())?;
        writeln!(f, "OR Gateways: {}", self.or_gateways.len())?;
        writeln!(f, "End Events: {}", self.end_events)?;
        writeln!(f, "Sequence Flows:")?;
        for sf in &self.sequence_flows {
            writeln!(f, "  - {} -> {} (weight: {})", sf.source_id, sf.target_id, sf.weight)?;
        }
        Ok(())
    }
}

impl Infoable for StochasticBusinessProcessModelAndNotation {
    fn info(&self, f: &mut impl std::io::Write) -> anyhow::Result<()> {
        writeln!(f, "Stochastic Business Process Model and Notation:")?;
        writeln!(f, "  Start Event: {}", self.start_event)?;
        writeln!(f, "  Tasks: {}", self.tasks.len())?;
        writeln!(f, "  XOR Gateways: {}", self.xor_gateways.len())?;
        writeln!(f, "  AND Gateways: {}", self.and_gateways.len())?;
        writeln!(f, "  OR Gateways: {}", self.or_gateways.len())?;
        writeln!(f, "  End Events: {}", self.end_events)?;
        writeln!(f, "  Sequence Flows: {}", self.sequence_flows.len())?;
        Ok(())
    }
}