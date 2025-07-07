use anyhow::{Result, anyhow};
use clap::builder::Str;
use std::fmt::Display;
use std::collections::HashMap;
use quick_xml::Reader;
use quick_xml::events::Event;


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

pub const EBI_BUSINESS_PROCESS_MODEL_AND_NOTATION: EbiFileHandler = EbiFileHandler {
    name: "business process model and notation",
    article: "a",
    file_extension: "bpmn",
    is_binary: false,
    format_specification: &FORMAT_SPECIFICATION,
    validator: Some(ebi_input::validate::<BusinessProcessModelAndNotation>),
    trait_importers: &[],
    object_importers: &[EbiObjectImporter::BusinessProcessModelAndNotation(
        BusinessProcessModelAndNotation::import_as_object,
    )],
    object_exporters: &[EbiObjectExporter::BusinessProcessModelAndNotation(
        BusinessProcessModelAndNotation::export_from_object,
    )],
    java_object_handlers: &[],
};

#[derive(Debug, Clone)]
pub struct BusinessProcessModelAndNotation {
    nodes: Vec<BPMNNode>,
    start_event: BPMNNode,
    xor_gateways: Vec<BPMNGateway>,
    and_gateways: Vec<BPMNGateway>,
    or_gateways: Vec<BPMNGateway>,
    end_events: Vec<BPMNNode>,
    sequence_flows: Vec<SequenceFlow>,
    label: HashMap<String, String>,
}

#[derive(Debug, Eq, Hash, PartialEq, Clone)]
pub struct BPMNNode {
    id: String,
    incoming: Vec<SequenceFlow>,
    outgoing: Vec<SequenceFlow>,
}

#[derive(Debug, Eq, Hash, PartialEq, Clone)]
pub struct BPMNGateway {
    id: String,
    direction: GatewayDirection,
    incoming: Vec<SequenceFlow>,
    outgoing: Vec<SequenceFlow>,
}

#[derive(Debug, Eq, Hash, PartialEq, Clone)]
pub struct SequenceFlow {
    source_id: String,
    target_id: String,
}


#[derive(Eq, Hash, Debug, Clone, PartialEq)]
enum GatewayDirection {
    Diverging,
    Converging,
}


impl Importable for BusinessProcessModelAndNotation {
    fn import_as_object(reader: &mut dyn std::io::BufRead) -> anyhow::Result<EbiObject> {
        Ok(EbiObject::BusinessProcessModelAndNotation(Self::import(
            reader,
        )?))
    }

    fn import(reader: &mut dyn std::io::BufRead) -> anyhow::Result<Self>
    where
        Self: Sized,
    {
        let mut xml_data = String::new();
        reader.read_to_string(&mut xml_data)?;

        let mut xml_reader = Reader::from_str(&xml_data);
        let mut buf = Vec::new();

        let mut nodes: HashMap<String, BPMNNode> = HashMap::new();
        let mut xor_gateways: Vec<BPMNGateway> = Vec::new();
        let mut and_gateways: Vec<BPMNGateway> = Vec::new();
        let mut or_gateways: Vec<BPMNGateway> = Vec::new();
        let mut end_events: Vec<BPMNNode> = Vec::new();
        let mut sequence_flows: Vec<SequenceFlow> = Vec::new();
        let mut label: HashMap<String, String> = HashMap::new();
        let mut start_event: Option<BPMNNode> = None;

        // Temporary storage for mapping IDs to node/gateway types
        let mut node_types: HashMap<String, String> = HashMap::new();

        while let Ok(event) = xml_reader.read_event_into(&mut buf) {
            match event {
                Event::Start(ref e) | Event::Empty(ref e) => {
                    let tag = xml_reader.decoder().decode(e.name().into_inner()).unwrap();
                    let tag = tag.split(':').last().unwrap();

                    match tag {
                        "startEvent" => {
                            let mut id = String::new();
                            for attr in e.attributes().flatten() {
                                if attr.key.as_ref() == b"id" {
                                    id = attr.unescape_value().unwrap().to_string();
                                }
                            }
                            let node = BPMNNode { id: id.clone(), incoming: Vec::new(), outgoing: Vec::new() };
                            node_types.insert(id.clone(), "startEvent".to_string());
                            start_event = Some(node.clone());
                            nodes.insert(id, node);
                        }
                        "endEvent" => {
                            let mut id = String::new();
                            for attr in e.attributes().flatten() {
                                if attr.key.as_ref() == b"id" {
                                    id = attr.unescape_value().unwrap().to_string();
                                }
                            }
                            let node = BPMNNode { id: id.clone(), incoming: Vec::new(), outgoing: Vec::new() };
                            node_types.insert(id.clone(), "endEvent".to_string());
                            end_events.push(node.clone());
                            nodes.insert(id, node);
                        }
                        "task" => {
                            let mut id = String::new();
                            let mut name = String::new();
                            for attr in e.attributes().flatten() {
                                match attr.key.as_ref() {
                                    b"id" => id = attr.unescape_value().unwrap().to_string(),
                                    b"name" => name = attr.unescape_value().unwrap().to_string(),
                                    _ => {}
                                }
                            }
                            let node = BPMNNode { id: id.clone(), incoming: Vec::new(), outgoing: Vec::new() };
                            node_types.insert(id.clone(), "task".to_string());
                            if !name.is_empty() {
                                label.insert(id.clone(), name);
                            }
                            nodes.insert(id, node);
                        }
                        "exclusiveGateway" | "parallelGateway" | "inclusiveGateway" => {
                            let mut id = String::new();
                            let mut direction = GatewayDirection::Diverging;
                            for attr in e.attributes().flatten() {
                                match attr.key.as_ref() {
                                    b"id" => id = attr.unescape_value().unwrap().to_string(),
                                    b"gatewayDirection" => {
                                        let dir = attr.unescape_value().unwrap();
                                        direction = match dir.as_ref() {
                                            "Converging" => GatewayDirection::Converging,
                                            _ => GatewayDirection::Diverging,
                                        };
                                    }
                                    _ => {}
                                }
                            }
                            let gateway = BPMNGateway {
                                id: id.clone(),
                                direction: direction.clone(),
                                incoming: Vec::new(),
                                outgoing: Vec::new(),
                            };
                            node_types.insert(id.clone(), tag.to_string());
                            match tag {
                                "exclusiveGateway" => xor_gateways.push(gateway),
                                "parallelGateway" => and_gateways.push(gateway),
                                "inclusiveGateway" => or_gateways.push(gateway),
                                _ => {}
                            }
                        }
                        "sequenceFlow" => {
                            let mut source_id = String::new();
                            let mut target_id = String::new();
                            for attr in e.attributes().flatten() {
                                match attr.key.as_ref() {
                                    b"sourceRef" => source_id = attr.unescape_value().unwrap().to_string(),
                                    b"targetRef" => target_id = attr.unescape_value().unwrap().to_string(),
                                    _ => {}
                                }
                            }
                            sequence_flows.push(SequenceFlow { source_id, target_id });
                        }
                        _ => {}
                    }
                }
                Event::Eof => break,
                _ => {}
            }
            buf.clear();
        }

        // Assign incoming/outgoing flows to nodes and gateways
        for flow in &sequence_flows {
            if let Some(node) = nodes.get_mut(&flow.source_id) {
                node.outgoing.push(flow.clone());
            }
            if let Some(node) = nodes.get_mut(&flow.target_id) {
                node.incoming.push(flow.clone());
            }
            for gateway in xor_gateways.iter_mut().chain(and_gateways.iter_mut()).chain(or_gateways.iter_mut()) {
                if gateway.id == flow.source_id {
                    gateway.outgoing.push(flow.clone());
                }
                if gateway.id == flow.target_id {
                    gateway.incoming.push(flow.clone());
                }
            }
        }

        Ok(BusinessProcessModelAndNotation {
            nodes: nodes.into_values().collect(),
            start_event: start_event.ok_or_else(|| anyhow!("No start event found"))?,
            xor_gateways,
            and_gateways,
            or_gateways,
            end_events,
            sequence_flows,
            label,
        })
    }
}

impl FromEbiTraitObject for BusinessProcessModelAndNotation {
    fn from_trait_object(object: ebi_input::EbiInput) -> Result<Box<Self>> {
        match object {
            EbiInput::Object(EbiObject::BusinessProcessModelAndNotation(e), _) => Ok(Box::new(e)),
            _ => Err(anyhow!(
                "cannot read {} {} as a business process model and notation",
                object.get_type().get_article(),
                object.get_type()
            )),
        }
    }
}

impl Exportable for BusinessProcessModelAndNotation {
    fn export_from_object(object: EbiOutput, f: &mut dyn std::io::Write) -> anyhow::Result<()> {
        match object {
            EbiOutput::Object(EbiObject::BusinessProcessModelAndNotation(bpmn)) => bpmn.export(f),
            _ => Err(anyhow!("Cannot export object as BPMN.")),
        }
    }

    fn export(&self, f: &mut dyn std::io::Write) -> anyhow::Result<()> {
        Ok(write!(f, "{}", self)?)
    }
}

impl Display for BusinessProcessModelAndNotation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        todo!()
    }
}

impl Infoable for BusinessProcessModelAndNotation {
    fn info(&self, f: &mut impl std::io::Write) -> anyhow::Result<()> {
        todo!()
    }
}


#[cfg(test)]
mod tests {
    use super::*;
    use std::io::Cursor;

    #[test]
    fn test_import_bpmn() {
        let bpmn_xml = r#"
        <?xml version="1.0" encoding="UTF-8"?>
        <definitions>
          <process>
            <startEvent id="start1"/>
            <task id="task1" name="Task 1"/>
            <task id="task2" name="Task 2"/>
            <task id="task3" name="Task 3"/>
            <exclusiveGateway id="xor1" gatewayDirection="Diverging"/>
            <exclusiveGateway id="xor2" gatewayDirection="Converging"/>
            <endEvent id="end1"/>
            <sequenceFlow sourceRef="start1" targetRef="task1"/>
            <sequenceFlow sourceRef="task1" targetRef="xor1"/>
            <sequenceFlow sourceRef="xor1" targetRef="task2"/>
            <sequenceFlow sourceRef="xor1" targetRef="task3"/>
            <sequenceFlow sourceRef="task2" targetRef="xor2"/>
            <sequenceFlow sourceRef="task3" targetRef="xor2"/>
            <sequenceFlow sourceRef="xor2" targetRef="end1"/>
          </process>
        </definitions>
        "#;

        let mut cursor = Cursor::new(bpmn_xml);
        let result = BusinessProcessModelAndNotation::import(&mut cursor);

        assert!(result.is_ok());
        let model = result.unwrap();

        // Check start event
        assert_eq!(model.start_event.id, "start1");
        // Check nodes (tasks)
        assert!(model.nodes.iter().any(|n| n.id == "task1"));
        assert!(model.nodes.iter().any(|n| n.id == "task2"));
        assert!(model.nodes.iter().any(|n| n.id == "task3"));
        // Check end events
        assert!(model.end_events.iter().any(|n| n.id == "end1"));
        // Check gateways
        assert!(model.xor_gateways.iter().any(|g| g.id == "xor1"));
        assert!(model.xor_gateways.iter().any(|g| g.id == "xor2"));
        // Check sequence flows (should be 7)
        assert_eq!(model.sequence_flows.len(), 7);
        // Check label for task1
        assert_eq!(model.label.get("task1").unwrap(), "Task 1");
        println!("BPMN moder {:#?}", model);
    }
}