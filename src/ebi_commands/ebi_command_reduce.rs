use crate::{
    ebi_framework::{
        ebi_command::EbiCommand,
        ebi_input::EbiInputType,
        ebi_output::{EbiOutput, EbiOutputType},
    },
    techniques::reduce::ReduceLanguageEquivalently,
};
use ebi_objects::{EbiObject, EbiObjectType, LabelledPetriNet, ProcessTree};

pub const EBI_REDUCE: EbiCommand = EbiCommand::Group {
    name_short: "red",
    name_long: Some("reduce"),
    explanation_short: "Reduce a process model language-equivalently.",
    explanation_long: None,
    children: &[&EBI_REDUCE_PETRI_NET, &EBI_REDUCE_PROCESS_TREE],
};

pub const EBI_REDUCE_PETRI_NET: EbiCommand = EbiCommand::Command {
    name_short: "lpn",
    name_long: Some("labelled-petri-net"),
    explanation_short: "Reduce a labelled Petri net language-equivalently.",
    explanation_long: None,
    latex_link: Some("\\cite{murata1989petri}"),
    cli_command: None,
    exact_arithmetic: true,
    input_types: &[&[&EbiInputType::Object(EbiObjectType::LabelledPetriNet)]],
    input_names: &["LPN"],
    input_helps: &["A labelled Petri net."],
    execute: |mut inputs, _| {
        let mut tree = inputs.remove(0).to_type::<LabelledPetriNet>()?;

        tree.reduce_language_equivalently();

        Ok(EbiOutput::Object(EbiObject::LabelledPetriNet(*tree)))
    },
    output_type: &EbiOutputType::ObjectType(EbiObjectType::LabelledPetriNet),
};

pub const EBI_REDUCE_PROCESS_TREE: EbiCommand = EbiCommand::Command {
    name_short: "ptr",
    name_long: Some("process-tree"),
    explanation_short: "Reduce a process tree language-equivalently.",
    explanation_long: None,
    latex_link: Some("\\cite{DBLP:journals/corr/abs-2203-10410}"),
    cli_command: None,
    exact_arithmetic: true,
    input_types: &[&[&EbiInputType::Object(EbiObjectType::ProcessTree)]],
    input_names: &["TREE"],
    input_helps: &["A process tree."],
    execute: |mut inputs, _| {
        let mut tree = inputs.remove(0).to_type::<ProcessTree>()?;

        tree.reduce_language_equivalently();

        Ok(EbiOutput::Object(EbiObject::ProcessTree(*tree)))
    },
    output_type: &EbiOutputType::ObjectType(EbiObjectType::ProcessTree),
};
