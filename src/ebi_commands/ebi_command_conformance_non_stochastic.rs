use anyhow::Context;
use ebi_objects::{EbiObject, EbiObjectType, StochasticLanguageOfAlignments};

use crate::{
    ebi_framework::{
        ebi_command::EbiCommand,
        ebi_input::EbiInputType,
        ebi_output::{EbiOutput, EbiOutputType},
        ebi_trait::EbiTrait,
    },
    ebi_traits::{
        ebi_trait_finite_language::EbiTraitFiniteLanguage,
        ebi_trait_finite_stochastic_language::EbiTraitFiniteStochasticLanguage,
        ebi_trait_semantics::EbiTraitSemantics,
    },
    techniques::{
        align::Align, escaping_edges_precision::EscapingEdgesPrecision, fitness::Fitness,
    },
};

pub const EBI_CONFORMANCE_NON_STOCHASTIC: EbiCommand = EbiCommand::Group {
    name_short: "confns",
    name_long: Some("conformance-non-stochastic"),
    explanation_short: "Compute conformance without considering a stochastic perspective.",
    explanation_long: None,
    children: &[
        &EBI_CONFORMANCE_NON_STOCHASTIC_ALIGNMENTS,
        &EBI_CONFORMANCE_NON_STOCHASTIC_ESCAPING_EDGES_PRECISION,
        &EBI_CONFORMANCE_NON_STOCHASTIC_SET_ALIGNMENTS,
        &EBI_CONFORMANCE_NON_STOCHASTIC_TRACE_FITNESS,
    ],
};

pub const EBI_CONFORMANCE_NON_STOCHASTIC_ALIGNMENTS: EbiCommand = EbiCommand::Command {
    name_short: "ali",
    name_long: Some("alignments"),
    library_name: "ebi_commands::ebi_command_conformance_non_stochastic::EBI_CONFORMANCE_NON_STOCHASTIC_ALIGNMENTS",
    explanation_short: "Compute alignments.",
    explanation_long: Some(
        "Compute alignments.\nThe model must be able to terminate and its states must be bounded. The search performed is not optimised. For Petri nets, the ProM implementation may be more efficient.",
    ),
    latex_link: Some(
        "Compute alignments according to the method described by Adriansyah~\\cite{DBLP:conf/edoc/AdriansyahDA11}. By default, all traces are computed concurrently on all CPU cores. If this requires too much RAM, please see speed trick~\\ref{speedtrick:multithreaded} in Section~\\ref{sec:speedtricks} for how to reduce the number of CPU cores utilised.",
    ),
    cli_command: None,
    exact_arithmetic: true,
    input_types: &[
        &[&EbiInputType::Trait(EbiTrait::FiniteStochasticLanguage)],
        &[&EbiInputType::Trait(EbiTrait::Semantics)],
    ],
    input_names: &["FILE_1", "FILE_2"],
    input_helps: &["The finite stochastic language.", "The model."],
    execute: |mut objects, _| {
        let log = objects
            .remove(0)
            .to_type::<dyn EbiTraitFiniteStochasticLanguage>()?;
        let mut model = objects.remove(0).to_type::<EbiTraitSemantics>()?;

        let result = model.align_stochastic_language(log)?;

        return Ok(EbiOutput::Object(
            EbiObject::StochasticLanguageOfAlignments(result),
        ));
    },
    output_type: &EbiOutputType::ObjectType(EbiObjectType::StochasticLanguageOfAlignments),
};

pub const EBI_CONFORMANCE_NON_STOCHASTIC_ESCAPING_EDGES_PRECISION: EbiCommand =
    EbiCommand::Command {
        name_short: "eep",
        name_long: Some("escaping-edges-precision"),
        library_name: "ebi_commands::ebi_command_conformance_non_stochastic::EBI_CONFORMANCE_NON_STOCHASTIC_ESCAPING_EDGES_PRECISION",
        explanation_short: "Computes the alignment-based escaping-edges-precision of an alignment and a model.",
        explanation_long: Some(
            "Computes a prefix automaton of the alignments, where the states represent model states that have been visited by the alignments. The states are weighed by the probability of traces visiting them. The precision is then the sum of the number of taken edges out of each state multiplied by the weight of the state, divided by the sum of the number of outgoing edges multiplied by the weight of the states.",
        ),
        latex_link: Some(
            "Compute escaping-edges precision using the method of Adriansyah and Munoz-Gama~\\cite{DBLP:conf/bpm/AdriansyahMCDA12}",
        ),
        cli_command: None,
        exact_arithmetic: true,
        input_types: &[
            &[&EbiInputType::Object(
                EbiObjectType::StochasticLanguageOfAlignments,
            )],
            &[&EbiInputType::Trait(EbiTrait::Semantics)],
        ],
        input_names: &["ALIGNMENTS", "MODEL"],
        input_helps: &[
            "The alignments.",
            "The model from which the alignments were computed.",
        ],
        execute: |mut objects, _| {
            let log = objects
                .remove(0)
                .to_type::<StochasticLanguageOfAlignments>()?;
            let model = objects.remove(0).to_type::<EbiTraitSemantics>()?;
            Ok(EbiOutput::Fraction(
                model
                    .escaping_edges_precision(*log)
                    .with_context(|| "computing escaping-edges precision")?,
            ))
        },
        output_type: &EbiOutputType::Fraction,
    };

pub const EBI_CONFORMANCE_NON_STOCHASTIC_SET_ALIGNMENTS: EbiCommand = EbiCommand::Command {
    name_short: "setali",
    name_long: Some("set-alignments"),
    library_name: "ebi_commands::ebi_command_conformance_non_stochastic::EBI_CONFORMANCE_NON_STOCHASTIC_SET_ALIGNMENTS",
    explanation_short: "Compute alignments as a set.",
    explanation_long: Some(
        "Compute a non-weighted set of alignments.\nThe model must be able to terminate and its states must be bounded. The search performed is not optimised. For Petri nets, the ProM implementation may be more efficient.",
    ),
    latex_link: Some(
        "Alignments according to the method described by Adriansyah~\\cite{DBLP:conf/edoc/AdriansyahDA11}. By default, all traces are computed concurrently on all CPU cores. If this requires too much RAM, please see speed trick~\\ref{speedtrick:multithreaded} in Section~\\ref{sec:speedtricks} for how to reduce the number of CPU cores utilised.",
    ),
    cli_command: None,
    exact_arithmetic: true,
    input_types: &[
        &[&EbiInputType::Trait(EbiTrait::FiniteLanguage)],
        &[&EbiInputType::Trait(EbiTrait::Semantics)],
    ],
    input_names: &["FILE_1", "FILE_2"],
    input_helps: &["The finite language.", "The model."],
    execute: |mut objects, _| {
        let log = objects.remove(0).to_type::<dyn EbiTraitFiniteLanguage>()?;
        let mut model = objects.remove(0).to_type::<EbiTraitSemantics>()?;

        let result = model.align_language(log)?;

        return Ok(EbiOutput::Object(EbiObject::LanguageOfAlignments(result)));
    },
    output_type: &EbiOutputType::ObjectType(EbiObjectType::LanguageOfAlignments),
};

pub const EBI_CONFORMANCE_NON_STOCHASTIC_TRACE_FITNESS: EbiCommand = EbiCommand::Command {
    name_short: "tfit",
    name_long: Some("trace-fitness"),
    library_name: "ebi_commands::ebi_command_conformance_non_stochastic::EBI_CONFORMANCE_NON_STOCHASTIC_TRACE_FITNESS",
    explanation_short: "Compute the trace-fitness of a stochastic language of alignments.",
    explanation_long: Some(
        "Compute the trace-fitness of a stochastic language of alignments: the number of synchronous moves divided by the total number of moves, both without silent moves.",
    ),
    latex_link: None,
    cli_command: None,
    exact_arithmetic: true,
    input_types: &[&[&EbiInputType::Object(
        EbiObjectType::StochasticLanguageOfAlignments,
    )]],
    input_names: &["ALIGNMENTS"],
    input_helps: &["The stochastic language of alignments."],
    execute: |mut objects, _| {
        let alignments = objects
            .remove(0)
            .to_type::<StochasticLanguageOfAlignments>()?;

        Ok(EbiOutput::Fraction(alignments.trace_fitness()))
    },
    output_type: &EbiOutputType::Fraction,
};
