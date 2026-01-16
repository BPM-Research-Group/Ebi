use super::ebi_command_sample::{self, SAMPLED_OBJECT_INPUTS};
use crate::{
    EbiInputTypeEnum,
    ebi_framework::{
        ebi_command::EbiCommand,
        ebi_input::{EbiInput, EbiInputType},
        ebi_output::{EbiOutput, EbiOutputType},
        ebi_trait::EbiTrait,
        ebi_trait_object::EbiTraitObject,
    },
    ebi_traits::{
        ebi_trait_finite_stochastic_language::EbiTraitFiniteStochasticLanguage,
        ebi_trait_queriable_stochastic_language::EbiTraitQueriableStochasticLanguage,
    },
    techniques::{
        chi_square_stochastic_conformance::ChiSquareStochasticConformance,
        earth_movers_stochastic_conformance::EarthMoversStochasticConformance,
        entropic_relevance::EntropicRelvance,
        hellinger_stochastic_conformance::HellingerStochasticConformance,
        jensen_shannon_stochastic_conformance::JensenShannonStochasticConformance,
        stochastic_markovian_abstraction::AbstractMarkovian,
        stochastic_markovian_abstraction_conformance::{DistanceMeasure, StochasticMarkovianConformance},
        unit_earth_movers_stochastic_conformance::UnitEarthMoversStochasticConformance,
    },
};
use anyhow::{Context, anyhow};
use ebi_objects::{EbiObject, EbiObjectType, ebi_arithmetic::Fraction};
use strum::VariantNames;

pub const EBI_CONFORMANCE: EbiCommand = EbiCommand::Group {
    name_short: "conf",
    name_long: Some("conformance"),
    explanation_short: "Check the conformance of two stochastic languages.",
    explanation_long: None,
    children: &[
        &CONFORMANCE_CSSC,
        &CONFORMANCE_EMSC,
        &CONFORMANCE_EMSC_SAMPLE,
        &CONFORMANCE_ER,
        &CONFORMANCE_HSC,
        &CONFORMANCE_JSSC,
        &CONFORMANCE_JSSC_SAMPLE,
        &CONFORMANCE_MARKOVIAN,
        &CONFORMANCE_UEMSC,
    ],
};

pub const CONFORMANCE_UEMSC: EbiCommand = EbiCommand::Command {
    name_short: "uemsc",
    name_long: Some("unit-earth-movers-stochastic-conformance"),
    explanation_short: "Compute unit-earth movers' stochastic conformance.",
    explanation_long: Some(
        "Compute unit-earth movers' stochastic conformance, also known as total variation distance.",
    ),
    latex_link: Some("\\cite{DBLP:conf/bpm/LeemansSA19}"),
    cli_command: None,
    exact_arithmetic: true,
    input_types: &[
        &[&EbiInputType::Trait(EbiTrait::FiniteStochasticLanguage)],
        &[&EbiInputType::Trait(EbiTrait::QueriableStochasticLanguage)],
    ],
    input_names: &["FILE_1", "FILE_2"],
    input_helps: &[
        "A finite stochastic language (log) to compare.",
        "A queriable stochastic language (model) to compare.",
    ],
    execute: |mut inputs, _| {
        let log: Box<dyn EbiTraitFiniteStochasticLanguage> = inputs
            .remove(0)
            .to_type::<dyn EbiTraitFiniteStochasticLanguage>(
        )?;
        let model = inputs
            .remove(0)
            .to_type::<dyn EbiTraitQueriableStochasticLanguage>()?;
        let uemsc = log
            .unit_earth_movers_stochastic_conformance(model)
            .context("cannot compute uEMSC")?;
        Ok(EbiOutput::Fraction(uemsc))
    },
    output_type: &EbiOutputType::Fraction,
};

pub const CONFORMANCE_ER: EbiCommand = EbiCommand::Command {
    name_short: "er",
    name_long: Some("entropic-relevance"),
    explanation_short: "Compute entropic relevance (uniform).",
    explanation_long: None,
    latex_link: Some(
        r"Entropic relevance is computed as follows:
        
        \begin{definition}[Entropic Relevance~\cite{DBLP:journals/is/AlkhammashPMG22}]
            \label{def:ER}
                Let $L$ be a finite stochastic language and let $M$ be a queriable stochastic langauge.
                Let $\Lambda$ be the set of all activities appearing in the traces of $L$.
                Then, the \emph{entropic relevance ($\entrel$) of $M$ to $L$} is defined as follows: 
                \begin{align*}
                    \entrel(L, M) ={}& H_0\left(\sum_{\sigma \in \bar{L},\, M(\sigma)>0}{L(\sigma)}\right) + 
                    \sum_{\sigma \in \bar{L}}L(\sigma) J(\sigma, M)\\
                    J(\sigma, M) ={}& \begin{cases}
                    -\log_2 M(\sigma) & M(\sigma) > 0\\
                    (1+|\sigma|) \log_2 (1 + |\Lambda|)) & \text{otherwise}
                    \end{cases}\\
                    H_0(x) ={}& -x \log_2{x} - (1-x) \log_2{(1-x)} \text{ with } H_0(0) = H_0(1) = 0 &\\
                \end{align*}       
            \end{definition}",
    ),
    cli_command: None,
    exact_arithmetic: false,
    input_types: &[
        &[&EbiInputType::Trait(EbiTrait::FiniteStochasticLanguage)],
        &[&EbiInputType::Trait(EbiTrait::QueriableStochasticLanguage)],
    ],
    input_names: &["FILE_1", "FILE_2"],
    input_helps: &[
        "A finite stochastic language (log) to compare.",
        "A queriable stochastic language (model) to compare.",
    ],
    execute: |mut inputs, _| {
        let log = inputs
            .remove(0)
            .to_type::<dyn EbiTraitFiniteStochasticLanguage>()?;
        let model = inputs
            .remove(0)
            .to_type::<dyn EbiTraitQueriableStochasticLanguage>()?;
        Ok(EbiOutput::LogDiv(
            log.entropic_relevance(model).context("cannot compute ER")?,
        ))
    },
    output_type: &EbiOutputType::LogDiv,
};

pub const CONFORMANCE_JSSC: EbiCommand = EbiCommand::Command {
    name_short: "jssc",
    name_long: Some("jensen-shannon"),
    explanation_short: "Compute Jensen-Shannon stochastic conformance.",
    explanation_long: None,
    latex_link: None,
    cli_command: None,
    exact_arithmetic: false,
    input_types: &[
        &[&EbiInputType::Trait(EbiTrait::FiniteStochasticLanguage)],
        &[
            &EbiInputType::Trait(EbiTrait::FiniteStochasticLanguage),
            &EbiInputType::Trait(EbiTrait::QueriableStochasticLanguage),
        ],
    ],
    input_names: &["FILE_1", "FILE_2"],
    input_helps: &[
        "A finite stochastic language to compare.",
        "A queriable stochastic language to compare.",
    ],
    execute: |mut inputs, _| {
        let event_log = inputs
            .remove(0)
            .to_type::<dyn EbiTraitFiniteStochasticLanguage>()?;

        match inputs.remove(0) {
            EbiInput::Trait(EbiTraitObject::FiniteStochasticLanguage(slang), _) => Ok(
                EbiOutput::RootLogDiv(event_log.jssc_log2log(slang).context("Compute JSSC.")?),
            ),
            EbiInput::Trait(EbiTraitObject::QueriableStochasticLanguage(slang), _) => Ok(
                EbiOutput::RootLogDiv(event_log.jssc_log2model(slang).context("Compute JSSC.")?),
            ),
            _ => Err(anyhow!("wrong input given")),
        }
    },
    output_type: &EbiOutputType::RootLogDiv,
};

pub const CONFORMANCE_JSSC_SAMPLE: EbiCommand = EbiCommand::Command {
    name_short: "jssc-sample",
    name_long: Some("jensen-shannon-sample"),
    explanation_short: "Compute Jensen-Shannon stochastic conformance with sampling.",
    explanation_long: None,
    latex_link: None,
    cli_command: None,
    exact_arithmetic: false,
    input_types: &[
        SAMPLED_OBJECT_INPUTS,
        SAMPLED_OBJECT_INPUTS,
        &[&EbiInputType::Usize(Some(1), None, None)],
    ],
    input_names: &["FILE_1", "FILE_2", "NUMBER_OF_TRACES"],
    input_helps: &[
        "A queriable stochastic language to compare.",
        "A queriable stochastic language to compare.",
        "Number of traces to sample.",
    ],
    execute: |mut inputs, _| {
        let object1 = inputs.remove(0);
        let object2 = inputs.remove(0);
        let number_of_traces = inputs.remove(0).to_type::<usize>()?;

        let lang1 = Box::new(ebi_command_sample::get_sampled_object(
            object1,
            *number_of_traces,
        )?);
        let lang2 = Box::new(ebi_command_sample::get_sampled_object(
            object2,
            *number_of_traces,
        )?);

        let lang1: Box<dyn EbiTraitFiniteStochasticLanguage> = lang1;
        Ok(EbiOutput::RootLogDiv(
            lang1
                .jssc_log2log(lang2)
                .context("Compute JSSC by sampling.")?,
        ))
    },
    output_type: &EbiOutputType::RootLogDiv,
};

pub const CONFORMANCE_EMSC: EbiCommand = EbiCommand::Command {
    name_short: "emsc",
    name_long: Some("earth-movers-stochastic-conformance"),
    explanation_short: "Compute Earth mover's stochastic conformance.",
    explanation_long: Some(
        "Compute Earth mover's stochastic conformance, also known as the Wasserstein distance.",
    ),
    latex_link: Some("\\cite{DBLP:journals/is/LeemansABP21}"),
    cli_command: None,
    exact_arithmetic: true,
    input_types: &[
        &[&EbiInputType::Trait(EbiTrait::FiniteStochasticLanguage)],
        &[&EbiInputType::Trait(EbiTrait::FiniteStochasticLanguage)],
    ],
    input_names: &["FILE_1", "FILE_2"],
    input_helps: &[
        "A finite stochastic language to compare.",
        "A finite stochastic language to compare.",
    ],
    execute: |mut inputs, _| {
        let mut lang_a = inputs
            .remove(0)
            .to_type::<dyn EbiTraitFiniteStochasticLanguage>()?;

        let mut lang_b = inputs
            .remove(0)
            .to_type::<dyn EbiTraitFiniteStochasticLanguage>()?;

        // Compute EMSC
        Ok(EbiOutput::Fraction(
            lang_a
                .earth_movers_stochastic_conformance(lang_b.as_mut())
                .context("Compute EMSC.")?,
        ))
    },
    output_type: &EbiOutputType::Fraction,
};

pub const CONFORMANCE_EMSC_SAMPLE: EbiCommand = EbiCommand::Command {
    name_short: "emsc-sample",
    name_long: Some("earth-movers-stochastic-conformance-sample"),
    explanation_short: "Compute Earth mover's stochastic conformance with sampling.",
    explanation_long: Some(
        "Compute Earth mover's stochastic conformance with sampling, also known as the Wasserstein distance.",
    ),
    latex_link: Some("\\cite{DBLP:journals/is/LeemansABP21}"),
    cli_command: None,
    exact_arithmetic: true,
    input_types: &[
        &[&EbiInputType::Trait(EbiTrait::FiniteStochasticLanguage)],
        SAMPLED_OBJECT_INPUTS,
        &[&EbiInputType::Usize(Some(1), None, None)],
    ],
    input_names: &["FILE_1", "FILE_2", "NUMBER_OF_TRACES"],
    input_helps: &[
        "A finite stochastic language to compare.",
        "A queriable stochastic language to compare.",
        "Number of traces to sample.",
    ],
    execute: |mut inputs, _| {
        let mut lang_a = inputs
            .remove(0)
            .to_type::<dyn EbiTraitFiniteStochasticLanguage>()?;

        let sampled_object = inputs.remove(0);
        let number_of_traces = inputs.remove(0).to_type::<usize>()?;

        // Sample the second input with the specified number of traces
        let mut lang_b = Box::new(ebi_command_sample::get_sampled_object(
            sampled_object,
            *number_of_traces,
        )?);

        // Compute EMSC
        Ok(EbiOutput::Fraction(
            lang_a
                .earth_movers_stochastic_conformance(lang_b.as_mut())
                .context("Compute EMSC.")?,
        ))
    },
    output_type: &EbiOutputType::Fraction,
};

pub const CONFORMANCE_HSC: EbiCommand = EbiCommand::Command {
    name_short: "hsc",
    name_long: Some("hellinger-stochastic-conformance"),
    explanation_short: "Compute Hellinger stochastic conformance.",
    explanation_long: Some(
        "Compute Hellinger stochastic conformance, also known as the Hellinger distance.",
    ),
    latex_link: None,
    cli_command: None,
    exact_arithmetic: true,
    input_types: &[
        &[&EbiInputType::Trait(EbiTrait::FiniteStochasticLanguage)],
        &[&EbiInputType::Trait(EbiTrait::QueriableStochasticLanguage)],
    ],
    input_names: &["FILE_1", "FILE_2"],
    input_helps: &[
        "A finite stochastic language (log) to compare.",
        "A queriable stochastic language (model) to compare.",
    ],
    execute: |mut inputs, _| {
        let log = inputs
            .remove(0)
            .to_type::<dyn EbiTraitFiniteStochasticLanguage>()?;
        let model = inputs
            .remove(0)
            .to_type::<dyn EbiTraitQueriableStochasticLanguage>()?;
        let hsc = log
            .hellinger_stochastic_conformance(model)
            .context("cannot compute HSC")?;
        Ok(EbiOutput::Fraction(hsc))
    },
    output_type: &EbiOutputType::Fraction,
};

pub const CONFORMANCE_CSSC: EbiCommand = EbiCommand::Command {
    name_short: "cssc",
    name_long: Some("chi-square-stochastic-conformance"),
    explanation_short: "Compute Chi-Square stochastic conformance.",
    explanation_long: Some(
        "Compute Chi-Square stochastic conformance, also known as the Chi-Square distance.",
    ),
    latex_link: None,
    cli_command: None,
    exact_arithmetic: true,
    input_types: &[
        &[&EbiInputType::Trait(EbiTrait::FiniteStochasticLanguage)],
        &[&EbiInputType::Trait(EbiTrait::QueriableStochasticLanguage)],
    ],
    input_names: &["FILE_1", "FILE_2"],
    input_helps: &[
        "A finite stochastic language (log) to compare.",
        "A queriable stochastic language (model) to compare.",
    ],
    execute: |mut inputs, _| {
        let log = inputs
            .remove(0)
            .to_type::<dyn EbiTraitFiniteStochasticLanguage>()?;
        let model = inputs
            .remove(0)
            .to_type::<dyn EbiTraitQueriableStochasticLanguage>()?;
        let cssc = log
            .chi_square_stochastic_conformance(model)
            .context("cannot compute CSSC")?;
        Ok(EbiOutput::Fraction(cssc))
    },
    output_type: &EbiOutputType::Fraction,
};

pub const CONFORMANCE_MARKOVIAN: EbiCommand = EbiCommand::Command {
    name_short: "sma",
    name_long: Some("stochastic-markovian-abstraction-based-conformance"),
    explanation_short: "Compute the conformance between two stochastic languages using a stochastic Markovian abstraction.",
    explanation_long: Some(
        "Compute the conformance between two stochastic languages using a stochastic Markovian abstraction, which represents languages based on the expected frequency of subtraces to handle partially matching traces.",
    ),
    latex_link: Some("\\cite{DBLP:conf/icpm/RochaLA24}"),
    cli_command: None,
    exact_arithmetic: true,
    input_types: &[
        &[
            &EbiInputType::Object(EbiObjectType::StochasticLabelledPetriNet),
            &EbiInputType::Trait(EbiTrait::FiniteStochasticLanguage),
        ],
        &[
            &EbiInputType::Object(EbiObjectType::StochasticLabelledPetriNet),
            &EbiInputType::Trait(EbiTrait::FiniteStochasticLanguage),
        ],
        &[&EbiInputType::Usize(Some(1), None, None)],
        &[&EbiInputTypeEnum!(DistanceMeasure)],
    ],
    input_names: &["FILE_1", "FILE_2", "K_ORDER", "MEASURE"],
    input_helps: &[
        "A finite stochastic language or a stochastic labelled Petri net (log) to compare. An SLPN must be livelock-free and bounded.",
        "A finite stochastic language or a stochastic labelled Petri net (model) to compare. An SLPN must be livelock-free and bounded.",
        "The order of the Markovian abstraction (length of subtraces).",
        "The stochastic conformance measure to be applied to the abstractions.",
    ],
    execute: |mut inputs, _| {
        let lang1 = inputs.remove(0);
        let lang2 = inputs.remove(0);
        let order = inputs.remove(0).to_type::<usize>()?;
        let measure = inputs.remove(0).to_type::<DistanceMeasure>()?;

        let delta = Fraction::from((1, 1000)); // default delta value

        //read abstractions
        let abstraction1 = match lang1 {
            EbiInput::Trait(EbiTraitObject::FiniteStochasticLanguage(slang), _) => {
                slang.abstract_markovian(*order, &delta)
            }
            EbiInput::Object(EbiObject::StochasticLabelledPetriNet(slpn), _) => {
                slpn.abstract_markovian(*order, &delta)
            }
            _ => unreachable!(),
        }?;
        let abstraction2 = match lang2 {
            EbiInput::Trait(EbiTraitObject::FiniteStochasticLanguage(slang), _) => {
                slang.abstract_markovian(*order, &delta)
            }
            EbiInput::Object(EbiObject::StochasticLabelledPetriNet(slpn), _) => {
                slpn.abstract_markovian(*order, &delta)
            }
            _ => unreachable!(),
        }?;

        let result = abstraction1.markovian_conformance(abstraction2, *measure)?;

        Ok(EbiOutput::Fraction(result))
    },
    output_type: &EbiOutputType::Fraction,
};
