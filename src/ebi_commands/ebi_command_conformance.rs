use anyhow::{Context, anyhow};

use crate::{ebi_framework::{ebi_command::EbiCommand, ebi_input::{EbiInput, EbiInputType}, ebi_object::EbiTraitObject, ebi_output::{EbiOutput, EbiOutputType}, ebi_trait::EbiTrait}, ebi_traits::{ebi_trait_finite_stochastic_language::EbiTraitFiniteStochasticLanguage, ebi_trait_queriable_stochastic_language::EbiTraitQueriableStochasticLanguage}, techniques::{entropic_relevance::EntropicRelvance, jensen_shannon_stochastic_conformance::JensenShannonStochasticConformance, unit_earth_movers_stochastic_conformance::UnitEarthMoversStochasticConformance}};
use super::ebi_command_sample::{self, SAMPLED_OBJECT_INPUTS};

pub const EBI_CONFORMANCE: EbiCommand = EbiCommand::Group { 
    name_short: "conf",
    name_long: Some("conformance"), 
    explanation_short: "Check the conformance of two stochastic languages.", 
    explanation_long: None, 
    children: &[
        &CONFORMANCE_ER,
        &CONFORMANCE_JSSC,
        &CONFORMANCE_JSSC_SAMPLE,
        &CONFORMANCE_UEMSC,
    ]
};

// pub const CONFORMANCE_EMSC: EbiCommand = EbiCommand::Command {
//     name_short: "emsc",
//     name_long: Some("earth-movers-stochastic-conformance"), 
//     explanation_short: "Compute earth movers' stochastic conformance.", 
//     explanation_long: None, 
//     latex_link: Some("\\cite{DBLP:conf/bpm/LeemansSA19}"), 
//     cli_command: None, 
//     exact_arithmetic: true, 
//     input_types: &[ 
//         &[&EbiInputType::Trait(EbiTrait::FiniteStochasticLanguage)], 
//         &[&EbiInputType::Trait(EbiTrait::FiniteStochasticLanguage)]
//     ], 
//     input_names: &["FILE_1", "FILE_2"], 
//     input_helps: &["A finite stochastic language (log) to compare.", "A finite stochastic language (log) to compare."],
//     execute: |mut inputs, _| {
//         let log = inputs.remove(0).to_type::<dyn EbiTraitFiniteStochasticLanguage>()?;
//         let model = inputs.remove(0).to_type::<dyn EbiTraitFiniteStochasticLanguage>()?;
//         Ok(EbiOutput::Fraction(earth_movers_stochastic_conformance::emsc(log.as_ref(), model.as_ref()).context("Computing EMSC.")?))
//     },
//     output: &EbiOutputType::Fraction
// };

// pub const CONFORMANCE_EMSC_SAMPLE: EbiCommand = EbiCommand::Command { 
//     name_short: "emsc-sample", 
//     name_long: Some("earth-movers-stochastic-conformance-sample"), 
//     explanation_short: "Compute earth movers' stochastic conformance with sampling.", 
//     explanation_long: None, 
//     latex_link: Some("\\cite{DBLP:conf/bpm/LeemansSA19}"),
//     cli_command: None, 
//     exact_arithmetic: false, 
//     input_types: &[ 
//         &[ &EbiInputType::Trait(EbiTrait::FiniteStochasticLanguage), &EbiInputType::Trait(EbiTrait::StochasticSemantics) ],
//         &[ &EbiInputType::Trait(EbiTrait::FiniteStochasticLanguage), &EbiInputType::Trait(EbiTrait::StochasticSemantics) ],
//         &[ &EbiInputType::Usize ] 
//     ], 
//     input_names: &["FILE_1", "FILE_2","NUMBER_OF_TRACES"], 
//     input_helps: &["A sampleable stochastic language to compare.", "A sampleabe stochastic language to compare.","Number of traces to sample."],
//     execute: |mut inputs, _| {
//         let object1 = inputs.remove(0);
//         let object2 = inputs.remove(0);
//         let number_of_traces = inputs.remove(0).to_type::<usize>()?;

//         let lang1 = match object1 {
//             EbiInput::Trait(EbiTraitObject::FiniteStochasticLanguage(slang), _) => slang,
//             EbiInput::Trait(EbiTraitObject::StochasticSemantics(semantics), _) => {
//                 Box::new(semantics.sample(*number_of_traces).context("Sample semantics.")?)
//             },
//             _ => unreachable!()
//         };

//         let lang2 = match object2 {
//             EbiInput::Trait(EbiTraitObject::FiniteStochasticLanguage(slang), _) => slang,
//             EbiInput::Trait(EbiTraitObject::StochasticSemantics(semantics), _) => {
//                 Box::new(semantics.sample(*number_of_traces).context("Sample semantics.")?)
//             },
//             _ => unreachable!()
//         };

//         Ok(EbiOutput::Fraction(earth_movers_stochastic_conformance::emsc(lang1.as_ref(), lang2.as_ref()).context("Compute JSSC by sampling.")?))
//     },
//     output: &EbiOutputType::Fraction 
// };

pub const CONFORMANCE_UEMSC: EbiCommand = EbiCommand::Command {
    name_short: "uemsc",
    name_long: Some("unit-earth-movers-stochastic-conformance"), 
    explanation_short: "Compute unit-earth movers' stochastic conformance.", 
    explanation_long: None, 
    latex_link: Some("\\cite{DBLP:conf/bpm/LeemansSA19}"), 
    cli_command: None, 
    exact_arithmetic: true, 
    input_types: &[ 
        &[&EbiInputType::Trait(EbiTrait::FiniteStochasticLanguage)], 
        &[&EbiInputType::Trait(EbiTrait::QueriableStochasticLanguage)]
    ], 
    input_names: &["FILE_1", "FILE_2"], 
    input_helps: &["A finite stochastic language (log) to compare.", "A queriable stochastic language (model) to compare."],
    execute: |mut inputs, _| {
        let log = inputs.remove(0).to_type::<dyn EbiTraitFiniteStochasticLanguage>()?;
        let model = inputs.remove(0).to_type::<dyn EbiTraitQueriableStochasticLanguage>()?;
        let uemsc = log.unit_earth_movers_stochastic_conformance(model).context("cannot compute uEMSC")?;
        Ok(EbiOutput::Fraction(uemsc))
    },
    output_type: &EbiOutputType::Fraction
};

pub const CONFORMANCE_ER: EbiCommand = EbiCommand::Command { 
    name_short: "er", 
    name_long: Some("entropic-relevance"), 
    explanation_short: "Compute entropic relevance (uniform).", 
    explanation_long: None, 
    latex_link: Some(r"Entropic relevance is computed as follows:
        
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
            \end{definition}"), 
    cli_command: None, 
    exact_arithmetic: true,
    input_types: &[ 
        &[&EbiInputType::Trait(EbiTrait::FiniteStochasticLanguage)], 
        &[&EbiInputType::Trait(EbiTrait::QueriableStochasticLanguage)]
    ], 
    input_names: &["FILE_1", "FILE_2"], 
    input_helps: &["A finite stochastic language (log) to compare.", "A queriable stochastic language (model) to compare."],
    execute: |mut inputs, _| {
        let log = inputs.remove(0).to_type::<dyn EbiTraitFiniteStochasticLanguage>()?;
        let model = inputs.remove(0).to_type::<dyn EbiTraitQueriableStochasticLanguage>()?;
        Ok(EbiOutput::LogDiv(log.er(model).context("cannot compute uEMSC")?))
    }, 
    output_type: &EbiOutputType::LogDiv
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
        &[ &EbiInputType::Trait(EbiTrait::FiniteStochasticLanguage)], 
        &[ &EbiInputType::Trait(EbiTrait::FiniteStochasticLanguage), &EbiInputType::Trait(EbiTrait::QueriableStochasticLanguage)] 
    ], 
    input_names: &["FILE_1", "FILE_2"], 
    input_helps: &["A finite stochastic language to compare.", "A queriable stochastic language to compare."],
    execute: |mut inputs, _| {
        let event_log = inputs.remove(0).to_type::<dyn EbiTraitFiniteStochasticLanguage>()?;

        match inputs.remove(0) {            
            EbiInput::Trait(EbiTraitObject::FiniteStochasticLanguage(slang), _) => {
                Ok(EbiOutput::RootLogDiv(event_log.jssc_log2log(slang).context("Compute JSSC.")?))
            },
            EbiInput::Trait(EbiTraitObject::QueriableStochasticLanguage(slang), _) => {
                Ok(EbiOutput::RootLogDiv(event_log.jssc_log2model(slang).context("Compute JSSC.")?))
            }
            _ => Err(anyhow!("wrong input given"))
        }
    },
    output_type: &EbiOutputType::RootLogDiv 
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
        &[ &EbiInputType::Usize ] 
    ], 
    input_names: &["FILE_1", "FILE_2", "NUMBER_OF_TRACES"], 
    input_helps: &["A queriable stochastic language to compare.", "A queriable stochastic language to compare.","Number of traces to sample."],
    execute: |mut inputs, _| {
        let object1 = inputs.remove(0);
        let object2 = inputs.remove(0);
        let number_of_traces = inputs.remove(0).to_type::<usize>()?;

        let lang1 = Box::new(ebi_command_sample::get_sampled_object(object1, *number_of_traces)?);
        let lang2 = Box::new(ebi_command_sample::get_sampled_object(object2, *number_of_traces)?);

        let lang1: Box<dyn EbiTraitFiniteStochasticLanguage> = lang1;
        Ok(EbiOutput::RootLogDiv(lang1.jssc_log2log(lang2).context("Compute JSSC by sampling.")?))
    },
    output_type: &EbiOutputType::RootLogDiv 
};