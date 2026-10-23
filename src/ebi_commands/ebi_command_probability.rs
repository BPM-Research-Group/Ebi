use crate::{
    ebi_framework::{
        ebi_command::EbiCommand,
        ebi_input::EbiInputType,
        ebi_output::{EbiOutput, EbiOutputType},
        ebi_trait::EbiTrait,
    },
    ebi_traits::{
        ebi_trait_finite_language::EbiTraitFiniteLanguage,
        ebi_trait_queriable_stochastic_language::EbiTraitQueriableStochasticLanguage,
        ebi_trait_stochastic_semantics::EbiTraitStochasticSemantics,
    },
    follower_semantics::FollowerSemantics,
    techniques::explain_trace::ExplainTrace,
};
use ebi_objects::{
    EbiObject, EbiObjectType, HasActivityKey,
    anyhow::Context,
    ebi_arithmetic::{ConstFraction, Fraction},
};

pub const EBI_PROBABILITY: EbiCommand = EbiCommand::Group {
    name_short: "prob",
    name_long: Some("probability"),
    explanation_short: "Compute the probability of a trace or specification on the model.",
    explanation_long: None,
    children: &[
        &EBI_PROBABILITY_LOG,
        &EBI_PROBABILITY_TRACE,
        &EBI_PROBABILITY_EXPLAIN_TRACE,
    ],
};

pub const EBI_PROBABILITY_LOG: EbiCommand = EbiCommand::Command {
    name_short: "log",
    name_long: None,
    explanation_short: "Compute the probability that a stochastic model produces any trace of a log.",
    explanation_long: None,
    latex_link: Some("~\\cite{DBLP:journals/is/LeemansMM24}"),
    cli_command: None,
    exact_arithmetic: true,
    input_types: &[
        &[&EbiInputType::Trait(EbiTrait::QueriableStochasticLanguage)],
        &[&EbiInputType::Trait(EbiTrait::FiniteLanguage)],
    ],
    input_names: &["FILE_1", "FILE_2"],
    input_helps: &[
        "The queriable stochastic language (model).",
        "The finite language (log).",
    ],
    execute: |mut inputs, _| {
        let mut model: Box<dyn EbiTraitQueriableStochasticLanguage> =
            inputs
                .remove(0)
                .to_type::<dyn EbiTraitQueriableStochasticLanguage>()?;
        let log = inputs.remove(0).to_type::<dyn EbiTraitFiniteLanguage>()?;

        let sum = model
            .get_probability_language(log)
            .with_context(|| "cannot compute probability")?;

        return Ok(EbiOutput::Fraction(sum));
    },
    output_type: &EbiOutputType::Fraction,
};

pub const EBI_PROBABILITY_TRACE: EbiCommand = EbiCommand::Command {
    name_short: "trac",
    name_long: Some("trace"),
    explanation_short: "Compute the probability of a trace in a stochastic model.",
    explanation_long: None,
    latex_link: Some("~\\cite{DBLP:journals/is/LeemansMM24}"),
    cli_command: None,
    exact_arithmetic: true,
    input_types: &[
        &[&EbiInputType::Trait(EbiTrait::QueriableStochasticLanguage)],
        &[&EbiInputType::StringList],
    ],
    input_names: &["FILE", "TRACE"],
    input_helps: &[
        "The queriable stochastic language (model).",
        "The remaining multiple standard arguments after FILE are interpreted as the TRACE. Provide event labels in trace order, separated as normal command line arguments (e.g., a space).",
    ],
    execute: |mut inputs, _| {
        let mut model = inputs
            .remove(0)
            .to_type::<dyn EbiTraitQueriableStochasticLanguage>()?;
        let trace = inputs.remove(0).to_type::<Vec<String>>()?;
        let trace = trace
            .iter()
            .map(|activity| activity.as_str())
            .collect::<Vec<_>>();
        let trace = model.activity_key_mut().process_trace_ref(&trace);

        log::trace!("compute probability of trace {:?}", trace);

        let result = model
            .get_probability(&FollowerSemantics::Trace(&trace))
            .with_context(|| format!("cannot compute probability of trace {:?}", trace))?;
        Ok(EbiOutput::Fraction(result))
    },
    output_type: &EbiOutputType::Fraction,
};

pub const EBI_PROBABILITY_EXPLAIN_TRACE: EbiCommand = EbiCommand::Command {
    name_short: "exptra",
    name_long: Some("explain-trace"),
    explanation_short: "Compute the most likely explanation of a trace given the stochastic model.",
    explanation_long: None,
    latex_link: None,
    cli_command: None,
    exact_arithmetic: true,
    input_types: &[
        &[&EbiInputType::Trait(EbiTrait::StochasticSemantics)],
        &[&EbiInputType::Fraction(
            Some(ConstFraction::zero()),
            Some(ConstFraction::one()),
            None,
        )],
        &[&EbiInputType::StringList],
    ],
    input_names: &["FILE", "VALUE", "TRACE"],
    input_helps: &[
        "The model.",
        "Balance between 0 (=only consider deviations) to 1 (=only consider weight in the model)",
        "The trace: the activity labels in trace order.",
    ],
    execute: |mut inputs, _| {
        let mut semantics = inputs.remove(0).to_type::<EbiTraitStochasticSemantics>()?;
        let balance = inputs.remove(0).to_type::<Fraction>()?;
        let trace = inputs.remove(0).to_type::<Vec<String>>()?;
        let trace = trace
            .iter()
            .map(|activity| activity.as_str())
            .collect::<Vec<_>>();
        let trace = semantics.activity_key_mut().process_trace_ref(&trace);

        log::trace!("explain the trace {:?} given the model", trace);

        let result = semantics
            .explain_trace(&trace, &balance)
            .with_context(|| format!("cannot explain the trace {:?}", trace))?;
        Ok(EbiOutput::Object(EbiObject::LanguageOfAlignments(result)))
    },
    output_type: &EbiOutputType::ObjectType(EbiObjectType::LanguageOfAlignments),
};

#[cfg(test)]
mod tests {
    use super::{EBI_PROBABILITY_EXPLAIN_TRACE, EBI_PROBABILITY_TRACE};

    #[test]
    fn prob_trac_and_exptra_are_exposed_to_python() {
        assert!(EBI_PROBABILITY_TRACE.is_in_python());
        assert!(EBI_PROBABILITY_EXPLAIN_TRACE.is_in_python());
    }

    #[test]
    fn prob_trac_and_exptra_are_not_exposed_to_javascript_or_java() {
        //JavascriptInput can only carry text or bytes, so it cannot carry a list of texts yet
        for command in [&EBI_PROBABILITY_TRACE, &EBI_PROBABILITY_EXPLAIN_TRACE] {
            assert!(!command.is_in_javascript());
            assert!(!command.is_in_java());
        }
    }
}
