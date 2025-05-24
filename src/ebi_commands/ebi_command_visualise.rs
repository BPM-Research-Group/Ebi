use crate::{
    ebi_framework::{
        ebi_command::EbiCommand,
        ebi_input::{EbiInput, EbiInputType},
        ebi_object::EbiObject,
        ebi_output::{EbiOutput, EbiOutputType},
        ebi_trait::EbiTrait,
    },
    ebi_objects::scalable_vector_graphics::{svg_to_pdf, to_svg_string_box},
    ebi_traits::ebi_trait_graphable::EbiTraitGraphable,
};

pub const EBI_VISUALISE: EbiCommand = EbiCommand::Group {
    name_short: "vis",
    name_long: Some("visualise"),
    explanation_short: "Visualse an object.",
    explanation_long: None,
    children: &[&EBI_VISUALISE_PDF, &EBI_VISUALISE_SVG, &EBI_VISUALISE_TEXT],
};

pub const EBI_VISUALISE_TEXT: EbiCommand = EbiCommand::Command {
    name_short: "txt",
    name_long: Some("text"),
    explanation_short: "Visualise an object as text.",
    explanation_long: None,
    latex_link: None,
    cli_command: None,
    exact_arithmetic: false,
    input_types: &[&[&EbiInputType::AnyObject]],
    input_names: &["FILE"],
    input_helps: &["Any file that can be visualised textually."],
    execute: |mut inputs, _| {
        let result = match inputs.remove(0) {
            EbiInput::Object(EbiObject::StochasticLabelledPetriNet(slpn), _) => slpn.to_string(),
            EbiInput::Object(EbiObject::LabelledPetriNet(lpn), _) => lpn.to_string(),
            EbiInput::Object(EbiObject::FiniteStochasticLanguage(lang), _) => lang.to_string(),
            EbiInput::Object(EbiObject::StochasticDeterministicFiniteAutomaton(sdfa), _) => {
                sdfa.to_string()
            }
            EbiInput::Object(EbiObject::EventLog(log), _) => log.to_string(),
            EbiInput::Object(EbiObject::FiniteLanguage(language), _) => language.to_string(),
            EbiInput::Object(EbiObject::DirectlyFollowsModel(d), _) => d.to_string(),
            EbiInput::Object(EbiObject::StochasticDirectlyFollowsModel(d), _) => d.to_string(),
            EbiInput::Object(EbiObject::LanguageOfAlignments(a), _) => a.to_string(),
            EbiInput::Object(EbiObject::StochasticLanguageOfAlignments(a), _) => a.to_string(),
            EbiInput::Object(EbiObject::DeterministicFiniteAutomaton(s), _) => s.to_string(),
            EbiInput::Object(EbiObject::ProcessTree(pt), _) => pt.to_string(),
            EbiInput::Object(EbiObject::StochasticProcessTree(pt), _) => pt.to_string(),
            EbiInput::Object(EbiObject::Executions(s), _) => s.to_string(),
            EbiInput::Object(EbiObject::DirectlyFollowsGraph(s), _) => s.to_string(),
            EbiInput::FileHandler(_) => unreachable!(),
            EbiInput::Trait(_, _) => unreachable!(),
            EbiInput::String(_) => unreachable!(),
            EbiInput::Usize(_) => unreachable!(),
            EbiInput::Fraction(_) => unreachable!(),
        };
        Ok(EbiOutput::String(result))
    },
    output_type: &EbiOutputType::String,
};

pub const EBI_VISUALISE_SVG: EbiCommand = EbiCommand::Command {
    name_short: "svg",
    name_long: None,
    explanation_short: "Visualise an object as scalable vector graphics.",
    explanation_long: None,
    latex_link: None,
    cli_command: None,
    exact_arithmetic: true,
    input_types: &[&[&EbiInputType::Trait(EbiTrait::Graphable)]],
    input_names: &["FILE"],
    input_helps: &["Any file that can be visualised as a graph."],
    execute: |mut inputs, _| {
        let result: Box<dyn EbiTraitGraphable + 'static> =
            inputs.remove(0).to_type::<dyn EbiTraitGraphable>()?;

        let svg_string = to_svg_string_box(result)?;
        return Ok(EbiOutput::SVG(svg_string));
    },
    output_type: &EbiOutputType::SVG,
};

pub const EBI_VISUALISE_PDF: EbiCommand = EbiCommand::Command {
    name_short: "pdf",
    name_long: None,
    explanation_short: "Visualise an object as portable document format.",
    explanation_long: None,
    latex_link: None,
    cli_command: None,
    exact_arithmetic: true,
    input_types: &[&[&EbiInputType::Trait(EbiTrait::Graphable)]],
    input_names: &["FILE"],
    input_helps: &["Any file that can be visualised as a graph."],
    execute: |mut inputs, _| {
        let result = inputs.remove(0).to_type::<dyn EbiTraitGraphable>()?;

        let svg_string = to_svg_string_box(result)?;
        let pdf = svg_to_pdf(&svg_string)?;

        return Ok(EbiOutput::PDF(pdf));
    },
    output_type: &EbiOutputType::PDF,
};

#[cfg(test)]
mod tests {

    use crate::{
        ebi_commands::ebi_command_visualise::EBI_VISUALISE_TEXT,
        ebi_framework::{ebi_command::EbiCommand, ebi_input::EbiInput, ebi_object::EbiTraitObject},
        ebi_objects::{
            finite_language::FiniteLanguage,
            stochastic_labelled_petri_net::EBI_STOCHASTIC_LABELLED_PETRI_NET,
        },
        math::{fraction::Fraction, traits::One},
    };

    #[test]
    fn visualise_as_text() {
        for (object, _, _, _) in crate::tests::get_all_test_files() {
            if let EbiInput::Object(_, _) = object {
                if let EbiCommand::Command { execute, .. } = EBI_VISUALISE_TEXT {
                    let _ = (execute)(vec![object], None);
                }
            }
        }
    }

    #[test]
    #[should_panic]
    fn unreachable_filehandler() {
        let object = EbiInput::FileHandler(EBI_STOCHASTIC_LABELLED_PETRI_NET);
        if let EbiCommand::Command { execute, .. } = EBI_VISUALISE_TEXT {
            let _ = (execute)(vec![object], None);
        }
    }

    #[test]
    #[should_panic]
    fn unreachable_string() {
        let object = EbiInput::String("abc".to_string());
        if let EbiCommand::Command { execute, .. } = EBI_VISUALISE_TEXT {
            let _ = (execute)(vec![object], None);
        }
    }

    #[test]
    #[should_panic]
    fn unreachable_usize() {
        let object = EbiInput::Usize(10);
        if let EbiCommand::Command { execute, .. } = EBI_VISUALISE_TEXT {
            let _ = (execute)(vec![object], None);
        }
    }

    #[test]
    #[should_panic]
    fn unreachable_fraction() {
        let object = EbiInput::Fraction(Fraction::one());
        if let EbiCommand::Command { execute, .. } = EBI_VISUALISE_TEXT {
            let _ = (execute)(vec![object], None);
        }
    }

    #[test]
    #[should_panic]
    fn unreachable_trait() {
        let net = "finite language\n0";
        let lang = net.parse::<FiniteLanguage>().unwrap();
        let object = EbiInput::Trait(
            EbiTraitObject::FiniteLanguage(Box::new(lang)),
            &EBI_STOCHASTIC_LABELLED_PETRI_NET,
        );
        if let EbiCommand::Command { execute, .. } = EBI_VISUALISE_TEXT {
            let _ = (execute)(vec![object], None);
        }
    }
}
