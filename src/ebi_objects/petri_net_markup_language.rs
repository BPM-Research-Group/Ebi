use std::io::{BufRead, Write};

use anyhow::{Result, anyhow};

use crate::{
    ebi_framework::{
        ebi_file_handler::EbiFileHandler,
        ebi_input::{EbiObjectImporter, EbiTraitImporter},
        ebi_object::EbiObject,
        ebi_output::{EbiObjectExporter, EbiOutput},
        exportable::Exportable,
        importable::Importable,
    },
    ebi_traits::{
        ebi_trait_graphable::{self, EbiTraitGraphable},
        ebi_trait_semantics::EbiTraitSemantics,
    },
};

use super::{
    deterministic_finite_automaton::DeterministicFiniteAutomaton,
    directly_follows_model::DirectlyFollowsModel, labelled_petri_net::LabelledPetriNet,
    process_tree::ProcessTree,
    stochastic_deterministic_finite_automaton::StochasticDeterministicFiniteAutomaton,
    stochastic_labelled_petri_net::StochasticLabelledPetriNet,
    stochastic_process_tree::StochasticProcessTree,
};

pub const FORMAT_SPECIFICATION: &str =
    "A Petri net markup language file follows the ISO 15909-2:2011 format~\\cite{pnml}. 
Parsing is performed by the Rust4PM crate~\\cite{DBLP:conf/bpm/KustersA24}.
For instance:
    \\lstinputlisting[language=xml, style=boxed]{../testfiles/a.pnml}";

pub const EBI_PETRI_NET_MARKUP_LANGUAGE: EbiFileHandler = EbiFileHandler {
    name: "Petri net markup language",
    article: "a",
    file_extension: "pnml",
    format_specification: &FORMAT_SPECIFICATION,
    validator: PetriNetMarkupLanguage::validate,
    trait_importers: &[
        EbiTraitImporter::Semantics(PetriNetMarkupLanguage::import_as_semantics),
        EbiTraitImporter::Graphable(ebi_trait_graphable::import::<PetriNetMarkupLanguage>),
    ],
    object_importers: &[EbiObjectImporter::LabelledPetriNet(
        PetriNetMarkupLanguage::import_as_object,
    )],
    object_exporters: &[
        EbiObjectExporter::DeterministicFiniteAutomaton(PetriNetMarkupLanguage::export_from_object),
        EbiObjectExporter::DirectlyFollowsModel(PetriNetMarkupLanguage::export_from_object),
        EbiObjectExporter::LabelledPetriNet(PetriNetMarkupLanguage::export_from_object),
        EbiObjectExporter::ProcessTree(PetriNetMarkupLanguage::export_from_object),
        EbiObjectExporter::StochasticProcessTree(PetriNetMarkupLanguage::export_from_object),
        EbiObjectExporter::StochasticLabelledPetriNet(PetriNetMarkupLanguage::export_from_object),
        EbiObjectExporter::StochasticDeterministicFiniteAutomaton(
            PetriNetMarkupLanguage::export_from_object,
        ),
    ],
    java_object_handlers: &[], //java translations covered by LabelledPetrinet
};

#[derive(Clone)]
pub struct PetriNetMarkupLanguage {
    pub(crate) net: process_mining::PetriNet,
}

impl PetriNetMarkupLanguage {
    /**
     * PNML is always translated to LPN in Ebi. Therefore, validate that step as well.
     */
    pub fn validate(reader: &mut dyn BufRead) -> Result<()> {
        let pnml = Self::import(reader)?;
        LabelledPetriNet::try_from(pnml)?;
        Ok(())
    }

    pub fn import_as_labelled_petri_net(reader: &mut dyn BufRead) -> Result<LabelledPetriNet> {
        match Self::import(reader) {
            Ok(pnml) => Ok(LabelledPetriNet::try_from(pnml)?),
            Err(x) => Err(x),
        }
    }

    pub fn import_as_semantics(reader: &mut dyn BufRead) -> Result<EbiTraitSemantics> {
        let pnml = Self::import(reader)?;
        let lpn = LabelledPetriNet::try_from(pnml)?;
        Ok(EbiTraitSemantics::Marking(Box::new(lpn)))
    }
}

impl Importable for PetriNetMarkupLanguage {
    fn import_as_object(reader: &mut dyn BufRead) -> Result<EbiObject> {
        Ok(EbiObject::LabelledPetriNet(
            Self::import(reader)?.try_into()?,
        ))
    }

    fn import(reader: &mut dyn BufRead) -> Result<Self>
    where
        Self: Sized,
    {
        match process_mining::petri_net::import_pnml::import_pnml_reader(&mut Box::new(reader)) {
            Ok(pnml) => Ok(Self { net: pnml }),
            Err(e) => Err(anyhow!("{}", e)),
        }
    }
}

impl Exportable for PetriNetMarkupLanguage {
    fn export_from_object(object: EbiOutput, f: &mut dyn Write) -> Result<()> {
        match object {
            EbiOutput::Object(EbiObject::DeterministicFiniteAutomaton(dfa)) => {
                <DeterministicFiniteAutomaton as TryInto<PetriNetMarkupLanguage>>::try_into(dfa)?
                    .export(f)
            }
            EbiOutput::Object(EbiObject::DirectlyFollowsModel(dfm)) => {
                <DirectlyFollowsModel as TryInto<PetriNetMarkupLanguage>>::try_into(dfm)?.export(f)
            }
            EbiOutput::Object(EbiObject::LabelledPetriNet(lpn)) => {
                <LabelledPetriNet as TryInto<PetriNetMarkupLanguage>>::try_into(lpn)?.export(f)
            }
            EbiOutput::Object(EbiObject::ProcessTree(tree)) => {
                <ProcessTree as TryInto<PetriNetMarkupLanguage>>::try_into(tree)?.export(f)
            }
            EbiOutput::Object(EbiObject::StochasticProcessTree(tree)) => {
                <StochasticProcessTree as TryInto<PetriNetMarkupLanguage>>::try_into(tree)?.export(f)
            }
            EbiOutput::Object(EbiObject::StochasticDeterministicFiniteAutomaton(sdfa)) => {
                <StochasticDeterministicFiniteAutomaton as TryInto<PetriNetMarkupLanguage>>::try_into(
                    sdfa,
                )
                ?.export(f)
            }
            EbiOutput::Object(EbiObject::StochasticLabelledPetriNet(slpn)) => {
                <StochasticLabelledPetriNet as TryInto<PetriNetMarkupLanguage>>::try_into(slpn)?
                    .export(f)
            }

            EbiOutput::Bool(_) => Err(anyhow!("Cannot export boolean as PNML.")),
            EbiOutput::ContainsRoot(_) => Err(anyhow!("Cannot export ContainsRoot as PNML.")),
            EbiOutput::Fraction(_) => Err(anyhow!("Cannot export fraction as PNML.")),
            EbiOutput::LogDiv(_) => Err(anyhow!("Cannot export LogDiv as PNML.")),
            EbiOutput::PDF(_) => Err(anyhow!("Cannot export PDF as PNML.")),
            EbiOutput::RootLogDiv(_) => Err(anyhow!("Cannot export RootLogDiv as PNML.")),
            EbiOutput::SVG(_) => Err(anyhow!("Cannot export SVG as PNML.")),
            EbiOutput::String(_) => Err(anyhow!("Cannot export string as PNML.")),
            EbiOutput::Usize(_) => Err(anyhow!("Cannot export integer as PNML.")),
            EbiOutput::Object(EbiObject::EventLog(_)) => {
                Err(anyhow!("Cannot export event log as PNML."))
            }
            EbiOutput::Object(EbiObject::Executions(_)) => {
                Err(anyhow!("Cannot export executions as PNML."))
            }
            EbiOutput::Object(EbiObject::FiniteLanguage(_)) => {
                Err(anyhow!("Cannot export finite language as PNML."))
            }
            EbiOutput::Object(EbiObject::FiniteStochasticLanguage(_)) => {
                Err(anyhow!("Cannot export finite stochastic language as PNML."))
            }
            EbiOutput::Object(EbiObject::LanguageOfAlignments(_)) => {
                Err(anyhow!("Cannot export language of alignments as PNML."))
            }
            EbiOutput::Object(EbiObject::StochasticLanguageOfAlignments(_)) => Err(anyhow!(
                "Cannot export stochastic language of alignments as PNML."
            )),
        }
    }

    fn export(&self, f: &mut dyn std::io::Write) -> Result<()> {
        process_mining::petri_net::export_pnml::export_petri_net_to_pnml(&self.net, f)?;
        Ok(())
    }
}

impl EbiTraitGraphable for PetriNetMarkupLanguage {
    fn to_dot(&self) -> Result<layout::topo::layout::VisualGraph> {
        TryInto::<LabelledPetriNet>::try_into(self.clone())?.to_dot()
    }
}

#[cfg(test)]
mod tests {
    use std::fs::File;

    use crate::{
        ebi_objects::petri_net_markup_language::PetriNetMarkupLanguage,
        ebi_traits::ebi_trait_semantics::EbiTraitSemantics, multiple_reader::MultipleReader,
    };

    #[test]
    fn pnml_empty() {
        let mut reader = MultipleReader::from_file(File::open("testfiles/empty.pnml").unwrap());
        let semantics =
            PetriNetMarkupLanguage::import_as_semantics(&mut reader.get().unwrap()).unwrap();

        if let EbiTraitSemantics::Marking(semantics) = semantics {
            let state = semantics.get_initial_state().unwrap();
            assert_eq!(semantics.get_enabled_transitions(&state).len(), 0);
        } else {
            assert!(false);
        }
    }
}
