use std::{
    collections::HashMap,
    io::{BufRead, Write},
};

use anyhow::{Result, anyhow};
use fraction::ToPrimitive;
use process_mining::{
    PetriNet,
    petri_net::petri_net_struct::{self, ArcType},
};

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
        ebi_trait_semantics::{EbiTraitSemantics, Semantics},
    },
};

use super::{
    labelled_petri_net::LabelledPetriNet, stochastic_labelled_petri_net::StochasticLabelledPetriNet,
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
        EbiObjectExporter::LabelledPetriNet(PetriNetMarkupLanguage::export_from_object),
        EbiObjectExporter::StochasticLabelledPetriNet(
            PetriNetMarkupLanguage::export_from_stochastic_labelled_petri_net,
        ),
        EbiObjectExporter::StochasticDeterministicFiniteAutomaton(
            PetriNetMarkupLanguage::export_from_stochastic_deterministic_finite_automaton,
        ),
        EbiObjectExporter::DirectlyFollowsModel(
            PetriNetMarkupLanguage::export_from_directly_follows_model,
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

    fn export_from_stochastic_labelled_petri_net(
        object: EbiOutput,
        f: &mut dyn std::io::Write,
    ) -> Result<()> {
        match object {
            EbiOutput::Object(EbiObject::StochasticLabelledPetriNet(slpn)) => {
                let lpn = <StochasticLabelledPetriNet as Into<LabelledPetriNet>>::into(slpn);
                PetriNetMarkupLanguage::try_from(lpn)?.export(f)
            }
            _ => unreachable!(),
        }
    }

    fn export_from_stochastic_deterministic_finite_automaton(
        object: EbiOutput,
        f: &mut dyn std::io::Write,
    ) -> Result<()> {
        match object {
            EbiOutput::Object(EbiObject::StochasticDeterministicFiniteAutomaton(sdfa)) => {
                let lpn = <StochasticLabelledPetriNet as Into<LabelledPetriNet>>::into(sdfa.into());
                PetriNetMarkupLanguage::try_from(lpn)?.export(f)
            }
            _ => unreachable!(),
        }
    }

    fn export_from_directly_follows_model(
        object: EbiOutput,
        f: &mut dyn std::io::Write,
    ) -> Result<()> {
        match object {
            EbiOutput::Object(EbiObject::DirectlyFollowsModel(dfm)) => {
                let lpn: LabelledPetriNet = dfm.into();
                PetriNetMarkupLanguage::try_from(lpn)?.export(f)
            }
            _ => unreachable!(),
        }
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
            EbiOutput::Object(EbiObject::LabelledPetriNet(lpn)) => {
                PetriNetMarkupLanguage::try_from(lpn)?.export(f)
            }
            _ => unreachable!(),
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

impl TryFrom<LabelledPetriNet> for PetriNetMarkupLanguage {
    type Error = anyhow::Error;

    fn try_from(lpn: LabelledPetriNet) -> std::result::Result<Self, Self::Error> {
        log::info!("Convert LPN into PNML.");

        let mut result = PetriNet::new();

        //create places
        let mut place2new_place = HashMap::new();
        {
            for place in 0..lpn.get_number_of_places() {
                let new_place = result.add_place(None);
                place2new_place.insert(place, new_place);
            }
        }

        //create transitions
        for transition in 0..lpn.get_number_of_transitions() {
            let new_transition = result.add_transition(
                match lpn.get_transition_label(transition) {
                    Some(activity) => {
                        Some(lpn.activity_key.get_activity_label(&activity).to_string())
                    }
                    None => None,
                },
                None,
            );

            //incoming
            {
                //transform to a map of places and arc weights
                let mut map = HashMap::new();
                for (pos, place) in lpn.transition2input_places[transition].iter().enumerate() {
                    *map.entry(*place).or_insert(0) +=
                        u32::try_from(lpn.transition2input_places_cardinality[transition][pos])?;
                }

                //add
                for (place, weight) in map {
                    let new_place = place2new_place
                        .get(&place)
                        .ok_or(anyhow!("Non-existing place referenced."))?;
                    result.add_arc(
                        ArcType::place_to_transition(*new_place, new_transition),
                        Some(weight),
                    );
                }
            }

            //outgoing
            {
                //transform to a map of places and arc weights
                let mut map = HashMap::new();
                for (pos, place) in lpn.transition2output_places[transition].iter().enumerate() {
                    *map.entry(*place).or_insert(0) +=
                        u32::try_from(lpn.transition2output_places_cardinality[transition][pos])?;
                }

                //add
                for (place, weight) in map {
                    let new_place = place2new_place
                        .get(&place)
                        .ok_or(anyhow!("Non-existing place referenced."))?;
                    result.add_arc(
                        ArcType::transition_to_place(new_transition, *new_place),
                        Some(weight.to_u32().ok_or(anyhow!("value out of bounds"))?),
                    );
                }
            }
        }

        //initial marking
        let mut new_initial_marking = petri_net_struct::Marking::new();
        for (place, cardinality) in lpn
            .initial_marking
            .get_place2token()
            .into_iter()
            .enumerate()
        {
            if cardinality > &0u64 {
                let new_place = place2new_place
                    .get(&place)
                    .ok_or(anyhow!("Non-existing place referenced."))?;
                new_initial_marking.insert(*new_place, *cardinality);
            }
        }
        result.initial_marking = Some(new_initial_marking);

        Ok(Self { net: result })
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
