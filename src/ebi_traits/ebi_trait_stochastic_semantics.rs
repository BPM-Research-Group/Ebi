use anyhow::{Result, anyhow};
use std::io::BufRead;

use crate::{
    ebi_framework::{
        activity_key::ActivityKey, ebi_input::EbiInput, ebi_object::EbiTraitObject,
        ebi_trait::FromEbiTraitObject, importable::Importable,
    },
    ebi_objects::{labelled_petri_net::LPNMarking, stochastic_process_tree_semantics::NodeStates},
    math::fraction::Fraction,
};

use super::ebi_trait_semantics::Semantics;

pub enum EbiTraitStochasticSemantics {
    Usize(Box<dyn StochasticSemantics<StoSemState = usize, SemState = usize, AliState = usize>>),
    Marking(
        Box<
            dyn StochasticSemantics<
                    StoSemState = LPNMarking,
                    SemState = LPNMarking,
                    AliState = LPNMarking,
                >,
        >,
    ),
    NodeStates(
        Box<
            dyn StochasticSemantics<
                    StoSemState = NodeStates,
                    SemState = NodeStates,
                    AliState = NodeStates,
                >,
        >,
    ),
}

impl FromEbiTraitObject for EbiTraitStochasticSemantics {
    fn from_trait_object(object: EbiInput) -> Result<Box<Self>> {
        match object {
            EbiInput::Trait(EbiTraitObject::StochasticSemantics(e), _) => Ok(Box::new(e)),
            _ => Err(anyhow!(
                "cannot read {} {} as a stochastic semantics",
                object.get_type().get_article(),
                object.get_type()
            )),
        }
    }
}

impl EbiTraitStochasticSemantics {
    pub fn get_activity_key(&self) -> &ActivityKey {
        match self {
            EbiTraitStochasticSemantics::Marking(sem) => sem.get_activity_key(),
            EbiTraitStochasticSemantics::Usize(sem) => sem.get_activity_key(),
            EbiTraitStochasticSemantics::NodeStates(sem) => sem.get_activity_key(),
        }
    }

    pub fn get_activity_key_mut(&mut self) -> &mut ActivityKey {
        match self {
            EbiTraitStochasticSemantics::Marking(sem) => sem.get_activity_key_mut(),
            EbiTraitStochasticSemantics::Usize(sem) => sem.get_activity_key_mut(),
            EbiTraitStochasticSemantics::NodeStates(sem) => sem.get_activity_key_mut(),
        }
    }
}

pub trait StochasticSemantics:
    Semantics<SemState = <Self as StochasticSemantics>::StoSemState>
{
    type StoSemState;

    /**
     *
     * @param transition
     * @return the weight of the transition. This might depend on the state.
     */
    fn get_transition_weight(
        &self,
        state: &<Self as StochasticSemantics>::StoSemState,
        transition: TransitionIndex,
    ) -> &Fraction;

    /**
     *
     * @param enabledTransitions
     * @return the sum of the weight of the enabled transitions
     */
    fn get_total_weight_of_enabled_transitions(
        &self,
        state: &<Self as StochasticSemantics>::StoSemState,
    ) -> anyhow::Result<Fraction>;
}

pub type TransitionIndex = usize;

pub trait ToStochasticSemantics: Importable + Sized {
    fn to_stochastic_semantics(self) -> EbiTraitStochasticSemantics;

    fn import_as_stochastic_semantics(
        reader: &mut dyn BufRead,
    ) -> Result<EbiTraitStochasticSemantics> {
        Ok(Self::import(reader)?.to_stochastic_semantics())
    }
}
