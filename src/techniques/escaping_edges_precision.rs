use std::collections::{HashMap, hash_map::Entry};

use crate::{
    ebi_framework::displayable::Displayable, ebi_traits::ebi_trait_semantics::EbiTraitSemantics,
    semantics::semantics::Semantics,
};
use anyhow::{Context, Result, anyhow};
use ebi_arithmetic::{Fraction, Zero};
use ebi_objects::{StochasticLanguageOfAlignments, ebi_objects::language_of_alignments::Move};

pub const UNMATCHING: &str = "alignments and model do not belong to one another";

pub trait EscapingEdgesPrecision {
    fn escaping_edges_precision(
        &self,
        alignments: StochasticLanguageOfAlignments,
    ) -> Result<Fraction>;
}

impl EscapingEdgesPrecision for EbiTraitSemantics {
    fn escaping_edges_precision(
        &self,
        alignments: StochasticLanguageOfAlignments,
    ) -> Result<Fraction> {
        match self {
            EbiTraitSemantics::Usize(semantics) => semantics.escaping_edges_precision(alignments),
            EbiTraitSemantics::Marking(semantics) => semantics.escaping_edges_precision(alignments),
            EbiTraitSemantics::NodeStates(semantics) => {
                semantics.escaping_edges_precision(alignments)
            }
        }
    }
}

impl<T, State> EscapingEdgesPrecision for T
where
    T: Semantics<SemState = State> + Send + Sync + ?Sized,
    State: Displayable,
{
    fn escaping_edges_precision(
        &self,
        alignments: StochasticLanguageOfAlignments,
    ) -> Result<Fraction> {
        //step 1: build prefix tree
        let initial_state = self
            .get_initial_state()
            .ok_or_else(|| anyhow!("{}: model has no initial state", UNMATCHING))?;
        let mut nodes = vec![PrefixTreeNode::new(
            self.get_enabled_transitions(&initial_state).len(),
        )];
        for (alignment, probability) in alignments {
            let mut state = initial_state.clone();
            let mut node_index = 0;
            nodes[node_index].probability += &probability;

            //walk through the moves
            for movee in alignment {
                match movee {
                    Move::LogMove(_) => {} //a log move is ignored in precision
                    Move::ModelMove(_, transition)
                    | Move::SynchronousMove(_, transition)
                    | Move::SilentMove(transition) => {
                        /*
                            Update the state.
                            As we cannot rely on the alignment and the model belonging to one another, perform an enablement check.
                        */
                        if !self.get_enabled_transitions(&state).contains(&transition) {
                            return Err(anyhow!(
                                "{}: transition {} is not enabled",
                                UNMATCHING,
                                transition
                            ));
                        }
                        self.execute_transition(&mut state, transition)
                            .with_context(|| format!("cannot execute transition"))?;

                        //process the model move
                        let new_node_index = nodes.len();
                        let mut new_node = None;
                        match nodes[node_index].children.entry(transition) {
                            Entry::Occupied(occupied_entry) => {
                                //existing child: follow the path
                                node_index = *occupied_entry.get();
                                nodes[node_index].probability += &probability;
                            }
                            Entry::Vacant(vacant_entry) => {
                                //new child: create node
                                new_node = Some(PrefixTreeNode {
                                    probability: probability.clone(),
                                    number_of_outgoing_edges_in_model: self
                                        .get_enabled_transitions(&state)
                                        .len(),
                                    children: HashMap::new(),
                                });
                                vacant_entry.insert(new_node_index);
                            }
                        }
                        if let Some(new_add) = new_node {
                            nodes.push(new_add);
                        }
                    }
                }
            }
        }

        //step 2: count edges and compute the precision
        let mut sum_weight_taken = Fraction::zero();
        let mut sum_weight_enabled = Fraction::zero();
        for node in nodes {
            if node.number_of_outgoing_edges_in_model != 0 {
                let mut weight_taken = node.probability.clone();
                weight_taken *= node.children.len();
                sum_weight_taken += weight_taken;

                let mut weight_enabled = node.probability;
                weight_enabled *= node.number_of_outgoing_edges_in_model;
                sum_weight_enabled += weight_enabled;
            }
        }

        sum_weight_taken /= sum_weight_enabled;
        Ok(sum_weight_taken)
    }
}

struct PrefixTreeNode {
    probability: Fraction,
    number_of_outgoing_edges_in_model: usize,
    children: HashMap<usize, usize>,
}

impl PrefixTreeNode {
    pub fn new(number_of_outgoing_edges_in_model: usize) -> Self {
        Self {
            probability: Fraction::zero(),
            number_of_outgoing_edges_in_model: number_of_outgoing_edges_in_model,
            children: HashMap::new(),
        }
    }
}

#[cfg(test)]
mod tests {
    use std::fs;

    use ebi_arithmetic::Fraction;
    use ebi_objects::{StochasticDeterministicFiniteAutomaton, StochasticLanguageOfAlignments};

    use crate::techniques::escaping_edges_precision::EscapingEdgesPrecision;

    #[test]
    fn precision_test() {
        let fin = fs::read_to_string("testfiles/aa-ab-ba.slang_a-b-c-livelock.sdfa.sali").unwrap();
        let sali = fin.parse::<StochasticLanguageOfAlignments>().unwrap();

        let fin = fs::read_to_string("testfiles/a-b-c-livelock.sdfa").unwrap();
        let sdfa = fin
            .parse::<StochasticDeterministicFiniteAutomaton>()
            .unwrap();

        assert_eq!(
            sdfa.escaping_edges_precision(sali).unwrap(),
            Fraction::from((17, 20))
        );
    }
}
