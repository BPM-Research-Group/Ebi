use std::collections::HashSet;

use crate::{
    ebi_framework::activity_key::Activity,
    ebi_objects::{
        directly_follows_graph::DirectlyFollowsGraph, directly_follows_model::DirectlyFollowsModel,
    },
    ebi_traits::ebi_trait_finite_stochastic_language::EbiTraitFiniteStochasticLanguage,
    math::{fraction::Fraction, traits::One},
};

use super::directly_follows_graph_abstractor::DirectlyFollowsGraphAbstractor;

pub trait DirectlyFollowsModelMiner {
    fn mine_directly_follows_model(&mut self) {
        self.mine_directly_follows_model_filtering(&Fraction::one());
    }

    /**
     * This will yield a DFM with at least the given fitness. This method may change the log in-place.
     */
    fn mine_directly_follows_model_filtering(
        &mut self,
        minimum_fitness: &Fraction,
    ) -> DirectlyFollowsModel;
}

impl DirectlyFollowsModelMiner for dyn EbiTraitFiniteStochasticLanguage {
    fn mine_directly_follows_model_filtering(
        &mut self,
        minimum_fitness: &Fraction,
    ) -> DirectlyFollowsModel {
        let mut dfm = self.abstract_to_directly_follows_graph();

        loop {
            //gather the edges to be filtered
            let edges_to_filter = get_edges_to_filter(&dfm);
            if edges_to_filter.is_empty() {
                return dfm.into();
            }

            //filter the log
            self.remove_traces_with_directly_follows_edge(edges_to_filter);

            if &self.get_probability_sum() < minimum_fitness {
                return dfm.into();
            }

            //create a new dfg
            dfm = self.abstract_to_directly_follows_graph();
        }
    }
}

impl dyn EbiTraitFiniteStochasticLanguage {
    fn remove_traces_with_directly_follows_edge(
        &mut self,
        edges_to_filter: HashSet<(Option<Activity>, Option<Activity>)>,
    ) {
        self.retain_traces(Box::new(move |trace, _| {
            let mut edge = (None, None);
            for activity in trace {
                let activity: &Activity = activity;
                edge = (edge.1, Some(activity.clone()));

                if edges_to_filter.contains(&edge) {
                    return false;
                }
            }

            edge = (edge.1, None);
            !edges_to_filter.contains(&edge)
        }));
    }
}

fn get_edges_to_filter(
    dfm: &DirectlyFollowsGraph,
) -> HashSet<(Option<Activity>, Option<Activity>)> {
    let mut min = &Fraction::one();
    let mut result = HashSet::new();

    //start edges
    for (activity, weight) in dfm.start_activities.iter() {
        if weight == min {
            result.insert((None, Some(activity.clone())));
        } else if weight < min {
            min = weight;
            result.clear();
            result.insert((None, Some(activity.clone())));
        }
    }

    //normal edges
    dfm.sources
        .iter()
        .zip(dfm.targets.iter().zip(dfm.weights.iter()))
        .for_each(|(source, (target, weight))| {
            if weight == min {
                result.insert((Some(source.clone()), Some(target.clone())));
            } else {
                min = weight;
                result.clear();
                result.insert((Some(source.clone()), Some(target.clone())));
            }
        });

    //end edges
    for (activity, weight) in dfm.end_activities.iter() {
        if weight == min {
            result.insert((Some(activity.clone()), None));
        } else if weight < min {
            min = weight;
            result.clear();
            result.insert((Some(activity.clone()), None));
        }
    }

    return result;
}
