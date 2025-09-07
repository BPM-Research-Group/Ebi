use std::collections::HashSet;

use anyhow::{Result, anyhow};
use ebi_arithmetic::{Fraction, One};
use ebi_objects::{Activity, DirectlyFollowsGraph};

use crate::ebi_traits::{
    ebi_trait_event_log::EbiTraitEventLog,
    ebi_trait_finite_stochastic_language::EbiTraitFiniteStochasticLanguage,
};

use super::directly_follows_graph_abstractor::DirectlyFollowsAbstractor;

pub trait DirectlyFollowsModelMinerFiltering {
    /**
     * This will yield a DFM with at least the given fitness. This method may change the log in-place.
     */
    fn mine_directly_follows_model_filtering(
        &mut self,
        minimum_fitness: &Fraction,
    ) -> Result<DirectlyFollowsGraph>;
}

impl DirectlyFollowsModelMinerFiltering for dyn EbiTraitEventLog {
    fn mine_directly_follows_model_filtering(
        &mut self,
        minimum_fitness: &Fraction,
    ) -> Result<DirectlyFollowsGraph> {
        if minimum_fitness > &Fraction::one() {
            return Err(anyhow!("cannot obtain a minimum fitness larger than 1"));
        }

        let mut dfg = self.abstract_to_directly_follows_graph();

        if minimum_fitness.is_one() {
            return Ok(dfg);
        }

        let mut minimum_fitness = minimum_fitness.clone();
        minimum_fitness *= self.number_of_traces();

        loop {
            //gather the edges to be filtered
            let edges_to_filter = get_edges_to_filter(&dfg);
            if edges_to_filter.is_empty() {
                return Ok(dfg);
            }

            //filter the log
            self.remove_traces_with_directly_follows_edge(edges_to_filter);

            if Fraction::from(self.number_of_traces()) < minimum_fitness {
                return Ok(dfg);
            }

            //create a new dfg
            dfg = self.abstract_to_directly_follows_graph();
        }
    }
}

impl DirectlyFollowsModelMinerFiltering for dyn EbiTraitFiniteStochasticLanguage {
    fn mine_directly_follows_model_filtering(
        &mut self,
        minimum_fitness: &Fraction,
    ) -> Result<DirectlyFollowsGraph> {
        if minimum_fitness > &Fraction::one() {
            return Err(anyhow!("cannot obtain a minimum fitness larger than 1"));
        }

        let mut dfg = self.abstract_to_directly_follows_graph();

        if minimum_fitness.is_one() {
            return Ok(dfg);
        }

        loop {
            //gather the edges to be filtered
            let edges_to_filter = get_edges_to_filter(&dfg);
            if edges_to_filter.is_empty() {
                return Ok(dfg);
            }

            //filter the log
            self.remove_traces_with_directly_follows_edge(edges_to_filter);

            if &self.get_probability_sum() < minimum_fitness {
                return Ok(dfg);
            }

            //create a new dfg
            dfg = self.abstract_to_directly_follows_graph();
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

impl dyn EbiTraitEventLog {
    fn remove_traces_with_directly_follows_edge(
        &mut self,
        edges_to_filter: HashSet<(Option<Activity>, Option<Activity>)>,
    ) {
        self.retain_traces(Box::new(move |trace| {
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

#[cfg(test)]
mod tests {
    use std::fs;

    use ebi_arithmetic::{Fraction, One};
    use ebi_objects::FiniteStochasticLanguage;

    use crate::{
        ebi_traits::ebi_trait_finite_stochastic_language::EbiTraitFiniteStochasticLanguage,
        semantics::semantics::Semantics,
        techniques::directly_follows_model_miner::DirectlyFollowsModelMinerFiltering,
    };

    #[test]
    fn dfm() {
        let fin = fs::read_to_string("testfiles/aa-ab-ba.slang").unwrap();
        let mut slang: Box<dyn EbiTraitFiniteStochasticLanguage> =
            Box::new(fin.parse::<FiniteStochasticLanguage>().unwrap());
        let dfm = slang
            .mine_directly_follows_model_filtering(&Fraction::one())
            .unwrap();

        let state = dfm.get_initial_state().unwrap();

        assert_eq!(dfm.get_enabled_transitions(&state).len(), 2);
    }
}
