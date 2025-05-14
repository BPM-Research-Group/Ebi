use crate::{
    ebi_objects::stochastic_directly_follows_model::StochasticDirectlyFollowsModel,
    ebi_traits::ebi_trait_finite_stochastic_language::EbiTraitFiniteStochasticLanguage,
};

pub trait DirectlyFollowsGraphAbstractor {
    fn abstract_to_directly_follows_graph(&self) -> StochasticDirectlyFollowsModel;
}

impl DirectlyFollowsGraphAbstractor for dyn EbiTraitFiniteStochasticLanguage {
    fn abstract_to_directly_follows_graph(&self) -> StochasticDirectlyFollowsModel {
        let mut result = StochasticDirectlyFollowsModel::new(self.get_activity_key().clone());

        //If the language has no traces, then the directly follows graph has no start activities and no empty traces.

        for (trace, probability) in self.iter_trace_probability() {
            let mut last_activity = None;
            for activity in trace {
                match last_activity {
                    Some(previous) => {
                        //edge
                        result.add_edge(previous, *activity, probability);
                    }
                    None => {
                        //start activity
                        result.add_start_activity(*activity, probability);
                    }
                }
                last_activity = Some(*activity);
            }

            match last_activity {
                Some(activity) => result.add_end_activity(activity, probability),
                None => result.add_empty_trace(probability),
            };
        }

        result
    }
}
