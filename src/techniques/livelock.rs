use crate::{
    ebi_framework::displayable::Displayable,
    semantics::{labelled_petri_net_semantics::LPNMarking, semantics::Semantics},
};
use ebi_objects::{
    AutomatonSemantics, AutomatonState, DeterministicFiniteAutomaton, DirectlyFollowsGraph,
    DirectlyFollowsModel, HasActivityKey, LabelledPetriNet, ProcessTree,
    StochasticDeterministicFiniteAutomaton, StochasticDirectlyFollowsModel,
    StochasticLabelledPetriNet, StochasticNondeterministicFiniteAutomaton,
    anyhow::{Result, anyhow},
    ebi_objects::process_tree::TreeMarking,
};
use std::collections::{HashMap, HashSet, hash_map::Entry};

pub trait IsPartOfLivelock {
    type LivState: Displayable;

    /**
     * If more than a couple of states need to be checked for livelocks, then use the livelock cache instead.
     */
    fn is_state_part_of_livelock(&self, state: &Self::LivState) -> Result<bool>;

    /**
     * If only a few livelocks need to be checked, then individual calls to is_state_part_of_livelock may be faster.
     * If more than a few livelock calls need to be made, use this cache object.
     */
    fn get_livelock_cache(&self) -> Box<dyn LiveLockCache<LivState = Self::LivState> + '_>;
}

pub trait LiveLockCache {
    type LivState;

    fn is_state_part_of_livelock(&mut self, state: &Self::LivState) -> Result<bool>;
}

impl IsPartOfLivelock for ProcessTree {
    type LivState = TreeMarking;

    fn is_state_part_of_livelock(&self, _state: &Self::LivState) -> Result<bool> {
        Ok(false)
    }

    fn get_livelock_cache(&self) -> Box<dyn LiveLockCache<LivState = Self::LivState> + '_> {
        Box::new(LiveLockCacheProcessTree {})
    }
}

pub struct LiveLockCacheProcessTree {}

impl LiveLockCache for LiveLockCacheProcessTree {
    type LivState = TreeMarking;

    fn is_state_part_of_livelock(&mut self, _: &Self::LivState) -> Result<bool> {
        Ok(false)
    }
}

/**
 * Map of states:
 * 0..nodes:        after executing the activity, we end up in this state.
 * nodes:           end
 * nodes + 1:       start
 *
 * Map of transitions:
 * 0..nodes:        the activity
 * nodes:           terminate
 */
macro_rules! dfm {
    ($t:ident, $u:ident, $s:ident) => {
        impl IsPartOfLivelock for $t {
            type LivState = $s;

            fn is_state_part_of_livelock(&self, state: &Self::LivState) -> Result<bool> {
                let mut queue = vec![];
                queue.push(state.clone());
                let mut visited = HashSet::new();
                visited.insert(state.clone());

                while let Some(state) = queue.pop() {
                    if self.is_final_state(&state) {
                        return Ok(false);
                    }

                    for transition in self.get_enabled_transitions(&state) {
                        let mut child_state = state.clone();
                        self.execute_transition(&mut child_state, transition)?;

                        if visited.insert(child_state.clone()) {
                            queue.push(child_state);
                        }
                    }
                }

                return Ok(true);
            }

            fn get_livelock_cache(&self) -> Box<dyn LiveLockCache<LivState = Self::LivState> + '_> {
                Box::new($u::new(&self))
            }
        }

        pub struct $u(Vec<bool>);
    };
}

macro_rules! dfm_cache {
    ($t:ident, $u:ident, $s:ident) => {
        impl $u {
            pub fn new(dfm: &$t) -> Self {
                let mut result = vec![true; dfm.node_2_activity.len() + 2];
                let mut queue = vec![];
                result[dfm.node_2_activity.len()] = false;
                (0..dfm.node_2_activity.len()).into_iter().for_each(|node| {
                    if dfm.is_end_node(node) {
                        result[node] = false;
                        queue.push(node)
                    }
                });

                // log::debug!("queue {:?}, result {:?}", queue, result);

                while let Some(state) = queue.pop() {
                    // log::debug!("queue {:?}, result {:?}, state {}", queue, result, state);

                    //walk over the edges that go into state (expensive :'( )
                    for (source, target) in dfm.sources.iter().zip(dfm.targets.iter()) {
                        if result[*source] && *target == state {
                            result[*source] = false;
                            queue.push(*source);
                        }
                    }
                }

                if (0..dfm.node_2_activity.len())
                    .into_iter()
                    .any(|node| !result[node])
                {
                    result[dfm.node_2_activity.len() + 1] = false;
                }

                Self(result)
            }
        }

        impl LiveLockCache for $u {
            type LivState = $s;

            fn is_state_part_of_livelock(&mut self, state: &Self::LivState) -> Result<bool> {
                self.0
                    .get(*state)
                    .copied()
                    .ok_or_else(|| anyhow!("index out of bounds"))
            }
        }
    };
}
dfm!(
    DirectlyFollowsModel,
    DirectlyFollowsModelLiveLockCache,
    usize
);
dfm!(
    StochasticDirectlyFollowsModel,
    StochasticDirectlyFollowsModelLiveLockCache,
    usize
);
dfm_cache!(
    DirectlyFollowsModel,
    DirectlyFollowsModelLiveLockCache,
    usize
);
dfm_cache!(
    StochasticDirectlyFollowsModel,
    StochasticDirectlyFollowsModelLiveLockCache,
    usize
);
dfm!(
    DirectlyFollowsGraph,
    DirectlyFollowsGraphLiveLockCache,
    AutomatonState
);

impl DirectlyFollowsGraphLiveLockCache {
    pub fn new(dfg: &DirectlyFollowsGraph) -> Self {
        let mut result = vec![true; dfg.number_of_states()];
        let mut queue = vec![];
        result[dfg.number_of_states() - 2] = false;
        for (activity, node) in dfg.activity_2_state.iter() {
            if dfg.is_end_node(activity) {
                result[node.0] = false;
                queue.push(*node)
            }
        }

        // log::debug!("queue {:?}, result {:?}", queue, result);

        while let Some(state) = queue.pop() {
            // log::debug!("queue {:?}, result {:?}, state {}", queue, result, state);

            //walk over the edges that go into state (expensive :'( )
            for (source, target) in dfg.sources.iter().zip(dfg.targets.iter()) {
                if result[source.0] && *target == state {
                    result[source.0] = false;
                    queue.push(*source);
                }
            }
        }

        if (0..dfg.activity_key().get_number_of_activities())
            .into_iter()
            .any(|node| !result[node])
        {
            result[dfg.activity_key.get_number_of_activities() + 1] = false;
        }

        Self(result)
    }
}

impl LiveLockCache for DirectlyFollowsGraphLiveLockCache {
    type LivState = AutomatonState;

    fn is_state_part_of_livelock(&mut self, state: &Self::LivState) -> Result<bool> {
        if state.0 > self.0.len() {
            return Err(anyhow!("State does not exist."));
        }
        Ok(self.0[state])
    }
}

macro_rules! lpn {
    ($t:ident, $u:ident) => {
        impl IsPartOfLivelock for $t {
            type LivState = LPNMarking;

            fn is_state_part_of_livelock(&self, state: &Self::LivState) -> Result<bool> {
                //for now, the following only works if the model is bounded
                let mut queue = vec![];
                queue.push(state.clone());
                let mut visited = HashSet::new();
                visited.insert(state.clone());

                while let Some(state) = queue.pop() {
                    if self.is_final_state(&state) {
                        return Ok(false);
                    }

                    for transition in self.get_enabled_transitions(&state) {
                        let mut child_state = state.clone();
                        self.execute_transition(&mut child_state, transition)?;

                        if visited.insert(child_state.clone()) {
                            queue.push(child_state);
                        }
                    }
                }

                return Ok(true);
            }

            fn get_livelock_cache(&self) -> Box<dyn LiveLockCache<LivState = Self::LivState> + '_> {
                Box::new($u::new(&self))
            }
        }

        pub struct $u<'a>(&'a $t, HashMap<LPNMarking, bool>, bool);

        impl<'a> $u<'a> {
            pub fn new(net: &'a $t) -> Self {
                //if there is a transition without an input place, the net is always livelocked.
                let trivial_livelock = net
                    .transition2input_places
                    .iter()
                    .any(|input_places| input_places.is_empty());

                Self(net, HashMap::new(), trivial_livelock)
            }
        }

        impl<'a> LiveLockCache for $u<'a> {
            type LivState = LPNMarking;

            fn is_state_part_of_livelock(&mut self, state: &Self::LivState) -> Result<bool> {
                if self.2 {
                    return Ok(true);
                }

                match self.1.entry(state.clone()) {
                    Entry::Occupied(occupied_entry) => Ok(*occupied_entry.get()),
                    Entry::Vacant(vacant_entry) => {
                        let answer = self.0.is_state_part_of_livelock(state)?;
                        vacant_entry.insert(answer);
                        Ok(answer)
                    }
                }
            }
        }
    };
}

macro_rules! dfa {
    ($t:ident, $u:ident) => {
        impl IsPartOfLivelock for $t {
            type LivState = usize;

            fn is_state_part_of_livelock(&self, state: &Self::LivState) -> Result<bool> {
                let mut queue = vec![];
                queue.push(state.clone());
                let mut visited = HashSet::new();
                visited.insert(state.clone());

                while let Some(state) = queue.pop() {
                    if self.is_final_state(&state) {
                        return Ok(false);
                    }

                    for transition in self.get_enabled_transitions(&state) {
                        let mut child_state = state.clone();
                        self.execute_transition(&mut child_state, transition)?;

                        if visited.insert(child_state.clone()) {
                            queue.push(child_state);
                        }
                    }
                }

                return Ok(true);
            }

            fn get_livelock_cache(&self) -> Box<dyn LiveLockCache<LivState = Self::LivState>> {
                Box::new($u::new(self))
            }
        }

        pub struct $u(Vec<bool>);

        impl $u {
            pub fn new(automaton: &$t) -> Self {
                let mut result = vec![true; automaton.number_of_states() + 2];
                let mut result_last = vec![true; automaton.number_of_states() + 2];

                //stage 1: set final states
                result[automaton.number_of_states() + 1] = false;
                for state in 0..automaton.number_of_states() {
                    if automaton.can_terminate_in_state(state) {
                        result[state] = false;
                    }
                }

                //stage 2: waterfall
                while result != result_last {
                    result_last.clone_from(&result);

                    for (source, target) in
                        automaton.get_sources().iter().zip(automaton.get_targets())
                    {
                        if !result[*target] {
                            result[*source] = false;
                        }
                    }
                }

                Self(result)
            }
        }

        impl LiveLockCache for $u {
            type LivState = usize;

            fn is_state_part_of_livelock(&mut self, state: &Self::LivState) -> Result<bool> {
                self.0
                    .get(*state)
                    .copied()
                    .ok_or_else(|| anyhow!("index out of bounds"))
            }
        }
    };
}

lpn!(LabelledPetriNet, LiveLockCacheLabelledPetriNet);
lpn!(
    StochasticLabelledPetriNet,
    LiveLockCacheStochasticLabelledPetriNet
);
dfa!(
    DeterministicFiniteAutomaton,
    LivelockCacheDeterministicFiniteAutomaton
);
dfa!(
    StochasticDeterministicFiniteAutomaton,
    LivelockCacheStochasticDeterministicAutomaton
);
dfa!(
    StochasticNondeterministicFiniteAutomaton,
    LivelockCacheStochasticNondeterministicAutomaton
);

#[cfg(test)]
mod tests {
    use std::fs;

    use crate::{semantics::semantics::Semantics, techniques::livelock::IsPartOfLivelock};
    use ebi_objects::{
        DeterministicFiniteAutomaton, DirectlyFollowsModel, LabelledPetriNet,
        StochasticDeterministicFiniteAutomaton, StochasticLabelledPetriNet,
    };

    #[test]
    fn livelock_lpn() {
        let fin = fs::read_to_string("testfiles/a-a-livelock.lpn").unwrap();
        let object = fin.parse::<LabelledPetriNet>().unwrap();

        assert!(
            object
                .get_livelock_cache()
                .is_state_part_of_livelock(&object.get_initial_state().unwrap())
                .unwrap()
        );
    }

    #[test]
    fn livelock_slpn() {
        let fin = fs::read_to_string("testfiles/infinite_bs.slpn").unwrap();
        let object = fin.parse::<StochasticLabelledPetriNet>().unwrap();

        assert!(
            !object
                .get_livelock_cache()
                .is_state_part_of_livelock(&object.get_initial_state().unwrap())
                .unwrap()
        );
    }

    #[test]
    fn livelock_dfa() {
        let fin = fs::read_to_string("testfiles/a-loop.dfa").unwrap();
        let object = fin.parse::<DeterministicFiniteAutomaton>().unwrap();

        let state = object.get_initial_state().unwrap();
        assert!(!object.is_state_part_of_livelock(&state).unwrap());

        assert!(
            !object
                .get_livelock_cache()
                .is_state_part_of_livelock(&state)
                .unwrap()
        );
    }

    #[test]
    fn livelock_sdfa() {
        let fin = fs::read_to_string("testfiles/a-loop.sdfa").unwrap();
        let object = fin
            .parse::<StochasticDeterministicFiniteAutomaton>()
            .unwrap();

        let state = object.get_initial_state().unwrap();
        assert!(!object.is_state_part_of_livelock(&state).unwrap());

        assert!(
            !object
                .get_livelock_cache()
                .is_state_part_of_livelock(&state)
                .unwrap()
        );
    }

    #[test]
    fn livelock_dfm() {
        let fin = fs::read_to_string("testfiles/a-b_star_empty_trace.dfm").unwrap();
        let object = fin.parse::<DirectlyFollowsModel>().unwrap();

        let state = object.get_initial_state().unwrap();
        assert!(!object.is_state_part_of_livelock(&state).unwrap());

        assert!(
            !object
                .get_livelock_cache()
                .is_state_part_of_livelock(&state)
                .unwrap()
        );
    }
}
