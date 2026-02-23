use ebi_objects::Activity;

#[derive(Clone, Debug)]
pub enum FollowerSemantics<'a> {
    Trace(&'a Vec<Activity>),
}

impl FollowerSemantics<'_> {
    /**
     *
     * @return The initial state.
     */
    pub fn get_initial_state(&self) -> usize {
        0
    }

    /**
     *
     * @param label
     * @return The new state, or null if the step cannot be taken.
     */
    pub fn take_step(&self, state: &usize, label: &Activity) -> Option<usize> {
        match self {
            FollowerSemantics::Trace(trace) => {
                if &trace[*state] == label {
                    return Some(state + 1);
                }
                return None;
            }
        }
    }

    pub fn is_final_state(&self, state: &usize) -> bool {
        match self {
            FollowerSemantics::Trace(trace) => {
                return *state == trace.len();
            }
        }
    }
}
