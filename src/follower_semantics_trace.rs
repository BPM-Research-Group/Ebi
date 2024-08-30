use crate::follower_semantics::FollowerSemantics;

pub struct FollowerSemanticsTrace<'a> {
    pub trace: &'a Vec<String>
}

impl FollowerSemantics<usize> for FollowerSemanticsTrace<'_> {
    fn get_initial_state(&self) -> usize {
        0
    }

    fn take_step(&self, state: &usize, label: &str) -> Option<usize> {
        if self.trace[*state] == label {
			return Some(state + 1);
		}
		return None;
    }

    fn is_final_state(&self, state: &usize) -> bool {
        *state == self.trace.len()
    }
}
