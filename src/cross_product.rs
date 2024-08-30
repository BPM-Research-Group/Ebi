use core::fmt;
use std::{ops::Add, collections::HashSet};
use anyhow::Result;
use fraction::{One, Zero};

use crate::math::{fraction::Fraction, matrix::Matrix};

pub trait CrossProductResult {
    /**
	 * The initial state will be reported twice: once as initial state, and
	 * again as a final or non-final state.
	 * 
	 * @param stateIndex
	 */
	fn report_initial_state(&mut self, state_index: usize);

	/**
	 * A state will be reported as either final, non-final, or dead.
	 * 
	 * @param stateIndex
	 * @param nextStateIndices
	 *            may contain duplicated values. List might be reused and
	 *            changed after this call returns, and changes by the
	 *            implementer will be overwritten.
	 * @param nextStateProbabilities
	 *            list might be reused and changed after this call returns, and
	 *            changes by the implementer will be overwritten.
	 */
	fn report_non_final_state(&mut self, state_index: usize, next_state_indices: Vec<usize>, next_state_probabilities: Vec<Fraction>);

	/**
	 * A state will be reported as either final, non-final, or dead. Multiple
	 * states might be reported as final.
	 * 
	 * @param stateIndex
	 */
	fn report_final_state(&mut self, state_index: usize);

	/**
	 * A state will be reported as either final, non-final, or dead.
	 * 
	 * A dead state is a state in the cross product that indicates that A made a
	 * move that was not supported by B. At most one state will be reported as
	 * dead.
	 * 
	 * 
	 * @param stateIndex
	 */
	fn report_dead_state(&mut self, state_index: usize);
}

pub struct CrossProductResultImpl {
    initial_state: usize,
    dead_state: usize,
	final_states: HashSet<usize>,
	next_states:  Vec<Vec<usize>>,
    next_state_probabilities: Vec<Vec<Fraction>>,
    de_duplication_cache: HashSet<usize>
}

impl CrossProductResultImpl{
    pub fn new() -> Self {
        Self { 
            initial_state: usize::MAX,
            dead_state: usize::MAX,
            final_states: HashSet::new(),
            next_states: vec![],
            next_state_probabilities: vec![],
            de_duplication_cache: HashSet::new()
         }
    }
}

impl CrossProductResult for CrossProductResultImpl {
    fn report_initial_state(&mut self, state_index: usize) {
        self.initial_state = state_index;
		self.ensure_capacity(state_index);

		// debug!("report initial state {}", state_index);
    }

    fn report_non_final_state(&mut self, state_index: usize, mut next_state_indices: Vec<usize>, mut next_state_probabilities: Vec<Fraction>) {
		self.de_duplicate(&mut next_state_indices, &mut next_state_probabilities);
		self.ensure_capacity(state_index);

		self.next_states[state_index] = next_state_indices;
		self.next_state_probabilities[state_index] = next_state_probabilities;
    }

    fn report_final_state(&mut self, state_index: usize) {
		self.ensure_capacity(state_index);
        self.final_states.insert(state_index);

		// debug!("report final state {}", state_index);
    }

    fn report_dead_state(&mut self, state_index: usize) {
        self.dead_state = state_index;

		// debug!("report dead state {}", state_index);
    }
}

impl CrossProductResultImpl {
    fn de_duplicate(&mut self, next_state_indexes: &mut Vec<usize>, next_state_probabilities: &mut Vec<Fraction>) {
		self.de_duplication_cache.clear();
        let mut index_a: usize = 0;
		while index_a < next_state_indexes.len() {
			let node_a = next_state_indexes[index_a];
			if self.de_duplication_cache.contains(&node_a) {
				//look for the duplicate
				for index_b in 0 .. index_a {
					let node_b = next_state_indexes[index_b];
					if node_a == node_b {
						next_state_probabilities[index_b] = next_state_probabilities.get(index_b).unwrap().add(&next_state_probabilities[index_a]);
						next_state_indexes.remove(index_a);
						next_state_probabilities.remove(index_a);
						index_a -= 1;
						break;
					}
				}
			}

			self.de_duplication_cache.insert(node_a);

            index_a += 1;
		}
	}
}

impl CrossProductResultImpl {
	
	fn number_of_states(&self) -> usize {
		self.next_states.len()
	}

	fn ensure_capacity(&mut self, state_index: usize) {
		while self.next_states.len() < state_index + 1 {
			self.next_states.push(Vec::new());
			self.next_state_probabilities.push(Vec::new());
		}
	}

	/**
	 * Structure of the LP model:
	 * 
	 * One row per state; one column per state.
	 * 
	 * @return
	 * @throws LpSolveException
	 */
	pub fn solve(&self) -> Result<Fraction> {
		if self.final_states.is_empty() {
			return Ok(Fraction::zero());
		}

		let mut matrix = Matrix::new_sized(self.number_of_states(), self.number_of_states() + 1, Fraction::zero());

		for state_index in 0 .. self.number_of_states() {
			matrix[state_index] = vec![Fraction::zero(); self.number_of_states() + 1];
			matrix[state_index][state_index] = Fraction::one();

			if state_index == self.dead_state {
				//a dead state has a 0 probability to end up in a final state
			} else if self.final_states.contains(&state_index) {
				//a final state has a 1 probability to end up in a final state
				matrix[state_index][self.number_of_states()] = Fraction::one();
			} else {
				//any other state has a probability equal to the weighted sum of its next states, to end up in a final state
				for i in 0 .. self.next_states[state_index].len() {
					let state_index_2 = self.next_states[state_index][i];
					let coeff = &self.next_state_probabilities[state_index][i];
					matrix[state_index][state_index_2] = -coeff;
				}
			}
		}

		matrix.solve()?;

		Ok(matrix[self.initial_state][self.number_of_states()].to_owned())
	}
}

impl fmt::Display for CrossProductResultImpl {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "initial state {}", self.initial_state)?;
		writeln!(f, "final states  {:?}", self.final_states)?;
		for state in 0 .. self.next_states.len() {
			writeln!(f, "state {}", state)?;
			if self.initial_state == state {
				writeln!(f, "\tinitial state")?;
			}
			if self.final_states.contains(&state) {
				writeln!(f, "\tfinal state")?;
			}
			for i in 0 .. self.next_states[state].len() {
				writeln!(f, "\tnext state {} with probability {}", self.next_states[state][i], self.next_state_probabilities[state][i])?;
			}
		}

		write!(f, "")
    }
}