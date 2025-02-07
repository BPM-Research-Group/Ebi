use anyhow::{anyhow, Result};
use std::fmt::{Debug, Display};

use super::{fraction::Fraction, matrix::Matrix, traits::{One, Signed, Zero}};

pub struct MarkovModel<S> {
    edges: Matrix,
    states: Vec<S>,
    initial_vector: Vec<Fraction>,
}

impl<S: PartialEq + Clone> MarkovModel<S> {
    pub fn new() -> Self {
        Self {
            edges: Matrix::new(),
            states: vec![],
            initial_vector: vec![],
        }
    }

    pub fn add_or_find_state(&mut self, state: S, initial_value: Fraction) -> (usize, bool) {
        if let Some(state_index) = self.states.iter().position(|s| s == &state) {
            //already present
            (state_index, false)
        } else {
            //not yet present
            let state_index = self.states.len();
            self.edges.ensure_capacity(
                self.states.len() + 1,
                self.states.len() + 1,
                &Fraction::zero(),
            );
            self.edges[state_index][state_index] = Fraction::one();
            self.states.push(state);
            self.initial_vector.push(initial_value);

            (state_index, true)
        }
    }

    pub fn contains_state(&self, state: &S) -> Option<usize> {
        self.states.iter().position(|s| s == state)
    }

    pub fn get_states_owned(self) -> Vec<S> {
        self.states
    }

    pub fn get_states(&self) -> &Vec<S> {
        &self.states
    }

    /**
     * Redirects flow from source -> source to source -> target
     */
    pub fn set_flow(&mut self, source: usize, target: usize, flow: &Fraction) {
        self.edges[source][source] -= flow;
        self.edges[source][target] += flow;
    }

    pub fn normalise_initial_vector(&mut self) -> Result<()> {
        let sum = self
            .initial_vector
            .iter()
            .fold(Fraction::zero(), |mut a, b| {
                a += b;
                a
            });
        if sum.is_positive() {
            self.initial_vector.retain_mut(|x| {
                *x /= &sum;
                true
            });
            Ok(())
        } else {
            Err(anyhow!("Initial vector has no probability mass."))
        }
    }

    pub fn set_states(&mut self, states: &Vec<bool>, state: S) {
        for (index, maybe) in states.iter().enumerate() {
            if *maybe {
                self.states[index] = state.clone();
            }
        }
    }

    /**
     * Remove all outgoing transitions of this state, and make the state absorbing.
     */
    pub fn make_states_absorbing(&mut self, states: &Vec<bool>) {
        for (state, maybe) in states.iter().enumerate() {
            if *maybe {
                self.edges[state] = vec![Fraction::zero(); self.states.len()];
                self.edges[state][state] = Fraction::one();
            }
        }
    }

    pub fn get_states_that_cannot_reach(&self, mut states_to_reach: Vec<usize>) -> Vec<bool> {
        let mut notseen = vec![true; self.states.len()];
        states_to_reach.iter().for_each(|x| notseen[*x] = false);

        while let Some(state) = states_to_reach.pop() {
            for state2 in 0..self.states.len() {
                if notseen[state2] && self.edges[state2][state].is_positive() {
                    notseen[state2] = false;
                    states_to_reach.push(state2);
                }
            }
        }
        notseen
    }

    /**
     * Raise the edge matrix to infinity / solve the Markov chain.
     */
    pub fn pow_infty(&mut self) -> Result<Vec<Fraction>> {
        // log::debug!("solve Markov model {}", self.edges);

        //create matrices A and B
        let mut a = Matrix::new(); //transient -> absorbing
        let mut b = Matrix::new(); //transient -> transient
        let mut absorbing_states = vec![]; //absorbing
        let mut transient_states = vec![]; //transient
        for state in 0..self.states.len() {
            if self.edges[state][state] == Fraction::one() {
                //absorbing state
                // log::debug!("\tabsorbing state {}", state);

                absorbing_states.push(state);

                // log::debug!("\t\tabsorbing states: {:?}", absorbing_states);
                // log::debug!("\t\ttransient states: {:?}", transient_states);

                //add column to A
                let column = a.get_number_of_columns();
                a.ensure_capacity(
                    a.get_number_of_rows(),
                    a.get_number_of_columns() + 1,
                    &Fraction::zero(),
                );
                for row_a in 0..transient_states.len() {
                    let transient_state = transient_states[row_a];
                    a.element_add(&row_a, &column, &self.edges[transient_state][state]);
                }
            } else {
                //transient state
                // log::debug!("\ttransient state {}", state);

                transient_states.push(state);

                // log::debug!("\t\tabsorbing states: {:?}", absorbing_states);
                // log::debug!("\t\ttransient states: {:?}", transient_states);

                //add row to A
                let row = a.get_number_of_rows();
                a.ensure_capacity(
                    a.get_number_of_rows() + 1,
                    a.get_number_of_columns(),
                    &Fraction::zero(),
                );
                for column_a in 0..absorbing_states.len() {
                    let absorbing_state = absorbing_states[column_a];
                    a.element_add(&row, &column_a, &self.edges[state][absorbing_state]);
                }

                //add column to B
                let row = b.get_number_of_rows();
                let column = b.get_number_of_columns();
                b.ensure_capacity(
                    transient_states.len(),
                    transient_states.len(),
                    &Fraction::zero(),
                );
                for i_b in 0..transient_states.len() {
                    let transient_state = transient_states[i_b];

                    // log::debug!("\t\tset matrix B for {},{}", transient_state, state);
                    b.element_add(&row, &i_b, &self.edges[state][transient_state]);

                    if i_b != transient_states.len() - 1 {
                        //avoid doubly adding to the corner cell
                        b.element_add(&i_b, &column, &self.edges[transient_state][state]);
                    }
                }
            }

            // log::debug!("\t\tmatrix A = {}", a);
            // log::debug!("\t\tlen A = {}x{}", a.get_number_of_rows(), a.get_number_of_columns());
            // log::debug!("\t\tmatrix B = {}", b);
            // log::debug!("\t\tlen B = {}x{}", b.get_number_of_rows(), b.get_number_of_columns());
        }

        // log::debug!("matrix A = {}", a);
        // log::debug!("matrix B = {}", b);

        b.identity_minus();

        // log::debug!("matrix I - B = {}", b);

        b.inverse()?;

        // log::debug!("matrix F = inv(I-B) = {}", b);

        let p = b * a;

        // log::debug!("matrix P = FA = {}", p);

        //construct the full matrix ((0, 0), (A, B))
        {
            //set the transient -> transient fields to 0
            for (x, transient_state1) in transient_states.iter().enumerate() {
                for transient_state2 in transient_states.iter().skip(x) {
                    self.edges[*transient_state1][*transient_state2] = Fraction::zero();
                }
            }

            //substitute the fundamental matrix
            for (i_tra, transient_state) in transient_states.iter().enumerate() {
                for (i_abs, absorbing_state) in absorbing_states.iter().enumerate() {
                    self.edges[*transient_state][*absorbing_state] = p[i_tra][i_abs].clone();
                }
            }
        }

        // log::debug!("solved matrix {}", self.edges);

        let x = self.edges.multiply_vector_matrix(&self.initial_vector);

        // log::debug!("result {}", Matrix::into(x.clone()));

        Ok(x)
    }
}

impl<S: Debug> Debug for MarkovModel<S> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("MarkovModel")
            .field("matrix", &self.edges)
            .field("states", &self.states)
            .field("initial_vector", &self.initial_vector)
            .finish()
    }
}

impl<S: Display> Display for MarkovModel<S> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "matrix {}, initial vector ", self.edges)?;
        Matrix::display_vector(f, &self.initial_vector)
    }
}
