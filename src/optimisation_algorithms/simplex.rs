use ndarray::*;

use crate::math::{
    fraction::Fraction,
    traits::{One, Signed, Zero},
};

/**
 * This is an adaption of the `simplex` crate, version 1.0.0 by Adrián Arroyo Calle.
 * It has been adapted to work with Ebi's fractions.
 */

/// Constraints you can add to your LP problem
///
/// Equal is x + y + z = N
/// LessThan is x + y + z <= N
/// GreaterThan is x + y + z >= N
pub enum SimplexConstraint {
    Equal(Vec<Fraction>, Fraction),
    LessThan(Vec<Fraction>, Fraction),
    GreaterThan(Vec<Fraction>, Fraction),
}

impl SimplexConstraint {
    fn get_vector(&self) -> &Vec<Fraction> {
        match self {
            SimplexConstraint::Equal(a, _b) => a,
            SimplexConstraint::LessThan(a, _b) => a,
            SimplexConstraint::GreaterThan(a, _b) => a,
        }
    }

    fn get_b(&self) -> &Fraction {
        match self {
            SimplexConstraint::Equal(_a, b) => b,
            SimplexConstraint::LessThan(_a, b) => b,
            SimplexConstraint::GreaterThan(_a, b) => b,
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum SimplexVar {
    Real,
    Slack(usize),
    NegativeSlack(usize),
    Artificial(usize),
}

impl SimplexVar {
    fn is_artificial(&self) -> bool {
        match self {
            SimplexVar::Artificial(_) => true,
            _ => false,
        }
    }

    fn is_slack(&self) -> bool {
        match self {
            SimplexVar::Slack(_) => true,
            _ => false,
        }
    }
}
/// The result of a Simplex calculation
///
/// UniqueOptimum means there's only one solution, and is an optimum
/// MultipleOptimum means there's an optimum, but with different solutions. Run solve again to get another solution.
/// InfiniteSolution means the problem is unbound, so the optimum is infinite
/// NoSolution means the problem doesn't seem to be feasible
#[derive(Debug, PartialEq)]
pub enum SimplexOutput {
    UniqueOptimum(Fraction),
    MultipleOptimum(Fraction),
    InfiniteSolution,
    NoSolution,
}

pub struct SimplexTable {
    objective: Vec<Fraction>,
    table: Array2<Fraction>,
    base: Vec<usize>,
    vars: Vec<SimplexVar>,
}

impl SimplexTable {
    fn get_entry_var(&self) -> Option<usize> {
        let mut entry_var = None;
        let mut max_entry = &-Fraction::one();
        for (i, z) in self.table.row(0).iter().enumerate() {
            if i == 0 || i == self.table.ncols() - 1 {
                continue;
            }
            if max_entry < z {
                max_entry = z;
                entry_var = Some(i);
            }
        }
        entry_var
    }

    fn get_exit_var(&self, entry_var: usize) -> Option<usize> {
        let mut exit_var = None;
        let mut min_entry = None;
        let b = self.table.column(self.table.ncols() - 1);
        for (i, z) in self.table.column(entry_var).iter().enumerate() {
            if i == 0 {
                continue;
            }
            if !z.is_positive() {
                continue;
            }
            let bi_div_z = &b[i] / z;
            if let Some(ref entry) = min_entry {
                if entry > &bi_div_z {
                    min_entry = Some(bi_div_z);
                    exit_var = Some(self.base[i - 1]);
                }
            } else {
                min_entry = Some(bi_div_z);
                exit_var = Some(self.base[i - 1]);
            }
        }
        exit_var
    }

    fn step(&mut self, entry_var: usize, exit_var: usize) {
        let exit_row = self.base.iter().position(|x| *x == exit_var).unwrap() + 1;
        let pivot = self.table.row(exit_row)[entry_var].clone();
        {
            let mut row = self.table.row_mut(exit_row);
            row /= pivot;
        }
        for i in 0..self.table.nrows() {
            if i == exit_row {
                continue;
            }
            let mut exit_row = self.table.row(exit_row).to_owned();
            let mut row = self.table.row_mut(i);
            let factor = -&row[entry_var];
            exit_row *= factor;
            row += &exit_row;
        }
        self.base = self
            .base
            .iter_mut()
            .map(|x| if *x == exit_var { entry_var } else { *x })
            .collect();
    }

    /// Solve your LP problem
    ///
    /// Try to solve the LP problem. It uses the "standard" Simplex algorithm, with Big M method
    /// There's no timeout, so it could run for a very long time if you're not careful.
    /// It returns a SimplexOutput, which has a description of the solution and the optimum value (if exists).
    /// ```
    /// # use ebi::math::fraction::Fraction;
    /// # use ebi::optimisation_algorithms::simplex::*;
    ///    let program = Simplex::minimize(vec![-Fraction::from(3), Fraction::from(1), -Fraction::from(2)])
    ///    .with(vec![
    ///        SimplexConstraint::LessThan(vec![Fraction::from(2), -Fraction::from(2), Fraction::from(3)], Fraction::from(5)),
    ///        SimplexConstraint::LessThan(vec![Fraction::from(1), Fraction::from(1), -Fraction::from(1)], Fraction::from(3)),
    ///        SimplexConstraint::LessThan(vec![Fraction::from(1), -Fraction::from(1), Fraction::from(1)], Fraction::from(2)),
    ///    ]);
    ///    let mut simplex = program.unwrap();
    ///    assert_eq!(simplex.solve(), SimplexOutput::MultipleOptimum(-Fraction::from(8)));
    ///    assert_eq!(simplex.get_var(1), Some(Fraction::from((5, 2))));
    ///    assert_eq!(simplex.get_var(2), Some(Fraction::from((3, 2))));
    ///    assert_eq!(simplex.get_var(3), Some(Fraction::from(1)));
    /// ```
    pub fn solve(&mut self) -> SimplexOutput {
        loop {
            if let Some(entry_var) = self.get_entry_var() {
                if let Some(exit_var) = self.get_exit_var(entry_var) {
                    self.step(entry_var, exit_var);
                } else {
                    return SimplexOutput::InfiniteSolution;
                }
            } else {
                panic!("Can't continue");
            }
            let mut optimum = true;
            let mut unique = true;
            for (i, z) in self.table.row(0).iter().skip(1).enumerate() {
                optimum = optimum && !z.is_positive();
                if !self.base.contains(&i) && i <= self.objective.len() {
                    unique = unique && (z < &self.objective[i]);
                }
            }
            if optimum {
                let optimum = &self.table.row(0)[self.table.ncols() - 1];
                for (i, var) in self.base.iter().enumerate() {
                    if self.vars[*var - 1].is_artificial() {
                        if self.table.row(i + 1)[self.table.ncols() - 1].is_positive() {
                            /* Artificial variable might have taken slack var value */
                            if self.vars[*var - 2].is_slack() {
                                if self.table.row(0)[*var - 1].is_zero() {
                                    continue;
                                }
                            }
                            return SimplexOutput::NoSolution;
                        }
                    }
                }
                if unique {
                    return SimplexOutput::UniqueOptimum(optimum.clone());
                } else {
                    return SimplexOutput::MultipleOptimum(optimum.clone());
                }
            }
        }
    }

    /// Gets the value of the N var in a solved problem
    ///
    /// ```
    /// # use ebi::math::fraction::Fraction;
    /// # use ebi::optimisation_algorithms::simplex::*;
    ///     
    ///    let program = Simplex::minimize(vec![-Fraction::from(3), Fraction::from(1), -Fraction::from(2)])
    ///    .with(vec![
    ///        SimplexConstraint::LessThan(vec![Fraction::from(2), -Fraction::from(2), Fraction::from(3)], Fraction::from(5)),
    ///        SimplexConstraint::LessThan(vec![Fraction::from(1), Fraction::from(1), -Fraction::from(1)], Fraction::from(3)),
    ///        SimplexConstraint::LessThan(vec![Fraction::from(1), -Fraction::from(1), Fraction::from(1)], Fraction::from(2)),
    ///    ]);
    ///    let mut simplex = program.unwrap();
    ///    assert_eq!(simplex.solve(), SimplexOutput::MultipleOptimum(-Fraction::from(8)));
    ///    assert_eq!(simplex.get_var(1), Some(Fraction::from((5, 2))));
    ///    assert_eq!(simplex.get_var(2), Some(Fraction::from((3, 2))));
    ///    assert_eq!(simplex.get_var(3), Some(Fraction::from(1)));
    /// ```
    pub fn get_var(&self, var: usize) -> Option<Fraction> {
        if var > self.objective.len() {
            return None;
        }
        for (i, v) in self.base.iter().enumerate() {
            if *v == var {
                return Some(self.table.row(i + 1)[self.table.ncols() - 1].clone());
            }
        }
        return Some(Fraction::zero());
    }
}

pub struct SimplexMinimizerBuilder {
    objective: Vec<Fraction>,
}

impl SimplexMinimizerBuilder {
    /// Add constraints to the problem
    ///
    /// Add constraints to your problem. All variables are already restricted to be equal or more than zero.
    /// Constraints must be of type SimplexConstraint. It will return a Result. If the generated matrix is not valid (wrong dimensions,...), it will return an Err(String).
    ///
    /// ```
    /// # use ebi::math::fraction::Fraction;
    /// # use ebi::optimisation_algorithms::simplex::*;
    /// let mut simplex = Simplex::minimize(vec![Fraction::from(1), -Fraction::from(2)])
    /// .with(vec![
    ///     SimplexConstraint::GreaterThan(vec![Fraction::from(1), Fraction::from(1)], Fraction::from(2)),
    ///     SimplexConstraint::GreaterThan(vec![-Fraction::from(1), Fraction::from(1)], Fraction::from(1)),
    ///     SimplexConstraint::LessThan(vec![Fraction::from(0), Fraction::from(1)], Fraction::from(3)),
    /// ]);
    /// ```
    /// would be like:
    /// ```ignore
    /// minimize z = x - 2y
    /// with
    ///      x + y >= 2
    ///      -x +y >= 1
    ///      y <= 3
    /// ```
    pub fn with(self, constraints: Vec<SimplexConstraint>) -> Result<SimplexTable, String> {
        let mut table = Vec::new();
        let mut vars = Vec::new();
        table.push(Fraction::one());
        for var in self.objective.iter() {
            table.push(-var);
            vars.push(SimplexVar::Real);
        }
        for (i, constraint) in constraints.iter().enumerate() {
            match constraint {
                SimplexConstraint::LessThan(_, _) => {
                    table.push(Fraction::zero());
                    vars.push(SimplexVar::Slack(i));
                }
                SimplexConstraint::GreaterThan(_, _) => {
                    table.push(Fraction::zero());
                    vars.push(SimplexVar::NegativeSlack(i));
                }
                _ => {}
            }
            table.push(Fraction::neg_infinity());
            vars.push(SimplexVar::Artificial(i));
        }
        table.push(Fraction::zero());

        for (i, constraint) in constraints.into_iter().enumerate() {
            table.push(Fraction::zero());
            for a in constraint.get_vector() {
                table.push(a.clone());
            }
            for var in vars.iter() {
                match var {
                    SimplexVar::Slack(j) => {
                        if *j == i {
                            table.push(Fraction::one());
                        } else {
                            table.push(Fraction::zero());
                        }
                    }
                    SimplexVar::NegativeSlack(j) => {
                        if *j == i {
                            table.push(-Fraction::one());
                        } else {
                            table.push(Fraction::zero());
                        }
                    }
                    SimplexVar::Artificial(j) => {
                        if *j == i {
                            table.push(Fraction::one());
                        } else {
                            table.push(Fraction::zero());
                        }
                    }
                    _ => {}
                }
            }
            table.push(constraint.get_b().clone());
        }
        let base: Vec<usize> = vars
            .iter()
            .enumerate()
            .filter_map(|(i, x)| if x.is_artificial() { Some(i + 1) } else { None })
            .collect();
        let table = Array2::from_shape_vec((base.len() + 1, vars.len() + 2), table);
        match table {
            Ok(table) => Ok(SimplexTable {
                objective: self.objective,
                table: table,
                base: base,
                vars: vars,
            }),
            Err(_) => Err(String::from("Invalid matrix")),
        }
    }
}

/// Initialize your Linnear Programming problem
///
/// Use it at the beginning, to define your problem.
///
/// ```
/// # use ebi::math::fraction::Fraction;
/// # use ebi::optimisation_algorithms::simplex::*;
/// let mut problem = Simplex::minimize(vec![Fraction::from(5), -Fraction::from(6)]);
/// ```
///
pub struct Simplex;

impl Simplex {
    /// Initialize a LP minimization problem
    ///
    /// Currently, only minimization is provided. Maximization can be achieved by changing the signs
    ///
    /// ```
    /// # use ebi::math::fraction::Fraction;
    /// # use ebi::optimisation_algorithms::simplex::*;
    /// let mut problem = Simplex::minimize(vec![Fraction::from(5), -Fraction::from(6)]);
    /// ```
    /// It initializes Simplex with a minimization objective function z = 5x - 6y
    pub fn minimize(objective: Vec<Fraction>) -> SimplexMinimizerBuilder {
        SimplexMinimizerBuilder {
            objective: objective,
        }
    }
}
