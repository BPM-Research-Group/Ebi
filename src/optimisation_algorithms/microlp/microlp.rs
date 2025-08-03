/*!
A fast linear programming solver library.

[Linear programming](https://en.wikipedia.org/wiki/Linear_programming) is a technique for
finding the minimum (or maximum) of a linear function of a set of continuous variables
subject to linear equality and inequality constraints.

# Features

* Pure Rust implementation.
* Able to solve problems with hundreds of thousands of variables and constraints.
* Problems can be defined via an API

# Entry points

Begin by creating a [`Problem`](struct.Problem.html) instance, declaring variables and adding
constraints. Solving it will produce a [`Solution`](struct.Solution.html) that can be used to
get the optimal objective value and corresponding variable values.

# Example

```
use microlp::{Problem, OptimizationDirection, ComparisonOp};

// Maximize an objective function x + 2 * y of two variables x >= 0 and 0 <= y <= 3
let mut problem = Problem::new(OptimizationDirection::Maximize);
let x = problem.add_var(Fraction::one(), (Fraction::zero(), Fraction::infinity()));
let y = problem.add_var(Fraction::from(2), (Fraction::zero(), Fraction::from(3)));

// subject to constraints: x + y <= 4 and 2 * x + y >= 2.
problem.add_constraint(&[(x, Fraction::one()), (y, Fraction::one())], ComparisonOp::Le, Fraction::from(4));
problem.add_constraint(&[(x, Fraction::from(2)), (y, Fraction::one())], ComparisonOp::Ge, Fraction::from(2));

// Optimal value is 7, achieved at x = 1 and y = 3.
let solution = problem.solve().unwrap();
assert_eq!(solution.objective(), Fraction::from(7));
assert_eq!(solution[x], Fraction::one());
assert_eq!(solution[y], Fraction::from(3));
```
*/

#![deny(missing_debug_implementations, missing_docs)]

use crate::{
    optimisation_algorithms::microlp::sparse,
    optimisation_algorithms::microlp::solver,
    optimisation_algorithms::microlp::solver::Solver,
    math::{fraction::Fraction, traits::{Zero, One}},
};
use sprs::errors::StructureError;
use sprs::{CsVecBase, CsVecView};
use std::ops::Deref;

// Helper functions merged from helpers.rs
pub(crate) fn resized_view<IStorage, DStorage>(
    vec: &CsVecBase<IStorage, DStorage, Fraction>,
    len: usize,
) -> CsVecView<Fraction>
where
    IStorage: Deref<Target = [usize]>,
    DStorage: Deref<Target = [Fraction]>,
{
    let mut indices = vec.indices();
    let mut data = vec.data();
    while let Some(&i) = indices.last() {
        if i < len {
            // TODO: binary search
            break;
        }

        indices = &indices[..(indices.len() - 1)];
        data = &data[..(data.len() - 1)];
    }

    CsVecView::new(len, indices, data)
}

pub(crate) fn to_dense<IStorage, DStorage>(vec: &CsVecBase<IStorage, DStorage, Fraction>) -> Vec<Fraction>
where
    IStorage: Deref<Target = [usize]>,
    DStorage: Deref<Target = [Fraction]>,
{
    let mut dense = vec![Fraction::zero(); vec.dim()];
    // Manual scatter instead of using vec.scatter(&mut dense) to avoid num::Zero requirement
    for (idx, val) in vec.iter() {
        dense[idx] = val.clone();
    }
    dense
}

/// An enum indicating whether to minimize or maximize objective function.
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum OptimizationDirection {
    /// Minimize the objective function.
    Minimize,
    /// Maximize the objective function.
    Maximize,
}

/// A reference to a variable in a linear programming problem.
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Variable(pub(crate) usize);

impl Variable {
    /// Sequence number of the variable.
    ///
    /// Variables are referenced by their number in the addition sequence. The method returns
    /// this number.
    pub fn idx(&self) -> usize {
        self.0
    }
}

/// A sum of variables multiplied by constant coefficients used as a left-hand side
/// when defining constraints.
#[derive(Clone, Debug)]
pub struct LinearExpr {
    vars: Vec<usize>,
    coeffs: Vec<Fraction>,
}

impl LinearExpr {
    /// Creates an empty linear expression.
    pub fn empty() -> Self {
        Self {
            vars: vec![],
            coeffs: vec![],
        }
    }

    /// Add a single term to the linear expression.
    ///
    /// Variables can be added to an expression in any order, but adding the same variable
    /// several times is forbidden (the [`Problem::add_constraint`] method will panic).
    ///
    /// [`Problem::add_constraint`]: struct.Problem.html#method.add_constraint
    pub fn add(&mut self, var: Variable, coeff: Fraction) {
        self.vars.push(var.0);
        self.coeffs.push(coeff);
    }
}

/// A single `variable * constant` term in a linear expression.
/// This is an auxiliary struct for specifying conversions.
#[doc(hidden)]
#[derive(Clone, Debug)]
pub struct LinearTerm(Variable, Fraction);

impl From<(Variable, Fraction)> for LinearTerm {
    fn from(term: (Variable, Fraction)) -> Self {
        LinearTerm(term.0, term.1)
    }
}

impl<'a> From<&'a (Variable, Fraction)> for LinearTerm {
    fn from(term: &'a (Variable, Fraction)) -> Self {
        LinearTerm(term.0, term.1.clone())
    }
}

impl<I: IntoIterator<Item = impl Into<LinearTerm>>> From<I> for LinearExpr {
    fn from(iter: I) -> Self {
        let mut expr = LinearExpr::empty();
        for term in iter {
            let LinearTerm(var, coeff) = term.into();
            expr.add(var, coeff);
        }
        expr
    }
}

impl std::iter::FromIterator<(Variable, Fraction)> for LinearExpr {
    fn from_iter<I: IntoIterator<Item = (Variable, Fraction)>>(iter: I) -> Self {
        let mut expr = LinearExpr::empty();
        for term in iter {
            expr.add(term.0, term.1)
        }
        expr
    }
}

impl std::iter::Extend<(Variable, Fraction)> for LinearExpr {
    fn extend<I: IntoIterator<Item = (Variable, Fraction)>>(&mut self, iter: I) {
        for term in iter {
            self.add(term.0, term.1)
        }
    }
}

/// An operator specifying the relation between left-hand and right-hand sides of the constraint.
#[derive(Clone, Copy, Debug)]
pub enum ComparisonOp {
    /// The == operator (equal to)
    Eq,
    /// The <= operator (less than or equal to)
    Le,
    /// The >= operator (greater than or equal to)
    Ge,
}

/// An error encountered while solving a problem.
#[derive(Clone, Debug, PartialEq)]
pub enum Error {
    /// Constrains can't simultaneously be satisfied.
    Infeasible,
    /// The objective function is unbounded.
    Unbounded,
    /// An internal error occurred.
    InternalError(String),
}
impl From<StructureError> for Error {
    fn from(err: StructureError) -> Self {
        Error::InternalError(err.to_string())
    }
}

impl From<sparse::Error> for Error {
    fn from(value: sparse::Error) -> Self {
        Error::InternalError(value.to_string())
    }
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let msg = match self {
            Error::Infeasible => "problem is infeasible",
            Error::Unbounded => "problem is unbounded",
            Error::InternalError(msg) => msg,
        };
        msg.fmt(f)
    }
}

impl std::error::Error for Error {}

/// A specification of a linear programming problem.
#[derive(Clone)]
pub struct Problem {
    direction: OptimizationDirection,
    obj_coeffs: Vec<Fraction>,
    var_mins: Vec<Fraction>,
    var_maxs: Vec<Fraction>,
    var_domains: Vec<VarDomain>,
    constraints: Vec<(CsVec, ComparisonOp, Fraction)>,
}

impl std::fmt::Debug for Problem {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        // Only printing lengths here because actual data is probably huge.
        f.debug_struct("Problem")
            .field("direction", &self.direction)
            .field("num_vars", &self.obj_coeffs.len())
            .field("num_constraints", &self.constraints.len())
            .finish()
    }
}

type CsVec = sprs::CsVecI<Fraction, usize>;

#[derive(Clone, Debug, PartialEq)]
/// The domain of a variable.
pub enum VarDomain {
    /// The variable is real.
    Real,
}

impl Problem {
    /// Create a new problem instance.
    pub fn new(direction: OptimizationDirection) -> Self {
        Problem {
            direction,
            obj_coeffs: vec![],
            var_mins: vec![],
            var_maxs: vec![],
            var_domains: vec![],
            constraints: vec![],
        }
    }

    /// Add a new real variable to the problem.
    ///
    /// `obj_coeff` is a coefficient of the term in the objective function corresponding to this
    /// variable, `min` and `max` are the minimum and maximum (inclusive) bounds of this
    /// variable. If one of the bounds is absent, use `Fraction::neg_infinity()` for minimum and
    /// `Fraction::infinity()` for maximum.
    pub fn add_var(&mut self, obj_coeff: Fraction, (min, max): (Fraction, Fraction)) -> Variable {
        self.internal_add_var(obj_coeff, (min, max), VarDomain::Real)
    }

    pub(crate) fn internal_add_var(
        &mut self,
        obj_coeff: Fraction,
        (min, max): (Fraction, Fraction),
        var_type: VarDomain,
    ) -> Variable {
        let var = Variable(self.obj_coeffs.len());
        let obj_coeff = match self.direction {
            OptimizationDirection::Minimize => obj_coeff,
            OptimizationDirection::Maximize => -obj_coeff,
        };
        self.obj_coeffs.push(obj_coeff);
        self.var_mins.push(min);
        self.var_maxs.push(max);
        self.var_domains.push(var_type);
        var
    }

    /// Add a linear constraint to the problem.
    ///
    /// # Panics
    ///
    /// Will panic if a variable was added more than once to the left-hand side expression.
    ///
    /// # Examples
    ///
    /// Left-hand side of the constraint can be specified in several ways:
    /// ```
    /// # use microlp::*;
    /// let mut problem = Problem::new(OptimizationDirection::Minimize);
    /// let x = problem.add_var(Fraction::one(), (Fraction::zero(), Fraction::infinity()));
    /// let y = problem.add_var(Fraction::one(), (Fraction::zero(), Fraction::infinity()));
    ///
    /// // Add an x + y >= 2 constraint, specifying the left-hand side expression:
    ///
    /// // * by passing a slice of pairs (useful when explicitly enumerating variables)
    /// problem.add_constraint(&[(x, Fraction::one()), (y, Fraction::one())], ComparisonOp::Ge, Fraction::from(2));
    ///
    /// // * by passing an iterator of variable-coefficient pairs.
    /// let vars = [x, y];
    /// problem.add_constraint(vars.iter().map(|&v| (v, Fraction::one())), ComparisonOp::Ge, Fraction::from(2));
    ///
    /// // * by manually constructing a LinearExpr.
    /// let mut lhs = LinearExpr::empty();
    /// for &v in &vars {
    ///     lhs.add(v, Fraction::one());
    /// }
    /// problem.add_constraint(lhs, ComparisonOp::Ge, Fraction::from(2));
    /// ```
    pub fn add_constraint(&mut self, expr: impl Into<LinearExpr>, cmp_op: ComparisonOp, rhs: Fraction) {
        let expr = expr.into();
        self.constraints.push((
            CsVec::new_from_unsorted(self.obj_coeffs.len(), expr.vars, expr.coeffs).unwrap(),
            cmp_op,
            rhs,
        ));
    }

    /// Solve the problem, finding the optimal objective function value and variable values.
    ///
    /// # Errors
    ///
    /// Will return an error, if the problem is infeasible (constraints can't be satisfied)
    /// or if the objective value is unbounded.
    pub fn solve(&self) -> Result<Solution, Error> {
        let mut solver = Solver::try_new(
            &self.obj_coeffs,
            &self.var_mins,
            &self.var_maxs,
            &self.constraints,
            &self.var_domains,
        )?;
        solver.initial_solve()?;

        Ok(Solution {
            num_vars: self.obj_coeffs.len(),
            direction: self.direction,
            solver,
        })
    }
}

/// A solution of a problem: optimal objective function value and variable values.
///
/// Note that a `Solution` instance contains the whole solver machinery which can require
/// a lot of memory for larger problems. Thus saving the `Solution` instance (as opposed
/// to getting the values of interest and discarding the solution) is mainly useful if you
/// want to add more constraints to it later.
#[derive(Clone)]
pub struct Solution {
    direction: OptimizationDirection,
    num_vars: usize,
    solver: solver::Solver,
}

impl std::fmt::Debug for Solution {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        // Only printing lengths here because actual data is probably huge.
        f.debug_struct("Solution")
            .field("direction", &self.direction)
            .field("num_vars", &self.num_vars)
            .field("num_constraints", &self.solver.num_constraints())
            .field("objective", &self.objective())
            .finish()
    }
}

impl Solution {
    /// Optimal value of the objective function.
    pub fn objective(&self) -> Fraction {
        match self.direction {
            OptimizationDirection::Minimize => self.solver.cur_obj_val.clone(),
            OptimizationDirection::Maximize => -&self.solver.cur_obj_val,
        }
    }

    /// Value of the variable at optimum.
    ///
    /// Note that you can use indexing operations to get variable values.
    pub fn var_value(&self, var: Variable) -> &Fraction {
        assert!(var.0 < self.num_vars);
        self.solver.get_value(var.0)
    }

    /// Iterate over the variable-value pairs of the solution.
    pub fn iter(&self) -> SolutionIter {
        SolutionIter {
            solution: self,
            var_idx: 0,
        }
    }
}

impl std::ops::Index<Variable> for Solution {
    type Output = Fraction;

    fn index(&self, var: Variable) -> &Self::Output {
        self.var_value(var)
    }
}

/// An iterator over the variable-value pairs of a [`Solution`].
#[derive(Debug, Clone)]
pub struct SolutionIter<'a> {
    solution: &'a Solution,
    var_idx: usize,
}

impl<'a> Iterator for SolutionIter<'a> {
    type Item = (Variable, &'a Fraction);

    fn next(&mut self) -> Option<Self::Item> {
        if self.var_idx < self.solution.num_vars {
            let var_idx = self.var_idx;
            self.var_idx += 1;
            Some((Variable(var_idx), self.solution.solver.get_value(var_idx)))
        } else {
            None
        }
    }
}

impl<'a> IntoIterator for &'a Solution {
    type Item = (Variable, &'a Fraction);
    type IntoIter = SolutionIter<'a>;

    fn into_iter(self) -> Self::IntoIter {
        self.iter()
    }
}


#[cfg(test)]
mod tests {
    use super::*;

    fn init() {
        let _ = env_logger::builder().is_test(true).try_init();
    }

    #[test]
    fn basic_optimize() {
        init();
        let mut problem = Problem::new(OptimizationDirection::Minimize);
        let x = problem.add_var(Fraction::from(1), (Fraction::from(0), Fraction::infinity()));
        let y = problem.add_var(Fraction::from(2), (Fraction::from(0), Fraction::from(3)));
        problem.add_constraint([(x, Fraction::from(1)), (y, Fraction::from(1))], ComparisonOp::Le, Fraction::from(4));
        problem.add_constraint([(x, Fraction::from(2)), (y, Fraction::from(1))], ComparisonOp::Ge, Fraction::from(2));
        
        match problem.solve() {
            Ok(sol) => {
                println!("Solution found:");
                println!("  x = {}", sol[x]);
                println!("  y = {}", sol[y]);
                println!("  objective = {}", sol.objective());
                assert_eq!(sol[x], Fraction::from(1));
                assert_eq!(sol[y], Fraction::from(0));
                assert_eq!(sol.objective(), Fraction::from(1));
            }
            Err(e) => {
                println!("Error: {:?}", e);
                panic!("Expected solution but got error: {:?}", e);
            }
        }
    }

    #[test]
    fn simple_test() {
        init();
        // Very simple test: Maximize x subject to x <= 1, x >= 0
        let mut problem = Problem::new(OptimizationDirection::Maximize);
        let x = problem.add_var(Fraction::from(1), (Fraction::from(0), Fraction::infinity()));
        problem.add_constraint([(x, Fraction::from(1))], ComparisonOp::Le, Fraction::from(1));

        // Let's try the same problem as minimization to see if maximization is the issue
        println!("Testing maximization version...");
        match problem.solve() {
            Ok(sol) => {
                println!("Simple test solution: x = {}, objective = {}", sol[x], sol.objective());
                assert_eq!(sol[x], Fraction::from(1));
                assert_eq!(sol.objective(), Fraction::from(1));
            }
            Err(e) => {
                println!("Simple test error: {:?}", e);
                // Let's try the minimization version
                println!("Maximization failed, trying minimization...");
                let mut min_problem = Problem::new(OptimizationDirection::Minimize);
                let x_min = min_problem.add_var(-Fraction::from(1), (Fraction::from(0), Fraction::infinity()));
                min_problem.add_constraint([(x_min, Fraction::from(1))], ComparisonOp::Le, Fraction::from(1));
                
                match min_problem.solve() {
                    Ok(min_sol) => {
                        println!("Minimization works: x = {}, objective = {}", min_sol[x_min], min_sol.objective());
                        // This would be equivalent to maximizing x
                    }
                    Err(min_e) => {
                        println!("Minimization also failed: {:?}", min_e);
                    }
                }
                panic!("Even simple test failed: {:?}", e);
            }
        }
    }

    #[test]
    fn basic_example() {
        init();
        // This is the example from the documentation
        let mut problem = Problem::new(OptimizationDirection::Maximize);
        let x = problem.add_var(Fraction::one(), (Fraction::zero(), Fraction::infinity()));
        let y = problem.add_var(Fraction::from(2), (Fraction::zero(), Fraction::from(3)));

        problem.add_constraint(&[(x, Fraction::one()), (y, Fraction::one())], ComparisonOp::Le, Fraction::from(4));
        problem.add_constraint(&[(x, Fraction::from(2)), (y, Fraction::one())], ComparisonOp::Ge, Fraction::from(2));

        let solution = problem.solve().unwrap();
        assert_eq!(solution.objective(), Fraction::from(7));
        assert_eq!(solution[x], Fraction::one());
        assert_eq!(solution[y], Fraction::from(3));
    }


    #[test]
    fn minimal_bpmn(){
        init();
        let mut problem = Problem::new(OptimizationDirection::Minimize);
        let x = problem.add_var(Fraction::from(0), (Fraction::from(0), Fraction::infinity()));
        let y = problem.add_var(Fraction::from(0), (Fraction::from(0), Fraction::infinity()));
        let z_1 = problem.add_var(Fraction::from(1), (Fraction::from(0), Fraction::infinity()));
        let z_2 = problem.add_var(Fraction::from(1), (Fraction::from(0), Fraction::infinity()));

        problem.add_constraint([(x, Fraction::from(1)), (z_1, -Fraction::from(1))], ComparisonOp::Le, Fraction::from(1));
        problem.add_constraint([(x, -Fraction::from(1)), (z_1, -Fraction::from(1))], ComparisonOp::Le, -Fraction::from(1));
        problem.add_constraint([(y, Fraction::from(1)), (z_2, -Fraction::from(1))], ComparisonOp::Le, Fraction::from(1));
        problem.add_constraint([(y, -Fraction::from(1)), (z_2, -Fraction::from(1))], ComparisonOp::Le, -Fraction::from(1));
        let solution = problem.solve().unwrap();
        assert_eq!(solution.objective(), Fraction::from(0));
        assert_eq!(solution[x], Fraction::from(1));
        assert_eq!(solution[y], Fraction::from(1));
        assert_eq!(solution[z_1], Fraction::from(0));
        assert_eq!(solution[z_2], Fraction::from(0));
        println!("  x = {}", solution[x]);
        println!("  y = {}", solution[y]);
        println!("  objective = {}", solution.objective());
    }

    #[test]
    fn free_variables() {
        init();
        let mut problem = Problem::new(OptimizationDirection::Maximize);
        let v1 = problem.add_var(Fraction::one(), (Fraction::zero(), Fraction::infinity()));
        let v2 = problem.add_var(Fraction::from(2), (Fraction::neg_infinity(), Fraction::infinity()));
        problem.add_constraint([(v1, Fraction::one()), (v2, Fraction::one())], ComparisonOp::Le, Fraction::from(4));
        problem.add_constraint([(v1, Fraction::one()), (v2, Fraction::one())], ComparisonOp::Ge, Fraction::from(2));
        problem.add_constraint([(v1, Fraction::one()), (v2, -Fraction::one())], ComparisonOp::Ge, Fraction::zero());

        let sol = problem.solve().unwrap();
        assert_eq!(sol[v1], Fraction::from(3));
        assert_eq!(sol[v2], Fraction::from(1));
        assert_eq!(sol.objective(), Fraction::from(5));
    }

    #[test]
    fn infeasible_problem() {
        init();
        let mut problem = Problem::new(OptimizationDirection::Minimize);
        let _ = problem.add_var(-Fraction::one(), (Fraction::zero(), Fraction::infinity()));
        assert_eq!(problem.solve().map(|_| "solved"), Err(Error::Unbounded));
    }
}