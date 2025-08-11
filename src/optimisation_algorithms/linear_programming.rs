/*!
A fast linear programming solver library, adapted from the MiniLP crate.

[Linear programming](https://en.wikipedia.org/wiki/Linear_programming) is a technique for
finding the minimum (or maximum) of a linear function of a set of continuous variables
subject to linear equality and inequality constraints.

# Features

* Pure Rust implementation.
* Able to solve problems with hundreds of thousands of variables and constraints.
* Incremental: add constraints to an existing solution without solving it from scratch.
* Problems can be defined via an API or parsed from an
  [MPS](https://en.wikipedia.org/wiki/MPS_(format)) file.

# Entry points

Begin by creating a [`Problem`](struct.Problem.html) instance, declaring variables and adding
constraints. Solving it will produce a [`Solution`](struct.Solution.html) that can be used to
get the optimal objective value, corresponding variable values and to add more constraints
to the problem.

# Example

```
# use ebi::optimisation_algorithms::{*, linear_programming::*};
# use ebi_arithmetic::{*,ebi_number::*,fraction::*};

// Maximise an objective function x + 2 * y of two variables x >= 0 and 0 <= y <= 3
let mut problem = Problem::new(OptimisationDirection::Maximise);
let x = problem.add_var(f1!(), (f0!(), Fraction::infinity()));
let y = problem.add_var(f!(2), (f0!(), f!(3)));

// subject to constraints: x + y <= 4 and 2 * x + y >= 2.
problem.add_constraint(&[(x, f1!()), (y, f1!())], ComparisonOp::Le, f!(4));
problem.add_constraint(&[(x, f!(2)), (y, f1!())], ComparisonOp::Ge, f!(2));

// Optimal value is 7, achieved at x = 1 and y = 3.
let solution = problem.solve().unwrap();
assert_eq!(solution.objective(), f!(7));
assert_eq!(solution[x], f1!());
assert_eq!(solution[y], f!(3));
```
*/

#![deny(missing_debug_implementations, missing_docs)]

/// An enum indicating whether to minimise or maximise objective function.
#[derive(Clone, Copy, Debug)]
pub enum OptimisationDirection {
    /// Minimise the objective function.
    Minimise,
    /// Maximise the objective function.
    Maximise,
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
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let msg = match self {
            Error::Infeasible => "problem is infeasible",
            Error::Unbounded => "problem is unbounded",
        };
        msg.fmt(f)
    }
}

impl std::error::Error for Error {}

/// A specification of a linear programming problem.
#[derive(Clone)]
pub struct Problem {
    direction: OptimisationDirection,
    obj_coeffs: Vec<Fraction>,
    var_mins: Vec<Fraction>,
    var_maxs: Vec<Fraction>,
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

pub(crate) type CsVec = sprs::CsVecI<Fraction, usize>;

impl Problem {
    /// Create a new problem instance.
    pub fn new(direction: OptimisationDirection) -> Self {
        Problem {
            direction,
            obj_coeffs: vec![],
            var_mins: vec![],
            var_maxs: vec![],
            constraints: vec![],
        }
    }

    /// Add a new variable to the problem.
    ///
    /// `obj_coeff` is a coefficient of the term in the objective function corresponding to this
    /// variable, `min` and `max` are the minimum and maximum (inclusive) bounds of this
    /// variable. If one of the bounds is absent, use `Fraction::neg_infinity()` for minimum and
    /// `Fraction::infinity()` for maximum.
    pub fn add_var(&mut self, obj_coeff: Fraction, (min, max): (Fraction, Fraction)) -> Variable {
        let var = Variable(self.obj_coeffs.len());
        let obj_coeff = match self.direction {
            OptimisationDirection::Minimise => obj_coeff,
            OptimisationDirection::Maximise => -obj_coeff,
        };
        self.obj_coeffs.push(obj_coeff);
        self.var_mins.push(min);
        self.var_maxs.push(max);
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
    /// # use ebi::optimisation_algorithms::{*, linear_programming::*};
    /// # use ebi_arithmetic::{*,ebi_number::*,fraction::*};
    /// let mut problem = Problem::new(OptimisationDirection::Minimise);
    /// let x = problem.add_var(f1!(), (f0!(), Fraction::infinity()));
    /// let y = problem.add_var(f1!(), (f0!(), Fraction::infinity()));
    ///
    /// // Add an x + y >= 2 constraint, specifying the left-hand side expression:
    ///
    /// // * by passing a slice of pairs (useful when explicitly enumerating variables)
    /// problem.add_constraint(&[(x, f1!()), (y, f1!())], ComparisonOp::Ge, f!(2));
    ///
    /// // * by passing an iterator of variable-coefficient pairs.
    /// let vars = [x, y];
    /// problem.add_constraint(vars.iter().map(|&v| (v, f1!())), ComparisonOp::Ge, f!(2));
    ///
    /// // * by manually constructing a LinearExpr.
    /// let mut lhs = LinearExpr::empty();
    /// for &v in &vars {
    ///     lhs.add(v, f1!());
    /// }
    /// problem.add_constraint(lhs, ComparisonOp::Ge, f!(2));
    /// ```
    pub fn add_constraint(
        &mut self,
        expr: impl Into<LinearExpr>,
        cmp_op: ComparisonOp,
        rhs: Fraction,
    ) {
        let expr = expr.into();
        self.constraints.push((
            CsVec::new(self.obj_coeffs.len(), expr.vars, expr.coeffs),
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
    direction: OptimisationDirection,
    num_vars: usize,
    solver: Solver,
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
            OptimisationDirection::Minimise => self.solver.cur_obj_val.clone(),
            OptimisationDirection::Maximise => -self.solver.cur_obj_val.clone(),
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

    /// Add another constraint and return the solution to the updated problem.
    ///
    /// This method will consume the solution and not return it in case of error. See also
    /// examples of specifying the left-hand side in the docs for the [`Problem::add_constraint`]
    /// method.
    ///
    /// [`Problem::add_constraint`]: struct.Problem.html#method.add_constraint
    ///
    /// # Errors
    ///
    /// Will return an error if the problem becomes infeasible with the additional constraint.
    pub fn add_constraint(
        mut self,
        expr: impl Into<LinearExpr>,
        cmp_op: ComparisonOp,
        rhs: Fraction,
    ) -> Result<Self, Error> {
        let expr = expr.into();
        self.solver.add_constraint(
            CsVec::new(self.num_vars, expr.vars, expr.coeffs),
            cmp_op,
            rhs,
        )?;
        Ok(self)
    }

    /// Fix the variable to the specified value and return the solution to the updated problem.
    ///
    /// This method will consume the solution and not return it in case of error.
    ///
    /// # Errors
    ///
    /// Will return an error if the problem becomes infeasible with the additional constraint.
    pub fn fix_var(mut self, var: Variable, val: Fraction) -> Result<Self, Error> {
        assert!(var.0 < self.num_vars);
        self.solver.fix_var(var.0, val)?;
        Ok(self)
    }

    /// If the variable was fixed with [`fix_var`](#method.fix_var) before, remove that constraint
    /// and return the solution to the updated problem and a boolean indicating if the variable was
    /// really fixed before.
    pub fn unfix_var(mut self, var: Variable) -> (Self, bool) {
        assert!(var.0 < self.num_vars);
        let res = self.solver.unfix_var(var.0);
        (self, res)
    }

    // TODO: remove_constraint

    /// Add a [Gomory cut] constraint to the problem and return the solution.
    ///
    /// [Gomory cut]: https://en.wikipedia.org/wiki/Cutting-plane_method#Gomory's_cut
    ///
    /// # Errors
    ///
    /// Will return an error if the problem becomes infeasible with the additional constraint.
    ///
    /// # Panics
    ///
    /// Will panic if the variable is not basic (variable is basic if it has value other than
    /// its bounds).
    pub fn add_gomory_cut(mut self, var: Variable) -> Result<Self, Error> {
        assert!(var.0 < self.num_vars);
        self.solver.add_gomory_cut(var.0)?;
        Ok(self)
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

use ebi_arithmetic::fraction::Fraction;

use crate::optimisation_algorithms::linear_programming_solver::Solver;

#[cfg(test)]
mod tests {
    use ebi_arithmetic::{
        ebi_number::{One, Signed, Zero},
        f, f0, f1,
    };

    use super::*;

    #[test]
    fn optimise() {
        let mut problem = Problem::new(OptimisationDirection::Maximise);
        let v1 = problem.add_var(f!(3), (f!(12), Fraction::infinity()));
        let v2 = problem.add_var(f!(4), (f!(5), Fraction::infinity()));
        problem.add_constraint(&[(v1, f1!()), (v2, f1!())], ComparisonOp::Le, f!(20));
        problem.add_constraint(&[(v1, f1!()), (v2, -f!(4))], ComparisonOp::Ge, -f!(20));

        let sol = problem.solve().unwrap();
        assert_eq!(sol[v1], f!(12));
        assert_eq!(sol[v2], f!(8));
        assert_eq!(sol.objective(), f!(68));
    }

    #[test]
    fn empty_expr_constraints() {
        let trivial = [
            (LinearExpr::empty(), ComparisonOp::Eq, f0!()),
            (LinearExpr::empty(), ComparisonOp::Ge, -f1!()),
            (LinearExpr::empty(), ComparisonOp::Le, f1!()),
        ];

        let mut problem = Problem::new(OptimisationDirection::Minimise);
        let _ = problem.add_var(f1!(), (f0!(), Fraction::infinity()));
        for (expr, op, b) in trivial.iter().cloned() {
            problem.add_constraint(expr, op, b);
        }
        assert_eq!(problem.solve().map(|s| s.objective()), Ok(f0!()));

        {
            let mut sol = problem.solve().unwrap();
            for (expr, op, b) in trivial.iter().cloned() {
                sol = sol.add_constraint(expr, op, b).unwrap();
            }
            assert_eq!(sol.objective(), f0!());
        }

        let infeasible = [
            (LinearExpr::empty(), ComparisonOp::Eq, f!(12)),
            (LinearExpr::empty(), ComparisonOp::Ge, f!(34)),
            (LinearExpr::empty(), ComparisonOp::Le, -f!(56)),
        ];

        for (expr, op, b) in infeasible.iter().cloned() {
            let mut cloned = problem.clone();
            cloned.add_constraint(expr, op, b);
            assert_eq!(cloned.solve().map(|_| "solved"), Err(Error::Infeasible));
        }

        for (expr, op, b) in infeasible.iter().cloned() {
            let sol = problem.solve().unwrap().add_constraint(expr, op, b);
            assert_eq!(sol.map(|_| "solved"), Err(Error::Infeasible));
        }

        let _ = problem.add_var(-f1!(), (f0!(), Fraction::infinity()));
        assert_eq!(problem.solve().map(|_| "solved"), Err(Error::Unbounded));
    }

    #[test]
    fn free_variables() {
        let mut problem = Problem::new(OptimisationDirection::Maximise);
        let v1 = problem.add_var(f1!(), (f0!(), Fraction::infinity()));
        let v2 = problem.add_var(f!(2), (Fraction::neg_infinity(), Fraction::infinity()));
        problem.add_constraint(&[(v1, f1!()), (v2, f1!())], ComparisonOp::Le, f!(4));
        problem.add_constraint(&[(v1, f1!()), (v2, f1!())], ComparisonOp::Ge, f!(2));
        problem.add_constraint(&[(v1, f1!()), (v2, -f1!())], ComparisonOp::Ge, f0!());

        let sol = problem.solve().unwrap();
        assert_eq!(sol[v1], f!(2));
        assert_eq!(sol[v2], f!(2));
        assert_eq!(sol.objective(), f!(6));
    }

    #[test]
    fn fix_unfix_var() {
        let mut problem = Problem::new(OptimisationDirection::Maximise);
        let v1 = problem.add_var(f1!(), (f0!(), f!(3)));
        let v2 = problem.add_var(f!(2), (f0!(), f!(3)));
        problem.add_constraint(&[(v1, f1!()), (v2, f1!())], ComparisonOp::Le, f!(4));
        problem.add_constraint(&[(v1, f1!()), (v2, f1!())], ComparisonOp::Ge, f1!());

        let orig_sol = problem.solve().unwrap();

        {
            let mut sol = orig_sol.clone().fix_var(v1, f!(1, 2)).unwrap();
            assert_eq!(sol[v1], f!(1, 2));
            assert_eq!(sol[v2], f!(3));
            assert_eq!(sol.objective(), f!(65, 10));

            sol = sol.unfix_var(v1).0;
            assert_eq!(sol[v1], f1!());
            assert_eq!(sol[v2], f!(3));
            assert_eq!(sol.objective(), f!(7));
        }

        {
            let mut sol = orig_sol.clone().fix_var(v2, f!(25, 10)).unwrap();
            assert_eq!(sol[v1], f!(15, 10));
            assert_eq!(sol[v2], f!(25, 10));
            assert_eq!(sol.objective(), f!(65, 10));

            sol = sol.unfix_var(v2).0;
            assert_eq!(sol[v1], f1!());
            assert_eq!(sol[v2], f!(3));
            assert_eq!(sol.objective(), f!(7));
        }
    }

    #[test]
    fn add_constraint() {
        let mut problem = Problem::new(OptimisationDirection::Minimise);
        let v1 = problem.add_var(f!(2), (f0!(), Fraction::infinity()));
        let v2 = problem.add_var(f1!(), (f0!(), Fraction::infinity()));
        problem.add_constraint(&[(v1, f1!()), (v2, f1!())], ComparisonOp::Le, f!(4));
        problem.add_constraint(&[(v1, f1!()), (v2, f1!())], ComparisonOp::Ge, f!(2));

        let orig_sol = problem.solve().unwrap();

        {
            let sol = orig_sol
                .clone()
                .add_constraint(&[(v1, -f1!()), (v2, f1!())], ComparisonOp::Le, f0!())
                .unwrap();

            assert_eq!(sol[v1], f1!());
            assert_eq!(sol[v2], f1!());
            assert_eq!(sol.objective(), f!(3));
        }

        {
            let sol = orig_sol
                .clone()
                .fix_var(v2, f!(15, 10))
                .unwrap()
                .add_constraint(&[(v1, -f1!()), (v2, f1!())], ComparisonOp::Le, f0!())
                .unwrap();
            assert_eq!(sol[v1], f!(15, 10));
            assert_eq!(sol[v2], f!(15, 10));
            assert_eq!(sol.objective(), f!(45, 10));
        }

        {
            let sol = orig_sol
                .clone()
                .add_constraint(&[(v1, -f1!()), (v2, f1!())], ComparisonOp::Ge, f!(3))
                .unwrap();

            assert_eq!(sol[v1], f0!());
            assert_eq!(sol[v2], f!(3));
            assert_eq!(sol.objective(), f!(3));
        }
    }

    #[test]
    fn gomory_cut() {
        let mut problem = Problem::new(OptimisationDirection::Minimise);
        let v1 = problem.add_var(f0!(), (f0!(), Fraction::infinity()));
        let v2 = problem.add_var(-f1!(), (f0!(), Fraction::infinity()));
        problem.add_constraint(&[(v1, f!(3)), (v2, f!(2))], ComparisonOp::Le, f!(6));
        problem.add_constraint(&[(v1, -f!(3)), (v2, f!(2))], ComparisonOp::Le, f0!());

        let mut sol = problem.solve().unwrap();
        assert_eq!(sol[v1], f1!());
        assert_eq!(sol[v2], f!(15, 10));
        assert_eq!(sol.objective(), -f!(15, 10));

        sol = sol.add_gomory_cut(v2).unwrap();
        assert!(Fraction::abs(&(&sol[v1] - &f!(2, 3))) < f!(1, 1000000000));
        assert_eq!(sol[v2], f1!());
        assert_eq!(sol.objective(), -f1!());

        sol = sol.add_gomory_cut(v1).unwrap();
        assert!(Fraction::abs(&(&sol[v1] - &f1!())) < f!(1, 1000000000));
        assert_eq!(sol[v2], f1!());
        assert_eq!(sol.objective(), -f1!());
    }
}
