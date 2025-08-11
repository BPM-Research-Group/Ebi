use ebi_arithmetic::{
    ebi_number::{One, Zero},
    fraction::Fraction,
};
use microlp::{ComparisonOp, Error, OptimizationDirection, VarDomain};

use crate::optimisation_algorithms::{
    mixed_integer_linear_programming_solver::Solver, mixed_integer_linear_programming_sparse,
};

pub type CsVec = sprs::CsVecI<Fraction, usize>;

impl From<mixed_integer_linear_programming_sparse::Error> for Error {
    fn from(value: mixed_integer_linear_programming_sparse::Error) -> Self {
        Error::InternalError(value.to_string())
    }
}

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
    /// variable. If one of the bounds is absent, use `Fraction::NEG_INFINITY` for minimum and
    /// `Fraction::INFINITY` for maximum.
    pub fn add_var(&mut self, obj_coeff: Fraction, (min, max): (Fraction, Fraction)) -> Variable {
        self.internal_add_var(obj_coeff, (min, max), VarDomain::Real)
    }

    /// Add a new integer variable to the problem.
    ///
    /// `obj_coeff` is a coefficient of the term in the objective function corresponding to this
    /// variable, `min` and `max` are the minimum and maximum (inclusive) bounds of this
    /// variable. If one of the bounds is absent, use `-Fraction::infinity()` for minimum and
    /// `Fraction::infinity()` for maximum.
    pub fn add_integer_var(&mut self, obj_coeff: Fraction, (min, max): (i32, i32)) -> Variable {
        self.internal_add_var(
            obj_coeff,
            (Fraction::from(min), Fraction::from(max)),
            VarDomain::Integer,
        )
    }

    /// Check if the problem has any integer variables.
    pub fn has_integer_vars(&self) -> bool {
        self.var_domains
            .iter()
            .any(|v| *v == VarDomain::Integer || *v == VarDomain::Boolean)
    }

    /// Add a new binary variable to the problem.
    ///
    /// `obj_coeff` is a coefficient of the term in the objective function corresponding to this variable.
    pub fn add_binary_var(&mut self, obj_coeff: Fraction) -> Variable {
        self.internal_add_var(
            obj_coeff,
            (Fraction::zero(), Fraction::one()),
            VarDomain::Boolean,
        )
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
    /// let x = problem.add_var(Fraction::from(1), (Fraction::from(0), Fraction::infinity()));
    /// let y = problem.add_var(Fraction::from(1), (Fraction::from(0), Fraction::infinity()));
    ///
    /// // Add an x + y >= 2 constraint, specifying the left-hand side expression:
    ///
    /// // * by passing a slice of pairs (useful when explicitly enumerating variables)
    /// problem.add_constraint(&[(x, Fraction::from(1)), (y, Fraction::from(1))], ComparisonOp::Ge, Fraction::from(2));
    ///
    /// // * by passing an iterator of variable-coefficient pairs.
    /// let vars = [x, y];
    /// problem.add_constraint(vars.iter().map(|&v| (v, Fraction::from(1))), ComparisonOp::Ge, Fraction::from(2));
    ///
    /// // * by manually constructing a LinearExpr.
    /// let mut lhs = LinearExpr::empty();
    /// for &v in &vars {
    ///     lhs.add(v, Fraction::from(1));
    /// }
    /// problem.add_constraint(lhs, ComparisonOp::Ge, Fraction::from(2));
    /// ```
    pub fn add_constraint(
        &mut self,
        expr: impl Into<LinearExpr>,
        cmp_op: ComparisonOp,
        rhs: Fraction,
    ) {
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

        if self.has_integer_vars() {
            let non_integer_solution = Solution {
                num_vars: self.obj_coeffs.len(),
                direction: self.direction,
                solver: solver.clone(),
            };
            solver.solve_integer(non_integer_solution, self.direction)?;
            let solution = Solution {
                num_vars: self.obj_coeffs.len(),
                direction: self.direction,
                solver,
            };
            Ok(solution)
        } else {
            Ok(Solution {
                num_vars: self.obj_coeffs.len(),
                direction: self.direction,
                solver,
            })
        }
    }
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
    pub(crate) solver: Solver,
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
            OptimizationDirection::Maximize => -self.solver.cur_obj_val.clone(),
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
            CsVec::new_from_unsorted(self.num_vars, expr.vars, expr.coeffs)
                .map_err(|v| Error::InternalError(v.2.to_string()))?,
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

#[cfg(test)]
mod tests {

    use ebi_arithmetic::fraction::Fraction;
    use microlp::{ComparisonOp, OptimizationDirection};

    use crate::optimisation_algorithms::mixed_integer_linear_programming::Problem;

    #[test]
    fn test_micro_lp() {
        // Maximize an objective function x + 2 * y of two continuous variables x >= 0 and 0 <= y <= 3
        let mut problem = Problem::new(OptimizationDirection::Maximize);
        let x = problem.add_var(Fraction::from(1), (Fraction::from(0), Fraction::infinity()));
        let y = problem.add_var(Fraction::from(2), (Fraction::from(0), Fraction::from(3)));

        // subject to constraints: x + y <= 4 and 2 * x + y >= 2.
        problem.add_constraint(
            &[(x, Fraction::from(1)), (y, Fraction::from(1))],
            ComparisonOp::Le,
            Fraction::from(4),
        );
        problem.add_constraint(
            &[(x, Fraction::from(2)), (y, Fraction::from(1))],
            ComparisonOp::Ge,
            Fraction::from(2),
        );

        // Optimal value is 7, achieved at x = 1 and y = 3.
        let solution = problem.solve().unwrap();
        assert_eq!(solution.objective(), Fraction::from(7));
        assert_eq!(solution[x], Fraction::from(1));
        assert_eq!(solution[y], Fraction::from(3));
    }
}
