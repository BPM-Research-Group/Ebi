use std::{
    mem,
    ops::{Index, IndexMut, Mul},
    sync::atomic::AtomicBool,
};

use anyhow::{anyhow, Result};
use rayon::iter::{IndexedParallelIterator, IntoParallelRefMutIterator, ParallelIterator};

use crate::math::traits::{One, Zero};

use super::fraction::Fraction;

/**
 * It is rather unfortunate, but there doesn't seem to be a Rust library that supports matrix operations (inversion) on fractional data types.
 * Hence, we have to do with these probably sub-optimal implementations.
 */
pub struct Matrix {
    rows: Vec<Vec<Fraction>>,
    number_of_columns: usize, //keep track of the number of columns, even if there are no rows
}

impl Matrix {
    pub fn new() -> Matrix {
        Matrix {
            rows: vec![],
            number_of_columns: 0,
        }
    }

    pub fn new_sized(rows: usize, columns: usize, value: Fraction) -> Self {
        Matrix {
            rows: vec![vec![value; columns]; rows],
            number_of_columns: columns,
        }
    }

    pub fn new_squared(size: usize, value: Fraction) -> Self {
        Matrix {
            rows: vec![vec![value; size]; size],
            number_of_columns: size,
        }
    }

    pub fn get_column(&self, column: usize) -> Vec<&Fraction> {
        let mut result = vec![];
        for row in &self.rows {
            result.push(&row[column]);
        }
        result
    }

    pub fn ensure_capacity(&mut self, rows: usize, columns: usize, value: &Fraction) {
        //first, increase the number of columns
        if self.rows.len() == 0 {
            //if there are no rows yet, then we just increase the column counter
            self.number_of_columns = columns;
        } else {
            let i = self.rows[0].len();
            if i < columns {
                for row in self.rows.iter_mut() {
                    row.extend(vec![value.clone(); columns - i]);
                }
                self.number_of_columns = columns;
            }
        }

        //second, increase the number of rows
        if rows > self.rows.len() {
            self.rows.extend(vec![
                vec![value.clone(); self.number_of_columns];
                rows - self.rows.len()
            ]);
        }
    }

    pub fn element_add(&mut self, row: &usize, column: &usize, value: &Fraction) {
        self.rows[*row][*column] += value;
    }

    pub fn inverse(&mut self) -> Result<()> {
        assert!(self.get_number_of_columns() == self.get_number_of_rows());

        //optimisation: size-zero matrix
        if self.get_number_of_rows().is_zero() {
            return Ok(());
        }

        //optimisation: size-one matrix
        if self.get_number_of_rows().is_one() {
            if self.rows[0][0].is_zero() {
                return Err(anyhow!("matrix is not invertible"));
            }

            self.rows[0][0] = self.rows[0][0].recip();
            return Ok(());
        }

        //optimisation: size-two matrix
        if self.get_number_of_rows() == 2 {
            //compute determinant
            let mut det = self.rows[0][0].clone();
            det *= &self.rows[1][1];
            let mut det2 = self.rows[0][1].clone();
            det2 *= &self.rows[1][0];
            det -= det2;

            if det.is_zero() {
                return Err(anyhow!("matrix is not invertible"));
            }

            // log::debug!("determinant {}", det);

            det = det.recip();

            //perform inverse
            let (row1, row2) = self.rows.split_at_mut(1);
            mem::swap(&mut row1[0][0], &mut row2[0][1]);

            self.rows[0][0] *= &det;
            self.rows[1][1] *= &det;

            self.rows[1][0] *= -&det;
            self.rows[0][1] *= -det;
            return Ok(());
        }

        // log::info!("compute inverse of {}", self);

        //extend the rows with the identity matrix
        let n = self.get_number_of_rows();
        for (r, row) in self.rows.iter_mut().enumerate() {
            row.extend(vec![Fraction::zero(); n]);
            row[n + r] = Fraction::one();
        }

        // log::info!("add identity       {}", self);

        //solve
        self.solve()?;

        // log::info!("solved             {}", self);

        //reduce the rows
        for (_, row) in self.rows.iter_mut().enumerate() {
            row.drain(0..n);
        }

        // log::info!("inverse done");

        Ok(())
    }

    pub fn identity_minus(&mut self) {
        assert!(self.get_number_of_rows() == self.get_number_of_columns());
        for i in 0..self.rows.len() {
            for j in 0..self.rows.len() {
                if i == j {
                    self.rows[i][i] *= -Fraction::one();
                    self.rows[i][i] += Fraction::one();
                } else {
                    self.rows[i][j] *= -Fraction::one();
                }
            }
        }
    }

    pub fn get_number_of_columns(&self) -> usize {
        self.number_of_columns
    }

    pub fn get_number_of_rows(&self) -> usize {
        self.rows.len()
    }

    pub fn solve(&mut self) -> Result<()> {
        // row-reduced echelon form
        if self.rows.len() == 0 {
            return Ok(());
        }
        for i in 0..self.rows.len() - 1 {
            if self.rows[i][i].is_zero() {
                continue;
            } else {
                for j in i..self.rows.len() - 1 {
                    let factor = &self.rows[j + 1][i] / &self.rows[i][i];
                    for k in i..self.get_number_of_columns() {
                        let mut old = self.rows[i][k].clone();
                        old *= &factor;
                        self.rows[j + 1][k] -= old;
                    }
                }
            }
        }

        // log::info!("row-reduced echelon{}", self);

        // log::info!("number of columns {}", self.get_number_of_columns());

        // log::info!("first step done");

        for i in (0..self.rows.len()).rev() {
            if self.rows[i][i].is_zero() {
                continue;
            } else {
                for j in (0..i).rev() {
                    let factor = &self.rows[j][i] / &self.rows[i][i];
                    for k in i..self.rows[0].len() {
                        let mut old = self.rows[i][k].clone();
                        old *= &factor;
                        self.rows[j][k] -= old;
                    }
                }
            }
        }

        // log::info!("second step        {}", self);

        // log::info!("second step done");

        let failed = AtomicBool::new(false);
        let number_of_columns = self.rows[0].len();
        let number_of_rows = self.get_number_of_rows();

        if number_of_rows > 100 {
            self.rows.par_iter_mut().enumerate().for_each(|(i, row)| {
                Self::solve_step_3(row, i, &failed, number_of_rows, number_of_columns);
            });
        } else {
            self.rows.iter_mut().enumerate().for_each(|(i, row)| {
                Self::solve_step_3(row, i, &failed, number_of_rows, number_of_columns);
            });
        }

        if failed.load(std::sync::atomic::Ordering::Relaxed) {
            return Err(anyhow!("matrix is not invertible"));
        }

        // log::info!("third step done");

        Ok(())
    }

    fn solve_step_3(
        row: &mut Vec<Fraction>,
        i: usize,
        failed: &AtomicBool,
        number_of_rows: usize,
        number_of_columns: usize,
    ) {
        let factor = row[i].clone();
        if factor.is_zero() {
            failed.store(true, std::sync::atomic::Ordering::Relaxed);
        } else {
            for j in number_of_rows..number_of_columns {
                row[j] /= &factor;
            }
            row[i] = Fraction::one();
        }
    }

    pub fn normalise_row_wise(&mut self) {
        self.rows.par_iter_mut().for_each(|row| {
            let sum = row.iter().fold(Fraction::zero(), |x, y| &x + y);
            row.iter_mut().for_each(|x| *x /= &sum);
        });
    }

    pub fn into(column_vector: Vec<Fraction>) -> Matrix {
        column_vector.into()
    }

    pub fn multiply_vector_matrix(&self, lhs: &Vec<Fraction>) -> Vec<Fraction> {
        assert!(self.get_number_of_rows() == lhs.len());

        let mut result = vec![Fraction::zero(); self.get_number_of_rows()];
        for row in 0..self.get_number_of_rows() {
            for column in 0..self.get_number_of_columns() {
                result[column] += &self.rows[row][column] * &lhs[row];
            }
        }

        result
    }

    pub fn display_vector(
        f: &mut std::fmt::Formatter<'_>,
        vector: &Vec<Fraction>,
    ) -> std::fmt::Result {
        write!(f, "{{")?;
        for (j, fraction) in vector.iter().enumerate() {
            write!(f, "{}", fraction.to_string())?;
            if j < vector.len() - 1 {
                write!(f, ", ")?;
            }
        }
        write!(f, "}}")
    }
}

impl Index<usize> for Matrix {
    type Output = Vec<Fraction>;

    fn index(&self, index: usize) -> &Self::Output {
        &self.rows[index]
    }
}

impl IndexMut<usize> for Matrix {
    fn index_mut(&mut self, index: usize) -> &mut Self::Output {
        &mut self.rows[index]
    }
}

impl Mul for Matrix {
    type Output = Matrix;

    fn mul(self, rhs: Self) -> Self::Output {
        // log::debug!("multiply {:?} with {:?}", self, rhs);
        assert!(self.get_number_of_columns() == rhs.get_number_of_rows());

        let n = self.get_number_of_rows();
        let m = self.get_number_of_columns();
        let p = rhs.get_number_of_columns();
        let mut rows = vec![vec![Fraction::zero(); p]; n];

        if self.get_number_of_rows() * rhs.get_number_of_columns() > 100 {
            //use multi-threaded version if there are enough columns to warrant the overhead (threshold is of course arbitrary)
            rows.par_iter_mut().enumerate().for_each(|(i, row)| {
                row.par_iter_mut().enumerate().for_each(|(j, element)| {
                    for k in 0..m {
                        *element += &self.rows[i][k] * &rhs.rows[k][j];
                    }
                });
            });
        } else {
            for i in 0..n {
                for j in 0..p {
                    for k in 0..m {
                        rows[i][j] += &self.rows[i][k] * &rhs.rows[k][j];
                    }
                }
            }
        }

        // log::debug!("multiplication done");

        Self {
            rows: rows,
            number_of_columns: p,
        }
    }
}

impl std::fmt::Display for Matrix {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{{{{")?;
        for (i, row) in self.rows.iter().enumerate() {
            for (j, fraction) in row.iter().enumerate() {
                write!(f, "{}", fraction.to_string())?;
                if j < row.len() - 1 {
                    write!(f, ", ")?;
                }
            }
            if i < self.rows.len() - 1 {
                write!(f, "}}, {{")?;
            }
        }
        write!(f, "}}}}")
    }
}

impl std::fmt::Debug for Matrix {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}x{} matrix",
            self.get_number_of_rows(),
            self.get_number_of_columns()
        )
    }
}

impl From<Vec<Fraction>> for Matrix {
    fn from(value: Vec<Fraction>) -> Self {
        Self {
            rows: vec![value],
            number_of_columns: 1,
        }
    }
}

impl From<Vec<Vec<Fraction>>> for Matrix {
    fn from(value: Vec<Vec<Fraction>>) -> Self {
        let columns = if value.len() == 0 { 0 } else { value[0].len() };
        Self {
            rows: value,
            number_of_columns: columns,
        }
    }
}

impl Mul<Vec<Fraction>> for Matrix {
    type Output = Vec<Fraction>;

    fn mul(self, rhs: Vec<Fraction>) -> Self::Output {
        assert!(self.get_number_of_columns() == rhs.len());

        let mut result = vec![Fraction::zero(); self.get_number_of_rows()];
        for row in 0..self.get_number_of_rows() {
            for column in 0..self.get_number_of_columns() {
                result[row] += &self.rows[row][column] * &rhs[column];
            }
        }

        result
    }
}

#[cfg(test)]
mod tests {
    use crate::math::{fraction::Fraction, matrix::Matrix};

    #[test]
    fn matrix_vector_multiplication() {
        let m: Matrix = vec![
            vec![6.into(), 2.into(), 4.into()],
            vec![(-1).into(), 4.into(), 3.into()],
            vec![(-2).into(), 9.into(), 3.into()],
        ]
        .into();
        let v: Vec<Fraction> = vec![4.into(), (-2).into(), 1.into()];

        let x = m * v;

        let t = vec![24.into(), (-9).into(), (-23).into()];

        assert_eq!(x, t);
    }
}