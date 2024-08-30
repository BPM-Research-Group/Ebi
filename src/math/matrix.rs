use std::{mem, ops::{Index, IndexMut, Mul}, sync::atomic::AtomicBool};

use fraction::{One, Zero};
use anyhow::{anyhow, Result};
use rayon::iter::{IndexedParallelIterator, IntoParallelIterator, IntoParallelRefMutIterator, ParallelIterator};

use super::fraction::Fraction;

/**
 * It is rather unfortunate, but there doesn't seem to be a Rust library that supports matrix operations (inversion) on fractional data types.
 * Hence, we have to do with these probably sub-optimal implementations.
 */
pub struct Matrix {
    rows: Vec<Vec<Fraction>>
}

impl Matrix {
    pub fn new() -> Matrix {
        Matrix {
            rows: vec![]
        }
    }

    pub fn new_sized(rows: usize, columns: usize, value: Fraction) -> Self {
        Matrix {
            rows: vec![vec![value; columns]; rows]
        }
    }

    pub fn new_squared(size: usize, value: Fraction) -> Self {
        Matrix {
            rows: vec![vec![value; size]; size]
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
        let i = self.get_number_of_columns();
        if i < columns {
            for row in self.rows.iter_mut() {
                row.extend(vec![value.clone(); columns - i]);
            }
        }
        //second, increase the number of rows
        self.rows.extend(vec![vec![value.clone(); columns]; rows - self.get_number_of_rows()]);
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

        // log::info!("compute inverse of {:?}", self);

        //extend the rows with the identity matrix
        let n = self.get_number_of_rows();
        for (r, row) in self.rows.iter_mut().enumerate() {
            row.extend(vec![Fraction::zero(); n]);
            row[n + r] = Fraction::one();
        }

        //solve
        self.solve()?;

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
        if self.rows.len() == 0 {
            return 0;
        }
        self.rows[0].len()
    }

    pub fn get_number_of_rows(&self) -> usize {
        self.rows.len()
    }
    
    pub fn solve(&mut self) -> Result<()> {
        // row-reduced echelon form
        if self.rows.len() == 0 {
            return Ok(());
        }
        for i in 0 .. self.rows.len() - 1 {
            if self.rows[i][i].is_zero() {
                continue;
            } else {
                for j in i .. self.rows.len() - 1 {
                    let factor = &self.rows[j + 1][i] / &self.rows[i][i];
                    for k in i .. self.get_number_of_columns() {
                        let mut old =  self.rows[i][k].clone();
                        old *= &factor;
                        self.rows[j + 1][k] -= old;
                    }
                }
            }
        }

        // log::info!("first step done");

        for i in (1..self.rows.len()).rev() {
            if self.rows[i][i].is_zero() {
                continue;
            } else {
                for j in (1 .. i+1).rev() {
                    let factor = &self.rows[j - 1][i] / &self.rows[i][i];
                    for k in (0..self.get_number_of_columns()).rev() {
                        let mut old = self.rows[i][k].clone();
                        old *= &factor;
                        self.rows[j - 1][k] -= old;
                    }
                }
            }
        }

        // log::info!("second step done");

        let mut failed = AtomicBool::new(false);
        let number_of_columns = self.get_number_of_columns();
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

    fn solve_step_3(row: &mut Vec<Fraction>, i: usize, failed: &AtomicBool, number_of_rows: usize, number_of_columns: usize) {
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

        if self.get_number_of_rows() * rhs.get_number_of_columns() > 100 { //use multi-threaded version if there are enough columns to warrant the overhead (threshold is of course arbitrary)
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
            rows: rows
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
        write!(f, "{}x{} matrix", self.get_number_of_rows(), self.get_number_of_columns())
    }
}