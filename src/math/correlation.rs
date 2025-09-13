use anyhow::{Result, anyhow};
use ebi_arithmetic::{Fraction, Zero};

use crate::math::root::Root;

pub fn correlation(pairs: &[(Fraction, Fraction)]) -> Result<Root> {
    let mut sum_xy = Fraction::zero();
    let mut sum_x = Fraction::zero();
    let mut sum_y = Fraction::zero();
    let mut sum_x_squared = Fraction::zero();
    let mut sum_y_squared = Fraction::zero();

    let n = Fraction::from(pairs.len());
    for (x, y) in pairs {
        sum_xy += x * y;
        sum_x += x;
        sum_y += y;
        sum_x_squared += x * x;
        sum_y_squared += y * y;
    }

    let num = Root::from(&(&n * &sum_xy) - &(&sum_x * &sum_y));
    let den_x = Root::of(&(&n * &sum_x_squared) - &(&sum_x * &sum_x))?;
    let den_y = Root::of(&(&n * &sum_y_squared) - &(&sum_y * &sum_y))?;

    if den_x.is_zero() || den_y.is_zero() {
        return Err(anyhow!("the standard deviation is zero"));
    }

    let result = num / (den_x * den_y);

    Ok(result)
}
