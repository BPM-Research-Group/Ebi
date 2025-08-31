use std::fmt::{self, Display};

use chrono::{DateTime, FixedOffset};
use ebi_arithmetic::Fraction;
use process_mining::event_log::AttributeValue;

#[derive(Debug)]
pub enum DataType {
    Categorical,
    Numerical(Fraction, Fraction), //minimum, maximum
    Time(DateTime<FixedOffset>, DateTime<FixedOffset>), //minimum, maximum
    Undefined,
}

impl DataType {
    pub fn init(value: &AttributeValue) -> Self {
        match value {
            AttributeValue::String(x) => {
                if let Ok(v) = x.parse::<Fraction>() {
                    Self::Numerical(v.clone(), v)
                } else if let Ok(v) = x.parse::<DateTime<FixedOffset>>() {
                    Self::Time(v, v)
                } else {
                    Self::Categorical
                }
            }
            AttributeValue::Date(x) => Self::Time(*x, *x),
            AttributeValue::Int(x) => Self::Numerical(Fraction::from(*x), Fraction::from(*x)),
            AttributeValue::Float(x) => Self::Numerical(
                x.to_string().parse::<Fraction>().unwrap(),
                x.to_string().parse::<Fraction>().unwrap(),
            ),
            AttributeValue::Boolean(_) => Self::Undefined,
            AttributeValue::ID(_) => Self::Undefined,
            AttributeValue::List(_) => Self::Undefined,
            AttributeValue::Container(_) => Self::Undefined,
            AttributeValue::None() => Self::Undefined,
        }
    }

    pub fn update(&mut self, value: &AttributeValue) {
        *self = match (&self, value) {
            (DataType::Categorical, AttributeValue::String(_)) => Self::Categorical,
            (DataType::Categorical, AttributeValue::Date(_)) => Self::Undefined,
            (DataType::Categorical, AttributeValue::Int(_)) => Self::Undefined,
            (DataType::Categorical, AttributeValue::Float(_)) => Self::Undefined,
            (DataType::Categorical, AttributeValue::Boolean(_)) => Self::Undefined,
            (DataType::Categorical, AttributeValue::ID(_)) => Self::Undefined,
            (DataType::Categorical, AttributeValue::List(_)) => Self::Undefined,
            (DataType::Categorical, AttributeValue::Container(_)) => Self::Undefined,
            (DataType::Categorical, AttributeValue::None()) => Self::Categorical,
            (DataType::Numerical(min, max), AttributeValue::String(s)) => {
                if let Ok(x1) = s.parse::<Fraction>() {
                    Self::Numerical(x1.clone().min(min.clone()), x1.max(max.clone()))
                } else {
                    Self::Undefined
                }
            }
            (DataType::Numerical(_, _), AttributeValue::Date(_)) => Self::Undefined,
            (DataType::Numerical(min, max), AttributeValue::Int(y)) => Self::Numerical(
                min.min(&mut Fraction::from(*y)).clone(),
                max.max(&mut Fraction::from(*y)).clone(),
            ),
            (DataType::Numerical(min, max), AttributeValue::Float(y)) => Self::Numerical(
                min.min(&mut y.to_string().parse::<Fraction>().unwrap())
                    .clone(),
                max.max(&mut y.to_string().parse::<Fraction>().unwrap())
                    .clone(),
            ),
            (DataType::Numerical(_, _), AttributeValue::Boolean(_)) => Self::Undefined,
            (DataType::Numerical(_, _), AttributeValue::ID(_)) => Self::Undefined,
            (DataType::Numerical(_, _), AttributeValue::List(_)) => Self::Undefined,
            (DataType::Numerical(_, _), AttributeValue::Container(_)) => Self::Undefined,
            (DataType::Numerical(min, max), AttributeValue::None()) => {
                Self::Numerical(min.clone(), max.clone())
            }
            (DataType::Time(min, max), AttributeValue::String(s)) => {
                if let Ok(v) = s.parse::<DateTime<FixedOffset>>() {
                    Self::Time(v.min(*min), v.max(*max))
                } else {
                    Self::Categorical
                }
            }
            (DataType::Time(min, max), AttributeValue::Date(y)) => {
                Self::Time(*min.min(y), *max.max(y))
            }
            (DataType::Time(_, _), AttributeValue::Int(_)) => Self::Undefined,
            (DataType::Time(_, _), AttributeValue::Float(_)) => Self::Undefined,
            (DataType::Time(_, _), AttributeValue::Boolean(_)) => Self::Undefined,
            (DataType::Time(_, _), AttributeValue::ID(_)) => Self::Undefined,
            (DataType::Time(_, _), AttributeValue::List(_)) => Self::Undefined,
            (DataType::Time(_, _), AttributeValue::Container(_)) => Self::Undefined,
            (DataType::Time(x, y), AttributeValue::None()) => Self::Time(*x, *y),
            (DataType::Undefined, AttributeValue::String(_)) => Self::Undefined,
            (DataType::Undefined, AttributeValue::Date(_)) => Self::Undefined,
            (DataType::Undefined, AttributeValue::Int(_)) => Self::Undefined,
            (DataType::Undefined, AttributeValue::Float(_)) => Self::Undefined,
            (DataType::Undefined, AttributeValue::Boolean(_)) => Self::Undefined,
            (DataType::Undefined, AttributeValue::ID(_)) => Self::Undefined,
            (DataType::Undefined, AttributeValue::List(_)) => Self::Undefined,
            (DataType::Undefined, AttributeValue::Container(_)) => Self::Undefined,
            (DataType::Undefined, AttributeValue::None()) => Self::Undefined,
        };
    }
}

impl Display for DataType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            DataType::Categorical => write!(f, "categorical"),
            DataType::Numerical(min, max) => write!(f, "numerical between {} and {}", min, max),
            DataType::Time(min, max) => write!(f, "time between {} and {}", min, max),
            DataType::Undefined => write!(f, "undefined"),
        }
    }
}