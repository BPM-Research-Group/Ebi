use std::fmt::Display;

pub trait Joiner {
    fn join_with(&self, sep: &str, last_sep: &str) -> String;
}

impl<T> Joiner for &[T]
where
    T: Display,
{
    fn join_with(&self, sep: &str, last_sep: &str) -> String {
        if self.len() == 1 {
            return self[0].to_string();
        }

        let (last, list) = self.split_last().unwrap();
        let mut iter = list.iter();
        let first = iter.next().unwrap();
        let mut result = first.to_string();

        for v in iter {
            result.push_str(sep);
            result += &v.to_string()
        }
        result.push_str(last_sep);
        result += &last.to_string();
        result
    }
}

impl<T> Joiner for Vec<T>
where
    T: Display,
{
    fn join_with(&self, sep: &str, last_sep: &str) -> String {
        let slice = &self[..];
        slice.join_with(sep, last_sep)
    }
}

#[cfg(test)]
mod tests {
    use super::Joiner;

    #[test]
    fn join_vec() {
        let x = vec!["a", "b", "c"];
        assert_eq!(x.join_with(", ", " and "), "a, b and c");

        let y = vec!["a".to_string(), "b".to_string(), "c".to_string()];
        let z = &y;
        assert_eq!(z.join_with(", ", " and "), "a, b and c");
        assert_eq!(x.as_slice().join_with(", ", " and "), "a, b and c");

        assert_eq!(vec!["a"].join_with(",", ","), "a");
    }
}
