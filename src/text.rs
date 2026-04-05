use regex::{Captures, Regex};
use std::fmt::Display;

pub trait Rank {
    fn rank(&self) -> String;
}

impl Rank for usize {
    fn rank(&self) -> String {
        match self + 1 {
            1 => "first".to_string(),
            2 => "second".to_string(),
            3 => "third".to_string(),
            4 => "fourth".to_string(),
            5 => "fifth".to_string(),
            6 => "sixth".to_string(),
            7 => "seventh".to_string(),
            8 => "eight".to_string(),
            9 => "ninth".to_string(),
            val if val > 20 && val % 10 == 1 => format!("{}st", val),
            val if val > 20 && val % 10 == 2 => format!("{}nd", val),
            val if val > 20 && val % 10 == 3 => format!("{}rd", val),
            val => format!("{}th", val),
        }
    }
}

pub trait Joiner {
    fn join_with(&self, sep: &str, last_sep: &str) -> String;
}

pub trait LatexEscaper {
    fn escape_latex(&self) -> String;
}

pub trait JavaEscaper {
    fn escape_java_string(&self) -> String;
    fn escape_java_code(&self) -> String;
}

pub trait HTMLEscaper {
    fn escape_html(&self) -> String;
    fn latex_to_html_string(&self) -> String;
    fn md_to_html_string(&self) -> String;
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

impl<T> LatexEscaper for T
where
    T: AsRef<str>,
{
    fn escape_latex(&self) -> String {
        self.as_ref().replace("_", "\\_")
    }
}

impl<T> JavaEscaper for T
where
    T: AsRef<str>,
{
    fn escape_java_string(&self) -> String {
        let str = self.as_ref().replace("\"", "\\\"").replace("\n", "");
        let re = Regex::new(r"~\\cite\{[^\}]*\}").unwrap();
        let str = re.replace_all(&str, " ");
        str.to_string()
    }

    fn escape_java_code(&self) -> String {
        self.as_ref()
            .replace(' ', "_")
            .replace("-", "_")
            .replace(".", "_")
    }
}

impl<T> HTMLEscaper for T
where
    T: AsRef<str>,
{
    fn escape_html(&self) -> String {
        self.as_ref().replace('<', "&lt;").replace(">", "&gt;")
    }

    fn latex_to_html_string(&self) -> String {
        let regex_tabularx = Regex::new(
            "(?s)\\\\begin\\{tabularx\\}\\{[^\\}]*\\}\\{[^\\}]*\\}(.*)\\\\end\\{tabularx\\}",
        )
        .unwrap();
        let base = regex_tabularx.replace_all(self.as_ref(), |caps: &Captures| {
            let x = caps[1].to_string();
            let x = x
                .replace("&", "</td><td>")
                .replace("\\\\", "</td></tr><tr><td>")
                .replace("\\toprule", "")
                .replace("\\midrule", "")
                .replace("\\bottomrule", "");
            format!("<table><tr><td>{}</td></tr></table>", x)
        });

        let base = &base
            .replace("\\\\", "<br>")
            .replace("\\_", "_")
            .replace("\\$", "$")
            .replace("\\#", "#")
            .replace("$\\leq$", "&lt;");

        let regex_cite = Regex::new("~\\\\cite\\{[^\\}]*\\}").unwrap();
        let base = regex_cite.replace_all(base, "");

        let regex_texttt = Regex::new("\\\\texttt\\{([^\\}]*)\\}").unwrap();
        let base = regex_texttt.replace_all(&base, "<span class=\"texttt\">${1}</span>");

        let regex_center = Regex::new("(?s)\\\\begin\\{center\\}(.*)\\\\end\\{center\\}").unwrap();
        let base = regex_center.replace_all(&base, "<br>${1}<br>");

        let regex_rotate = Regex::new("(?s)\\\\rotatebox\\{([0-9]+)\\}\\{([^\\}]*)\\}").unwrap();
        let base = regex_rotate.replace_all(&base, "${2}");

        let regex_link =
            Regex::new("\\\\hyperref\\[filehandler:[^\\]]*\\]\\{([^(]*)\\(\\.([^)]*)\\)\\}")
                .unwrap();
        let base = regex_link.replace_all(
            &base,
            "${1} (<a href=\"file_handlers.html#${2}\">.${2}</a>)",
        );

        let regex_itemize =
            Regex::new("(?s)\\\\begin\\{itemize\\}(.*)\\\\end\\{itemize\\}").unwrap();
        let base = regex_itemize.replace_all(&base, |caps: &Captures| {
            let x = caps[1].to_string();
            let x = x.replace("\\item", "<li>");
            format!("<ul>{}</ul>", x)
        });

        base.to_string()
    }

    fn md_to_html_string(&self) -> String {
        let base = self.as_ref();

        let regex_enumerate =
            Regex::new("(?m)(?s)(((^1\\.[^\\n]*\\n)+(\\n|(    |\\t)[^\\n]*\\n)*)+)").unwrap();
        let base = regex_enumerate.replace_all(&base, |caps: &Captures| {
            let regex_item = Regex::new("(?m)^1\\.").unwrap();
            let x = caps[1].to_string();
            let x = regex_item.replace_all(&x, "<li>\n");

            let regex_code = Regex::new("(?m)((^(?:        |\\t\\t)([^\\n]*\\n))+)").unwrap();
            let x = regex_code.replace_all(&x, |caps: &Captures| {
                let regex_space = Regex::new("(?:        |\\t\\t)(([^\\n]*\\n))").unwrap();
                format!(
                    "<pre>{}</pre>",
                    regex_space.replace_all(&caps[1], "<code>${1}</code>")
                )
            });
            format!("<ol>{}</ol>\n", x)
        });

        let regex_h1 = Regex::new("(?m)\\n*^# (.*)$\\n+").unwrap();
        let base = regex_h1.replace_all(&base, "\n<h1>${1}</h1>\n");

        let regex_h2 = Regex::new("(?m)\\n*^## (.*)$\\n+").unwrap();
        let base = regex_h2.replace_all(&base, "\n<h2>${1}</h2>\n");

        let regex_link = Regex::new("\\[([^\\]]*)\\]\\(([^\\)]*)\\)").unwrap();
        let base = regex_link.replace_all(&base, "<a href=\"${2}\">${1}</a>");

        let regex_newline = Regex::new("(?m)\\n\\n+").unwrap();
        let base = regex_newline.replace_all(&base, "<p>\n");

        base.to_string()
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
