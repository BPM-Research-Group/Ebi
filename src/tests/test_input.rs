use crate::{
    ebi_framework::{
        ebi_file_handler::{EBI_FILE_HANDLERS, EbiFileHandler},
        ebi_input::{self, EbiInputType},
        ebi_trait::EbiTrait,
    },
    multiple_reader::MultipleReader,
};
use ebi_objects::{EbiObject, ebi_arithmetic::Fraction};
use itertools::Itertools;
use std::{
    fmt::Debug,
    fs::{self, File},
    path::PathBuf,
};

#[allow(unused)]
#[derive(Clone)]
pub(crate) enum TestInput {
    Trait(EbiTrait, PathBuf), //a trait cannot be cloned, thus we must parse it every time in the cartesian product
    Object(EbiObject, &'static EbiFileHandler, PathBuf),
    String(String, &'static EbiInputType),
    Usize(usize, &'static EbiInputType),
    FileHandler(EbiFileHandler),
    Fraction(Fraction, &'static EbiInputType),
    StringList(Vec<String>, &'static EbiInputType),
}

impl TestInput {
    #[cfg(feature = "python")]
    pub(crate) fn to_ebi_input(self) -> EbiInput {
        match self {
            TestInput::Trait(etrait, file) => {
                use crate::{
                    ebi_framework::ebi_input::{self, EbiInput},
                    multiple_reader::MultipleReader,
                };
                use std::fs::File;

                let mut reader = MultipleReader::from_file(File::open(file).unwrap());
                match ebi_input::read_as_trait(&etrait, &mut reader, None, 0) {
                    Ok((object, file_handler)) => EbiInput::Trait(object, file_handler),
                    Err(_) => panic!(),
                }
            }
            TestInput::Object(o, fh, _) => EbiInput::Object(o, fh),
            TestInput::String(s, input_type) => EbiInput::String(s, input_type),
            TestInput::Usize(u, input_type) => EbiInput::Usize(u, input_type),
            TestInput::FileHandler(fh) => EbiInput::FileHandler(fh),
            TestInput::Fraction(f, input_type) => EbiInput::Fraction(f, input_type),
            TestInput::StringList(l, input_type) => EbiInput::StringList(l, input_type),
        }
    }

    /// The positional argument that gives this input on a command line.
    ///
    /// A file handler is given by its extension. Returns None for a file
    /// handler that the command line cannot name, as it has no validator.
    pub fn to_cli_argument(&self) -> Option<String> {
        match self {
            TestInput::Trait(_, path_buf) | TestInput::Object(_, _, path_buf) => {
                Some(path_buf.to_str().unwrap().to_string())
            }
            TestInput::String(s, _) => Some(s.clone()),
            TestInput::Usize(u, _) => Some(u.to_string()),
            TestInput::FileHandler(file_handler) => file_handler
                .validator
                .is_some()
                .then(|| file_handler.file_extension.to_string()),
            TestInput::Fraction(f, _) => Some(f.to_string()),
            TestInput::StringList(l, _) => Some(l.join(" ")),
        }
    }

    pub fn to_unique_string(&self) -> String {
        match self {
            TestInput::Trait(ebi_trait, path_buf) => {
                format!("trait {}#{}", ebi_trait, path_buf.to_str().unwrap())
            }
            TestInput::Object(ebi_object, _, path_buf) => {
                format!(
                    "object {}#{}",
                    ebi_object.get_type(),
                    path_buf.to_str().unwrap()
                )
            }
            TestInput::String(s, _) => format!("string {s}"),
            TestInput::Usize(u, _) => format!("usize {u}"),
            TestInput::FileHandler(ebi_file_handler) => format!("filehandler {ebi_file_handler}"),
            TestInput::Fraction(f, _) => format!("fraction {f}"),
            TestInput::StringList(l, _) => format!("stringlist {l:?}"),
        }
    }

    pub(crate) fn find_inputs(input_typess: &[&[&'static EbiInputType]]) -> Vec<Vec<TestInput>> {
        let mut it = input_typess.iter();
        let mut result = if let Some(input_types) = it.next() {
            Self::find_inputs_for_position(input_types)
                .into_iter()
                .map(|x| vec![x])
                .collect()
        } else {
            vec![]
        };
        while let Some(input_types) = it.next() {
            let add = Self::find_inputs_for_position(input_types);
            result = result
                .into_iter()
                .cartesian_product(add.into_iter())
                .map(|(mut xs, x)| {
                    xs.push(x);
                    xs
                })
                .collect();
        }
        return result;
    }

    fn find_inputs_for_position(input_types: &[&'static EbiInputType]) -> Vec<TestInput> {
        let mut result = vec![];
        for input_type in input_types {
            match input_type {
                EbiInputType::FileHandler => {
                    result.append(
                        &mut EBI_FILE_HANDLERS
                            .iter()
                            .map(|f| TestInput::FileHandler(f.clone()))
                            .collect::<Vec<_>>(),
                    );
                }
                EbiInputType::Fraction(_, _, Some(default)) => {
                    result.push(TestInput::Fraction(default.to_fraction(), &input_type));
                }
                EbiInputType::Fraction(Some(min), _, _) => {
                    result.push(TestInput::Fraction(min.to_fraction(), &input_type));
                }
                EbiInputType::Fraction(_, Some(max), _) => {
                    result.push(TestInput::Fraction(max.to_fraction(), &input_type));
                }
                EbiInputType::Fraction(_, _, _) => {
                    result.push(TestInput::Fraction(Fraction::from((1, 2)), &input_type));
                }
                EbiInputType::StringList => {
                    result.push(TestInput::StringList(
                        vec!["a".to_string(), "b".to_string()],
                        &input_type,
                    ));
                }
                EbiInputType::String(_, Some(default)) => {
                    result.push(TestInput::String(default.to_string(), &input_type));
                }
                EbiInputType::String(Some(allowed_values), None) => {
                    result.push(TestInput::String(
                        allowed_values[0].to_string(),
                        &input_type,
                    ));
                }
                EbiInputType::String(None, None) => {
                    result.push(TestInput::String("some string".to_string(), &input_type));
                }
                EbiInputType::Usize(_, _, Some(default)) => {
                    //to keep testing feasible, do not use a default more than 10
                    result.push(TestInput::Usize(*default.min(&10), &input_type));
                }
                EbiInputType::Usize(Some(min), _, _) => {
                    result.push(TestInput::Usize(*min, &input_type));
                }
                EbiInputType::Usize(_, Some(max), _) => {
                    result.push(TestInput::Usize(*max, &input_type));
                }
                EbiInputType::Usize(_, _, _) => {
                    result.push(TestInput::Usize(10, &input_type));
                }
                _ => {
                    //look through all files
                    let files = fs::read_dir("./testfiles").unwrap();
                    for path in files {
                        let file = path.unwrap();

                        //for now, we skip files with loops and livelocks, as the techniques cannot handle them yet
                        if !file.file_name().into_string().unwrap().contains("livelock")
                            && !file.file_name().into_string().unwrap().contains("infinite")
                            && !file
                                .file_name()
                                .into_string()
                                .unwrap()
                                .contains("unbounded")
                            && !file.file_name().into_string().unwrap().contains("loop")
                            && !file.file_name().into_string().unwrap().contains("pdc")
                            && !file
                                .file_name()
                                .into_string()
                                .unwrap()
                                .contains("irregular")
                        {
                            let path = test_file_path(&file.file_name().into_string().unwrap());
                            let mut reader = MultipleReader::from_file(File::open(&path).unwrap());

                            match input_type {
                                EbiInputType::Trait(etrait) => {
                                    //try to parse a trait

                                    match ebi_input::read_as_trait(etrait, &mut reader, None, 0) {
                                        Ok((_, _)) => {
                                            result.push(TestInput::Trait(*etrait, path));
                                        }
                                        Err(_) => {}
                                    }
                                }
                                EbiInputType::Object(etype) => {
                                    //try to parse a specific object
                                    match ebi_input::read_as_object(etype, &mut reader, None, 0) {
                                        Ok((object, file_handler)) => {
                                            result.push(TestInput::Object(
                                                object,
                                                file_handler,
                                                path,
                                            ));
                                        }
                                        Err(_) => {}
                                    }
                                }
                                EbiInputType::AnyObject => {
                                    match ebi_input::read_as_any_object(&mut reader, None, 0) {
                                        Ok((object, file_handler)) => {
                                            result.push(TestInput::Object(
                                                object,
                                                file_handler,
                                                path,
                                            ));
                                        }
                                        Err(_) => {}
                                    }
                                }
                                _ => {}
                            }
                        }
                    }
                }
            }
        }

        result
    }
}

/// The directory that the test files live in, relative to the crate root.
const TEST_FILES_DIRECTORY: &str = "./testfiles";

/// The path of a file in the test files directory.
///
/// The path is built with forward slashes on every platform, rather than
/// with `Path::join` or `DirEntry::path`, which use the platform separator
/// and so yield `./testfiles\x.xes` on Windows. The path is written into
/// generated Rust string literals, where a backslash is an escape character,
/// and into the keys of the fallible test list, which must be identical on
/// every platform. Windows opens forward-slash paths without trouble.
pub(crate) fn test_file_path(file_name: &str) -> PathBuf {
    PathBuf::from(format!("{TEST_FILES_DIRECTORY}/{file_name}"))
}

impl Debug for TestInput {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Trait(arg0, arg1) => f.debug_tuple("Trait").field(arg0).field(arg1).finish(),
            Self::Object(_, _, arg2) => f.debug_tuple("Object").field(arg2).finish(),
            Self::String(arg0, _) => f.debug_tuple("String").field(arg0).finish(),
            Self::Usize(arg0, _) => f.debug_tuple("Usize").field(arg0).finish(),
            Self::FileHandler(arg0) => f.debug_tuple("FileHandler").field(&arg0.name).finish(),
            Self::Fraction(arg0, _) => f.debug_tuple("Fraction").field(arg0).finish(),
            Self::StringList(arg0, _) => f.debug_tuple("StringList").field(arg0).finish(),
        }
    }
}

impl Ord for TestInput {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.to_unique_string().cmp(&other.to_unique_string())
    }
}

impl PartialOrd for TestInput {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Eq for TestInput {}

impl PartialEq for TestInput {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Trait(l0, l1), Self::Trait(r0, r1)) => l0 == r0 && l1 == r1,
            (Self::Object(_, l1, l2), Self::Object(_, r1, r2)) => l1 == r1 && l2 == r2,
            (Self::String(l0, l1), Self::String(r0, r1)) => l0 == r0 && l1 == r1,
            (Self::Usize(l0, l1), Self::Usize(r0, r1)) => l0 == r0 && l1 == r1,
            (Self::FileHandler(l0), Self::FileHandler(r0)) => l0 == r0,
            (Self::Fraction(l0, l1), Self::Fraction(r0, r1)) => l0 == r0 && l1 == r1,
            _ => false,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::{TEST_FILES_DIRECTORY, TestInput, test_file_path};
    use crate::ebi_framework::ebi_file_handler::{
        EBI_FILE_HANDLERS, EbiFileHandler,
    };
    use std::{
        fs::{self, File},
        str::FromStr,
    };

    #[test]
    fn file_handler_cli_argument_round_trips() {
        for file_handler in EBI_FILE_HANDLERS {
            let argument =
                TestInput::FileHandler(file_handler.clone()).to_cli_argument();
            match argument {
                //the command line can name the handler: it must read back
                Some(text) => assert_eq!(
                    &EbiFileHandler::from_str(&text).unwrap(),
                    file_handler
                ),
                //otherwise the command line must be unable to name it
                None => assert!(file_handler.validator.is_none()),
            }
        }
    }

    #[test]
    fn test_file_path_uses_forward_slashes() {
        let path = test_file_path("a-b.xes");
        assert_eq!(path.to_str().unwrap(), "./testfiles/a-b.xes");
    }

    #[test]
    fn test_file_paths_open_and_are_portable() {
        for entry in fs::read_dir(TEST_FILES_DIRECTORY).unwrap() {
            let name = entry.unwrap().file_name().into_string().unwrap();
            let path = test_file_path(&name);
            let text = path.to_str().unwrap();

            // The text ends up inside generated string literals and
            // fallible-list keys, so it must not depend on the platform.
            assert!(!text.contains('\\'), "backslash in {text}");
            assert!(File::open(&path).is_ok(), "cannot open {text}");
        }
    }
}
