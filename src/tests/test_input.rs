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
        }
    }

    // #[cfg(test)]
    // pub(crate) fn get_input_type(&self) -> Option<&'static EbiInputType> {
    //     match self {
    //         TestInput::Trait(_, _) => None,
    //         TestInput::Object(_, _, _) => None,
    //         TestInput::String(_, ebi_input_type) => Some(ebi_input_type),
    //         TestInput::Usize(_, ebi_input_type) => Some(ebi_input_type),
    //         TestInput::FileHandler(_) => None,
    //         TestInput::Fraction(_, ebi_input_type) => Some(ebi_input_type),
    //     }
    // }

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
        }
    }

    // pub fn from_unique_string(string: &str, input_types: &[&'static EbiInputType]) -> Result<Self> {
    //     let (variant, value) = string
    //         .split_once(' ')
    //         .ok_or_else(|| anyhow!("not a valid testinput {}", string))?;
    //     match variant {
    //         "trait" => {
    //             let (trait_name, path) = value
    //                 .split_once('#')
    //                 .ok_or_else(|| anyhow!("not a valid testinput {}", string))?;
    //             let path_buf = path.parse::<PathBuf>()?;
    //             let ebi_trait = trait_name.parse::<EbiTrait>()?;
    //             Ok(Self::Trait(ebi_trait, path_buf))
    //         }
    //         "object" => {
    //             let (_object_name, path) = value
    //                 .split_once('#')
    //                 .ok_or_else(|| anyhow!("not a valid testinput {}", string))?;
    //             let path_buf = path.parse::<PathBuf>()?;
    //             let reader = MultipleReader::File(File::open(path)?);
    //             if let Ok(EbiInput::Object(ebi_object, file_handler)) =
    //                 attempt_parse(input_types, reader)
    //             {
    //                 Ok(Self::Object(ebi_object, file_handler, path_buf))
    //             } else {
    //                 Err(anyhow!("not a valid testinput {}", string))
    //             }
    //         }
    //         "string" => {
    //             for input_type in input_types {
    //                 if let EbiInputType::String(allowed_values, _) = input_type {
    //                     if let Some(allowed_values) = allowed_values
    //                         && allowed_values.contains(&value)
    //                     {
    //                         return Ok(TestInput::String(value.to_string(), input_type));
    //                     } else if allowed_values.is_none() {
    //                         return Ok(TestInput::String(value.to_string(), input_type));
    //                     }
    //                 }
    //             }
    //             Err(anyhow!("not a valid testinput {}", string))
    //         }
    //         "usize" => {
    //             let reader = MultipleReader::String(value.to_string());
    //             if let Ok(EbiInput::Usize(val, file_handler)) = attempt_parse(input_types, reader) {
    //                 Ok(Self::Usize(val, file_handler))
    //             } else {
    //                 Err(anyhow!("not a valid testinput {}", string))
    //             }
    //         }
    //         "filehandler" => {
    //             let reader = MultipleReader::String(value.to_string());
    //             if let Ok(EbiInput::FileHandler(file_handler)) = attempt_parse(input_types, reader)
    //             {
    //                 Ok(Self::FileHandler(file_handler))
    //             } else {
    //                 Err(anyhow!("not a valid testinput {}", string))
    //             }
    //         }
    //         "fraction" => {
    //             let reader = MultipleReader::String(value.to_string());
    //             if let Ok(EbiInput::Fraction(val, file_handler)) =
    //                 attempt_parse(input_types, reader)
    //             {
    //                 Ok(Self::Fraction(val, file_handler))
    //             } else {
    //                 Err(anyhow!("not a valid testinput {}", string))
    //             }
    //         }
    //         _ => Err(anyhow!("not a valid testinput {}", string)),
    //     }
    // }

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
                            let mut reader =
                                MultipleReader::from_file(File::open(file.path()).unwrap());

                            match input_type {
                                EbiInputType::Trait(etrait) => {
                                    //try to parse a trait

                                    match ebi_input::read_as_trait(etrait, &mut reader, None, 0) {
                                        Ok((_, _)) => {
                                            result.push(TestInput::Trait(*etrait, file.path()));
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
                                                file.path(),
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
                                                file.path(),
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

impl Debug for TestInput {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Trait(arg0, arg1) => f.debug_tuple("Trait").field(arg0).field(arg1).finish(),
            Self::Object(_, _, arg2) => f.debug_tuple("Object").field(arg2).finish(),
            Self::String(arg0, _) => f.debug_tuple("String").field(arg0).finish(),
            Self::Usize(arg0, _) => f.debug_tuple("Usize").field(arg0).finish(),
            Self::FileHandler(arg0) => f.debug_tuple("FileHandler").field(&arg0.name).finish(),
            Self::Fraction(arg0, _) => f.debug_tuple("Fraction").field(arg0).finish(),
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
