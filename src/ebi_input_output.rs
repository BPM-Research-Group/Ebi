use std::{collections::{BTreeSet, HashSet}, borrow::Borrow, fmt::{Display, Pointer}, hash::Hash, path::PathBuf};
use anyhow::Result;

use clap::{builder::ValueParser, value_parser};
use strum::IntoEnumIterator;

use crate::{ebi_commands::{ebi_command::{EbiCommand, EBI_COMMANDS}, ebi_command_info::Infoable}, ebi_objects::ebi_object::{EbiObject, EbiObjectType, EbiTraitObject}, ebi_traits::ebi_trait::{EbiTrait, FromEbiTraitObject}, export::Exportable, file_handler::{EbiFileHandler, EBI_FILE_HANDLERS}, math::{fraction::{Fraction, FractionNotParsedYet}, log_div::LogDiv, root::ContainsRoot, root_log_div::RootLogDiv}};

#[derive(PartialEq,Eq)]
pub enum EbiInputType {
    Trait(EbiTrait),
    ObjectType(EbiObjectType),
    AnyObject,
    FileHandler,
    String,
    Usize,
    Fraction,
}

impl EbiInputType {

    pub fn get_article(&self) -> &str {
        match self {
            EbiInputType::Trait(t) => t.get_article(),
            EbiInputType::ObjectType(o) => o.get_article(),
            EbiInputType::AnyObject => "an",
            EbiInputType::String => "a",
            EbiInputType::Usize => "an",
            EbiInputType::FileHandler => "a",
            EbiInputType::Fraction => "a",
        }
    }

    pub fn get_parser_of_list(traits: &[&EbiInputType]) -> ValueParser {
        match traits[0] {
            EbiInputType::Trait(_) => value_parser!(PathBuf),
            EbiInputType::ObjectType(_) => value_parser!(PathBuf),
            EbiInputType::AnyObject => value_parser!(PathBuf),
            EbiInputType::String => value_parser!(String).into(),
            EbiInputType::Usize => value_parser!(usize).into(),
            EbiInputType::FileHandler => value_parser!(EbiFileHandler).into(),
            EbiInputType::Fraction => value_parser!(FractionNotParsedYet).into(),
        }
    }

    pub fn get_possible_inputs(traits: &[&'static EbiInputType]) -> Vec<String> {
        let mut result = HashSet::new();

        for input_type in traits {
            match input_type {
                EbiInputType::Trait(t) => {
                    result.extend(Self::show_file_handlers(t.get_file_handlers()));
                },
                EbiInputType::ObjectType(o) => {
                    result.extend(Self::show_file_handlers(o.get_file_handlers()));
                },
                EbiInputType::AnyObject => {
                    result.extend(Self::show_file_handlers(EBI_FILE_HANDLERS.iter().collect()));
                },
                EbiInputType::String => {result.insert("text".to_string());},
                EbiInputType::Usize => {result.insert("integer".to_string());},
                EbiInputType::FileHandler => {
                    let extensions: Vec<String> = EBI_FILE_HANDLERS.iter().map(|file_type| file_type.file_extension.to_string()).collect();
                    result.insert("the file extension of any file type supported by Ebi (".to_owned() + &extensions.join_with(", ", " or ") + ")");
                },
                EbiInputType::Fraction => {result.insert("fraction".to_string());},
            };
        }

        result.into_iter().collect::<Vec<_>>()
    }

    pub fn possible_inputs_as_strings_with_articles(traits: &[&'static EbiInputType], last_connector: &str) -> String {
        let mut list = Self::get_possible_inputs(traits);
        list.join_with(", ", last_connector)
    }

    pub fn show_file_handlers(file_handlers: Vec<&'static EbiFileHandler>) -> Vec<String> {
        file_handlers.iter().map(|file_handler| format!("{}", file_handler)).collect::<Vec<_>>()
    }
    
    pub fn get_applicable_commands(&self) -> BTreeSet<Vec<&'static EbiCommand>> {
        let mut result = EBI_COMMANDS.get_command_paths();
        result.retain(|path| {
            if let EbiCommand::Command { name_short, name_long, explanation_short, explanation_long, latex_link, cli_command, exact_arithmetic, input_types, input_names, input_helps, execute, output } = path[path.len() - 1] {
                for input_typess in input_types.iter() {
                    for input_typesss in input_typess.iter() {
                        if input_typesss == &self {
                            return true;
                        }
                    }
                }
            }
            false
        });
        result
    }
}

impl Display for EbiInputType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            EbiInputType::Trait(t) => t.fmt(f),
            EbiInputType::ObjectType(o) => o.fmt(f),
            EbiInputType::AnyObject => write!(f, "object"),
            EbiInputType::String => write!(f, "text"),
            EbiInputType::Usize => write!(f, "integer"),
            EbiInputType::FileHandler => write!(f, "file"),
            EbiInputType::Fraction => write!(f, "fraction"),
        }
    }
}

pub enum EbiInput {
    Trait(EbiTraitObject, &'static EbiFileHandler),
    Object(EbiObject, &'static EbiFileHandler),
    String(String),
    Usize(usize),
    FileHandler(EbiFileHandler),
    Fraction(Fraction),
}

impl EbiInput {
    pub fn to_type<T: FromEbiTraitObject + ?Sized>(self) -> Result<Box<T>> {
        FromEbiTraitObject::from_trait_object(self)
    }

    pub fn get_type(&self) -> EbiInputType {
        match self {
            EbiInput::Trait(t, _) => EbiInputType::Trait(t.get_trait()),
            EbiInput::Object(o, _) => EbiInputType::ObjectType(o.get_type()),
            EbiInput::String(_) => EbiInputType::String,
            EbiInput::Usize(_) => EbiInputType::Usize,
            EbiInput::FileHandler(_) => EbiInputType::FileHandler,
            EbiInput::Fraction(_) => EbiInputType::Fraction,
        }
    }
}

pub trait Joiner {
    fn join_with(&self, sep: &str, last_sep: &str) -> String;
}

impl <T> Joiner for &[T] where T: Display {
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

impl <T> Joiner for &Vec<T> where T: Display {
    fn join_with(&self, sep: &str, last_sep: &str) -> String {
        let slice = &self[..];
        slice.join_with(sep, last_sep)
    }
}

impl <T> Joiner for Vec<T> where T: Display {
    fn join_with(&self, sep: &str, last_sep: &str) -> String {
        let slice = &self[..];
        slice.join_with(sep, last_sep)
    }
}