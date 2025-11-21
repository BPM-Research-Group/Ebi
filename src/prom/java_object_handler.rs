use std::{collections::HashSet, fmt::Display};

use ebi_objects::EbiObjectType;

use crate::ebi_framework::{
    ebi_file_handler::{EBI_FILE_HANDLERS, EbiFileHandler, get_file_handlers},
    ebi_input::EbiInputType,
    ebi_output::EbiOutputType,
    ebi_trait::EbiTrait,
};

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub struct JavaObjectHandler {
    pub name: &'static str, //name to be used in java function names to indicate the Ebi object that is being handled. Must be unique. Must not contain spaces.
    pub java_class: &'static str, //The full path of the java class to/from which the Ebi object is to be translated
    pub translator_ebi_to_java: Option<&'static str>, //Full path of the java function that translates from a String returned by Ebi to the given java class
    pub translator_java_to_ebi: Option<&'static str>, //Full paht of the java function that translates from the java class to a String that can be read by Ebi
    pub input_gui: Option<&'static str>, //If not none, the given function will be called to create a gui for the user to input a value.
}

impl Display for JavaObjectHandler {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "{}", self.name)
    }
}

pub trait JavaObjectHandlerQueryExport {
    fn get_java_object_handlers_that_can_export(&self) -> HashSet<JavaObjectHandler>;
}

pub trait JavaObjectHandlerQueryImport {
    fn get_java_object_handlers_that_can_import(&self) -> HashSet<JavaObjectHandler>;
}

impl JavaObjectHandlerQueryExport for EbiOutputType {
    fn get_java_object_handlers_that_can_export(&self) -> HashSet<JavaObjectHandler> {
        let mut result = HashSet::new();
        for exporter in self.get_exporters() {
            for java_object_handler in exporter.get_java_object_handlers() {
                if java_object_handler.translator_ebi_to_java.is_some() {
                    result.insert(java_object_handler.to_owned());
                }
            }
        }
        result
    }
}

impl JavaObjectHandlerQueryImport for EbiInputType {
    fn get_java_object_handlers_that_can_import(&self) -> HashSet<JavaObjectHandler> {
        let mut result = HashSet::new();
        for java_object_handler in self.get_java_object_handlers() {
            if java_object_handler.translator_java_to_ebi.is_some() {
                result.insert(java_object_handler.to_owned());
            }
        }
        result
    }
}

impl JavaObjectHandlerQueryExport for EbiObjectType {
    fn get_java_object_handlers_that_can_export(&self) -> HashSet<JavaObjectHandler> {
        EbiOutputType::ObjectType(self.clone()).get_java_object_handlers_that_can_export()
    }
}

impl JavaObjectHandlerQueryImport for EbiObjectType {
    fn get_java_object_handlers_that_can_import(&self) -> HashSet<JavaObjectHandler> {
        EbiInputType::Object(self.clone()).get_java_object_handlers_that_can_import()
    }
}

impl JavaObjectHandlerQueryImport for EbiTrait {
    fn get_java_object_handlers_that_can_import(&self) -> HashSet<JavaObjectHandler> {
        EbiInputType::Trait(self.clone()).get_java_object_handlers_that_can_import()
    }
}

pub trait JavaInputTypes {
    fn get_java_object_handlers(&self) -> Vec<&JavaObjectHandler>;
}

impl JavaInputTypes for EbiInputType {
    fn get_java_object_handlers(&self) -> Vec<&JavaObjectHandler> {
        match self {
            EbiInputType::Trait(t) => get_file_handlers_java(t.get_file_handlers()),
            EbiInputType::Object(o) => get_file_handlers_java(get_file_handlers(o)),
            EbiInputType::AnyObject => get_file_handlers_java(EBI_FILE_HANDLERS.iter().collect()),
            EbiInputType::String(_, _) => {
                let mut x = vec![];
                x.extend(JAVA_OBJECT_HANDLERS_STRING);
                x
            }
            EbiInputType::Usize(_, _, _) => {
                let mut x = vec![];
                x.extend(JAVA_OBJECT_HANDLERS_USIZE);
                x
            }
            EbiInputType::FileHandler => {
                //not supported in Java;
                vec![]
            }
            EbiInputType::Fraction(_, _, _) => {
                let mut x = vec![];
                x.extend(JAVA_OBJECT_HANDLERS_FRACTION);
                x
            }
        }
    }
}

pub fn get_possible_inputs_with_java(traits: &[&EbiInputType]) -> Vec<JavaObjectHandler> {
    let mut result = HashSet::new();

    for input_type in traits {
        result.extend(input_type.get_java_object_handlers());
    }

    result = result
        .into_iter()
        .filter(|java_object_handler| java_object_handler.translator_java_to_ebi.is_some())
        .collect();

    result.into_iter().cloned().collect::<Vec<_>>()
}

pub fn get_file_handlers_java(
    file_handlers: Vec<&'static EbiFileHandler>,
) -> Vec<&'static JavaObjectHandler> {
    file_handlers.iter().fold(vec![], |mut list, file_handler| {
        list.extend(file_handler.java_object_handlers);
        list
    })
}

pub const JAVA_OBJECT_HANDLERS_STRING: &[JavaObjectHandler] = &[JavaObjectHandler {
    name: "string",
    java_class: "String",
    translator_ebi_to_java: Some("org.processmining.ebi.objects.EbiString.fromEbiString"),
    translator_java_to_ebi: Some("org.processmining.ebi.objects.EbiString.toEbiString"),
    input_gui: Some("org.processmining.ebi.objects.EbiString.create_input_panel"),
}];
pub const JAVA_OBJECT_HANDLERS_USIZE: &[JavaObjectHandler] = &[JavaObjectHandler {
    name: "usize",
    java_class: "Integer",
    translator_ebi_to_java: Some("org.processmining.ebi.objects.EbiInteger.fromEbiString"),
    translator_java_to_ebi: Some("org.processmining.ebi.objects.EbiInteger.toEbiString"),
    input_gui: Some("org.processmining.ebi.objects.EbiInteger.create_input_panel"),
}];
pub const JAVA_OBJECT_HANDLERS_FRACTION: &[JavaObjectHandler] = &[JavaObjectHandler {
    name: "fraction",
    java_class: "org.apache.commons.math3.fraction.BigFraction",
    translator_ebi_to_java: Some("org.processmining.ebi.objects.EbiFraction.fromEbiString"),
    translator_java_to_ebi: Some("org.processmining.ebi.objects.EbiFraction.toEbiString"),
    input_gui: Some("org.processmining.ebi.objects.EbiFraction.create_input_panel"),
}];
pub const JAVA_OBJECT_HANDLERS_LOGDIV: &[JavaObjectHandler] = &[JavaObjectHandler {
    name: "logdiv",
    java_class: "org.processmining.ebi.objects.EbiLogDiv",
    translator_ebi_to_java: Some("org.processmining.ebi.objects.EbiLogDiv.fromEbiString"),
    translator_java_to_ebi: None,
    input_gui: None,
}];
pub const JAVA_OBJECT_HANDLERS_CONTAINSROOT: &[JavaObjectHandler] = &[JavaObjectHandler {
    name: "containsroot_html",
    java_class: "org.processmining.framework.util.HTMLToString",
    translator_ebi_to_java: Some("org.processmining.ebi.objects.EbiContainsRoot.fromEbiString"),
    translator_java_to_ebi: None,
    input_gui: None,
}];
pub const JAVA_OBJECT_HANDLERS_ROOTLOGDIV: &[JavaObjectHandler] = &[JavaObjectHandler {
    name: "rootlogdiv",
    java_class: "org.processmining.framework.util.HTMLToString",
    translator_ebi_to_java: Some("org.processmining.ebi.objects.EbiRootLogDiv.fromEbiString"),
    translator_java_to_ebi: None,
    input_gui: None,
}];
pub const JAVA_OBJECT_HANDLERS_BOOL: &[JavaObjectHandler] = &[JavaObjectHandler {
    name: "boolean",
    java_class: "java.lang.Boolean",
    translator_ebi_to_java: Some("org.processmining.ebi.objects.EbiBoolean.fromEbiString"),
    translator_java_to_ebi: Some("org.processmining.ebi.objects.EbiBoolean.toEbiString"),
    input_gui: None,
}];

#[cfg(test)]
mod test {
    use crate::{
        ebi_framework::ebi_input::EbiInputType,
        prom::java_object_handler::{JavaInputTypes, get_possible_inputs_with_java},
    };
    use strum::IntoEnumIterator;

    #[test]
    fn input_types() {
        for input_type in EbiInputType::iter() {
            let list = vec![&input_type];
            input_type.get_article();
            EbiInputType::get_parser_of_list(&list);
            input_type.get_java_object_handlers();
            input_type.get_applicable_commands();
            input_type.to_string();
            EbiInputType::get_possible_inputs(&list);
            EbiInputType::get_possible_inputs_with_latex(&list);
            get_possible_inputs_with_java(&list);
            EbiInputType::possible_inputs_as_strings_with_articles(&list, "xyz");
        }
    }
}
