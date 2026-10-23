use crate::{
    ebi_framework::{
        ebi_command::EbiCommand,
        ebi_file_handler::{EbiFileHandler, get_file_handlers},
        ebi_input::{EbiInput, EbiInputType, attempt_parse, read_as_any_object},
        ebi_output::{self, EbiOutput},
        ebi_trait::EbiTrait,
        ebi_trait_object::EbiTraitObject,
        trait_importers::ToQueriableStochasticLanguageTrait,
    },
    multiple_reader::MultipleReader,
    python::python_import::ImportableFromPM4Py,
};
use ebi_objects::{
    EbiObject, EventLogPython,
    anyhow::{Result, anyhow},
    ebi_arithmetic::{exact::set_exact_globally, is_exact_globally},
};
use pyo3::{exceptions::PyValueError, prelude::*};

/// A model (or language) that has been parsed once and can be passed to any function in place of its text.
#[pyclass(name = "Model")]
pub struct Model {
    object: EbiObject,
    /// The arithmetic mode in which the object was parsed; it can only be used directly in the same mode.
    exact: bool,
}

#[pymethods]
impl Model {
    #[staticmethod]
    fn from_text(text: &str) -> PyResult<Self> {
        set_exact_globally(true);
        let mut reader = MultipleReader::String(text.to_string());
        let (object, _) = read_as_any_object(&mut reader, None, 0)
            .map_err(|e| PyValueError::new_err(format!("Could not read a model: {e}")))?;
        Ok(Model { object, exact: true })
    }

    fn to_text(&self) -> PyResult<String> {
        object_to_text(&self.object).map_err(|e| PyValueError::new_err(e.to_string()))
    }

    #[getter]
    fn object_type(&self) -> String {
        self.object.get_type().to_string()
    }
}

impl Model {
    pub(crate) fn into_input(&self, wanted: &[&'static EbiInputType]) -> PyResult<EbiInput> {
        object_into_input(self.object.clone(), wanted, self.exact == is_exact_globally())
            .map(|(input, _)| input)
            .map_err(|e| PyValueError::new_err(e.to_string()))
    }
}

/// A PM4Py event log that has been converted once and can be passed to any function in place of the PM4Py log.
#[pyclass(name = "Log")]
pub struct Log {
    log: EventLogPython,
}

#[pymethods]
impl Log {
    #[staticmethod]
    fn from_pm4py(log: &Bound<'_, PyAny>) -> PyResult<Self> {
        let log = EventLogPython::py_extract_object(log)
            .map_err(|e| PyValueError::new_err(format!("Could not read a PM4Py event log: {e}")))?;
        Ok(Log { log })
    }
}

impl Log {
    pub(crate) fn into_input(&self, wanted: &[&'static EbiInputType]) -> PyResult<EbiInput> {
        self.log.clone().py_object_to_ebi_input(wanted).ok_or_else(|| {
            PyValueError::new_err(format!(
                "an event log cannot be used here; expected {}",
                EbiInputType::get_possible_inputs(wanted).join(", ")
            ))
        })
    }
}

/// How a held object was turned into a command input.
#[derive(Debug, PartialEq, Eq)]
pub(crate) enum Conversion {
    /// The held value was converted directly.
    Direct,
    /// The held value was written as text and parsed again, as for a text argument.
    ViaText,
}

/// Turn a held object into the input a command asks for.
/// `allow_direct` is false when the held value cannot be used as is, for example because it was parsed in another arithmetic mode.
pub(crate) fn object_into_input(
    object: EbiObject,
    wanted: &[&'static EbiInputType],
    allow_direct: bool,
) -> Result<(EbiInput, Conversion)> {
    if allow_direct {
        for &input_type in wanted {
            match input_type {
                EbiInputType::Object(object_type) if &object.get_type() == object_type => {
                    let file_handler = file_handler_of(&object)?;
                    return Ok((EbiInput::Object(object, file_handler), Conversion::Direct));
                }
                EbiInputType::AnyObject => {
                    let file_handler = file_handler_of(&object)?;
                    return Ok((EbiInput::Object(object, file_handler), Conversion::Direct));
                }
                EbiInputType::Trait(EbiTrait::QueriableStochasticLanguage) => {
                    if let Some(trait_object) = queriable_stochastic_language(&object) {
                        let file_handler = file_handler_of(&object)?;
                        return Ok((EbiInput::Trait(trait_object, file_handler), Conversion::Direct));
                    }
                }
                _ => {}
            }
        }
    }

    //only an object or a trait can be read from the text of an object; a list of texts would accept any text
    let wants_an_object = wanted.iter().any(|input_type| {
        matches!(
            input_type,
            EbiInputType::Object(_) | EbiInputType::AnyObject | EbiInputType::Trait(_)
        )
    });
    if !wants_an_object {
        return Err(anyhow!(
            "a {} cannot be used here; expected {}",
            object.get_type(),
            EbiInputType::get_possible_inputs(wanted).join(", ")
        ));
    }

    let input = attempt_parse(wanted, MultipleReader::String(object_to_text(&object)?))?;
    Ok((input, Conversion::ViaText))
}

/// The value-based counterpart of the trait importers, for the traits that have been added so far.
fn queriable_stochastic_language(object: &EbiObject) -> Option<EbiTraitObject> {
    Some(match object {
        EbiObject::StochasticLabelledPetriNet(o) => o.clone().to_queriable_stochastic_language_ebi_trait_object(),
        EbiObject::StochasticProcessTree(o) => o.clone().to_queriable_stochastic_language_ebi_trait_object(),
        EbiObject::StochasticNondeterministicFiniteAutomaton(o) => o.clone().to_queriable_stochastic_language_ebi_trait_object(),
        EbiObject::StochasticDeterministicFiniteAutomaton(o) => o.clone().to_queriable_stochastic_language_ebi_trait_object(),
        EbiObject::FiniteStochasticLanguage(o) => o.clone().to_queriable_stochastic_language_ebi_trait_object(),
        EbiObject::DirectlyFollowsGraph(o) => o.clone().to_queriable_stochastic_language_ebi_trait_object(),
        EbiObject::StochasticDirectlyFollowsModel(o) => o.clone().to_queriable_stochastic_language_ebi_trait_object(),
        _ => return None,
    })
}

fn file_handler_of(object: &EbiObject) -> Result<&'static EbiFileHandler> {
    get_file_handlers(&object.get_type())
        .into_iter()
        .next()
        .ok_or_else(|| anyhow!("no file type for {}", object.get_type()))
}

fn object_to_text(object: &EbiObject) -> Result<String> {
    let output = EbiOutput::Object(object.clone());
    let exporter = EbiCommand::select_exporter(&output.get_type(), None, None)?;
    if exporter.is_binary() {
        return Err(anyhow!("{} cannot be written as text", object.get_type()));
    }
    ebi_output::export_to_string(output, &exporter)
}

#[cfg(test)]
mod tests {
    use super::{Conversion, object_into_input};
    use crate::{
        ebi_framework::{
            ebi_input::{EbiInput, EbiInputType, read_as_any_object},
            ebi_trait::EbiTrait,
        },
        multiple_reader::MultipleReader,
    };
    use ebi_objects::{EbiObject, EbiObjectType};
    use std::fs::File;

    fn slpn() -> EbiObject {
        let mut reader =
            MultipleReader::from_file(File::open("testfiles/aa-ab-ba_occ.slpn").unwrap());
        read_as_any_object(&mut reader, None, 0).unwrap().0
    }

    #[test]
    fn an_object_of_the_wanted_type_is_passed_as_it_is() {
        let wanted: &[&'static EbiInputType] =
            &[&EbiInputType::Object(EbiObjectType::StochasticLabelledPetriNet)];
        let (input, conversion) = object_into_input(slpn(), wanted, true).unwrap();
        assert_eq!(conversion, Conversion::Direct);
        assert!(matches!(input, EbiInput::Object(EbiObject::StochasticLabelledPetriNet(_), _)));
    }

    #[test]
    fn a_stochastic_net_is_converted_directly_to_a_queriable_stochastic_language() {
        let wanted: &[&'static EbiInputType] =
            &[&EbiInputType::Trait(EbiTrait::QueriableStochasticLanguage)];
        let (input, conversion) = object_into_input(slpn(), wanted, true).unwrap();
        assert_eq!(conversion, Conversion::Direct);
        assert_eq!(input.get_type(), *wanted[0]);
    }

    #[test]
    fn a_trait_without_a_direct_conversion_goes_through_text() {
        let wanted: &[&'static EbiInputType] =
            &[&EbiInputType::Trait(EbiTrait::StochasticSemantics)];
        let (input, conversion) = object_into_input(slpn(), wanted, true).unwrap();
        assert_eq!(conversion, Conversion::ViaText);
        assert_eq!(input.get_type(), *wanted[0]);
    }

    #[test]
    fn the_direct_conversion_is_not_used_when_it_is_not_allowed() {
        let wanted: &[&'static EbiInputType] =
            &[&EbiInputType::Trait(EbiTrait::QueriableStochasticLanguage)];
        let (input, conversion) = object_into_input(slpn(), wanted, false).unwrap();
        assert_eq!(conversion, Conversion::ViaText);
        assert_eq!(input.get_type(), *wanted[0]);
    }

    #[test]
    fn an_object_that_cannot_be_used_is_an_error() {
        let wanted: &[&'static EbiInputType] = &[&EbiInputType::Usize(None, None, None)];
        assert!(object_into_input(slpn(), wanted, true).is_err());
    }

    #[test]
    fn an_object_is_not_taken_for_a_list_of_texts() {
        let wanted: &[&'static EbiInputType] = &[&EbiInputType::StringList];
        assert!(object_into_input(slpn(), wanted, true).is_err());
    }
}
