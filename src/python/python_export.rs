use crate::ebi_framework::{
    ebi_command::EbiCommand,
    ebi_output::{self, EbiOutput},
};
use ebi_objects::{
    EbiObject, EventLogPython, NumberOfTraces,
    ebi_arithmetic::{
        Fraction, MaybeExact, fraction::approximate::Approximate, malachite::Natural,
    },
};
use process_mining::core::event_data::case_centric::AttributeValue;
use pyo3::{
    IntoPyObjectExt, Py, PyAny, PyResult, Python,
    exceptions::PyValueError,
    types::{IntoPyDict, PyAnyMethods, PyDict, PyList, PyString},
};

pub trait ExportableToPM4Py {
    /// Exports an Ebi Object as its PM4Py equivalent.
    fn export_to_pm4py(&self, py: Python<'_>) -> PyResult<Py<PyAny>>;
}

impl ExportableToPM4Py for EbiOutput {
    fn export_to_pm4py(&self, py: Python<'_>) -> PyResult<Py<PyAny>> {
        match self {
            EbiOutput::Fraction(frac) => frac.export_to_pm4py(py),
            EbiOutput::Bool(value) => value.export_to_pm4py(py),
            EbiOutput::Usize(value) => value.export_to_pm4py(py),
            EbiOutput::Object(EbiObject::EventLogPython(event_log)) => {
                // special case: export EventLog directly
                event_log.export_to_pm4py(py)
            }
            // default: no exporter available (not implemented yet or no equivalent in PM4Py) -> return string representation
            EbiOutput::Object(_)
            | EbiOutput::String(_)
            | EbiOutput::LogDiv(_)
            | EbiOutput::ContainsRoot(_)
            | EbiOutput::RootLogDiv(_) => {
                let exporter =
                    EbiCommand::select_exporter(&self.get_type(), None, None).map_err(|e| {
                        pyo3::exceptions::PyException::new_err(format!("Export error: {}", e))
                    })?;

                let output_string = if exporter.is_binary() {
                    let bytes =
                        ebi_output::export_to_bytes(self.clone(), exporter).map_err(|e| {
                            pyo3::exceptions::PyException::new_err(format!("Export error: {}", e))
                        })?;
                    String::from_utf8(bytes).map_err(|e| {
                        pyo3::exceptions::PyException::new_err(format!(
                            "UTF8 conversion error: {}",
                            e
                        ))
                    })?
                } else {
                    ebi_output::export_to_string(self.clone(), exporter).map_err(|e| {
                        pyo3::exceptions::PyException::new_err(format!("Export error: {}", e))
                    })?
                };
                let py_str = PyString::new(py, &output_string);
                Ok(py_str.into())
            }
        }
    }
}

impl ExportableToPM4Py for bool {
    fn export_to_pm4py(&self, py: Python<'_>) -> PyResult<Py<PyAny>> {
        Ok(self.into_py_any(py)?)
    }
}

impl ExportableToPM4Py for usize {
    fn export_to_pm4py(&self, py: Python<'_>) -> PyResult<Py<PyAny>> {
        // Convert the Rust `usize` to a Python integer
        Ok(self.into_py_any(py)?)
    }
}

impl ExportableToPM4Py for Fraction {
    fn export_to_pm4py(&self, py: Python<'_>) -> PyResult<Py<PyAny>> {
        if let Ok(rat) = self.exact_ref() {
            // malachite does not offer any direct way to convert Rational to f64

            //create an approximation float
            let approx_float = match rat.clone().approximate() {
                Ok(x) => x,
                Err(e) => {
                    return Err(PyValueError::new_err(format!(
                        "Cannot approximate the fraction {}",
                        e
                    )));
                }
            };
            let py_approx_float = approx_float.into_py_any(py)?;

            //create the exact values
            let numerator = rat.numerator_ref();
            let py_numerator = natural_to_num_biguints(numerator).into_py_any(py)?;

            let denominator = rat.denominator_ref();
            let py_denominator = natural_to_num_biguints(denominator).into_py_any(py)?;

            //create the list
            let py_events = vec![py_approx_float, py_numerator, py_denominator];
            let py_list = PyList::new(py, py_events)?;

            Ok(py_list.into_py_any(py)?)
        } else if let Ok(flo) = self.approx_ref() {
            Ok(flo.into_py_any(py)?)
        } else {
            Err(PyValueError::new_err(
                "Cannot export a Fraction that combines Exact and Approx",
            ))
        }
    }
}

pub fn natural_to_num_biguints(n: &Natural) -> num_bigint::BigUint {
    let n_limbs = n.to_limbs_asc();
    let n_limbs_u32: Vec<u32> = n_limbs
        .iter()
        .flat_map(|u| vec![*u as u32, (u >> 32) as u32])
        .collect();
    num_bigint::BigUint::from_slice(&n_limbs_u32)
}

impl ExportableToPM4Py for EventLogPython {
    fn export_to_pm4py(&self, py: Python<'_>) -> PyResult<Py<PyAny>> {
        let pm4py_module = py.import("pm4py.objects.log.obj")?;
        let py_event_cls = pm4py_module.getattr("Event")?;
        let py_trace_cls = pm4py_module.getattr("Trace")?;
        let py_log_cls = pm4py_module.getattr("EventLog")?;

        // Collect traces
        let mut py_traces = Vec::with_capacity(self.number_of_traces());

        for (_trace_idx, rust_trace) in self.log.rust4pm_log.traces.iter().enumerate() {
            // Collect events
            let mut py_events = Vec::with_capacity(rust_trace.events.len());

            for rust_event in &rust_trace.events {
                let py_dict = PyDict::new(py);

                for attr in &rust_event.attributes {
                    let py_val = match &attr.value {
                        AttributeValue::String(s) => s.into_py_any(py)?,
                        AttributeValue::Int(i) => i.into_py_any(py)?,
                        AttributeValue::Float(f) => f.into_py_any(py)?,
                        AttributeValue::Boolean(b) => b.into_py_any(py)?,
                        AttributeValue::Date(dt) => dt.to_rfc3339().into_py_any(py)?,
                        AttributeValue::ID(id) => id.to_string().into_py_any(py)?,
                        AttributeValue::List(list) => {
                            let py_list = PyList::new(
                                py,
                                list.iter()
                                    .map(|a| a.value.export_to_pm4py(py))
                                    .collect::<Result<Vec<_>, _>>()?,
                            )?;
                            py_list.into_py_any(py)?
                        }
                        AttributeValue::Container(container) => {
                            let py_container = PyDict::new(py);
                            for attr in container.iter() {
                                py_container
                                    .set_item(&attr.key, attr.value.export_to_pm4py(py)?)?;
                            }
                            py_container.into_py_any(py)?
                        }
                        AttributeValue::None() => py.None(),
                    };
                    py_dict.set_item(&attr.key, py_val)?;
                }

                let py_event = py_event_cls.call1((py_dict,))?;
                py_events.push(py_event);
            }

            let py_trace_list = PyList::new(py, py_events)?;

            // Trace attributes
            let py_trace_attrs = PyDict::new(py);
            for attr in &rust_trace.attributes {
                let py_val = match &attr.value {
                    AttributeValue::String(s) => s.into_py_any(py)?,
                    AttributeValue::Int(i) => i.into_py_any(py)?,
                    AttributeValue::Float(f) => f.into_py_any(py)?,
                    AttributeValue::Boolean(b) => b.into_py_any(py)?,
                    AttributeValue::Date(dt) => dt.to_rfc3339().into_py_any(py)?,
                    AttributeValue::ID(id) => id.to_string().into_py_any(py)?,
                    AttributeValue::List(list) => {
                        let py_list = PyList::new(
                            py,
                            list.iter()
                                .map(|a| a.value.export_to_pm4py(py))
                                .collect::<Result<Vec<_>, _>>()?,
                        )?;
                        py_list.into_py_any(py)?
                    }
                    AttributeValue::Container(container) => {
                        let py_container = PyDict::new(py);
                        for attr in container.iter() {
                            py_container.set_item(&attr.key, attr.value.export_to_pm4py(py)?)?;
                        }
                        py_container.into_py_any(py)?
                    }
                    AttributeValue::None() => py.None(),
                };
                py_trace_attrs.set_item(&attr.key, py_val)?;
            }

            let py_trace = py_trace_cls.call(
                (py_trace_list,),
                Some(&[("attributes", py_trace_attrs)].into_py_dict(py)?),
            )?;
            py_traces.push(py_trace);
        }

        let py_log_list = PyList::new(py, py_traces)?;

        // Log attributes
        let py_log_attrs = PyDict::new(py);
        for attr in &self.log.rust4pm_log.attributes {
            let py_val = match &attr.value {
                AttributeValue::String(s) => s.into_py_any(py)?,
                AttributeValue::Int(i) => i.into_py_any(py)?,
                AttributeValue::Float(f) => f.into_py_any(py)?,
                AttributeValue::Boolean(b) => b.into_py_any(py)?,
                AttributeValue::Date(dt) => dt.to_rfc3339().into_py_any(py)?,
                AttributeValue::ID(id) => id.to_string().into_py_any(py)?,
                AttributeValue::List(list) => {
                    let py_list = PyList::new(
                        py,
                        list.iter()
                            .map(|a| a.value.export_to_pm4py(py))
                            .collect::<Result<Vec<_>, _>>()?,
                    )?;
                    py_list.into_py_any(py)?
                }
                AttributeValue::Container(container) => {
                    let py_container = PyDict::new(py);
                    for attr in container.iter() {
                        py_container.set_item(&attr.key, attr.value.export_to_pm4py(py)?)?;
                    }
                    py_container.into_py_any(py)?
                }
                AttributeValue::None() => py.None(),
            };
            py_log_attrs.set_item(&attr.key, py_val)?;
        }

        let py_log = py_log_cls.call(
            (py_log_list,),
            Some(&[("attributes", py_log_attrs)].into_py_dict(py)?),
        )?;

        Ok(py_log.into())
    }
}

impl ExportableToPM4Py for AttributeValue {
    fn export_to_pm4py(&self, py: Python<'_>) -> PyResult<Py<PyAny>> {
        match self {
            AttributeValue::String(s) => Ok(s.into_py_any(py)?),
            AttributeValue::Date(dt) => Ok(dt.to_rfc3339().into_py_any(py)?), // export as ISO string
            AttributeValue::Int(i) => Ok(i.into_py_any(py)?),
            AttributeValue::Float(f) => Ok(f.into_py_any(py)?),
            AttributeValue::Boolean(b) => Ok(b.into_py_any(py)?),
            AttributeValue::ID(uuid) => Ok(uuid.to_string().into_py_any(py)?), // export as string
            AttributeValue::List(attrs) => {
                let py_list = PyList::new(
                    py,
                    attrs
                        .iter()
                        .map(|a| a.value.export_to_pm4py(py))
                        .collect::<PyResult<Vec<_>>>()?,
                )?;
                Ok(py_list.into_py_any(py)?)
            }
            AttributeValue::Container(attrs) => {
                let py_dict = PyDict::new(py);
                for attr in attrs {
                    py_dict.set_item(&attr.key, attr.value.export_to_pm4py(py)?)?;
                }
                Ok(py_dict.into_py_any(py)?)
            }
            AttributeValue::None() => Ok(py.None()),
        }
    }
}
