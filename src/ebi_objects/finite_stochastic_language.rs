use anyhow::{anyhow, Context, Error, Result};
use std::{
    collections::{hash_map::Entry, HashMap},
    fmt,
    io::{self, BufRead, Write},
    str::FromStr,
};

use crate::{
    ebi_framework::{
        activity_key::{Activity, ActivityKey, ActivityKeyTranslator},
        ebi_file_handler::EbiFileHandler,
        ebi_input::{self, EbiInput, EbiObjectImporter, EbiTraitImporter},
        ebi_object::EbiObject,
        ebi_output::{EbiObjectExporter, EbiOutput},
        ebi_trait::FromEbiTraitObject,
        exportable::Exportable,
        importable::Importable,
        infoable::Infoable,
    },
    ebi_traits::{
        ebi_trait_event_log::IndexTrace,
        ebi_trait_finite_language::EbiTraitFiniteLanguage,
        ebi_trait_finite_stochastic_language::{self, EbiTraitFiniteStochasticLanguage},
        ebi_trait_iterable_language::{self, EbiTraitIterableLanguage},
        ebi_trait_iterable_stochastic_language::{self, EbiTraitIterableStochasticLanguage},
        ebi_trait_queriable_stochastic_language::{self, EbiTraitQueriableStochasticLanguage},
        ebi_trait_semantics::{EbiTraitSemantics, ToSemantics},
        ebi_trait_stochastic_deterministic_semantics::{
            EbiTraitStochasticDeterministicSemantics, ToStochasticDeterministicSemantics,
        },
        ebi_trait_stochastic_semantics::{EbiTraitStochasticSemantics, ToStochasticSemantics},
    },
    follower_semantics::FollowerSemantics,
    line_reader::LineReader,
    math::fraction::Fraction,
};

use super::{
    finite_language::FiniteLanguage,
    finite_stochastic_language_semantics::FiniteStochasticLanguageSemantics,
    stochastic_deterministic_finite_automaton::StochasticDeterministicFiniteAutomaton,
};

pub const HEADER: &str = "finite stochastic language";

pub const FORMAT_SPECIFICATION: &str = "A finite language is a line-based structure. Lines starting with a \\# are ignored.
    This first line is exactly `finite stochastic language'.
    The second line is the number of traces in the language.
    For each trace, the first line is the probability of the trace as a positive fraction or a decimal value.
    The second line contains the number of events in the trace.
    Then, each subsequent line contains the activity name of one event.

    The sum of the probabilities of the traces in the language needs to be $\\leq$ 1.
    
    For instance:
    \\lstinputlisting[language=ebilines, style=boxed]{../testfiles/aa-ab-ba.slang}";

pub const EBI_FINITE_STOCHASTIC_LANGUAGE: EbiFileHandler = EbiFileHandler {
    name: "finite stochastic language",
    article: "a",
    file_extension: "slang",
    format_specification: &FORMAT_SPECIFICATION,
    validator: ebi_input::validate::<FiniteStochasticLanguage>,
    trait_importers: &[
        EbiTraitImporter::IterableLanguage(
            ebi_trait_iterable_language::import::<FiniteStochasticLanguage>,
        ),
        EbiTraitImporter::FiniteLanguage(FiniteStochasticLanguage::import_as_finite_language),
        EbiTraitImporter::FiniteStochasticLanguage(
            ebi_trait_finite_stochastic_language::import::<FiniteStochasticLanguage>,
        ),
        EbiTraitImporter::QueriableStochasticLanguage(
            ebi_trait_queriable_stochastic_language::import::<FiniteStochasticLanguage>,
        ),
        EbiTraitImporter::IterableStochasticLanguage(
            ebi_trait_iterable_stochastic_language::import::<FiniteStochasticLanguage>,
        ),
        EbiTraitImporter::StochasticSemantics(
            FiniteStochasticLanguage::import_as_stochastic_semantics,
        ),
        EbiTraitImporter::StochasticDeterministicSemantics(
            FiniteStochasticLanguage::import_as_stochastic_deterministic_semantics,
        ),
        EbiTraitImporter::Semantics(FiniteStochasticLanguage::import_as_semantics),
    ],
    object_importers: &[
        EbiObjectImporter::FiniteStochasticLanguage(FiniteStochasticLanguage::import_as_object),
        EbiObjectImporter::StochasticDeterministicFiniteAutomaton(
            FiniteStochasticLanguage::import_as_stochastic_deterministic_finite_automaton,
        ),
    ],
    object_exporters: &[
        EbiObjectExporter::FiniteStochasticLanguage(FiniteStochasticLanguage::export_from_object),
        EbiObjectExporter::EventLog(FiniteStochasticLanguage::export_from_event_log),
    ],
    java_object_handlers: &[],
};

#[derive(Clone, Debug, ActivityKey)]
pub struct FiniteStochasticLanguage {
    activity_key: ActivityKey,
    traces: HashMap<Vec<Activity>, Fraction>,
}

impl FiniteStochasticLanguage {
    /**
     * Does not normalise the distribution.
     */
    pub fn new_raw(traces: HashMap<Vec<Activity>, Fraction>, activity_key: ActivityKey) -> Self {
        Self {
            activity_key: activity_key,
            traces: traces,
        }
    }

    pub fn import_as_finite_language(
        reader: &mut dyn BufRead,
    ) -> Result<Box<dyn EbiTraitFiniteLanguage>> {
        let lang = Self::import(reader)?;
        Ok(Box::new(lang.to_finite_language()))
    }

    pub fn normalise_before(traces: &mut HashMap<Vec<String>, Fraction>) {
        if traces.len() != 0 {
            let sum = traces.values().fold(Fraction::zero(), |mut x, y| {
                x += y;
                x
            });
            log::info!("the extracted traces cover a sum of {}", sum);
            traces.retain(|_, v| {
                *v /= &sum;
                true
            });
        }
    }

    pub fn normalise(&mut self) {
        if self.len() != 0 {
            let sum = self.get_probability_sum();
            log::info!("the extracted traces cover a sum of {}", sum);
            self.traces.retain(|_, v| {
                *v /= &sum;
                true
            });
        }
    }

    pub fn get_stochastic_deterministic_finite_automaton(
        &self,
    ) -> StochasticDeterministicFiniteAutomaton {
        log::info!("convert finite stochastic language to sdfa");

        let mut result = StochasticDeterministicFiniteAutomaton::new();
        let mut final_states = HashMap::new();
        result.set_activity_key(&self.activity_key);

        //create automaton
        for (trace, probability) in &self.traces {
            let mut state = result.get_initial_state();
            for activity in trace {
                state = result.take_or_add_transition(state, *activity, probability.clone());
            }

            match final_states.entry(state) {
                Entry::Occupied(mut e) => *e.get_mut() += Fraction::one(),
                Entry::Vacant(e) => {
                    e.insert(Fraction::one());
                }
            }
        }

        //count
        let mut sums = final_states;
        for (source, _, _, probability) in &result {
            match sums.entry(*source) {
                Entry::Occupied(mut e) => *e.get_mut() += probability,
                Entry::Vacant(e) => {
                    e.insert(probability.clone());
                }
            }
        }

        //normalise
        result.scale_outgoing_probabilities(sums);

        result
    }

    pub fn to_finite_language(self) -> FiniteLanguage {
        log::info!("create finite language");

        let mut map = FiniteLanguage::new_hashmap();
        for (trace, _) in self.traces {
            map.insert(trace);
        }

        FiniteLanguage::from((self.activity_key, map))
    }

    fn contains(&self, atrace_b: Vec<&str>, probability_b: &Fraction) -> bool {
        for trace_a in self.traces.iter() {
            let atrace_a = self.activity_key.deprocess_trace(&trace_a.0);

            if atrace_a == atrace_b && trace_a.1 == probability_b {
                return true;
            }
        }
        return false;
    }

    fn export_from_event_log(object: EbiOutput, f: &mut dyn Write) -> Result<()> {
        match object {
            EbiOutput::Object(EbiObject::EventLog(log)) => {
                log.get_finite_stochastic_language().export(f)
            }
            _ => unreachable!(),
        }
    }

    pub fn import_as_stochastic_deterministic_finite_automaton(
        reader: &mut dyn BufRead,
    ) -> Result<EbiObject> {
        let slang = Self::import(reader)?;
        Ok(EbiObject::StochasticDeterministicFiniteAutomaton(
            slang.get_stochastic_deterministic_finite_automaton(),
        ))
    }
}

impl FromEbiTraitObject for FiniteStochasticLanguage {
    fn from_trait_object(object: EbiInput) -> Result<Box<Self>> {
        match object {
            EbiInput::Object(EbiObject::FiniteStochasticLanguage(e), _) => Ok(Box::new(e)),
            _ => Err(anyhow!(
                "cannot read {} {} as a finite language",
                object.get_type().get_article(),
                object.get_type()
            )),
        }
    }
}

impl EbiTraitIterableLanguage for FiniteStochasticLanguage {
    fn iter(&self) -> Box<dyn Iterator<Item = &Vec<Activity>> + '_> {
        Box::new(self.traces.keys())
    }
}

impl EbiTraitFiniteLanguage for FiniteStochasticLanguage {}

impl EbiTraitFiniteStochasticLanguage for FiniteStochasticLanguage {
    fn get_trace_probability(&self, trace_index: usize) -> Option<&Fraction> {
        Some(self.traces.iter().nth(trace_index)?.1)
    }

    fn to_finite_stochastic_language(&self) -> FiniteStochasticLanguage {
        self.clone()
    }

    fn translate(&mut self, target_activity_key: &mut ActivityKey) {
        // Create a translator that maps activities from the current activity key to the target one
        let translator = ActivityKeyTranslator::new(&self.activity_key, target_activity_key);

        // Iterate over all the traces in the language
        let translated_traces: HashMap<Vec<Activity>, Fraction> = self
            .traces
            .drain() // `drain` is used to take ownership of the original traces (use `into_iter()` or `drain()` if we want to consume)
            .map(|(trace, fraction)| {
                // Translate each trace using the translator
                let translated_trace = translator.translate_trace(&trace);

                // Return the translated trace with its associated fraction
                (translated_trace, fraction)
            })
            .collect();

        // Update the traces in the language with the translated ones
        self.traces = translated_traces;
    }

    fn get_probability_sum(&self) -> Fraction {
        self.traces.values().fold(Fraction::zero(), |mut x, y| {
            x += y;
            x
        })
    }
}

impl Eq for FiniteStochasticLanguage {}

impl PartialEq for FiniteStochasticLanguage {
    fn eq(&self, other: &Self) -> bool {
        if self.traces.len() != other.traces.len() {
            return false;
        }
        for trace_b in other.traces.iter() {
            let atrace_b = other.activity_key.deprocess_trace(&trace_b.0);
            if !self.contains(atrace_b, trace_b.1) {
                return false;
            }
        }

        return true;
    }
}

impl IndexTrace for FiniteStochasticLanguage {
    fn len(&self) -> usize {
        self.traces.len()
    }

    fn get_trace(&self, trace_index: usize) -> Option<&Vec<Activity>> {
        self.traces.keys().nth(trace_index)
    }
}

impl From<HashMap<Vec<String>, Fraction>> for FiniteStochasticLanguage {
    /**
     * Normalises the distribution. Use new_raw to avoid normalisation.
     */
    fn from(mut value: HashMap<Vec<String>, Fraction>) -> Self {
        Self::normalise_before(&mut value);
        let mut activity_key = ActivityKey::new();
        let a_traces = value
            .into_iter()
            .map(|(trace, probability)| (activity_key.process_trace(&trace), probability))
            .collect();
        Self {
            activity_key: activity_key,
            traces: a_traces,
        }
    }
}

impl From<(HashMap<Vec<Activity>, Fraction>, ActivityKey)> for FiniteStochasticLanguage {
    /**
     * Normalises the distribution. Use new_raw to avoid normalisation.
     */
    fn from(value: (HashMap<Vec<Activity>, Fraction>, ActivityKey)) -> Self {
        let mut result = Self {
            activity_key: value.1,
            traces: value.0,
        };
        result.normalise();
        result
    }
}

impl FromStr for FiniteStochasticLanguage {
    type Err = Error;

    fn from_str(s: &str) -> std::prelude::v1::Result<Self, Self::Err> {
        let mut reader = io::Cursor::new(s);
        Self::import(&mut reader)
    }
}

impl Importable for FiniteStochasticLanguage {
    fn import_as_object(reader: &mut dyn BufRead) -> Result<EbiObject> {
        Ok(EbiObject::FiniteStochasticLanguage(Self::import(reader)?))
    }

    fn import(reader: &mut dyn BufRead) -> Result<Self> {
        let mut lreader = LineReader::new(reader);

        let head = lreader
            .next_line_string()
            .with_context(|| format!("failed to read header, which should be `{}`", HEADER))?;
        if head != HEADER {
            return Err(anyhow!(
                "first line should be exactly `{}`, but found `{}`",
                HEADER,
                head
            ));
        }

        let number_of_traces = lreader
            .next_line_index()
            .context("failed to read number of places")?;

        let mut traces = HashMap::new();
        let mut sum = Fraction::zero();
        let mut activity_key = ActivityKey::new();
        for trace_i in 0..number_of_traces {
            let probability = lreader.next_line_weight().with_context(|| {
                format!(
                    "failed to read weight for trace {} at line {}",
                    trace_i,
                    lreader.get_last_line_number()
                )
            })?;

            if !probability.is_positive() {
                return Err(anyhow!(
                    "trace {} at line {} has non-positive probability",
                    trace_i,
                    lreader.get_last_line_number()
                ));
            } else if probability > Fraction::one() {
                return Err(anyhow!(
                    "trace {} at line {} has a probability higher than 1",
                    trace_i,
                    lreader.get_last_line_number()
                ));
            }

            sum += probability.clone();

            let number_of_events = lreader.next_line_index().with_context(|| {
                format!(
                    "failed to read number of events for trace {} at line {}",
                    trace_i,
                    lreader.get_last_line_number()
                )
            })?;

            let mut trace = vec![];
            trace.reserve_exact(number_of_events);

            for event_i in 0..number_of_events {
                let event = lreader.next_line_string().with_context(|| {
                    format!(
                        "failed to read event {} of trace {} at line {}",
                        event_i,
                        trace_i,
                        lreader.get_last_line_number()
                    )
                })?;
                trace.push(event);
            }

            let trace = activity_key.process_trace(&trace);
            if traces.insert(trace, probability).is_some() {
                return Err(anyhow!(
                    "trace {} ending at line {} appears twice in language",
                    trace_i,
                    lreader.get_last_line_number()
                ));
            }
        }

        if sum > Fraction::one() {
            return Err(anyhow!(
                "probabilities in stochastic language sum to {}, which is greater than 1",
                sum
            ));
        }

        Ok(Self {
            activity_key: activity_key,
            traces: traces,
        })
    }
}

impl Exportable for FiniteStochasticLanguage {
    fn export_from_object(object: EbiOutput, f: &mut dyn Write) -> Result<()> {
        match object {
            EbiOutput::Object(EbiObject::FiniteStochasticLanguage(slang)) => slang.export(f),
            _ => unreachable!(),
        }
    }

    fn export(&self, f: &mut dyn std::io::Write) -> Result<()> {
        Ok(write!(f, "{}", self)?)
    }
}

impl Infoable for FiniteStochasticLanguage {
    fn info(&self, f: &mut impl std::io::Write) -> Result<()> {
        writeln!(f, "Number of traces\t{}", self.traces.len())?;
        writeln!(
            f,
            "Number of events\t{}",
            self.traces.iter().map(|t| t.0.len()).sum::<usize>()
        )?;
        writeln!(
            f,
            "Number of activities\t{}",
            (self as &dyn EbiTraitFiniteStochasticLanguage)
                .get_activity_key()
                .get_number_of_activities()
        )?;
        writeln!(f, "Sum of probabilities\t{:.4}", self.get_probability_sum())?;

        Ok(write!(f, "")?)
    }
}

impl fmt::Display for FiniteStochasticLanguage {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        writeln!(f, "{}", HEADER)?;
        writeln!(f, "# number of traces\n{}", self.traces.len())?;

        for (pos, (trace, probability)) in self.traces.iter().enumerate() {
            writeln!(f, "# trace {}", pos)?;

            writeln!(f, "# probability\n{}", probability)?;

            writeln!(f, "# number of events\n{}", trace.len())?;
            for event in trace {
                writeln!(f, "{}", self.activity_key.get_activity_label(event))?;
            }
        }

        write!(f, "")
    }
}

impl ToSemantics for FiniteStochasticLanguage {
    fn to_semantics(self) -> EbiTraitSemantics {
        EbiTraitSemantics::Usize(Box::new(FiniteStochasticLanguageSemantics::from_language(
            &self,
        )))
    }
}

impl ToStochasticSemantics for FiniteStochasticLanguage {
    fn to_stochastic_semantics(self) -> EbiTraitStochasticSemantics {
        EbiTraitStochasticSemantics::Usize(Box::new(
            FiniteStochasticLanguageSemantics::from_language(&self),
        ))
    }
}

impl ToStochasticDeterministicSemantics for FiniteStochasticLanguage {
    fn to_stochastic_deterministic_semantics(self) -> EbiTraitStochasticDeterministicSemantics {
        EbiTraitStochasticDeterministicSemantics::Usize(Box::new(
            self.get_stochastic_deterministic_finite_automaton(),
        ))
    }
}

impl EbiTraitIterableStochasticLanguage for FiniteStochasticLanguage {
    fn iter_trace_probability(&self) -> Box<dyn Iterator<Item = (&Vec<Activity>, &Fraction)> + '_> {
        Box::new(self.traces.iter())
    }

    fn get_probability(&self, trace_index: usize) -> Option<&Fraction> {
        Some(self.traces.iter().nth(trace_index)?.1)
    }
}

impl EbiTraitQueriableStochasticLanguage for FiniteStochasticLanguage {
    fn get_probability(&self, follower: &FollowerSemantics) -> Result<Fraction> {
        match follower {
            FollowerSemantics::Trace(trace) => {
                return match self.traces.get(*trace) {
                    Some(x) => Ok(x.clone()),
                    None => Ok(Fraction::zero()),
                };
            }
        }
    }
}
