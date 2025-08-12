use std::{
    cmp::Ordering,
    collections::{HashMap, hash_map::Entry},
    fmt::Display,
    io::BufRead,
};

use anyhow::{Context, Result, anyhow};
use ebi_arithmetic::{
    ebi_number::{Signed, Zero},
    fraction::Fraction,
};
use ebi_derive::ActivityKey;
use layout::topo::layout::VisualGraph;
use serde_json::Value;

use crate::{
    ebi_framework::{
        activity_key::{
            Activity, ActivityKey, ActivityKeyTranslator, HasActivityKey, TranslateActivityKey,
        },
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
        ebi_trait_activities,
        ebi_trait_graphable::{self, EbiTraitGraphable},
        ebi_trait_semantics::{EbiTraitSemantics, ToSemantics},
        ebi_trait_stochastic_deterministic_semantics::{
            EbiTraitStochasticDeterministicSemantics, ToStochasticDeterministicSemantics,
        },
        ebi_trait_stochastic_semantics::{EbiTraitStochasticSemantics, ToStochasticSemantics},
    },
    format_comparison, json,
};

use super::{
    directly_follows_model::DirectlyFollowsModel,
    stochastic_directly_follows_model::StochasticDirectlyFollowsModel,
};

pub const FORMAT_SPECIFICATION: &str = concat!(
    "A directly follows graph is a JSON structure.
    
    For instance:
    \\lstinputlisting[language=ebilines, style=boxed]{../testfiles/aa-ab-ba.dfg}",
    format_comparison!()
);

pub const EBI_DIRECTLY_FOLLOWS_GRAPH: EbiFileHandler = EbiFileHandler {
    name: "directly follows graph",
    article: "a",
    file_extension: "dfg",
    is_binary: false,
    format_specification: &FORMAT_SPECIFICATION,
    validator: Some(ebi_input::validate::<DirectlyFollowsGraph>),
    trait_importers: &[
        EbiTraitImporter::Activities(ebi_trait_activities::import::<DirectlyFollowsGraph>),
        EbiTraitImporter::Semantics(DirectlyFollowsGraph::import_as_semantics),
        EbiTraitImporter::StochasticSemantics(DirectlyFollowsGraph::import_as_stochastic_semantics),
        EbiTraitImporter::StochasticDeterministicSemantics(
            DirectlyFollowsGraph::import_as_stochastic_deterministic_semantics,
        ),
        EbiTraitImporter::Graphable(ebi_trait_graphable::import::<DirectlyFollowsGraph>),
    ],
    object_importers: &[
        EbiObjectImporter::DirectlyFollowsGraph(DirectlyFollowsGraph::import_as_object),
        EbiObjectImporter::StochasticDirectlyFollowsModel(
            DirectlyFollowsGraph::import_as_stochastic_directly_follows_model,
        ),
        EbiObjectImporter::DirectlyFollowsModel(
            DirectlyFollowsGraph::import_as_directly_follows_model,
        ),
        EbiObjectImporter::LabelledPetriNet(DirectlyFollowsGraph::import_as_labelled_petri_net),
        EbiObjectImporter::StochasticLabelledPetriNet(
            DirectlyFollowsGraph::import_as_stochastic_labelled_petri_net,
        ),
    ],
    object_exporters: &[EbiObjectExporter::DirectlyFollowsGraph(
        DirectlyFollowsGraph::export_from_object,
    )],
    java_object_handlers: &[], //java translations covered by LabelledPetrinet
};

#[derive(ActivityKey, Clone, Debug)]
pub struct DirectlyFollowsGraph {
    pub(crate) activity_key: ActivityKey,
    pub(crate) empty_traces_weight: Fraction,
    pub(crate) sources: Vec<Activity>, //edge -> source of edge
    pub(crate) targets: Vec<Activity>, //edge -> target of edge
    pub(crate) weights: Vec<Fraction>, //edge -> how often observed
    pub(crate) start_activities: HashMap<Activity, Fraction>, //activity -> how often observed
    pub(crate) end_activities: HashMap<Activity, Fraction>, //activity -> how often observed
}

impl DirectlyFollowsGraph {
    pub fn new(activity_key: ActivityKey) -> Self {
        Self {
            empty_traces_weight: Fraction::zero(),
            activity_key: activity_key,
            sources: vec![],
            targets: vec![],
            weights: vec![],
            start_activities: HashMap::new(),
            end_activities: HashMap::new(),
        }
    }

    pub fn import_as_directly_follows_model(reader: &mut dyn BufRead) -> Result<EbiObject> {
        let dfg = Self::import(reader)?;
        Ok(EbiObject::DirectlyFollowsModel(dfg.into()))
    }

    pub fn import_as_stochastic_directly_follows_model(
        reader: &mut dyn BufRead,
    ) -> Result<EbiObject> {
        let dfg = Self::import(reader)?;
        Ok(EbiObject::StochasticDirectlyFollowsModel(dfg.into()))
    }

    pub fn import_as_labelled_petri_net(reader: &mut dyn BufRead) -> Result<EbiObject> {
        let dfg = Self::import(reader)?;
        Ok(EbiObject::LabelledPetriNet(dfg.into()))
    }

    pub fn import_as_stochastic_labelled_petri_net(reader: &mut dyn BufRead) -> Result<EbiObject> {
        let dfg = Self::import(reader)?;
        Ok(EbiObject::StochasticLabelledPetriNet(dfg.into()))
    }

    pub fn get_max_state(&self) -> usize {
        self.activity_key.get_number_of_activities() + 2
    }

    pub fn edge_weight(&self, source: Activity, target: Activity) -> Option<&Fraction> {
        let (found, from) = self.binary_search(source, target);
        if found {
            Some(&self.weights[from])
        } else {
            None
        }
    }

    pub fn is_start_node(&self, node: Activity) -> bool {
        match self.start_activities.get(&node) {
            Some(w) => w.is_positive(),
            None => false,
        }
    }

    pub fn is_end_node(&self, node: Activity) -> bool {
        match self.end_activities.get(&node) {
            Some(w) => w.is_positive(),
            None => false,
        }
    }

    pub fn activity_cardinality(&self, activity: Activity) -> Fraction {
        let mut result = match self.end_activities.get(&activity) {
            Some(a) => a.clone(),
            None => Fraction::zero(),
        };

        let (_, mut i) = self.binary_search(activity, self.activity_key.get_activity_by_id(0));
        while i < self.sources.len() && self.sources[i] == activity {
            if self.weights[i].is_positive() {
                result += &self.weights[i];
            }
            i += 1;
        }

        result
    }

    pub fn has_empty_traces(&self) -> bool {
        self.empty_traces_weight.is_positive()
    }

    pub fn add_empty_trace(&mut self, weight: &Fraction) {
        self.empty_traces_weight += weight;
    }

    pub fn add_start_activity(&mut self, activity: Activity, weight: &Fraction) {
        match self.start_activities.entry(activity) {
            Entry::Occupied(mut occupied_entry) => *occupied_entry.get_mut() += weight,
            Entry::Vacant(vacant_entry) => {
                vacant_entry.insert(weight.clone());
            }
        }
    }

    pub fn add_end_activity(&mut self, activity: Activity, weight: &Fraction) {
        match self.end_activities.entry(activity) {
            Entry::Occupied(mut occupied_entry) => *occupied_entry.get_mut() += weight,
            Entry::Vacant(vacant_entry) => {
                vacant_entry.insert(weight.clone());
            }
        }
    }

    pub fn add_edge(&mut self, source: Activity, target: Activity, weight: &Fraction) {
        let (found, from) = self.binary_search(source, target);
        if found {
            //edge already present
            self.weights[from] += weight;
        } else {
            //new edge
            self.sources.insert(from, source);
            self.targets.insert(from, target);
            self.weights.insert(from, weight.clone());
        }
    }

    pub(crate) fn binary_search(&self, source: Activity, target: Activity) -> (bool, usize) {
        if self.sources.is_empty() {
            return (false, 0);
        }

        let mut size = self.sources.len();
        let mut left = 0;
        let mut right = size;
        while left < right {
            let mid = left + size / 2;

            let cmp = Self::compare(source, target, self.sources[mid], self.targets[mid]);

            left = if cmp == Ordering::Less { mid + 1 } else { left };
            right = if cmp == Ordering::Greater { mid } else { right };
            if cmp == Ordering::Equal {
                assert!(mid < self.sources.len());
                return (true, mid);
            }

            size = right - left;
        }

        assert!(left <= self.sources.len());
        (false, left)
    }

    fn compare(
        source1: Activity,
        activity1: Activity,
        source2: Activity,
        activity2: Activity,
    ) -> Ordering {
        if source1 < source2 {
            return Ordering::Greater;
        } else if source1 > source2 {
            return Ordering::Less;
        } else if activity2 > activity1 {
            return Ordering::Greater;
        } else if activity2 < activity1 {
            return Ordering::Less;
        } else {
            return Ordering::Equal;
        }
    }
}

impl Importable for DirectlyFollowsGraph {
    fn import_as_object(reader: &mut dyn std::io::BufRead) -> anyhow::Result<EbiObject> {
        Ok(EbiObject::DirectlyFollowsGraph(Self::import(reader)?))
    }

    fn import(reader: &mut dyn std::io::BufRead) -> anyhow::Result<Self>
    where
        Self: Sized,
    {
        let json: Value = serde_json::from_reader(reader)?;

        let mut result = DirectlyFollowsGraph::new(ActivityKey::new());

        let activities = json::read_field_object(&json, "activities")
            .with_context(|| format!("reading field `activities`"))?;
        let frequencies: Result<HashMap<Activity, Fraction>> = activities
            .into_iter()
            .map(|(activity, json)| {
                let frequency = json::read_fraction(json)
                    .with_context(|| format!("read activity frequency of {}", activity))?;
                let activity = result.activity_key.process_activity(activity);
                Ok((activity, frequency))
            })
            .collect();
        let mut frequencies_in = frequencies?;
        let mut frequencies_out = frequencies_in.clone();

        let edges = json::read_field_list(&json, "directly_follows_relations")
            .with_context(|| format!("reading field `directly follows relations`"))?;
        for (i, edge) in edges.into_iter().enumerate() {
            let edge_list = json::read_list(edge).with_context(|| format!("reading edge {}", i))?;
            let sourcetarget = json::read_list(
                edge_list
                    .get(0)
                    .ok_or_else(|| anyhow!("could not read source and target of edge {}", i))?,
            )?;

            let source_act = json::read_string(
                sourcetarget
                    .get(0)
                    .ok_or_else(|| anyhow!("could not read source of edge {}", i))?,
            )
            .with_context(|| format!("reading source of edge {}", i))?;
            let source = result.activity_key.process_activity(source_act);

            let target_act = json::read_string(
                sourcetarget
                    .get(1)
                    .ok_or_else(|| anyhow!("could not read target of edge {}", i))?,
            )
            .with_context(|| format!("reading target of edge {}", i))?;
            let target = result.activity_key.process_activity(target_act);

            let weight = json::read_fraction(
                edge_list
                    .get(1)
                    .ok_or_else(|| anyhow!("could not read weight of edge {}", i))?,
            )
            .with_context(|| format!("reading weight of edge {}", i))?;

            result.add_edge(source, target, &weight);

            //keep track of how many executions are left for source
            match frequencies_out.entry(source) {
                Entry::Occupied(mut occupied_entry) => {
                    let pre_weight = occupied_entry.get();
                    if pre_weight < &weight {
                        return Err(anyhow!(
                            "activity {} has too many outgoing edges as of edge {}",
                            source_act,
                            i
                        ));
                    } else {
                        *occupied_entry.get_mut() -= &weight
                    }
                }
                Entry::Vacant(_) => {
                    //activity not declared
                    return Err(anyhow!(
                        "non-declared activity {} used as source of edge {}",
                        source_act,
                        i
                    ));
                }
            }

            //keep track of how many executions are left for source
            match frequencies_in.entry(target) {
                Entry::Occupied(mut occupied_entry) => {
                    let pre_weight = occupied_entry.get();
                    if pre_weight < &weight {
                        return Err(anyhow!(
                            "activity {} has too many incoming edges as of edge {}",
                            target_act,
                            i
                        ));
                    } else {
                        *occupied_entry.get_mut() -= &weight
                    }
                }
                Entry::Vacant(_) => {
                    //activity not declared
                    return Err(anyhow!(
                        "non-declared activity {} used as target of edge {}",
                        target_act,
                        i
                    ));
                }
            }
        }

        let start_activities = json::read_field_list(&json, "start_activities")
            .with_context(|| format!("reading field `start activities`"))?;
        let err: Result<()> = start_activities
            .into_iter()
            .map(|json| {
                let act = json::read_string(json)?;
                let activity = result.activity_key.process_activity(act);

                match frequencies_in.entry(activity) {
                    Entry::Occupied(occupied_entry) => {
                        let weight = occupied_entry.remove();
                        if weight.is_positive() {
                            result.add_start_activity(activity, &weight);
                        }
                    }
                    Entry::Vacant(_) => {
                        //activity not declared
                        return Err(anyhow!(format!(
                            "non-declared activity {} used as start activity",
                            act
                        )));
                    }
                }
                Ok(())
            })
            .collect();
        err?;

        let end_activities = json::read_field_list(&json, "end_activities")
            .with_context(|| format!("reading field `end activities`"))?;
        let err: Result<()> = end_activities
            .into_iter()
            .map(|json| {
                let act = json::read_string(json)?;
                let activity = result.activity_key.process_activity(act);

                match frequencies_out.entry(activity) {
                    Entry::Occupied(occupied_entry) => {
                        let weight = occupied_entry.remove();
                        if weight.is_positive() {
                            result.add_end_activity(activity, &weight);
                        }
                    }
                    Entry::Vacant(_) => {
                        //activity not declared
                        return Err(anyhow!(format!(
                            "non-declared activity {} used as end activity",
                            act
                        )));
                    }
                }
                Ok(())
            })
            .collect();
        err?;

        return Ok(result);
    }
}

impl FromEbiTraitObject for DirectlyFollowsGraph {
    fn from_trait_object(object: ebi_input::EbiInput) -> Result<Box<Self>> {
        match object {
            EbiInput::Object(EbiObject::DirectlyFollowsGraph(e), _) => Ok(Box::new(e)),
            _ => Err(anyhow!(
                "cannot read {} {} as a directly follows graph",
                object.get_type().get_article(),
                object.get_type()
            )),
        }
    }
}

impl Exportable for DirectlyFollowsGraph {
    fn export_from_object(object: EbiOutput, f: &mut dyn std::io::Write) -> Result<()> {
        match object {
            EbiOutput::Object(EbiObject::DirectlyFollowsGraph(dfa)) => dfa.export(f),
            _ => Err(anyhow!("Cannot export to DFG.")),
        }
    }

    fn export(&self, f: &mut dyn std::io::Write) -> Result<()> {
        Ok(write!(f, "{}", self)?)
    }
}

impl TranslateActivityKey for DirectlyFollowsGraph {
    fn translate_using_activity_key(&mut self, to_activity_key: &mut ActivityKey) {
        let translator = ActivityKeyTranslator::new(&self.activity_key, to_activity_key);

        //extract all edges
        let mut old_sources = vec![];
        let mut old_targets = vec![];
        let mut old_weights = vec![];
        std::mem::swap(&mut self.sources, &mut old_sources);
        std::mem::swap(&mut self.targets, &mut old_targets);
        std::mem::swap(&mut self.weights, &mut old_weights);

        //re-add all edges
        for (source, (target, weight)) in old_sources
            .into_iter()
            .zip(old_targets.into_iter().zip(old_weights.into_iter()))
        {
            let source = translator.translate_activity(&source);
            let target = translator.translate_activity(&target);
            let (_, from) = self.binary_search(source, target);
            self.sources.insert(from, source);
            self.targets.insert(from, target);
            self.weights.insert(from, weight);
        }

        //extract all start activities
        let mut old_start = HashMap::new();
        let mut old_end = HashMap::new();
        std::mem::swap(&mut self.start_activities, &mut old_start);
        std::mem::swap(&mut self.end_activities, &mut old_end);

        self.start_activities = old_start
            .into_iter()
            .map(|(a, w)| (translator.translate_activity(&a), w))
            .collect();
        self.end_activities = old_end
            .into_iter()
            .map(|(a, w)| (translator.translate_activity(&a), w))
            .collect();

        self.activity_key = to_activity_key.clone();
    }
}

impl Infoable for DirectlyFollowsGraph {
    fn info(&self, f: &mut impl std::io::Write) -> Result<()> {
        writeln!(f, "Number of edges\t\t\t{}", self.sources.len())?;
        writeln!(
            f,
            "Number of activities\t\t{}",
            self.activity_key.activity2name.len()
        )?;
        writeln!(
            f,
            "Number of start activities\t{}",
            self.start_activities.len()
        )?;
        writeln!(f, "Number of end activities\t{}", self.end_activities.len())?;

        let mut sum: Fraction = self.weights.iter().sum();
        sum += &self.start_activities.values().sum::<Fraction>();
        sum += &self.end_activities.values().sum::<Fraction>();
        writeln!(f, "Sum weight of edges\t\t{}", sum)?;

        writeln!(f, "")?;
        self.get_activity_key().info(f)?;

        Ok(write!(f, "")?)
    }
}

impl Display for DirectlyFollowsGraph {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let activities = Value::Object(
            self.activity_key
                .get_activities()
                .iter()
                .map(|activity| {
                    (
                        self.activity_key.get_activity_label(activity).to_string(),
                        { Value::String(self.activity_cardinality(**activity).to_string()) },
                    )
                })
                .collect(),
        );
        let start_activities = Value::Array(
            self.start_activities
                .iter()
                .filter_map(|(a, w)| {
                    if w.is_positive() {
                        Some(Value::String(
                            self.activity_key.get_activity_label(a).to_string(),
                        ))
                    } else {
                        None
                    }
                })
                .collect(),
        );
        let end_activities = Value::Array(
            self.end_activities
                .iter()
                .filter_map(|(a, w)| {
                    if w.is_positive() {
                        Some(Value::String(
                            self.activity_key.get_activity_label(a).to_string(),
                        ))
                    } else {
                        None
                    }
                })
                .collect(),
        );
        let edges = Value::Array(
            self.sources
                .iter()
                .zip(self.targets.iter().zip(self.weights.iter()))
                .map(|(source, (target, weight))| {
                    Value::Array(vec![
                        Value::Array(vec![
                            Value::String(self.activity_key.get_activity_label(source).to_string()),
                            Value::String(self.activity_key.get_activity_label(target).to_string()),
                        ]),
                        Value::String(weight.to_string()),
                    ])
                })
                .collect(),
        );
        let json = serde_json::json!(
            {
                "activities": activities,
                "start_activities": start_activities,
                "end_activities": end_activities,
                "directly_follows_relations": edges
            }
        );
        let x = serde_json::to_string(&json).unwrap();
        write!(f, "{}", x)
    }
}

impl EbiTraitGraphable for DirectlyFollowsGraph {
    fn to_dot(&self) -> Result<layout::topo::layout::VisualGraph> {
        let mut graph = VisualGraph::new(layout::core::base::Orientation::LeftToRight);

        //source + sink
        let source = <dyn EbiTraitGraphable>::create_place(&mut graph, "");
        let sink = <dyn EbiTraitGraphable>::create_place(&mut graph, "");

        //empty traces
        if self.empty_traces_weight.is_positive() {
            <dyn EbiTraitGraphable>::create_edge(
                &mut graph,
                &source,
                &sink,
                &format!("{}", self.empty_traces_weight),
            );
        }

        //nodes
        let mut nodes = vec![sink; self.activity_key.get_number_of_activities()];
        for n in &self.activity_key.get_activities() {
            let id = self.activity_key.get_id_from_activity(*n);
            nodes[id] = <dyn EbiTraitGraphable>::create_transition(
                &mut graph,
                self.activity_key.get_activity_label(n),
                "",
            );
        }

        //start activities
        for (activity, weight) in self.start_activities.iter() {
            if weight.is_positive() {
                <dyn EbiTraitGraphable>::create_edge(
                    &mut graph,
                    &source,
                    &nodes[self.activity_key.get_id_from_activity(activity)],
                    &format!("{}", weight),
                );
            }
        }

        //end activities
        for (activity, weight) in self.end_activities.iter() {
            if weight.is_positive() {
                <dyn EbiTraitGraphable>::create_edge(
                    &mut graph,
                    &nodes[self.activity_key.get_id_from_activity(activity)],
                    &sink,
                    &format!("{}", weight),
                );
            }
        }

        //edges
        for (source, (target, weight)) in self
            .sources
            .iter()
            .zip(self.targets.iter().zip(self.weights.iter()))
        {
            <dyn EbiTraitGraphable>::create_edge(
                &mut graph,
                &nodes[self.activity_key.get_id_from_activity(source)],
                &nodes[self.activity_key.get_id_from_activity(target)],
                &format!("{}", weight),
            );
        }

        Ok(graph)
    }
}

impl ToSemantics for DirectlyFollowsGraph {
    fn to_semantics(self) -> EbiTraitSemantics {
        let dfm: DirectlyFollowsModel = self.into();
        EbiTraitSemantics::Usize(Box::new(dfm))
    }
}

impl ToStochasticSemantics for DirectlyFollowsGraph {
    fn to_stochastic_semantics(self) -> EbiTraitStochasticSemantics {
        let dfm: StochasticDirectlyFollowsModel = self.into();
        EbiTraitStochasticSemantics::Usize(Box::new(dfm))
    }
}

impl ToStochasticDeterministicSemantics for DirectlyFollowsGraph {
    fn to_stochastic_deterministic_semantics(self) -> EbiTraitStochasticDeterministicSemantics {
        let dfm: StochasticDirectlyFollowsModel = self.into();
        EbiTraitStochasticDeterministicSemantics::UsizeDistribution(Box::new(dfm))
    }
}
