use std::{
    cmp::Ordering,
    collections::{HashMap, hash_map::Entry},
    io::BufRead,
};

use anyhow::{Context, Result, anyhow};
use serde_json::Value;

use crate::{
    ebi_framework::{
        activity_key::{Activity, ActivityKey},
        ebi_file_handler::EbiFileHandler,
        ebi_input::{self, EbiObjectImporter, EbiTraitImporter},
        ebi_object::EbiObject,
        importable::Importable,
    },
    ebi_traits::{
        ebi_trait_graphable::EbiTraitGraphable, ebi_trait_semantics::{EbiTraitSemantics, ToSemantics}, ebi_trait_stochastic_deterministic_semantics::{EbiTraitStochasticDeterministicSemantics, ToStochasticDeterministicSemantics}, ebi_trait_stochastic_semantics::{EbiTraitStochasticSemantics, ToStochasticSemantics}
    },
    json,
    math::{fraction::Fraction, traits::Zero},
};

use super::{
    directly_follows_model::DirectlyFollowsModel,
    stochastic_directly_follows_model::StochasticDirectlyFollowsModel,
};

pub const FORMAT_SPECIFICATION: &str = "A directly follows graph is a JSON structure.";

pub const EBI_DIRECTLY_FOLLOWS_GRAPH: EbiFileHandler = EbiFileHandler {
    name: "directly follows graph",
    article: "a",
    file_extension: "dfg",
    format_specification: &FORMAT_SPECIFICATION,
    validator: Some(ebi_input::validate::<DirectlyFollowsGraph>),
    trait_importers: &[
        EbiTraitImporter::Semantics(DirectlyFollowsGraph::import_as_semantics),
        EbiTraitImporter::StochasticSemantics(DirectlyFollowsGraph::import_as_stochastic_semantics),
        EbiTraitImporter::StochasticDeterministicSemantics(
            DirectlyFollowsGraph::import_as_stochastic_deterministic_semantics,
        ),
        EbiTraitImporter::Graphable(DirectlyFollowsGraph::import_as_graphable),
    ],
    object_importers: &[
        EbiObjectImporter::StochasticDirectlyFollowsModel(DirectlyFollowsGraph::import_as_object),
        EbiObjectImporter::DirectlyFollowsModel(
            DirectlyFollowsGraph::import_as_directly_follows_model,
        ),
        EbiObjectImporter::LabelledPetriNet(DirectlyFollowsGraph::import_as_labelled_petri_net),
        EbiObjectImporter::StochasticLabelledPetriNet(
            DirectlyFollowsGraph::import_as_stochastic_labelled_petri_net,
        ),
    ],
    object_exporters: &[],
    java_object_handlers: &[], //java translations covered by LabelledPetrinet
};

#[derive(Clone)]
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

    pub fn import_as_labelled_petri_net(reader: &mut dyn BufRead) -> Result<EbiObject> {
        let dfg = Self::import(reader)?;
        Ok(EbiObject::LabelledPetriNet(dfg.into()))
    }

    pub fn import_as_stochastic_labelled_petri_net(reader: &mut dyn BufRead) -> Result<EbiObject> {
        let dfg = Self::import(reader)?;
        Ok(EbiObject::StochasticLabelledPetriNet(dfg.into()))
    }

    pub fn import_as_graphable(reader: &mut dyn BufRead) -> Result<Box<dyn EbiTraitGraphable>> {
        let dfg: StochasticDirectlyFollowsModel = Self::import(reader)?.into();
        Ok(Box::new(dfg))
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
    fn import_as_object(
        reader: &mut dyn std::io::BufRead,
    ) -> anyhow::Result<crate::ebi_framework::ebi_object::EbiObject> {
        Ok(EbiObject::StochasticDirectlyFollowsModel(
            Self::import(reader)?.try_into()?,
        ))
    }

    fn import(reader: &mut dyn std::io::BufRead) -> anyhow::Result<Self>
    where
        Self: Sized,
    {
        let json: Value = serde_json::from_reader(reader)?;

        let mut result = DirectlyFollowsGraph::new(ActivityKey::new());

        let activities = json::read_field_object(&json, "activities")
            .with_context(|| format!("reading field `activities`"))?;
        let frequencies: Result<HashMap<Activity, usize>> = activities
            .into_iter()
            .map(|(activity, json)| {
                let frequency = json::read_number(json)
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

            let weight = json::read_number(
                edge_list
                    .get(1)
                    .ok_or_else(|| anyhow!("could not read weight of edge {}", i))?,
            )
            .with_context(|| format!("reading weight of edge {}", i))?;

            result.add_edge(source, target, &Fraction::from((weight, 1)));

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
                        *occupied_entry.get_mut() -= weight
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
                        *occupied_entry.get_mut() -= weight
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
                        if weight > 0 {
                            result.add_start_activity(activity, &Fraction::from((weight, 1)));
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
                        if weight > 0 {
                            result.add_end_activity(activity, &Fraction::from((weight, 1)));
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