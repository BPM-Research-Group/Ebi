use std::{
    cmp::Ordering,
    collections::{HashMap, hash_map::Entry},
};

use crate::{
    ebi_framework::activity_key::{
        Activity, ActivityKey, ActivityKeyTranslator, TranslateActivityKey,
    },
    math::{fraction::Fraction, traits::Zero},
};

#[derive(ActivityKey,Clone)]
pub struct StochasticDirectlyFollowsModel {
    pub(crate) activity_key: ActivityKey,
    pub(crate) empty_traces_weight: Fraction,
    pub(crate) sources: Vec<Activity>, //edge -> source of edge
    pub(crate) targets: Vec<Activity>, //edge -> target of edge
    pub(crate) weights: Vec<Fraction>, //edge -> how often observed
    pub(crate) start_activities: HashMap<Activity, Fraction>, //activity -> how often observed
    pub(crate) end_activities: HashMap<Activity, Fraction>, //activity -> how often observed
}

impl StochasticDirectlyFollowsModel {
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

impl TranslateActivityKey for StochasticDirectlyFollowsModel {
    fn translate_using_activity_key(&mut self, to_activity_key: &mut ActivityKey) {
        let translator = ActivityKeyTranslator::new(&self.activity_key, to_activity_key);

        self.start_activities = self
            .start_activities
            .iter()
            .map(|(activity, weight)| (translator.translate_activity(&activity), weight.clone()))
            .collect();

        self.end_activities = self
            .end_activities
            .iter()
            .map(|(activity, weight)| (translator.translate_activity(&activity), weight.clone()))
            .collect();

        let old_sources = self.sources.clone();
        let old_targets = self.targets.clone();
        let old_weights = self.weights.clone();

        self.sources = vec![];
        self.targets = vec![];
        self.weights = vec![];

        self.activity_key = to_activity_key.clone();

        for (source, (target, weight)) in old_sources
            .into_iter()
            .zip(old_targets.into_iter().zip(old_weights.into_iter()))
        {
            self.add_edge(source, target, &weight);
        }
    }
}
