use std::{
    collections::{HashMap, HashSet},
    hash::Hash,
};

/// Keep track of groups.
pub struct Components<T>
where
    T: Clone + Hash + PartialEq + Eq,
{
    activities: Vec<T>,
    groups: Vec<usize>,
}

impl<T> Components<T>
where
    T: Clone + Hash + PartialEq + Eq,
{
    pub fn new(activities: Vec<T>) -> Self {
        let n = activities.len();
        Self {
            activities,
            groups: (0..n).collect(),
        }
    }

    pub fn merge_components(&mut self, a: T, b: T) {
        let g_a = self
            .activities
            .iter()
            .position(|x| x == &a)
            .map(|i| self.groups[i]);
        let g_b = self
            .activities
            .iter()
            .position(|x| x == &b)
            .map(|i| self.groups[i]);
        if let (Some(group_a), Some(group_b)) = (g_a, g_b) {
            if group_a != group_b {
                for g in self.groups.iter_mut() {
                    if *g == group_a {
                        *g = group_b;
                    }
                }
            }
        }
    }

    pub fn build_components(&self) -> HashMap<usize, HashSet<T>> {
        let mut map: HashMap<usize, HashSet<T>> = HashMap::new();
        for (index, activity) in self.activities.iter().enumerate() {
            map.entry(self.groups[index]).or_default().insert(activity.clone());
        }
        map
    }

    pub fn into_components(self) -> Vec<HashSet<T>> {
        self.build_components().into_values().collect()
    }

    pub fn same_component(&self, a: T, b: T) -> bool {
        let ga = self
            .activities
            .iter()
            .position(|x| x == &a)
            .map(|i| self.groups[i]);
        let gb = self
            .activities
            .iter()
            .position(|x| x == &b)
            .map(|i| self.groups[i]);

        ga.is_some() && ga == gb
    }
}
