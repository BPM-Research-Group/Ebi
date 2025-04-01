use std::{borrow::Borrow, collections::{HashMap, HashSet}, fmt::{Display, Debug}, hash::Hash};

#[cfg(test)]
use uuid::Uuid;

pub trait HasActivityKey {
    fn get_activity_key(&self) -> &ActivityKey;

    fn get_activity_key_mut(&mut self) -> &mut ActivityKey;
}

#[derive(Clone,Copy,Ord,Eq,PartialEq,PartialOrd)]
#[cfg(not(test))]
pub struct Activity {
    id: usize,
}

#[derive(Clone,Copy,Eq)]
#[cfg(test)]
pub struct Activity {
    id: usize,
    activity_key_uuid: Uuid //In testing, an uuid is kept of the activity key.
}

impl PartialEq<usize> for Activity {
    fn eq(&self, other: &usize) -> bool {
        &self.id == other
    }
}

impl Hash for Activity {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.id.hash(state);
    }
}

impl Display for Activity {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "ac{}", self.id)
    }
}

impl Debug for Activity {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "ac{}", self.id)
    }
}

impl PartialOrd<usize> for Activity {
    fn partial_cmp(&self, other: &usize) -> Option<std::cmp::Ordering> {
        self.id.partial_cmp(other)
    }
}

#[cfg(test)]
impl PartialEq for Activity {
    fn eq(&self, other: &Self) -> bool {
        assert!(self.activity_key_uuid == other.activity_key_uuid, "cannot compare activities of different activity keys");

        self.id == other.id
    }
}

#[cfg(test)]
impl Ord for Activity {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        assert!(self.activity_key_uuid == other.activity_key_uuid, "cannot compare activities of different activity keys");

        self.id.cmp(&other.id)
    }
}

#[cfg(test)]
impl PartialOrd for Activity {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        assert!(self.activity_key_uuid == other.activity_key_uuid, "cannot compare activities of different activity keys");

        self.id.partial_cmp(&other.id)
    }
}

#[derive(Clone,Debug)]
#[cfg(test)]
pub struct ActivityKey {
    pub name2activity: HashMap<String, Activity>,
    pub activity2name: Vec<String>,
    pub next_index: usize,
    uuid: Uuid
}

#[derive(Clone,Debug)]
#[cfg(not(test))]
pub struct ActivityKey {
    pub name2activity: HashMap<String, Activity>,
    pub activity2name: Vec<String>,
    pub next_index: usize,
}

impl <'a> ActivityKey {
    #[cfg(test)]
    pub fn new() -> Self {
        Self {
            name2activity: HashMap::new(),
            activity2name: vec![],
            next_index: 0,
            uuid: Uuid::new_v4()
        }
    }

    #[cfg(not(test))]
    pub fn new() -> Self {
        Self {
            name2activity: HashMap::new(),
            activity2name: vec![],
            next_index: 0
        }
    }

    pub fn get_number_of_activities(&self) -> usize {
        return self.name2activity.len()
    }

    #[cfg(test)]
    pub fn process_trace(&mut self, trace: &Vec<String>) -> Vec<Activity> {
        let mut result = vec![];
        for activity in trace {
            match self.name2activity.get(activity) {
                Some(index) => result.push(*index),
                None => {
                    let index = Activity{id: self.next_index, activity_key_uuid: self.uuid};
                    result.push(index.clone());
                    self.activity2name.push(activity.clone());
                    self.name2activity.insert(activity.clone(), index);
                    self.next_index += 1;
                },
            }
        }
        return result;
    }

    #[cfg(not(test))]
    pub fn process_trace(&mut self, trace: &Vec<String>) -> Vec<Activity> {
        let mut result = vec![];
        for activity in trace {
            match self.name2activity.get(activity) {
                Some(index) => result.push(*index),
                None => {
                    let index = Activity{id: self.next_index};
                    result.push(index.clone());
                    self.activity2name.push(activity.clone());
                    self.name2activity.insert(activity.clone(), index);
                    self.next_index += 1;
                },
            }
        }
        return result;
    }

    #[cfg(test)]
    pub fn process_trace_ref(&mut self, trace: &Vec<&str>) -> Vec<Activity> {
        let mut result = vec![];
        for activity in trace {
            match self.name2activity.get(&activity.to_string()) {
                Some(index) => result.push(*index),
                None => {
                    let index = Activity{id: self.next_index, activity_key_uuid: self.uuid};
                    result.push(index.clone());
                    self.activity2name.push(activity.to_string());
                    self.name2activity.insert(activity.to_string(), index);
                    self.next_index += 1;
                },
            }
        }
        return result;
    }

    #[cfg(not(test))]
    pub fn process_trace_ref(&mut self, trace: &Vec<&str>) -> Vec<Activity> {
        let mut result = vec![];
        for activity in trace {
            match self.name2activity.get(&activity.to_string()) {
                Some(index) => result.push(*index),
                None => {
                    let index = Activity{id: self.next_index};
                    result.push(index.clone());
                    self.activity2name.push(activity.to_string());
                    self.name2activity.insert(activity.to_string(), index);
                    self.next_index += 1;
                },
            }
        }
        return result;
    }

    #[cfg(test)]
    pub fn get_activity_label(&self, activity: &Activity) -> &str {
        assert!(self.uuid == activity.activity_key_uuid, "cannot get activity label of activity of different activity key");
        &self.activity2name[activity.id]
    }

    #[cfg(not(test))]
    pub fn get_activity_label(&self, activity: &Activity) -> &str {
        &self.activity2name[activity.id]
    }

    #[cfg(test)]
    pub fn process_activity(&mut self, activity: &str) -> Activity {
        match self.name2activity.get(activity) {
            Some(index) => return *index,
            None => {
                let result = Activity{id: self.next_index, activity_key_uuid: self.uuid};
                self.activity2name.push(activity.to_string());
                self.name2activity.insert(activity.to_string(), result);
                self.next_index += 1;
                return result;
            },
        }
    }

    #[cfg(not(test))]
    pub fn process_activity(&mut self, activity: &str) -> Activity {
        match self.name2activity.get(activity) {
            Some(index) => return *index,
            None => {
                let result = Activity{id: self.next_index};
                self.activity2name.push(activity.to_string());
                self.name2activity.insert(activity.to_string(), result);
                self.next_index += 1;
                return result;
            },
        }
    }
    
    #[cfg(test)]
    pub fn get_activity_by_id(&self, activity_id: usize) -> Activity {
        Activity {
            id: activity_id,
            activity_key_uuid: self.uuid
        }
    }

    #[cfg(not(test))]
    pub fn get_activity_by_id(&self, activity_id: usize) -> Activity {
        Activity {
            id: activity_id,
        }
    }

    pub fn get_id_from_activity(&self, activity: impl Borrow<Activity>) -> usize {
        activity.borrow().id
    }

    pub fn deprocess_trace(&self, trace: &Vec<Activity>) -> Vec<&str> {
        trace.iter().map(|activity| self.get_activity_label(activity)).collect()
    }

    pub fn deprocess_set(&'a self, set: &HashSet<Vec<Activity>>) -> HashSet<Vec<&'a str>> {
        set.iter().map(|trace| self.deprocess_trace(trace)).collect()
    }
    
    pub fn deprocess_activity(&self, activity: &Activity) -> &str {
       self.get_activity_label(activity) 
    }
}

impl Display for ActivityKey {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for (i, label) in self.activity2name.iter().enumerate() {
            write!(f, "ac{}: {}, ", i, label)?;
        }
        write!(f, "")
    }
}

pub struct ActivityKeyTranslator {
    from2to: Vec<Activity>
}

impl ActivityKeyTranslator {
    pub fn new(from: &ActivityKey, to: &mut ActivityKey) -> Self {
        let mut from2to = vec![];

        for label_from in &from.activity2name {
            let index_to = to.process_activity(&label_from);
            from2to.push(index_to);
        }

        Self {
            from2to: from2to
        }
    }

    pub fn translate_activity(&self, activity: &Activity) -> Activity {
        self.from2to[activity.id]
    }

    pub fn translate_trace(&self, trace: &Vec<Activity>) -> Vec<Activity> {
        let mut result = Vec::with_capacity(trace.len());
        for from in trace {
            result.push(self.from2to[from.id]);
        }
        result
    }
}