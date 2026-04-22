use bitvec::{bitvec, vec::BitVec};
use ebi_objects::{
    Activity, ActivityKey, Executions,
    ebi_arithmetic::{Fraction, One},
};
use itertools::Itertools;
use std::fmt::Display;

#[derive(Debug)]
pub struct ResourceModel {
    resource_key: ActivityKey,
    transition_2_resources: Vec<BitVec>,
}

impl ResourceModel {
    pub fn from_executions(executions: &Executions) -> Self {
        let number_of_resources = executions.resource_key.get_number_of_activities();
        let mut transition_2_resources = vec![];
        for execution in executions.executions.iter() {
            if let Some(resource) = execution.resource {
                //ensure length
                while transition_2_resources.len() <= execution.fired_transition {
                    transition_2_resources.push(bitvec![0; number_of_resources]);
                }
                transition_2_resources[execution.fired_transition].set(resource.id, true);
            }
        }

        Self {
            resource_key: executions.resource_key.clone(),
            transition_2_resources,
        }
    }

    pub fn initial_resource_state(&self) -> ResourceMarking {
        ResourceMarking(vec![None; self.resource_key.get_number_of_activities()])
    }
}

impl Display for ResourceModel {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(
            f,
            "Resource model: {}",
            self.transition_2_resources
                .iter()
                .enumerate()
                .map(|(transition, resources)| format!(
                    "transition {} can be executed by {}",
                    transition,
                    resources
                        .iter_ones()
                        .map(|r| self
                            .resource_key
                            .get_activity_label(&self.resource_key.get_activity_by_id(r)))
                        .join(", ")
                ))
                .join(", ")
        )
    }
}

pub struct ResourceMarking(Vec<Option<usize>>);

impl ResourceMarking {
    pub fn execute_transition(
        &mut self,
        resource_model: &ResourceModel,
        resource: Option<Activity>,
    ) {
        if let Some(resource) = resource {
            //we know that the resource became unoccupied with the execution of this transition
            self.0[resource.id] = None;
        }
    }

    pub fn resource_utilisation(
        &self,
        resource_model: &ResourceModel,
        resource: Activity,
        transition: usize,
    ) -> Option<Fraction> {
        Some(Fraction::one())
    }
}

#[cfg(test)]
mod tests {
    use crate::techniques::resource_utilisation::ResourceModel;
    use ebi_objects::Executions;
    use std::fs;

    #[test]
    fn executions() {
        let fin = fs::read_to_string("testfiles/a-b.exs").unwrap();
        let exs = fin.parse::<Executions>().unwrap();

        let resource_model = ResourceModel::from_executions(&exs);

        println!("{}", resource_model);
    }
}
