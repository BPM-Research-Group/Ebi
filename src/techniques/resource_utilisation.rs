use bitvec::{bitvec, vec::BitVec};
use ebi_objects::{
    Activity, ActivityKey, Executions,
    anyhow::{Result, anyhow},
    ebi_arithmetic::{Fraction, f},
};
use itertools::Itertools;
use std::fmt::Display;

pub fn set_resource_utilisations(executions: &mut Executions) -> Result<()> {
    //discover a resource model
    let resource_model = ResourceModel::from_executions(&executions);

    //find the earliest execution where each execution could start
    let (execution_2_starting, execution_2_started_by) = execution_2_starts(executions);

    //initialise resource marking and result of resource utilisations
    let mut resource_marking = resource_model.empty_resource_marking();
    let mut execution_2_resource_utilisation = vec![None; executions.executions.len()];
    {
        for (execution_i, started_by) in execution_2_started_by.iter().enumerate() {
            if let Some(resource) = executions.executions[execution_i].resource
                && started_by.is_none()
            {
                //set resource utilisation
                let execution = &executions.executions[execution_i];
                execution_2_resource_utilisation[execution_i] = resource_model
                    .resource_utilisation(&resource_marking, execution.fired_transition);

                //set resource marking
                resource_marking.occupy(resource)?;
            }
        }
    }

    for (execution_i, execution) in executions.executions.iter().enumerate() {
        //update resource marking
        {
            //free this resource as it is completed
            if let Some(resource) = execution.resource {
                resource_marking.free(resource)?;
            }

            //compute and set resource utilisation of later executions
            for later_execution_i in &execution_2_starting[execution_i] {
                let later_execution = &executions.executions[*later_execution_i];
                if later_execution.resource.is_some() {
                    execution_2_resource_utilisation[*later_execution_i] = resource_model
                        .resource_utilisation(&resource_marking, later_execution.fired_transition);
                }
            }

            //start later executions
            for later_execution in &execution_2_starting[execution_i] {
                if let Some(resource) = executions.executions[*later_execution].resource {
                    resource_marking.occupy(resource)?;
                }
            }
        }
    }

    //merge resource utilisations
    executions
        .executions
        .iter_mut()
        .zip(execution_2_resource_utilisation)
        .for_each(|(execution, utilisation)| execution.resource_utilisation = utilisation);

    Ok(())
}

/// Returns a map execution -> executions that the execution caused to start.
fn execution_2_starts(executions: &Executions) -> (Vec<Vec<usize>>, Vec<Option<usize>>) {
    let mut execution_2_starting = vec![vec![]; executions.executions.len()];
    let mut execution_2_started_by = vec![None; executions.executions.len()];

    for (execution_i, _) in executions.executions.iter().enumerate() {
        let control_flow_enabling_execution: Option<usize> =
            find_execution_that_enables(executions, execution_i);
        let resource_enabling_execution: Option<usize> =
            find_execution_that_last_used_resource(executions, execution_i);

        let s = match (control_flow_enabling_execution, resource_enabling_execution) {
            (None, None) => None,
            (None, Some(y)) => Some(y),
            (Some(x), None) => Some(x),
            (Some(x), Some(y)) => Some(x.max(y)),
        };

        if let Some(s) = s {
            execution_2_starting[s].push(execution_i);
        };
        execution_2_started_by[execution_i] = s;
    }

    (execution_2_starting, execution_2_started_by)
}

fn find_execution_that_enables(executions: &Executions, execution_i: usize) -> Option<usize> {
    let trace = executions.executions[execution_i].trace;
    let move_index_of_enablement = executions.executions[execution_i].move_index_of_enablement?;

    let mut i = execution_i - 1; //given that there is a move index on which we depend, that move index must have a smaller execution index.
    loop {
        let exe = &executions.executions[i];
        if exe.trace == trace && exe.move_index == move_index_of_enablement {
            return Some(i);
        }
        i -= 1;
    }
}

/// Return the execution before this execution that was performed by the same resource.
fn find_execution_that_last_used_resource(
    executions: &Executions,
    execution_i: usize,
) -> Option<usize> {
    let resource = executions.executions[execution_i].resource?;
    if execution_i == 0 {
        return None;
    }

    let mut i = execution_i - 1;
    loop {
        if let Some(res) = executions.executions[i].resource
            && res == resource
        {
            return Some(i);
        }

        if i > 0 {
            i -= 1;
        } else {
            return None;
        }
    }
}

#[derive(Debug)]
pub struct ResourceModel {
    resource_key: ActivityKey,
    transition_2_resources: Vec<BitVec<usize>>,
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

    pub fn empty_resource_marking(&self) -> ResourceMarking {
        ResourceMarking(vec![false; self.resource_key.get_number_of_activities()])
    }

    fn resource_utilisation(
        &self,
        resource_marking: &ResourceMarking,
        transition: usize,
    ) -> Option<Fraction> {
        let can_execute = self.transition_2_resources[transition].count_ones();
        let occupied = self.transition_2_resources[transition]
            .iter_ones()
            .filter(|res| resource_marking.0[*res])
            .count();

        Some(f!((occupied, can_execute)))
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

#[derive(Debug)]
pub struct ResourceMarking(Vec<bool>);

impl ResourceMarking {
    pub fn free(&mut self, resource: Activity) -> Result<()> {
        if !self.0[resource.id] {
            return Err(anyhow!("Resource {} was not busy.", resource));
        }
        self.0[resource.id] = false;
        Ok(())
    }

    pub fn occupy(&mut self, resource: Activity) -> Result<()> {
        if self.0[resource.id] {
            return Err(anyhow!("Resource {} was already busy.", resource));
        }
        self.0[resource.id] = true;
        Ok(())
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
