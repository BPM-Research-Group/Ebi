use crate::ebi_traits::ebi_trait_event_log_event_attributes::EbiTraitEventLogEventAttributes;
use ebi_objects::{
    Executions,
    anyhow::{Result, anyhow},
    ebi_objects::executions::Execution,
};

pub trait TimestampsOrdered {
    /// Checks whether the timestamps are in increasing order.
    fn are_timestamps_ordered(&self) -> Result<bool>;
}

impl TimestampsOrdered for Executions {
    fn are_timestamps_ordered(&self) -> Result<bool> {
        self.executions.are_timestamps_ordered()
    }
}

impl TimestampsOrdered for Vec<Execution> {
    fn are_timestamps_ordered(&self) -> Result<bool> {
        let mut last_timestamp = None;
        for execution in self {
            if let Some(time) = execution.time_of_execution {
                if let Some(lt) = last_timestamp
                    && time < lt
                {
                    log::info!(
                        "A trace contains a timestamp {time} which is before an earlier timestamp {lt}."
                    );
                    return Ok(false);
                }
                last_timestamp = Some(time);
            }
        }
        Ok(true)
    }
}

impl TimestampsOrdered for dyn EbiTraitEventLogEventAttributes {
    fn are_timestamps_ordered(&self) -> Result<bool> {
        for trace in self
            .iter_time_and_events()
            .ok_or_else(|| anyhow!("Log does not have a time attribute."))?
        {
            let mut last_timestamp = None;
            for (_, timestamp) in trace {
                if let Some(time) = timestamp {
                    if let Some(lt) = last_timestamp
                        && time < lt
                    {
                        log::info!(
                            "A trace contains a timestamp {time} which is before an earlier timestamp {lt}."
                        );
                        return Ok(false);
                    }
                    last_timestamp = Some(time);
                }
            }
        }
        Ok(true)
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        ebi_framework::trait_importers::ToEventLogEventAttributesTrait,
        techniques::timestamps_ordered::TimestampsOrdered,
    };
    use ebi_objects::ebi_objects::event_log_event_attributes::EventLogEventAttributes;
    use std::fs::{self};

    #[test]
    fn equal_order() {
        let fin = fs::read_to_string("testfiles/a-b.xes").unwrap();
        let log = fin.parse::<EventLogEventAttributes>().unwrap();
        let d = log.to_event_log_event_attributes_trait();

        assert!(d.are_timestamps_ordered().unwrap())
    }
}
