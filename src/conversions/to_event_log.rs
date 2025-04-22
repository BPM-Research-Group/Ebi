use crate::ebi_objects::{compressed_event_log::CompressedEventLog, event_log::EventLog};

impl From<CompressedEventLog> for EventLog {
    fn from(value: CompressedEventLog) -> Self {
        log::info!("Convert compressed event log into event log.");
        value.log
    }
}