use crate::ebi_objects::{compressed_event_log::CompressedEventLog, event_log::EventLog};

impl From<EventLog> for CompressedEventLog {
    fn from(value: EventLog) -> Self {
        log::info!("Convert event log into compressed event log.");
        Self { log: value }
    }
}
