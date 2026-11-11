use crate::ebi_framework::{ebi_command::EbiCommand, ebi_output::{self, EbiOutput}};
use core::result::Result;
use std::path::PathBuf;
use jni::objects::JString;

pub trait ExportableToJava {
    /// Exports an Ebi Output as its Java equivalent.
    fn export_to_java<'caller>(
        self,
        env: &mut jni::Env<'caller>,
    ) -> Result<JString<'caller>, jni::errors::Error>;
}

impl ExportableToJava for EbiOutput {
    fn export_to_java<'caller>(
        self,
        env: &mut jni::Env<'caller>,
    ) -> Result<JString<'caller>, jni::errors::Error> {
        let mut p = PathBuf::new();
        p.set_file_name("file");
        p.set_extension("txt");
        let exporter = EbiCommand::select_exporter(&self.get_type(), Some(&p), None).unwrap();
        match ebi_output::export_to_string(self, &exporter) {
            Ok(s) => JString::from_str(env, s),
            Err(err) => {
                env.throw(format!("Ebi error: {}", err.to_string().as_str()))?;
                Ok(JString::null())
            },
        }
    }
}
