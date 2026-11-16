use crate::{
    ebi_framework::{
        ebi_command::{EbiCommand, EBI_COMMANDS},
        ebi_output::EbiOutput,
    },
    surfaces::java::java_link::java_function_name,
};
use ebi_objects::anyhow::Result;
use inflector::Inflector;

pub fn generate_java_module_java() -> Result<EbiOutput> {
    let mut functions = String::new();
    for path in EBI_COMMANDS.get_command_paths() {
        if path.last().unwrap().is_in_java() {
            let body = ebi_command_to_java_function(&path)?;
            functions.push_str(&body);
            functions.push_str("\n");
        }
    }

    Ok(EbiOutput::String(format!(
        r###"// This file has been automatically generated. Manual changes will be overridden.

package org.ebitools;

import java.io.File;
import java.io.FileOutputStream;
import java.io.InputStream;
import java.io.OutputStream;
import java.util.Date;

public class Ebi {{
    static {{
		try {{
			// load from path initially
			System.loadLibrary("ebi");
			System.out.println("Ebi library loaded from path.");
        }} catch (UnsatisfiedLinkError e) {{
			// if that's not available, load from JAR
			try {{
				String path = "Ebi_" + new Date().getTime();
				String os = System.getProperty("os.name").toLowerCase();
				InputStream in;
				File fileOut;
				if (os.contains("win")) {{
					// windows
					in = Ebi.class.getResourceAsStream("/ebi.dll");
					fileOut = new File(System.getProperty("java.io.tmpdir") + "/" + path + ".dll");
                }} else {{
					// linux
					in = Ebi.class.getResourceAsStream("/libebi.so");
					fileOut = new File(System.getProperty("java.io.tmpdir") + "/" + path + ".so");
                }}
				OutputStream out = new FileOutputStream(fileOut);
				in.transferTo(out);
				in.close();
				out.close();
				System.load(fileOut.getAbsolutePath());
				System.out.println("Ebi library loaded from jar.");
				fileOut.deleteOnExit();
            }} catch (Exception f) {{
				System.out.println("Could not load the Ebi library.");
				f.printStackTrace();
            }}
        }}
    }}

    public static native String callEbi(String command_name, String output_format, String[] inputs);

{functions}
}}"###
    )))
}

fn ebi_command_to_java_function(path: &Vec<&EbiCommand>) -> Result<String> {
    let fn_name = java_function_name(path).to_camel_case();

    // Extract input_types
    let input_types = if let EbiCommand::Command { input_types, .. } = path[path.len() - 1] {
        input_types
    } else {
        return Ok(String::new());
    };
    let args = (0..input_types.len())
        .map(|i| format!("String arg{}", i))
        .collect::<Vec<_>>()
        .join(", ");

    Ok(format!("\tpublic static native String {fn_name}(String output_format, {args});"))
}
