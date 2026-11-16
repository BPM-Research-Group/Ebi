package org.processmining.ebi;
//to compile: from the Ebi root:
// cargo run --features java -- itself java rust -o src/surfaces/java/java_autogen.rs
// cargo build --features java
//to run: from the surfaces/java folder:
// javac org/processmining/ebi/TestEbi.java
// java -Djava.library.path="../../../target/debug" org.processmining.ebi.TestEbi

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;

public class TestEbi {

    // static native String callEbi(String command_name, String output_format, String[] inputs);

    // static native String information(String input);

    // static {
    //     try {
    //         Path p = Paths.get("target/debug/libebi.so");
    //         System.load(p.toAbsolutePath().toString());
    //         // System.loadLibrary("ebi");
    //         System.out.println("Ebi library loaded");
    //     } catch (Exception e) {
    //         e.printStackTrace();
    //     }
    // }

    public static void main(String args[]) {
        try {
            //call directly
            String log = Files.readString(Path.of("../../../testfiles/aa.slang"), StandardCharsets.UTF_8);
            String info = Ebi.information(".txt", log);
            System.out.println(info);
            
            //call through polymorphic function
            String result = Ebi.callEbi("Ebi information", ".txt", new String[] { log });
            System.out.println(result);
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

}
