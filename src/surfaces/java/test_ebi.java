import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;

public class test_ebi {

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
            String log = Files.readString(Path.of("testfiles/aa.slang"), StandardCharsets.UTF_8);
            String info = ebi.information(log);
            System.out.println(info);
            
            //call through polymorphic function
            String result = ebi.callEbi("Ebi information", ".txt", new String[] { log });
            System.out.println(result);
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

}
