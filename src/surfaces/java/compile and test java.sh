#to compile: from the Ebi root:
cargo run --features java -- itself java rust -o src/surfaces/java/java_autogen.rs
cargo run --features java -- itself java java -o src/surfaces/java/org/ebitools/Ebi.java
cargo build --features java --release
cargo build --target x86_64-pc-windows-gnu --release --features java

#to pack: from the src/surfaces/java folder:
cd src/surfaces/java
jar cvf target/Ebi.jar org/ebitools/Ebi.java -C ../../../target/release libebi.so -C ../../../target/x86_64-pc-windows-gnu/release/ ebi.dll

#to run the test: from the src/surfaces/java/target folder:
mkdir target
cd target
cp ../EbiTest.java .
javac -cp '.:Ebi.jar' EbiTest.java 
java -cp '.:Ebi.jar' EbiTest