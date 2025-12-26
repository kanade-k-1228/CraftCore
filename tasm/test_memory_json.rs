// Test file for Memory JSON serialization/deserialization

use tasm::linker::memory::Memory;

fn main() {
    // Create a Memory instance with sections
    let memory = Memory::new(0, 0x10000)
        .section("const", 0x3000, 0x5000)
        .section("static", 0x5000, 0x10000)
        .section("heap", 0x10000, 0x20000);

    // Serialize to JSON
    match memory.to_json() {
        Ok(json) => {
            println!("Serialized Memory to JSON:");
            println!("{}", json);

            // Deserialize from JSON
            match Memory::from_json(&json) {
                Ok(deserialized) => {
                    println!("\nDeserialized Memory from JSON:");
                    println!("{:?}", deserialized);

                    // Verify they are equal by re-serializing
                    if let Ok(json2) = deserialized.to_json() {
                        println!("\nRe-serialized JSON:");
                        println!("{}", json2);

                        if json == json2 {
                            println!("\n✓ JSON serialization/deserialization is working correctly!");
                        } else {
                            println!("\n✗ JSON serialization/deserialization mismatch!");
                        }
                    }
                }
                Err(e) => eprintln!("Failed to deserialize from JSON: {}", e),
            }
        }
        Err(e) => eprintln!("Failed to serialize to JSON: {}", e),
    }
}