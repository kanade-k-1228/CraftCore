use super::structs::*;
use std::collections::HashMap;

/// Generate binary from allocated sections
pub fn generate(allocated: &Allocated) -> Result<Vec<u8>, String> {
    // Create a memory map (address -> data)
    let mut memory: HashMap<usize, Vec<u8>> = HashMap::new();

    for section in &allocated.sections {
        match &section.data {
            SectionData::Data(size) => {
                // Reserve space for data (initialized to zero)
                let data = vec![0u8; size * 4]; // 4 bytes per word
                memory.insert(section.addr, data);
            }
            SectionData::Code(code) => {
                let mut code_bytes = Vec::new();

                for line in code {
                    // Resolve symbols in the instruction
                    let inst = resolve_instruction(&line, &allocated.symbols)?;

                    // Convert instruction to binary
                    let binary = inst_to_binary(&inst)?;

                    // Add to code bytes (little-endian, 4 bytes per instruction)
                    code_bytes.push((binary & 0xFF) as u8);
                    code_bytes.push(((binary >> 8) & 0xFF) as u8);
                    code_bytes.push(((binary >> 16) & 0xFF) as u8);
                    code_bytes.push(((binary >> 24) & 0xFF) as u8);
                }

                memory.insert(section.addr, code_bytes);
            }
        }
    }

    // Flatten memory into a contiguous binary
    // Find the range of addresses
    let mut min_addr = usize::MAX;
    let mut max_addr = 0;

    for (addr, data) in &memory {
        min_addr = min_addr.min(*addr);
        max_addr = max_addr.max(*addr + data.len() / 4);
    }

    // Create output binary
    let size = (max_addr - min_addr) * 4;
    let mut output = vec![0u8; size];

    // Copy sections into output
    for (addr, data) in memory {
        let offset = (addr - min_addr) * 4;
        output[offset..offset + data.len()].copy_from_slice(&data);
    }

    Ok(output)
}

/// Resolve symbols in an instruction
fn resolve_instruction(
    line: &AsmLine,
    _symbols: &HashMap<String, usize>,
) -> Result<AsmInst, String> {
    // For now, we'll just return the instruction as-is
    // In a full implementation, we would:
    // 1. Check if the instruction references any symbols
    // 2. Look up symbol addresses in the symbol table
    // 3. Patch the instruction with the resolved address

    // This is a simplified version - symbol resolution would happen here
    Ok(line.inst.clone())
}

/// Convert instruction to 32-bit binary
fn inst_to_binary(inst: &AsmInst) -> Result<u32, String> {
    match inst {
        AsmInst::Inst(i) => {
            // Convert instruction to Op, then to binary
            let op = i.clone().to_op();
            Ok(op.to_bin())
        }
        AsmInst::Label(_) => {
            // Labels don't generate code
            Ok(0)
        }
        AsmInst::Org(_) => {
            // Org directives don't generate code
            Ok(0)
        }
    }
}
