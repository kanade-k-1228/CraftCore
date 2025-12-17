pub mod structs;

use crate::collect::ConstMap;
use crate::convert::types::Code;
use bimap::BiMap;
use std::collections::HashMap;

/// Resolve symbols in the code using the memory maps
pub fn resolve_symbols(
    codes: &HashMap<String, Code>,
    pmmap: &BiMap<String, u16>,
    dmmap: &BiMap<String, u16>,
) -> HashMap<String, Code> {
    let mut resolved = HashMap::new();

    for (name, code) in codes {
        let mut resolved_code = code.clone();

        // Resolve symbols in each instruction
        for inst in &mut resolved_code.instructions {
            for symbol in &inst.symbols {
                // Try to find symbol in program memory (functions/labels)
                if let Some(_addr) = pmmap.get_by_left(symbol) {
                    // Update instruction with resolved address
                    // This is simplified - actual implementation would update the instruction
                } else if let Some(_addr) = dmmap.get_by_left(symbol) {
                    // Symbol is in data memory
                    // Update instruction with resolved address
                }
                // If symbol not found, it will remain unresolved (error handling needed)
            }
        }

        resolved.insert(name.clone(), resolved_code);
    }

    resolved
}

/// Generate program binary from resolved code
pub fn generate_program_binary(
    resolved: &HashMap<String, Code>,
    pmmap: &BiMap<String, u16>,
) -> Result<Vec<u8>, String> {
    let mut binary = Vec::new();

    // Sort codes by their addresses
    let mut sorted_codes: Vec<_> = resolved
        .iter()
        .filter_map(|(name, code)| pmmap.get_by_left(name).map(|addr| (*addr, code)))
        .collect();
    sorted_codes.sort_by_key(|(addr, _)| *addr);

    // Generate binary for each code block
    for (_addr, code) in sorted_codes {
        for line in &code.instructions {
            if let structs::AsmInst::Inst(inst) = &line.inst {
                // Convert instruction to binary
                let op = inst.clone().to_op();
                let bin = op.to_bin();
                // Convert u32 to bytes (little-endian)
                binary.extend_from_slice(&bin.to_le_bytes());
            }
            // Skip labels and other non-instructions
        }
    }

    Ok(binary)
}

/// Generate data binary from constants
pub fn generate_data_binary(
    consts: &ConstMap,
    dmmap: &BiMap<String, u16>,
) -> Result<Vec<u8>, String> {
    let mut binary = Vec::new();

    // Sort constants by their addresses
    let mut sorted_consts: Vec<_> = consts
        .0
        .iter()
        .filter_map(|(name, (_, value, _))| dmmap.get_by_left(name).map(|addr| (*addr, value)))
        .collect();
    sorted_consts.sort_by_key(|(addr, _)| *addr);

    // Generate binary for each constant
    for (_addr, value) in sorted_consts {
        // Convert constant value to bytes
        // This is simplified - actual implementation would handle different types
        match value {
            crate::eval::constexpr::ConstExpr::Number(n) => {
                binary.extend_from_slice(&(*n as u16).to_le_bytes());
            }
            _ => {
                // Handle other constant types
            }
        }
    }

    Ok(binary)
}
