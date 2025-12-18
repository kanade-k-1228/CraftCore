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
    consts: &ConstMap,
) -> HashMap<String, Code> {
    let mut resolved = HashMap::new();

    for (name, code) in codes {
        let mut resolved_code = code.clone();

        // Resolve symbols in each instruction
        for inst in &mut resolved_code.instructions {
            if inst.symbols.is_empty() {
                continue;
            }

            // Get the symbol (assuming one symbol per instruction)
            let symbol = &inst.symbols[0];

            // Try to resolve symbol - check constants first (for immediate values),
            // then program memory (functions/labels), then data memory (statics)
            let addr = consts.0.get(symbol).and_then(|(_, const_expr, _)| {
                    // Check if it's a constant - use its value directly
                    if let crate::eval::constexpr::ConstExpr::Number(n) = const_expr {
                        Some(*n as u16)
                    } else {
                        None
                    }
                })
                .or_else(|| pmmap.get_by_left(symbol).copied())
                .or_else(|| dmmap.get_by_left(symbol).copied());

            if let Some(resolved_addr) = addr {
                // Update instruction with resolved address
                if let structs::AsmInst::Inst(inner_inst) = &inst.inst {
                    use arch::inst::Inst;
                    let updated_inst = match inner_inst {
                        Inst::LOADI(rd, _) => Inst::LOADI(*rd, resolved_addr),
                        Inst::STORE(rs2, rs1, _) => Inst::STORE(*rs2, *rs1, resolved_addr),
                        Inst::LOAD(rd, rs, _) => Inst::LOAD(*rd, *rs, resolved_addr),
                        Inst::IF(cond, _) => Inst::IF(*cond, resolved_addr),
                        Inst::IFR(cond, _) => Inst::IFR(*cond, resolved_addr),
                        Inst::JUMP(_) => Inst::JUMP(resolved_addr),
                        Inst::JUMPR(_) => Inst::JUMPR(resolved_addr),
                        Inst::CALL(_) => Inst::CALL(resolved_addr),
                        Inst::ADDI(rd, rs, _) => Inst::ADDI(*rd, *rs, resolved_addr),
                        Inst::SUBI(rd, rs, _) => Inst::SUBI(*rd, *rs, resolved_addr),
                        Inst::ANDI(rd, rs, _) => Inst::ANDI(*rd, *rs, resolved_addr),
                        Inst::ORI(rd, rs, _) => Inst::ORI(*rd, *rs, resolved_addr),
                        Inst::XORI(rd, rs, _) => Inst::XORI(*rd, *rs, resolved_addr),
                        Inst::EQI(rd, rs, _) => Inst::EQI(*rd, *rs, resolved_addr),
                        Inst::NEQI(rd, rs, _) => Inst::NEQI(*rd, *rs, resolved_addr),
                        Inst::LTI(rd, rs, _) => Inst::LTI(*rd, *rs, resolved_addr),
                        Inst::LTSI(rd, rs, _) => Inst::LTSI(*rd, *rs, resolved_addr),
                        _ => inner_inst.clone(),
                    };
                    inst.inst = structs::AsmInst::Inst(updated_inst);
                    // Clear symbols after resolution
                    inst.symbols.clear();
                }
            }
            // If symbol not found, it will remain unresolved (error handling needed)
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
