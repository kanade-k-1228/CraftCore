use crate::convert::types::Code;
use crate::error::LinkError;
use crate::symbols::Symbols;
use indexmap::IndexMap;
use std::collections::HashMap;

/// Resolve symbols in the code using the memory maps
pub fn resolve_symbols<'a>(
    codes: &HashMap<&'a str, Code>,
    imap: &IndexMap<String, usize>,
    dmap: &IndexMap<String, usize>,
    symbols: &Symbols,
) -> HashMap<&'a str, Code> {
    let mut resolved = HashMap::new();

    for (&name, code) in codes {
        let mut resolved_insts = Vec::new();

        // Resolve symbols in each instruction
        for (inst, symbol_opt) in &code.0 {
            if let Some(symbol) = symbol_opt {
                // Try to resolve symbol - check constants first (for immediate values),
                // then program memory (functions/labels), then data memory (statics)
                let addr = symbols
                    .consts
                    .0
                    .get(symbol.as_str())
                    .and_then(|(_, const_expr, _, _)| {
                        // Check if it's a constant - use its value directly
                        if let crate::eval::constexpr::ConstExpr::Number(n) = const_expr {
                            Some(*n as usize)
                        } else {
                            None
                        }
                    })
                    .or_else(|| imap.get(symbol).copied())
                    .or_else(|| dmap.get(symbol).copied());

                if let Some(resolved_addr) = addr {
                    // Update instruction with resolved address
                    use arch::inst::Inst;
                    let addr_u16 = resolved_addr as u16;
                    let updated_inst = match inst {
                        Inst::LOADI(rd, _) => Inst::LOADI(*rd, addr_u16),
                        Inst::STORE(rs2, rs1, _) => Inst::STORE(*rs2, *rs1, addr_u16),
                        Inst::LOAD(rd, rs, _) => Inst::LOAD(*rd, *rs, addr_u16),
                        Inst::IF(cond, _) => Inst::IF(*cond, addr_u16),
                        Inst::IFR(cond, _) => Inst::IFR(*cond, addr_u16),
                        Inst::JUMP(_) => Inst::JUMP(addr_u16),
                        Inst::JUMPR(_) => Inst::JUMPR(addr_u16),
                        Inst::CALL(_) => Inst::CALL(addr_u16),
                        Inst::ADDI(rd, rs, _) => Inst::ADDI(*rd, *rs, addr_u16),
                        Inst::SUBI(rd, rs, _) => Inst::SUBI(*rd, *rs, addr_u16),
                        Inst::ANDI(rd, rs, _) => Inst::ANDI(*rd, *rs, addr_u16),
                        Inst::ORI(rd, rs, _) => Inst::ORI(*rd, *rs, addr_u16),
                        Inst::XORI(rd, rs, _) => Inst::XORI(*rd, *rs, addr_u16),
                        Inst::EQI(rd, rs, _) => Inst::EQI(*rd, *rs, addr_u16),
                        Inst::NEQI(rd, rs, _) => Inst::NEQI(*rd, *rs, addr_u16),
                        Inst::LTI(rd, rs, _) => Inst::LTI(*rd, *rs, addr_u16),
                        Inst::LTSI(rd, rs, _) => Inst::LTSI(*rd, *rs, addr_u16),
                        _ => inst.clone(),
                    };
                    resolved_insts.push((updated_inst, None));
                } else {
                    // Symbol not found - keep original
                    resolved_insts.push((inst.clone(), Some(symbol.clone())));
                }
            } else {
                // No symbol to resolve
                resolved_insts.push((inst.clone(), None));
            }
        }

        resolved.insert(name, Code(resolved_insts));
    }

    resolved
}

/// Generate program binary from resolved code
pub fn generate_program_binary<'a>(
    resolved: &HashMap<&'a str, Code>,
    pmmap: &IndexMap<String, usize>,
) -> Result<Vec<u8>, LinkError> {
    let mut binary = Vec::new();

    // Sort codes by their addresses
    let mut sorted_codes: Vec<_> = resolved
        .iter()
        .filter_map(|(&name, code)| pmmap.get(&name.to_string()).map(|addr| (*addr, code)))
        .collect();
    sorted_codes.sort_by_key(|(addr, _)| *addr);

    // Generate binary for each code block
    for (_addr, code) in sorted_codes {
        for (inst, _symbol) in &code.0 {
            // Convert instruction to binary
            let op = inst.clone().to_op();
            let bin = op.to_bin();
            // Convert u32 to bytes (little-endian)
            binary.extend_from_slice(&bin.to_le_bytes());
        }
    }

    Ok(binary)
}

/// Generate data binary from constants
pub fn generate_data_binary(
    symbols: &Symbols,
    dmmap: &IndexMap<String, usize>,
) -> Result<Vec<u8>, LinkError> {
    let mut binary = Vec::new();

    // Sort constants by their addresses
    let mut sorted_consts: Vec<_> = symbols
        .consts
        .0
        .iter()
        .filter_map(|(&name, (_, value, _, _))| dmmap.get(name).map(|addr| (*addr, value)))
        .collect();
    sorted_consts.sort_by_key(|(addr, _)| *addr);

    // Generate binary for each constant
    for (_addr, value) in sorted_consts {
        // Convert constant value to bytes
        // This is simplified - actual implementation would handle different types
        match value {
            crate::eval::constexpr::ConstExpr::Number(n) => {
                binary.extend_from_slice(&(*n as u32).to_le_bytes());
            }
            _ => {
                // Handle other constant types
            }
        }
    }

    Ok(binary)
}
