use crate::convert::types::Code;
use crate::error::LinkError;
use crate::symbols::Symbols;
use indexmap::IndexMap;

/// Resolve symbols in the code using the memory maps
pub fn resolve_symbols<'a>(
    codes: &IndexMap<&'a str, Code>,
    imap: &IndexMap<String, usize>,
    dmap: &IndexMap<String, usize>,
    symbols: &Symbols,
) -> IndexMap<&'a str, Code> {
    let mut resolved = IndexMap::new();

    for (&name, code) in codes {
        let mut resolved_insts = Vec::new();

        // Resolve symbols in each instruction
        for (inst, symbol_opt) in &code.0 {
            if let Some(symbol) = symbol_opt {
                // Try to resolve symbol - check constants first (for immediate values),
                // then program memory (functions/labels), then data memory (statics)
                let addr = symbols
                    .consts()
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

pub fn genibin<'a>(
    codes: &IndexMap<&'a str, Code>,
    pmmap: &IndexMap<String, usize>,
) -> Result<Vec<u8>, LinkError> {
    let max_addr = pmmap
        .iter()
        .filter_map(|(name, addr)| {
            codes.get(name.as_str()).map(|code| {
                let size = code.0.len() * 4; // Each instruction is 4 bytes
                addr + size
            })
        })
        .max()
        .unwrap_or(0);

    // Create binary with proper size, filled with zeros
    let mut binary = vec![0u8; max_addr];

    // Place each code block at its specified address
    for (&name, code) in codes {
        if let Some(&addr) = pmmap.get(&name.to_string()) {
            let mut offset = addr;
            for (inst, _) in &code.0 {
                let op = inst.clone().to_op();
                let bin = op.to_bin();
                let bytes = bin.to_le_bytes();

                // Ensure we don't write past the end of the binary
                if offset + 4 <= binary.len() {
                    binary[offset..offset + 4].copy_from_slice(&bytes);
                    offset += 4;
                }
            }
        }
    }

    Ok(binary)
}

pub fn gencbin(symbols: &Symbols, dmmap: &IndexMap<String, usize>) -> Result<Vec<u8>, LinkError> {
    // Find the maximum address to determine binary size
    let max_addr = dmmap
        .iter()
        .filter_map(|(name, addr)| {
            symbols
                .consts()
                .get(name.as_str())
                .map(|(ty, _, _, _)| addr + ty.sizeof())
        })
        .max()
        .unwrap_or(0);

    // Create binary with proper size, filled with zeros
    let mut binary = vec![0u8; max_addr];

    // Place each constant at its specified address
    for (&name, (_, value, _, _)) in symbols.consts().iter() {
        if let Some(&addr) = dmmap.get(name) {
            let bytes = value.serialize();
            let end = (addr + bytes.len()).min(binary.len());
            if addr < binary.len() {
                binary[addr..end].copy_from_slice(&bytes[..end - addr]);
            }
        }
    }

    Ok(binary)
}
