use crate::compile::{Code, Imm};
use crate::error::LinkError;
use crate::eval::eval::Evaluator;
use indexmap::IndexMap;

/// Resolve symbols in the code using the memory maps
pub fn resolve_symbols<'a>(
    codes: &IndexMap<&'a str, Code>,
    imap: &IndexMap<String, usize>,
    dmap: &IndexMap<String, usize>,
    evaluator: &Evaluator,
) -> IndexMap<&'a str, Code> {
    let mut resolved = IndexMap::new();

    for (&name, code) in codes {
        let mut resolved_insts = Vec::new();

        // Resolve symbols in each instruction
        for (inst, symbol_opt) in &code.0 {
            if let Some(imm) = symbol_opt {
                match imm {
                    Imm::Symbol(symbol, calc_offset) => {
                        // Simple symbol resolution - the offset has already been calculated in asm2code.rs
                        // The symbol here is the base identifier, and calc_offset is the calculated offset from expressions
                        let addr = evaluator
                            .consts()
                            .get(symbol.as_str())
                            .and_then(|(_, value, _, _)| {
                                // Check if it's a constant - use its value directly
                                if let crate::eval::constexpr::ConstExpr::Number(n) = value {
                                    Some(*n as usize)
                                } else {
                                    None
                                }
                            })
                            .or_else(|| imap.get(symbol).copied())
                            .or_else(|| dmap.get(symbol).copied());

                        if let Some(resolved_addr) = addr {
                            // Update instruction with resolved address
                            // Add the calculated offset to the resolved base address
                            use arch::inst::Inst;
                            let final_addr = (resolved_addr + calc_offset) as u16;
                            let updated_inst = match inst {
                                Inst::LOADI(rd, _) => Inst::LOADI(*rd, final_addr),
                                Inst::STORE(rs2, rs1, _) => Inst::STORE(*rs2, *rs1, final_addr),
                                Inst::LOAD(rd, rs, _) => Inst::LOAD(*rd, *rs, final_addr),
                                Inst::JUMPIF(cond, _) => Inst::JUMPIF(*cond, final_addr),
                                Inst::JUMPIFR(cond, _) => Inst::JUMPIFR(*cond, final_addr),
                                Inst::JUMP(_) => Inst::JUMP(final_addr),
                                Inst::JUMPR(_) => Inst::JUMPR(final_addr),
                                Inst::CALL(_) => Inst::CALL(final_addr),
                                Inst::ADDI(rd, rs, _) => Inst::ADDI(*rd, *rs, final_addr),
                                Inst::SUBI(rd, rs, _) => Inst::SUBI(*rd, *rs, final_addr),
                                Inst::ANDI(rd, rs, _) => Inst::ANDI(*rd, *rs, final_addr),
                                Inst::ORI(rd, rs, _) => Inst::ORI(*rd, *rs, final_addr),
                                Inst::XORI(rd, rs, _) => Inst::XORI(*rd, *rs, final_addr),
                                Inst::EQI(rd, rs, _) => Inst::EQI(*rd, *rs, final_addr),
                                Inst::NEQI(rd, rs, _) => Inst::NEQI(*rd, *rs, final_addr),
                                Inst::LTI(rd, rs, _) => Inst::LTI(*rd, *rs, final_addr),
                                Inst::LTSI(rd, rs, _) => Inst::LTSI(*rd, *rs, final_addr),
                                _ => inst.clone(),
                            };
                            resolved_insts.push((updated_inst, None));
                        } else {
                            // Symbol not found - keep original
                            resolved_insts.push((
                                inst.clone(),
                                Some(Imm::Symbol(symbol.clone(), *calc_offset)),
                            ));
                        }
                    }
                    Imm::Literal(_) => {
                        // Literal values don't need resolution
                        resolved_insts.push((inst.clone(), Some(imm.clone())));
                    }
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

pub fn gencbin(
    evaluator: &Evaluator,
    dmmap: &IndexMap<String, usize>,
) -> Result<Vec<u8>, LinkError> {
    // Find the maximum address to determine binary size
    let max_addr = dmmap
        .iter()
        .filter_map(|(name, addr)| {
            evaluator
                .consts()
                .get(name.as_str())
                .map(|(norm_type, _, _, _)| addr + norm_type.sizeof())
        })
        .max()
        .unwrap_or(0);

    // Create binary with proper size, filled with zeros
    let mut binary = vec![0u8; max_addr];

    // Place each constant at its specified address
    for (name, (_, value, _, _)) in evaluator.consts() {
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
