use crate::compile::{Code, Imm};
use crate::error::Error;
use crate::eval::global::Global;
use indexmap::IndexMap;
use arch::reg::Reg;

/// Resolve symbols in the code using the memory maps
/// Returns a map of code blocks with resolved u16 immediates
pub fn resolve_symbols<'a>(
    codes: &IndexMap<&'a str, Code>,
    imap: &IndexMap<String, usize>,
    dmap: &IndexMap<String, usize>,
    evaluator: &Global,
) -> IndexMap<&'a str, Vec<arch::inst::Inst<Reg, u16>>> {
    let mut resolved = IndexMap::new();

    for (&name, code) in codes {
        let mut resolved_insts = Vec::new();

        // Resolve symbols in each instruction
        for inst in &code.0 {
            // Use the resolve method to convert Inst<Reg, Imm> to Inst<Reg, u16>
            let resolved_inst = inst.clone().resolve(|imm| match imm {
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
                        .or_else(|| imap.get(&symbol).copied())
                        .or_else(|| dmap.get(&symbol).copied());

                    if let Some(resolved_addr) = addr {
                        // Add the calculated offset to the resolved base address
                        (resolved_addr + calc_offset) as u16
                    } else {
                        // Symbol not found - use 0 as placeholder
                        // In a real system, this should probably be an error
                        0
                    }
                }
                Imm::Literal(val) => val,
            });
            resolved_insts.push(resolved_inst);
        }

        resolved.insert(name, resolved_insts);
    }

    resolved
}

pub fn genibin<'a>(
    codes: &IndexMap<&'a str, Vec<arch::inst::Inst<Reg, u16>>>,
    pmmap: &IndexMap<String, usize>,
) -> Result<Vec<u8>, Error> {
    let max_addr = pmmap
        .iter()
        .filter_map(|(name, addr)| {
            codes.get(name.as_str()).map(|code| {
                let size = code.len() * 4; // Each instruction is 4 bytes
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
            for inst in code {
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

pub fn gencbin(evaluator: &Global, dmmap: &IndexMap<String, usize>) -> Result<Vec<u8>, Error> {
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
            let bytes = value.bin();
            let end = (addr + bytes.len()).min(binary.len());
            if addr < binary.len() {
                binary[addr..end].copy_from_slice(&bytes[..end - addr]);
            }
        }
    }

    Ok(binary)
}
