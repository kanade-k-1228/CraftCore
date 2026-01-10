use crate::compile::{Code, Imm};
use crate::error::Error;
use crate::eval::global::Global;
use arch::reg::Reg;
use indexmap::IndexMap;

pub fn resolve_symbols<'a>(
    codes: &IndexMap<&'a str, Code>,
    imap: &IndexMap<String, usize>,
    dmap: &IndexMap<String, usize>,
) -> IndexMap<&'a str, Vec<arch::inst::Inst<Reg, u16>>> {
    let mut resolved = IndexMap::new();
    for (&name, code) in codes {
        let mut resolved_insts = Vec::new();
        for inst in &code.0 {
            let resolved_inst = inst.clone().resolve(|imm| match imm {
                Imm::Symbol(s, offset) => match dmap.get(&s) {
                    Some(addr) => (addr + offset) as u16,
                    None => todo!("Failed to find symbol: {}", s),
                },
                Imm::Label(label) => match imap.get(&label) {
                    Some(&addr) => addr as u16,
                    None => todo!("Failed to find label: {}", label),
                },
                Imm::Lit(val) => val as u16,
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
