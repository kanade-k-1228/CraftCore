use crate::error::Error;
use crate::eval::code::Imm;
use crate::eval::global::Global;
use arch::reg::Reg;
use indexmap::IndexMap;

pub fn resolve_symbols<'a>(
    global: &'a Global<'a>,
    imap: &IndexMap<String, usize>,
    dmap: &IndexMap<String, usize>,
) -> Result<IndexMap<String, Vec<arch::inst::Inst<Reg, u16>>>, Error> {
    let mut resolved = IndexMap::new();
    for name in imap.keys() {
        let code = global.code(name)?;
        let mut resolved_insts = Vec::new();
        for inst in &code.0 {
            let resolved_inst = inst.clone().resolve(|imm| match imm {
                Imm::Lit(val) => val as u16,
                Imm::Label(label) => match imap.get(&label) {
                    Some(&addr) => addr as u16,
                    None => todo!("Failed to find label: {}", label),
                },
                Imm::Const(_, val) => val as u16,
                Imm::Symbol(s, offset) => match dmap.get(&s) {
                    Some(addr) => (addr + offset) as u16,
                    None => todo!("Failed to find symbol: {}", s),
                },
            });
            resolved_insts.push(resolved_inst);
        }
        resolved.insert(name.to_string(), resolved_insts);
    }
    Ok(resolved)
}

pub fn genibin(
    codes: &IndexMap<String, Vec<arch::inst::Inst<Reg, u16>>>,
    pmmap: &IndexMap<String, usize>,
) -> Result<Vec<u8>, Error> {
    let max_addr = pmmap
        .iter()
        .filter_map(|(name, addr)| {
            codes.get(name).map(|code| {
                let size = code.len() * 4; // Each instruction is 4 bytes
                addr + size
            })
        })
        .max()
        .unwrap_or(0);

    // Create binary with proper size, filled with zeros
    let mut binary = vec![0u8; max_addr];

    // Place each code block at its specified address
    for (name, code) in codes {
        if let Some(&addr) = pmmap.get(name) {
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

pub fn gencbin(global: &Global, dmmap: &IndexMap<String, usize>) -> Result<Vec<u8>, Error> {
    // Find the maximum address to determine binary size
    let max_addr = dmmap
        .iter()
        .filter_map(|(name, addr)| {
            global
                .get_const_resolved(name.as_str())
                .map(|(norm_type, _, _)| addr + norm_type.sizeof())
        })
        .max()
        .unwrap_or(0);

    // Create binary with proper size, filled with zeros
    let mut binary = vec![0u8; max_addr];

    // Place each constant at its specified address
    for name in global.consts() {
        if let Some((_, value, _)) = global.get_const_resolved(name) {
            if let Some(&addr) = dmmap.get(name) {
                let bytes = value.bin();
                let end = (addr + bytes.len()).min(binary.len());
                if addr < binary.len() {
                    binary[addr..end].copy_from_slice(&bytes[..end - addr]);
                }
            }
        }
    }

    Ok(binary)
}
