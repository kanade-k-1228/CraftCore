use crate::error::Error;
use crate::eval::code::Imm;
use crate::eval::global::Global;
use indexmap::IndexMap;

pub fn genibin<'a>(
    global: &'a Global<'a>,
    imap: &IndexMap<String, usize>,
    dmap: &IndexMap<String, usize>,
) -> Result<Vec<u8>, Error> {
    // Calculate max address
    let max_addr = imap
        .iter()
        .filter_map(|(name, &addr)| global.code(name).ok().map(|code| addr + code.0.len() * 4))
        .max()
        .unwrap_or(0);

    let mut binary = vec![0u8; max_addr];

    // Resolve symbols and write to binary
    for (name, &addr) in imap {
        let code = global.code(name)?;
        let mut offset = addr;
        for inst in &code.0 {
            let resolved = inst.clone().resolve(|imm| match imm {
                Imm::Lit(val) => val as u16,
                Imm::Label(label) => imap.get(&label).map_or(0, |&a| a as u16),
                Imm::Const(_, val) => val as u16,
                Imm::Symbol(s, ofs) => dmap.get(&s).map_or(0, |&a| (a + ofs) as u16),
            });
            let bytes = resolved.to_op().to_bin().to_le_bytes();
            if offset + 4 <= binary.len() {
                binary[offset..offset + 4].copy_from_slice(&bytes);
                offset += 4;
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
