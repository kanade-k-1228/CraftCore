use crate::error::Error;
use crate::eval::code::Imm;
use crate::eval::global::Global;
use indexmap::IndexMap;

pub fn genibin<'a>(
    global: &'a Global<'a>,
    imap: &IndexMap<String, usize>,
    dmap: &IndexMap<String, usize>,
) -> Result<Vec<u8>, Error> {
    let max_addr = imap
        .iter()
        .filter_map(|(name, &addr)| global.code(name).ok().map(|code| (addr + code.0.len()) * 4))
        .max()
        .unwrap_or(0);

    let mut binary = vec![0u8; max_addr];

    // Resolve symbols and write to binary
    for (name, &addr) in imap {
        let code = global.code(name)?;
        let mut offset = addr * 4;
        for inst in &code.0 {
            let resolved = inst.clone().resolve(|imm| match imm {
                Imm::Lit(val) => val as u16,
                Imm::Label(label) => imap.get(&label).map_or(0, |&a| a as u16),
                Imm::Const(_, val) => val as u16,
                Imm::Symbol(s, ofs) => dmap.get(&s).map_or(0, |&a| (a + ofs) as u16),
                Imm::ScopeExit(id) | Imm::ScopeEntry(id) => {
                    panic!("internal error: unpatched scope placeholder (id={})", id)
                }
                Imm::FrameRel(_) => {
                    panic!("internal error: unresolved frame placeholder reached linker")
                }
            });
            let bytes = resolved.encode().to_le_bytes();
            if offset + 4 <= binary.len() {
                binary[offset..offset + 4].copy_from_slice(&bytes);
                offset += 4;
            }
        }
    }

    Ok(binary)
}

pub fn gencbin(global: &Global, dmmap: &IndexMap<String, usize>) -> Result<Vec<u8>, Error> {
    let max_words = dmmap
        .iter()
        .filter_map(|(name, addr)| {
            global
                .get_const_resolved(name.as_str())
                .map(|(norm_type, _, _)| addr + norm_type.sizeof())
        })
        .max()
        .unwrap_or(0);

    let mut words = vec![0u16; max_words];

    for name in global.consts() {
        if let Some((_, value, _)) = global.get_const_resolved(name) {
            if let Some(&addr) = dmmap.get(name) {
                let payload = value.bin();
                let end = (addr + payload.len()).min(words.len());
                if addr < words.len() {
                    words[addr..end].copy_from_slice(&payload[..end - addr]);
                }
            }
        }
    }

    Ok(words.into_iter().flat_map(|w| w.to_le_bytes()).collect())
}
