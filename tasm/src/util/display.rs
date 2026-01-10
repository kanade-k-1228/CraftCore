use crate::compile::Code;
use crate::eval::global::Global;
use color_print::cprintln;
use indexmap::IndexMap;

pub fn binprint<'a>(
    imap: &IndexMap<String, usize>,
    dmap: &IndexMap<String, usize>,
    codes: &IndexMap<&'a str, Code>,
    evaluator: &Global<'a>,
) {
    // Program Memory Layout
    let mut iblocks: Vec<_> = imap
        .iter()
        .map(|(name, addr)| {
            let code = codes.get(name.as_str());
            let size = code.map_or(0, |c| c.0.len());
            // Get type information
            let (type_info, signature) = if evaluator.asms().contains_key(name.as_str()) {
                ("asm", String::new())
            } else if let Some((norm_type, _, _)) = evaluator.funcs().get(name.as_str()) {
                ("func", norm_type.fmt())
            } else {
                ("unknown", String::new())
            };

            (name.clone(), *addr, size, type_info, signature, code)
        })
        .collect();
    iblocks.sort_by_key(|(_, addr, _, _, _, _)| *addr);

    for (name, addr, size, kind, ty, code) in iblocks {
        print!("{} + {}\r", "-".repeat(18), "-".repeat(39));
        match kind {
            "asm" => cprintln!("{} + <red>{}</red> ", "-".repeat(18), name),
            "func" => cprintln!("{} + <green>{}</green> : {} ", "-".repeat(18), name, ty),
            _ => unreachable!(),
        }

        if let Some(code) = code {
            let mut current_addr = addr;
            for inst in &code.0 {
                let asm_text = inst.cformat();
                // Resolve Imm to u16 for now (placeholder - actual resolution would happen in linking)
                let resolved_inst = inst.clone().resolve(|imm| match imm {
                    crate::compile::Imm::Literal(val) => val,
                    crate::compile::Imm::Symbol(_, _) => 0, // Placeholder for unresolved symbols
                });
                let bin = resolved_inst.to_op().to_bin();
                let bytes = bin.to_le_bytes();
                cprintln!(
                    "[{:0>4X}] {:0>2X} {:0>2X} {:0>2X} {:0>2X} | {}",
                    current_addr,
                    bytes[0],
                    bytes[1],
                    bytes[2],
                    bytes[3],
                    asm_text
                );
                current_addr += 1;
            }
        } else {
            for a in addr..(addr + size) {
                println!("| 0x{:04X} : ", a);
            }
        }
    }
    println!("{} + {}", "-".repeat(18), "-".repeat(39));

    // Data Memory Layout
    let mut dblocks: Vec<_> = dmap
        .iter()
        .map(|(name, addr)| {
            let (size, ty, kind) =
                if let Some((norm_type, _, _)) = evaluator.statics().get(name.as_str()) {
                    (norm_type.sizeof(), norm_type.fmt(), "static")
                } else if let Some((norm_type, _, _, _)) = evaluator.consts().get(name.as_str()) {
                    (norm_type.sizeof(), norm_type.fmt(), "const")
                } else {
                    (0, "unknown".to_string(), "unknown")
                };
            (kind, name.clone(), *addr, size, ty)
        })
        .collect();
    dblocks.sort_by_key(|(_, _, addr, _, _)| *addr);

    for (kind, name, addr, size, ty) in dblocks {
        print!("[{:04X}:{:04X}] ", addr, addr + size);
        match kind {
            "static" => cprintln!("<cyan>{}</cyan> : {}", name, ty),
            "const" => cprintln!("<yellow>{}</yellow> : {}", name, ty),
            _ => unreachable!(),
        }
    }

    println!("{}", "-".repeat(60));
}
