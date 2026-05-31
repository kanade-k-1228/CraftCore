use crate::eval::code::Imm;
use crate::eval::global::Global;
use arch::op::Op;
use arch::reg::Reg;
use color_print::{cformat, cprintln};
use indexmap::IndexMap;

fn fmt_imm(imm: &Imm, dmap: &IndexMap<String, usize>) -> String {
    match imm {
        Imm::Lit(val) => format!("0x{:04X}", val),
        Imm::Label(name) => name.clone(),
        Imm::Const(name, val) => format!("0x{:04X} ({})", val, name),
        Imm::Symbol(name, offset) => {
            let addr = dmap.get(name).map_or(0, |&addr| addr + offset);
            match *offset {
                0 => format!("0x{:04X} ({})", addr, name),
                _ => format!("0x{:04X} ({}.0x{:04X})", addr, name, offset),
            }
        }
        Imm::ScopeExit(id) => format!("<exit scope#{}>", id),
        Imm::ScopeEntry(id) => format!("<entry scope#{}>", id),
    }
}

fn fmt_op(op: &Op<Reg, Imm>, dmap: &IndexMap<String, usize>) -> String {
    match op {
        Op::CALC(alu, rd, rs1, rs2) => cformat!(
            "<r>{:<8}</><b>{:<6} {:<2} {:<2} {:<2}</>",
            "calc",
            format!("{:?}", alu).to_lowercase(),
            rd,
            rs1,
            rs2
        ),
        Op::CALCI(alu, rd, rs, imm) => {
            let imm = fmt_imm(imm, dmap);
            cformat!(
                "<r>{:<8}</><b>{:<6} {:<2} {:<2} <y>{}</>",
                "calci",
                format!("{:?}", alu).to_lowercase(),
                rd,
                rs,
                imm
            )
        }
        Op::LOAD(rd, rs, imm) => {
            let imm = fmt_imm(imm, dmap);
            cformat!(
                "<r>{:<8}</><b>{:<6} {:<2} {:<2} <y>{}</>",
                "load",
                "",
                rd,
                rs,
                imm
            )
        }
        Op::STORE(rs2, rs1, imm) => {
            let imm = fmt_imm(imm, dmap);
            cformat!(
                "<r>{:<8}</><b>{:<6} {:<2} {:<2} <y>{}</>",
                "store",
                "",
                rs2,
                rs1,
                imm
            )
        }
        Op::CTRL(rd, rs1, rs2, imm) => {
            let imm = fmt_imm(imm, dmap);
            cformat!(
                "<r>{:<8}</><b>{:<2} {:<2} {:<2} <y>{}</>",
                "ctrl",
                rd,
                rs1,
                rs2,
                imm
            )
        }
    }
}

fn resolve_imm(imm: Imm, dmap: &IndexMap<String, usize>) -> u16 {
    match imm {
        Imm::Lit(val) => val as u16,
        Imm::Label(_) => 0,
        Imm::Const(_, val) => val as u16,
        Imm::Symbol(name, offset) => dmap.get(&name).map_or(0, |&a| (a + offset) as u16),
        Imm::ScopeExit(_) | Imm::ScopeEntry(_) => 0,
    }
}

pub fn binprint<'a>(
    imap: &IndexMap<String, usize>,
    dmap: &IndexMap<String, usize>,
    global: &'a Global<'a>,
) {
    let sep = format!("{} + {}", "-".repeat(18), "-".repeat(39));

    // Program Memory
    let mut iblocks: Vec<_> = imap.iter().map(|(name, &addr)| (addr, name)).collect();
    iblocks.sort_by_key(|(addr, _)| *addr);

    for (mut addr, name) in iblocks {
        print!("{sep}\r");
        println!("{} + {}", "-".repeat(18), name);

        if let Ok(code) = global.code(name) {
            for op in &code.0 {
                let resolved = op.clone().resolve(|imm| resolve_imm(imm, dmap));
                let bytes = resolved.encode().to_le_bytes();
                cprintln!(
                    "[{:04X}] {:02X} {:02X} {:02X} {:02X} | {}",
                    addr,
                    bytes[0],
                    bytes[1],
                    bytes[2],
                    bytes[3],
                    fmt_op(op, dmap)
                );
                addr += 1;
            }
        }
    }
    println!("{sep}");

    // Data Memory
    let mut dblocks: Vec<_> = dmap
        .iter()
        .filter_map(|(name, &addr)| {
            if let Some((ty, _)) = global.get_static_resolved(name) {
                return Some((addr, name.as_str(), ty.sizeof(), ty.fmt(), "static"));
            }
            if let Some((ty, _, _)) = global.get_const_resolved(name) {
                return Some((addr, name.as_str(), ty.sizeof(), ty.fmt(), "const"));
            }
            None
        })
        .collect();
    dblocks.sort_by_key(|(addr, _, _, _, _)| *addr);

    for (addr, name, size, ty, kind) in dblocks {
        print!("[{:04X}:{:04X}] ", addr, addr + size);
        match kind {
            "static" => cprintln!("<cyan>{}</cyan> : {}", name, ty),
            "const" => cprintln!("<yellow>{}</yellow> : {}", name, ty),
            _ => {}
        }
    }
    println!("{}", "-".repeat(60));
}
