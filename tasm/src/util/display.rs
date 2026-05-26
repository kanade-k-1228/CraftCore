use crate::eval::code::Imm;
use crate::eval::global::Global;
use arch::inst::Inst;
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

fn fmt_inst(inst: &Inst<Reg, Imm>, dmap: &IndexMap<String, usize>) -> String {
    macro_rules! rrr {
        ($name:expr, $rd:expr, $rs1:expr, $rs2:expr) => {{
            cformat!("<r>{:<8}</><b>{:<2} {:<2} {:<2}</>", $name, $rd, $rs1, $rs2)
        }};
    }

    macro_rules! rri {
        ($name:expr, $rd:expr, $rs1:expr, $imm:expr) => {{
            let imm = fmt_imm($imm, dmap);
            cformat!("<r>{:<8}</><b>{:<2} {:<2} <y>{}</>", $name, $rd, $rs1, imm)
        }};
    }

    match inst {
        Inst::ADD(rd, rs1, rs2) => rrr!("add", rd, rs1, rs2),
        Inst::SUB(rd, rs1, rs2) => rrr!("sub", rd, rs1, rs2),
        Inst::AND(rd, rs1, rs2) => rrr!("and", rd, rs1, rs2),
        Inst::OR(rd, rs1, rs2) => rrr!("or", rd, rs1, rs2),
        Inst::XOR(rd, rs1, rs2) => rrr!("xor", rd, rs1, rs2),
        Inst::EQ(rd, rs1, rs2) => rrr!("eq", rd, rs1, rs2),
        Inst::NEQ(rd, rs1, rs2) => rrr!("neq", rd, rs1, rs2),
        Inst::LT(rd, rs1, rs2) => rrr!("lt", rd, rs1, rs2),
        Inst::LTS(rd, rs1, rs2) => rrr!("lts", rd, rs1, rs2),
        Inst::SR(rd, rs1) => rrr!("sr", rd, rs1, ""),
        Inst::SRS(rd, rs1) => rrr!("srs", rd, rs1, ""),
        Inst::SRR(rd, rs1) => rrr!("srr", rd, rs1, ""),
        Inst::SL(rd, rs1) => rrr!("sl", rd, rs1, ""),
        Inst::SLR(rd, rs1) => rrr!("slr", rd, rs1, ""),
        Inst::NOP() => rrr!("nop", "", "", ""),
        Inst::MOV(rd, rs1) => rrr!("mov", rd, rs1, ""),
        Inst::ADDI(rd, rs1, imm) => rri!("addi", rd, rs1, imm),
        Inst::SUBI(rd, rs1, imm) => rri!("subi", rd, rs1, imm),
        Inst::ANDI(rd, rs1, imm) => rri!("andi", rd, rs1, imm),
        Inst::ORI(rd, rs1, imm) => rri!("ori", rd, rs1, imm),
        Inst::XORI(rd, rs1, imm) => rri!("xori", rd, rs1, imm),
        Inst::EQI(rd, rs1, imm) => rri!("eqi", rd, rs1, imm),
        Inst::NEQI(rd, rs1, imm) => rri!("neqi", rd, rs1, imm),
        Inst::LTI(rd, rs1, imm) => rri!("lti", rd, rs1, imm),
        Inst::LTSI(rd, rs1, imm) => rri!("ltsi", rd, rs1, imm),
        Inst::NOT(rd, rs1) => rrr!("not", rd, rs1, ""),
        Inst::LOADI(rd, imm) => rri!("loadi", rd, "", imm),
        Inst::LOAD(rd, rs1, imm) => rri!("load", rd, rs1, imm),
        Inst::STORE(rs2, rs1, imm) => rri!("store", rs2, rs1, imm),
        Inst::JUMPIF(rs2, imm) => rri!("jumpif", "", rs2, imm),
        Inst::JUMPIFR(rs2, imm) => rri!("jumpifr", "", rs2, imm),
        Inst::JUMP(imm) => rri!("jump", "", "", imm),
        Inst::JUMPR(imm) => rri!("jumpr", "", "", imm),
        Inst::CALL(imm) => rri!("call", "", "", imm),
        Inst::CALLR(reg) => rrr!("callr", "", reg, ""),
        Inst::RET() => rrr!("ret", "", "", ""),
        Inst::IRET() => rrr!("iret", "", "", ""),
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
            for inst in &code.0 {
                let resolved = inst.clone().resolve(|imm| resolve_imm(imm, dmap));
                let bytes = resolved.to_op().to_bin().to_le_bytes();
                cprintln!(
                    "[{:04X}] {:02X} {:02X} {:02X} {:02X} | {}",
                    addr,
                    bytes[0],
                    bytes[1],
                    bytes[2],
                    bytes[3],
                    fmt_inst(inst, dmap)
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
