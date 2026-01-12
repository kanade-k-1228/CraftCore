use crate::eval::code::{Code, Imm};
use crate::eval::global::Global;
use arch::inst::Inst;
use arch::reg::Reg;
use color_print::{cformat, cprintln};
use indexmap::IndexMap;

/// Format an immediate value with resolved address for symbols
fn format_imm(imm: &Imm, dmap: &IndexMap<String, usize>) -> String {
    match imm {
        Imm::Lit(val) => format!("0x{:04X}", val),
        Imm::Label(name) => name.clone(),
        Imm::Const(name, val) => format!("0x{:04X} ({})", val, name),
        Imm::Symbol(name, offset) => {
            let resolved_addr = dmap.get(name).map_or(0, |&addr| addr + offset);
            format!("0x{:04X} ({}.0x{:04X})", resolved_addr, name, offset)
        }
    }
}

/// Format an instruction with resolved symbol addresses
fn format_inst(inst: &Inst<Reg, Imm>, dmap: &IndexMap<String, usize>) -> String {
    macro_rules! rrr {
        ($name:expr, $rd:expr, $rs1:expr, $rs2:expr) => {
            cformat!("<r>{:<6}</><b>{:<2} {:<2} {:<2}</>", $name, $rd, $rs1, $rs2)
        };
    }

    macro_rules! rri {
        ($name:expr, $rd:expr, $rs1:expr, $imm:expr) => {
            cformat!(
                "<r>{:<6}</><b>{:<2} {:<2} <y>{}</>",
                $name,
                $rd,
                $rs1,
                format_imm($imm, dmap)
            )
        };
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
        Inst::JUMPIF(rs2, imm) => rri!("jumpif", rs2, "", imm),
        Inst::JUMPIFR(rs2, imm) => rri!("jumpifr", rs2, "", imm),
        Inst::JUMP(imm) => rri!("jump", "", "", imm),
        Inst::JUMPR(imm) => rri!("jumpr", "", "", imm),
        Inst::CALL(imm) => rri!("call", "", "", imm),
        Inst::RET() => rrr!("ret", "", "", ""),
        Inst::IRET() => rrr!("iret", "", "", ""),
    }
}

pub fn binprint<'a>(
    imap: &IndexMap<String, usize>,
    dmap: &IndexMap<String, usize>,
    codes: &IndexMap<&'a str, Code>,
    global: &Global<'a>,
) {
    // Program Memory Layout
    let mut iblocks: Vec<_> = imap
        .iter()
        .map(|(name, addr)| {
            let code = codes.get(name.as_str());
            let size = code.map_or(0, |c| c.0.len());
            // Get type information
            let (type_info, signature) = if global.get(name.as_str()).is_some() {
                ("asm", String::new())
            } else if let Some(norm_type) = global.get_func_resolved(name.as_str()) {
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
                let asm_text = format_inst(inst, dmap);
                // Resolve Imm to u16 for now (placeholder - actual resolution would happen in linking)
                let resolved_inst = inst.clone().resolve(|imm| match imm {
                    Imm::Lit(val) => val as u16,
                    Imm::Label(_) => 0, // Placeholder for unresolved labels
                    Imm::Const(_, val) => val as u16,
                    Imm::Symbol(name, offset) => {
                        dmap.get(&name).map_or(0, |&addr| (addr + offset) as u16)
                    }
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
                if let Some((norm_type, _)) = global.get_static_resolved(name.as_str()) {
                    (norm_type.sizeof(), norm_type.fmt(), "static")
                } else if let Some((norm_type, _, _)) = global.get_const_resolved(name.as_str()) {
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
