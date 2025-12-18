use crate::collect::{AsmMap, ConstMap, FuncMap, StaticMap};
use crate::convert::Code;
use crate::link::structs::AsmInst;
use bimap::BiMap;
use color_print::{cprint, cprintln};
use std::collections::HashMap;

pub fn binprint(
    imap: &BiMap<String, u16>,
    dmap: &BiMap<String, u16>,
    codes: &HashMap<String, Code>,
    statics: &StaticMap,
    consts: &ConstMap,
    asms: &AsmMap,
    funcs: &FuncMap,
) {
    // Program Memory Layout
    println!("+-[Inst]-+------------------------------------------------------------------------");
    let mut iblocks: Vec<_> = imap
        .iter()
        .map(|(name, addr)| {
            let code = codes.get(name);
            let size = code.map_or(0, |c| {
                c.instructions
                    .iter()
                    .filter(|line| matches!(line.inst, AsmInst::Inst(_)))
                    .count()
            }) as u16;
            // Get type information
            let (type_info, signature) = if asms.0.contains_key(name) {
                ("asm", String::new())
            } else if let Some((func_type, _)) = funcs.0.get(name) {
                ("func", func_type.fmt())
            } else {
                ("unknown", String::new())
            };

            (name.clone(), *addr, size, type_info, signature, code)
        })
        .collect();
    iblocks.sort_by_key(|(_, addr, _, _, _, _)| *addr);

    for (name, addr, size, kind, signature, code) in iblocks {
        match kind {
            "asm" => cprintln!("+--------+ <red>{}</red>", name),
            "func" => cprintln!("+--------+ <green>{}</green> : {}", name, signature),
            _ => unreachable!(),
        }

        if let Some(code) = code {
            let mut current_addr = addr;
            for line in &code.instructions {
                match &line.inst {
                    AsmInst::Inst(inst) => {
                        let asm_text = inst.cformat();
                        cprintln!("| 0x{:04X} : {}", current_addr, asm_text);
                        current_addr += 1;
                    }
                    AsmInst::Label(label) => {
                        cprintln!("| <m>{}</m>:", label);
                    }
                    AsmInst::Org(new_addr) => {
                        current_addr = *new_addr;
                    }
                }
            }
        } else {
            for a in addr..(addr + size) {
                println!("| 0x{:04X} : ", a);
            }
        }
    }

    // Data Memory Layout
    println!("+-[Data]-+------------------------------------------------------------------------");

    let mut dblocks: Vec<_> = dmap
        .iter()
        .map(|(name, addr)| {
            let (size, type_str, is_static) = if let Some((ty, _)) = statics.0.get(name) {
                (ty.sizeof() as u16, ty.fmt(), true)
            } else if let Some((ty, _, _)) = consts.0.get(name) {
                (ty.sizeof() as u16, ty.fmt(), false)
            } else {
                (0, "unknown".to_string(), false)
            };
            let end_addr = if size > 0 { addr + size - 1 } else { *addr };
            (name.clone(), *addr, end_addr, type_str, is_static)
        })
        .collect();
    dblocks.sort_by_key(|(_, addr, _, _, _)| *addr);

    for (name, addr, end_addr, type_str, is_static) in dblocks {
        cprint!("| 0x{:04X} : 0x{:04X} | ", addr, end_addr);

        // Display colored name
        if is_static {
            cprint!("<cyan>{}</cyan>", name);
        } else {
            cprint!("<yellow>{}</yellow>", name);
        }

        // Display type
        cprintln!(" : {}", type_str);
    }

    println!("+--------+------------------------------------------------------------------------");
}
