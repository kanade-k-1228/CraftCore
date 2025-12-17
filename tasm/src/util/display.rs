use crate::collect::{AsmMap, ConstMap, FuncMap, StaticMap};
use crate::convert::Code;
use bimap::BiMap;
use color_print::{cprint, cprintln};
use std::collections::HashMap;

pub fn print_binary(
    pmmap: &BiMap<String, u16>,
    dmmap: &BiMap<String, u16>,
    codes: &HashMap<String, Code>,
    statics: &StaticMap,
    consts: &ConstMap,
    asms: &AsmMap,
    funcs: &FuncMap,
) {
    // Program Memory Layout
    println!("+-[Inst]----------+---------------------------------------------------------------");

    let mut prog_entries: Vec<_> = pmmap
        .iter()
        .map(|(name, addr)| {
            let code = codes.get(name);
            let size = code.map_or(0, |c| c.instructions.len()) as u16;
            let end_addr = if size > 0 { addr + size - 1 } else { *addr };

            // Get type information
            let (type_info, signature) = if asms.0.contains_key(name) {
                ("asm", String::new())
            } else if let Some((func_type, _)) = funcs.0.get(name) {
                ("func", func_type.format_inline())
            } else {
                ("unknown", String::new())
            };

            (name.clone(), *addr, end_addr, type_info, signature)
        })
        .collect();
    prog_entries.sort_by_key(|(_, addr, _, _, _)| *addr);

    for (name, addr, end_addr, type_info, signature) in prog_entries {
        cprint!("| 0x{:04X} : 0x{:04X} | ", addr, end_addr);

        // Display colored name
        match type_info {
            "asm" => cprint!("<red>{}</red>", name),
            "func" => cprint!("<green>{}</green>", name),
            _ => cprint!("{}", name),
        }

        // Display type signature for functions
        if !signature.is_empty() {
            cprintln!(" : {}", signature);
        } else {
            cprintln!("");
        }
    }

    // Data Memory Layout
    println!("+-[Data]----------+---------------------------------------------------------------");

    let mut data_entries: Vec<_> = dmmap
        .iter()
        .map(|(name, addr)| {
            let (size, type_str, is_static) = if let Some((norm_type, _)) = statics.0.get(name) {
                (norm_type.sizeof() as u16, norm_type.format_inline(), true)
            } else if let Some((norm_type, _, _)) = consts.0.get(name) {
                (norm_type.sizeof() as u16, norm_type.format_inline(), false)
            } else {
                (0, "unknown".to_string(), false)
            };
            let end_addr = if size > 0 { addr + size - 1 } else { *addr };
            (name.clone(), *addr, end_addr, type_str, is_static)
        })
        .collect();
    data_entries.sort_by_key(|(_, addr, _, _, _)| *addr);

    for (name, addr, end_addr, type_str, is_static) in data_entries {
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

    println!("+-----------------+---------------------------------------------------------------");
}
