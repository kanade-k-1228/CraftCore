use bimap::BiMap;
use clap::Parser;
use color_print::{cprint, cprintln};
use std::collections::HashMap;
use std::fs::File;
use std::io::{BufRead, BufReader};
use tasm::collect::{AsmMap, ConstMap, FuncMap, StaticMap, TypeMap};
use tasm::grammer::lexer::LineLexer;
use tasm::grammer::parser::Parser as TasmParser;

fn print_detailed_memory_layout(
    pmmap: &BiMap<String, u16>,
    dmmap: &BiMap<String, u16>,
    codes: &HashMap<String, tasm::convert::Code>,
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

#[derive(Debug, clap::Parser)]
#[clap(author, version, about)]
struct Args {
    /// Input files
    #[clap(default_value = "main.tasm")]
    input: Vec<String>,

    /// Output file
    #[clap(short, long, default_value = "main.bin")]
    output: String,

    /// Enable verbose output
    #[clap(short, long)]
    verbose: bool,
}

fn main() {
    let args = Args::parse();

    // 1. Parse input files and tokenize
    let mut tokens = vec![];
    for (file_idx, input) in args.input.iter().enumerate() {
        let file = File::open(&input).expect(&format!("Failed to open file: {}", input));
        let reader = BufReader::new(file);
        for (line_idx, line) in reader.lines().enumerate() {
            let line = line.expect("Failed to read line");
            let toks = LineLexer::new(&line, file_idx, line_idx).parse();
            tokens.extend(toks);
        }
    }

    // 2. Parse tokens into AST
    let (ast, errors) = TasmParser::new(tokens.into_iter()).parse();
    if !errors.is_empty() {
        eprintln!("Parser errors:");
        for e in &errors {
            eprintln!("  {:?}", e);
        }
        std::process::exit(1);
    }

    // 3. Collect global symbols
    let consts = ConstMap::collect(&ast).unwrap();
    let types = TypeMap::collect(&ast, &consts).unwrap();
    let statics = StaticMap::collect(&ast, &consts, &types).unwrap();
    let asms = AsmMap::collect(&ast).unwrap();
    let funcs = FuncMap::collect(&ast, &consts, &types, &statics).unwrap();

    // 4. Generate code from functions and assembly blocks
    let func_codes = tasm::convert::func2code(&ast, &consts, &types, &statics, &funcs);
    let asm_codes = tasm::convert::asm2code(&ast, &asms, &consts);

    // Combine function and assembly codes
    let mut codes: HashMap<String, tasm::convert::Code> = HashMap::new();
    codes.extend(func_codes);
    codes.extend(asm_codes);

    // 5. Allocate global objects
    let mut imap: BiMap<String, u16> = BiMap::new(); // Inst memory map
    let mut dmap: BiMap<String, u16> = BiMap::new(); // Data memory map

    // Allocate program memory for functions and assembly blocks
    let mut pmem_addr = 0x0000u16;
    for (name, code) in &codes {
        imap.insert(name.clone(), pmem_addr);
        pmem_addr += code.instructions.len() as u16;
    }

    // Allocate data memory for statics and constants
    let mut dmem_addr = 0x0000u16;
    for (name, (norm_type, _)) in statics.0.iter() {
        dmap.insert(name.clone(), dmem_addr);
        dmem_addr += norm_type.sizeof() as u16;
    }
    for (name, (norm_type, _, _)) in consts.0.iter() {
        dmap.insert(name.clone(), dmem_addr);
        dmem_addr += norm_type.sizeof() as u16;
    }

    if args.verbose {
        print_detailed_memory_layout(&imap, &dmap, &codes, &statics, &consts, &asms, &funcs);
    }

    // 6. Resolve symbols
    let resolved = tasm::link::resolve_symbols(&codes, &imap, &dmap);

    // 7. Generate binary
    let ibin = tasm::link::generate_program_binary(&resolved, &imap).unwrap();
    let dbin = tasm::link::generate_data_binary(&consts, &dmap).unwrap();

    // 8. Write output to file
    std::fs::write(&args.output, ibin).expect(&format!("Failed to write to file: {}", args.output));

    // Write data binary if exists
    if !dbin.is_empty() {
        let data_output = args.output.replace(".bin", ".data.bin");
        std::fs::write(&data_output, dbin)
            .expect(&format!("Failed to write data file: {}", data_output));

        println!(
            "Successfully compiled {} to {} and {}",
            args.input.join(", "),
            args.output,
            data_output
        );
    } else {
        println!(
            "Successfully compiled {} to {}",
            args.input.join(", "),
            args.output
        );
    }
}
