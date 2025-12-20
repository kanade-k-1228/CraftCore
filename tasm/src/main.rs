use bimap::BiMap;
use clap::Parser;
use indexmap::IndexMap;
use std::collections::HashMap;
use std::fs::File;
use std::io::{BufRead, BufReader};
use tasm::allocate::allocator::allocate;
use tasm::collect::{AsmMap, ConstMap, FuncMap, StaticMap, TypeMap};
use tasm::grammer::lexer::LineLexer;
use tasm::grammer::parser::Parser as TasmParser;
use tasm::util::display::binprint;

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
    let asms = AsmMap::collect(&ast, &consts).unwrap();
    let funcs = FuncMap::collect(&ast, &consts, &types, &statics).unwrap();

    // 4. Generate code from functions and assembly blocks
    let func_codes = tasm::convert::func2code(&ast, &consts, &types, &statics, &funcs);
    let asm_codes = tasm::convert::asm2code(&asms, &consts);

    // Combine function and assembly codes
    let mut codes: HashMap<String, tasm::convert::Code> = HashMap::new();
    codes.extend(func_codes);
    codes.extend(asm_codes);

    // 5. Allocate global objects
    // Prepare instruction memory items for allocation
    let mut inst_items = IndexMap::new();

    // Add assembly blocks with their fixed addresses (first, to maintain priority)
    for (name, (fixed_addr, _def)) in &asms.0 {
        if let Some(code) = codes.get(name) {
            let addr = fixed_addr.map(|a| a as u16);
            // Count only actual instructions
            let inst_count = code.0.len() as u16;
            inst_items.insert(name.clone(), (inst_count, addr));
        }
    }

    // Add functions without fixed addresses
    for (name, code) in &codes {
        if !asms.0.contains_key(name) {
            // Count only actual instructions
            let inst_count = code.0.len() as u16;
            inst_items.insert(name.clone(), (inst_count, None));
        }
    }

    // Prepare data memory items for allocation
    let mut data_items = IndexMap::new();

    // Add statics with their fixed addresses (first, to maintain priority)
    for (name, (norm_type, fixed_addr, _def)) in statics.0.iter() {
        let addr = fixed_addr.map(|a| a as u16);
        data_items.insert(name.clone(), (norm_type.sizeof() as u16, addr));
    }

    // Add constants without fixed addresses
    for (name, (norm_type, _, _, _def)) in consts.0.iter() {
        if !statics.0.contains_key(name) {
            data_items.insert(name.clone(), (norm_type.sizeof() as u16, None));
        }
    }

    // Allocate memory using the allocate function
    let inst_allocations = allocate(inst_items).expect("Failed to allocate instruction memory");
    let data_allocations = allocate(data_items).expect("Failed to allocate data memory");

    // Convert allocations to BiMaps
    let mut imap: BiMap<String, u16> = BiMap::new(); // Inst memory map
    let mut dmap: BiMap<String, u16> = BiMap::new(); // Data memory map

    for (name, addr) in inst_allocations {
        imap.insert(name, addr);
    }
    for (name, addr) in data_allocations {
        dmap.insert(name, addr);
    }

    // Note: Label addresses are no longer tracked in Code type
    // Labels should be resolved during code generation phase

    // 6. Resolve symbols
    let resolved = tasm::link::resolve_symbols(&codes, &imap, &dmap, &consts);

    if args.verbose {
        binprint(&imap, &dmap, &codes, &statics, &consts, &asms, &funcs);
    }

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
