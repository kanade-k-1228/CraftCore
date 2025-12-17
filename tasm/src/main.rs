use bimap::BiMap;
use clap::Parser;
use std::collections::HashMap;
use std::fs::File;
use std::io::{BufRead, BufReader};
use tasm::collect::{AsmMap, ConstMap, FuncMap, StaticMap, TypeMap};
use tasm::convert::types::BiMapExt;
use tasm::grammer::lexer::LineLexer;
use tasm::grammer::parser::Parser as TasmParser;

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

    if args.verbose {
        println!("\n=== Collected Information ===\n");
        consts.print();
        types.print();
        statics.print();
        asms.print();
        funcs.print();
    }

    // 4. Generate code from functions and assembly blocks
    let func_codes = tasm::convert::func2code(&ast, &consts, &types, &statics, &funcs);
    let asm_codes = tasm::convert::asm2code(&ast, &asms, &consts);

    // Combine function and assembly codes
    let mut codes: HashMap<String, tasm::convert::Code> = HashMap::new();
    codes.extend(func_codes);
    codes.extend(asm_codes);

    // 5. Allocate global objects
    let mut dmmap: BiMap<String, u16> = BiMap::new();
    let mut pmmap: BiMap<String, u16> = BiMap::new();

    // Allocate program memory for functions and assembly blocks
    let mut pmem_addr = 0x0000u16;
    for (name, code) in &codes {
        pmmap.insert(name.clone(), pmem_addr);
        pmem_addr += code.instructions.len() as u16;
    }

    // Allocate data memory for statics and constants
    let mut dmem_addr = 0x0000u16;
    for (name, (norm_type, _)) in statics.0.iter() {
        dmmap.insert(name.clone(), dmem_addr);
        dmem_addr += norm_type.sizeof() as u16;
    }
    for (name, (norm_type, _, _)) in consts.0.iter() {
        dmmap.insert(name.clone(), dmem_addr);
        dmem_addr += norm_type.sizeof() as u16;
    }

    if args.verbose {
        println!("\n=== Memory Allocation ===");
        println!("Program Memory:");
        pmmap.print();
        println!("Data Memory:");
        dmmap.print();
    }

    // 6. Resolve symbols
    let resolved = tasm::link::resolve_symbols(&codes, &pmmap, &dmmap);

    // 7. Generate binary
    let pbin = tasm::link::generate_program_binary(&resolved, &pmmap).unwrap();
    let dbin = tasm::link::generate_data_binary(&consts, &dmmap).unwrap();

    // 8. Write output to file
    std::fs::write(&args.output, pbin).expect(&format!("Failed to write to file: {}", args.output));

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
