use std::collections::HashMap;
use std::fs;

use bimap::BiMap;
use clap::Parser;
use indexmap::IndexMap;

use tasm::gen::maps::{generate_data_map, generate_function_map, generate_static_map};
use tasm::grammer::lexer::Lexer;
use tasm::grammer::parser::Parser as TasmParser;
use tasm::linker::allocator::Allocator;
use tasm::linker::binary::{generate_data_binary, generate_program_binary, resolve_symbols};
use tasm::symbols::Symbols;
use tasm::util::display::binprint;

#[derive(Debug, clap::Parser)]
#[clap(author, version, about)]
struct Args {
    /// Input files
    #[clap(default_value = "main.tasm")]
    src: Vec<String>,

    /// Output file
    #[clap(short, long, default_value = "out")]
    out: String,

    /// Enable verbose output
    #[clap(short, long)]
    verbose: bool,
}

fn main() {
    let args = Args::parse();

    // 1. Read source files
    let sources: Vec<(String, String)> = args
        .src
        .iter()
        .map(|path| {
            (
                path.clone(),
                fs::read_to_string(path).expect(&format!("Failed to read file: {}", path)),
            )
        })
        .collect();

    // 2. Parse input files and tokenize
    let tokens = {
        let mut tokens = vec![];
        for (path, text) in sources.iter() {
            let lexer = Lexer::new(path, text);
            tokens.extend(lexer.parse());
        }
        tokens
    };

    // 3. Parse tokens into AST
    let (ast, errors) = TasmParser::new(tokens.into_iter()).parse();
    if !errors.is_empty() {
        eprintln!("Parser errors:");
        for e in &errors {
            eprintln!("  {:?}", e);
        }
        std::process::exit(1);
    }

    // 4. Collect symbols
    let symbols = Symbols::collect(&ast).unwrap();

    // 5. Generate code from functions and assembly blocks
    let func_codes = tasm::convert::func2code(&symbols).unwrap();
    let asm_codes = tasm::convert::asm2code(&symbols).unwrap();
    let codes: HashMap<_, _> = func_codes.into_iter().chain(asm_codes).collect();

    // 6. Collect global objects
    // TODO: Remove unused objects
    let iitems = {
        let mut iitems = IndexMap::new();
        for (&name, (addr, _)) in &symbols.asms.0 {
            if let Some(code) = codes.get(name) {
                iitems.insert(name.to_string(), (code.0.len(), *addr));
            }
        }
        for (&name, code) in &codes {
            if !symbols.asms.0.contains_key(name) {
                iitems.insert(name.to_string(), (code.0.len(), None));
            }
        }
        iitems
    };
    let ditems = {
        let mut ditems = IndexMap::new();
        for (&name, (ty, addr, _)) in symbols.statics.0.iter() {
            ditems.insert(name.to_string(), (ty.sizeof(), *addr));
        }
        for (&name, (ty, _, _, _)) in symbols.consts.0.iter() {
            if !symbols.statics.0.contains_key(name) {
                ditems.insert(name.to_string(), (ty.sizeof(), None));
            }
        }
        ditems
    };

    // 7. Allocate objects
    let mut iallocator = Allocator::new(0, usize::MAX);
    let mut dallocator = Allocator::new(0, usize::MAX);

    // Allocate items with fixed addresses first
    for (name, (size, addr)) in &iitems {
        if let Some(addr) = addr {
            iallocator
                .allocate(*addr, *size, name)
                .expect("Failed to allocate instruction memory");
        }
    }
    for (name, (size, addr)) in &ditems {
        if let Some(addr) = addr {
            dallocator
                .allocate(*addr, *size, name)
                .expect("Failed to allocate data memory");
        }
    }

    // Allocate items without fixed addresses using section
    for (name, (size, addr)) in &iitems {
        if addr.is_none() {
            iallocator
                .section(0, usize::MAX, *size, name)
                .expect("Failed to allocate instruction memory");
        }
    }
    for (name, (size, addr)) in &ditems {
        if addr.is_none() {
            dallocator
                .section(0, usize::MAX, *size, name)
                .expect("Failed to allocate data memory");
        }
    }

    let imap: BiMap<String, usize> = iallocator.allocations().into_iter().collect();
    let dmap: BiMap<String, usize> = dallocator.allocations().into_iter().collect();

    // 8. Resolve symbols
    let resolved = resolve_symbols(&codes, &imap, &dmap, &symbols);
    if args.verbose {
        binprint(&imap, &dmap, &codes, &symbols);
    }

    // 9. Generate binary
    let ibin = generate_program_binary(&resolved, &imap).unwrap(); // Instruction binary
    let cbin = generate_data_binary(&symbols, &dmap).unwrap(); // Constant binary

    // 10. Generate map files
    let fmap = generate_function_map(&symbols, &imap);
    let smap = generate_static_map(&symbols, &dmap);
    let dmap_content = generate_data_map(&dmap);

    // 11. Write output to file
    fs::create_dir(&args.out).expect(&format!("Failed to create directory: {}", args.out));
    fs::write(&format!("{}/i.bin", args.out), ibin)
        .expect(&format!("Failed to write to file: {}/i.bin", args.out));
    fs::write(&format!("{}/c.bin", args.out), cbin)
        .expect(&format!("Failed to write to file: {}/c.bin", args.out));
    fs::write(&format!("{}/f.yaml", args.out), fmap)
        .expect(&format!("Failed to write to file: {}/f.yaml", args.out));
    fs::write(&format!("{}/s.yaml", args.out), smap)
        .expect(&format!("Failed to write to file: {}/s.yaml", args.out));
    fs::write(&format!("{}/d.yaml", args.out), dmap_content)
        .expect(&format!("Failed to write to file: {}/d.yaml", args.out));
}
