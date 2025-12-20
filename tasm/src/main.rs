use std::collections::HashMap;
use std::fs::{self, File};
use std::io::{BufRead, BufReader};

use bimap::BiMap;
use clap::Parser;
use indexmap::IndexMap;

use tasm::allocate::allocator::allocate;
use tasm::gen::maps::{generate_data_map, generate_function_map, generate_static_map};
use tasm::grammer::lexer::LineLexer;
use tasm::grammer::parser::Parser as TasmParser;
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

    // 1. Parse input files and tokenize
    let mut tokens = vec![];
    for (ifile, input) in args.src.iter().enumerate() {
        let file = File::open(&input).expect(&format!("Failed to open file: {}", input));
        let reader = BufReader::new(file);
        for (iline, line) in reader.lines().enumerate() {
            let line = line.expect("Failed to read line");
            let toks = LineLexer::new(&line, ifile, iline).parse();
            tokens.extend(toks);
        }
    }
    let tokens = tokens;

    // 2. Parse tokens into AST
    let (ast, errors) = TasmParser::new(tokens.into_iter()).parse();
    if !errors.is_empty() {
        eprintln!("Parser errors:");
        for e in &errors {
            eprintln!("  {:?}", e);
        }
        std::process::exit(1);
    }

    // 3. Collect symbols
    let symbols = Symbols::collect(&ast).unwrap();

    // 4. Generate code from functions and assembly blocks
    let func_codes = tasm::convert::func2code(&symbols).unwrap();
    let asm_codes = tasm::convert::asm2code(&symbols).unwrap();
    let codes: HashMap<_, _> = func_codes.into_iter().chain(asm_codes).collect();

    // 5. Collect global objects
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

    // 6. Allocate objects
    let iallocs = allocate(iitems, (0, usize::MAX)).expect("Failed to allocate instruction memory");
    let dallocs = allocate(ditems, (0, usize::MAX)).expect("Failed to allocate data memory");
    let imap: BiMap<String, usize> = iallocs.into_iter().collect();
    let dmap: BiMap<String, usize> = dallocs.into_iter().collect();

    // 7. Resolve symbols
    let resolved = tasm::link::resolve_symbols(&codes, &imap, &dmap, &symbols);
    if args.verbose {
        binprint(&imap, &dmap, &codes, &symbols);
    }

    // 8. Generate binary
    let ibin = tasm::link::generate_program_binary(&resolved, &imap).unwrap(); // Instruction binary
    let cbin = tasm::link::generate_data_binary(&symbols, &dmap).unwrap(); // Constant binary

    // 9. Generate map files
    let fmap = generate_function_map(&symbols, &imap);
    let smap = generate_static_map(&symbols, &dmap);
    let dmap_content = generate_data_map(&dmap);

    // 10. Write output to file
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
