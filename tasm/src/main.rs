use std::collections::HashMap;
use std::fs::File;
use std::io::{BufRead, BufReader};

use bimap::BiMap;
use clap::Parser;
use indexmap::IndexMap;

use tasm::allocate::allocator::allocate;
use tasm::grammer::lexer::LineLexer;
use tasm::grammer::parser::Parser as TasmParser;
use tasm::symbols::Symbols;
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
    for (ifile, input) in args.input.iter().enumerate() {
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
    let iallocs = allocate(iitems).expect("Failed to allocate instruction memory");
    let dallocs = allocate(ditems).expect("Failed to allocate data memory");
    let imap: BiMap<String, usize> = iallocs.into_iter().collect();
    let dmap: BiMap<String, usize> = dallocs.into_iter().collect();

    // 7. Resolve symbols
    let resolved = tasm::link::resolve_symbols(&codes, &imap, &dmap, &symbols.consts);
    if args.verbose {
        binprint(&imap, &dmap, &codes, &symbols);
    }

    // 8. Generate binary
    let ibin = tasm::link::generate_program_binary(&resolved, &imap).unwrap();
    let dbin = tasm::link::generate_data_binary(&symbols.consts, &dmap).unwrap();

    // 9. Write output to file
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
