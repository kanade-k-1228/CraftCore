use std::fs;

use clap::Parser;
use indexmap::{IndexMap, IndexSet};

#[derive(Debug, clap::Parser)]
#[clap(author, version, about)]
struct Args {
    /// Input files
    #[clap(default_value = "main.tasm")]
    src: Vec<String>,

    /// Output binary file
    #[clap(short, long, default_value = "main.bin", value_name = "FILE")]
    out: String,

    /// Output rom file
    #[clap(short, long, default_value = "rom.bin", value_name = "FILE")]
    rom: String,

    /// Generate map file
    #[clap(short = 'm', long = "map", num_args = 0..=1, default_missing_value = "map.yaml", value_name = "FILE")]
    map: Option<String>,

    /// Enable verbose output
    #[clap(short, long)]
    verbose: bool,
}

fn main() -> Result<(), tasm::Error> {
    let args = Args::parse();

    // 1. Read source files
    let sources = {
        let mut sources = vec![];
        for path in &args.src {
            let content = fs::read_to_string(path)?;
            sources.push((path, content));
        }
        sources
    };

    // 2. Parse input files and tokenize
    let tokens = {
        let mut tokens = vec![];
        for (path, text) in sources.iter() {
            let toks = tasm::Lexer::new(path, text).parse();
            tokens.extend(toks);
        }
        tokens
    };

    // 3. Parse tokens into AST
    let (ast, errors) = tasm::Parser::new(tokens.into_iter()).parse();
    if !errors.is_empty() {
        for e in &errors {
            eprintln!("error: {}", e);
        }
        std::process::exit(-1);
    }

    // 4. Evaluator Database
    let global = tasm::Global::new(&ast)?;

    // 5. Resolve dependencies from entry points
    let (labels, symbols) =
        global.deps(&["reset", "irq", "main"], IndexSet::new(), IndexSet::new())?;

    // 6-1. Allocate code objects
    let mut ialoc = tasm::Memory::new(0, 0x10000)
        .section("reset", 0x0000, 0x0004)
        .section("irq", 0x0004, 0x0008)
        .section("code", 0x0008, 0x10000)
        .allocator();

    // Pass 1: fixed-address `asm` blocks.
    for name in labels.iter() {
        let code = global.code(name)?;
        if let Some(Some(addr)) = global.get_asm_resolved(name) {
            ialoc.allocate(addr, code.0.len(), name)?;
        }
    }

    // Pass 2: auto-allocate the rest (`asm` without `@`, and any `fn`).
    for name in labels.iter() {
        let code = global.code(name)?;
        if !matches!(global.get_asm_resolved(name), Some(Some(_))) {
            ialoc.section("code", code.0.len(), name)?;
        }
    }

    // 6-2. Allocate data objects
    let mut daloc = tasm::Memory::new(0, 0x10000)
        .section("const", 0x3000, 0x5000)
        .section("static", 0x5000, 0x10000)
        .allocator();

    let (fixed, auto) = global.dataobjs()?;
    for (name, size, addr) in fixed {
        if symbols.contains(name) {
            daloc.allocate(addr, size, name)?;
        }
    }
    for (name, size, section) in auto {
        if symbols.contains(name) {
            daloc.section(section, size, name)?;
        }
    }

    let imap: IndexMap<String, usize> = ialoc.allocations().into_iter().collect();
    let dmap: IndexMap<String, usize> = daloc.allocations().into_iter().collect();

    // 7. Generate binary
    if args.verbose {
        tasm::binprint(&imap, &dmap, &global);
    }
    let main_bin = tasm::genibin(&global, &imap, &dmap)?;
    let const_bin = tasm::gencbin(&global, &dmap)?;
    let symbol_map = tasm::SymbolMap::generate(&global, &imap, &dmap);

    // 9. Write output files
    fs::write(&args.out, main_bin)?;
    fs::write(&args.rom, const_bin)?;
    if let Some(ref file) = args.map {
        fs::write(&file, symbol_map.to_yaml())?;
    }
    Ok(())
}
