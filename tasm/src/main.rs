use std::fs;

use clap::Parser;
use indexmap::IndexMap;

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
            eprintln!("  {:?}", e);
        }
        std::process::exit(-1);
    }

    // 4. Evaluator
    let global = tasm::Global::new(&ast)?;

    // 5. Generate code from functions and assembly blocks
    let asms = tasm::asm::asm2code(&global)?;
    let funcs = tasm::func::func2code(&global)?;
    let codes: IndexMap<_, _> = funcs.into_iter().chain(asms).collect();

    // 6. Code dependencies graph
    let deps = tasm::Deps::build(&codes);

    // 7. Collect used objects
    let (used_labels, used_symbols) = deps.entries(&["reset", "irq", "main"]);

    // 8. Allocate objects
    let imem = tasm::Memory::new(0, 0x10000)
        .section("reset", 0x0000, 0x0004)
        .section("irq", 0x0004, 0x0008)
        .section("code", 0x0008, 0x10000);

    let dmem = tasm::Memory::new(0, 0x10000)
        .section("reg", 0x0000, 0x0010)
        .section("csr", 0x0010, 0x0100)
        .section("ioreg", 0x0100, 0x1000)
        .section("vram", 0x1000, 0x3000)
        .section("const", 0x3000, 0x5000)
        .section("static", 0x5000, 0x10000);

    let mut ialoc = tasm::Allocator::new(0, 0x10000);
    let mut daloc = tasm::Allocator::new(0, 0x10000);

    // Allocate instruction items with fixed addresses first
    for (&name, code) in &codes {
        if used_labels.contains(name) {
            if let Some((Some(addr), _)) = global.asms().get(name) {
                ialoc.allocate(*addr, code.0.len(), name)?;
            }
        }
    }

    // Allocate instruction items without fixed addresses
    for (&name, code) in &codes {
        if !used_labels.contains(name) {
            continue;
        }
        let has_fixed = global
            .asms()
            .get(name)
            .is_some_and(|(addr, _)| addr.is_some());
        if !has_fixed {
            ialoc.section(imem.get("code")?, code.0.len(), name)?;
        }
    }

    // Allocate static items with fixed addresses first
    for (name, (norm_type, addr, _)) in global.statics() {
        if used_symbols.contains(name) {
            if let Some(addr) = addr {
                daloc.allocate(addr, norm_type.sizeof(), name)?;
            }
        }
    }

    // Allocate const items with fixed addresses
    for (name, (norm_type, _, addr, _)) in global.consts() {
        if used_symbols.contains(name) {
            if let Some(addr) = addr {
                daloc.allocate(addr, norm_type.sizeof(), name)?;
            }
        }
    }

    // Allocate static items without fixed addresses
    for (name, (norm_type, addr, _)) in global.statics() {
        if used_symbols.contains(name) && addr.is_none() {
            daloc.section(dmem.get("static")?, norm_type.sizeof(), name)?;
        }
    }

    // Allocate const items without fixed addresses
    for (name, (norm_type, _, addr, _)) in global.consts() {
        if used_symbols.contains(name) && addr.is_none() {
            daloc.section(dmem.get("const")?, norm_type.sizeof(), name)?;
        }
    }

    let imap: IndexMap<String, usize> = ialoc.allocations().into_iter().collect();
    let dmap: IndexMap<String, usize> = daloc.allocations().into_iter().collect();

    // 9. Resolve symbols
    let resolved = tasm::resolve_symbols(&codes, &imap, &dmap);
    if args.verbose {
        tasm::binprint(&imap, &dmap, &codes, &global);
    }

    // 10. Generate binary
    let main_bin = tasm::genibin(&resolved, &imap)?;
    let const_bin = tasm::gencbin(&global, &dmap)?;
    let symbol_map = tasm::SymbolMap::generate(&global, &imap, &dmap);

    // 12. Write output files
    fs::write(&args.out, main_bin)?;
    fs::write(&args.rom, const_bin)?;
    if let Some(ref file) = args.map {
        fs::write(&file, symbol_map.to_yaml())?;
    }
    Ok(())
}
