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
    let mut sources = Vec::new();
    for path in &args.src {
        let content = fs::read_to_string(path)?;
        sources.push((path.clone(), content));
    }

    // 2. Parse input files and tokenize
    let tokens = {
        let mut tokens = vec![];
        for (path, text) in sources.iter() {
            let lexer = tasm::Lexer::new(path, text);
            tokens.extend(lexer.parse());
        }
        tokens
    };

    // 3. Parse tokens into AST
    let (ast, errors) = tasm::Parser::new(tokens.into_iter()).parse();
    if !errors.is_empty() {
        for e in &errors {
            eprintln!("  {:?}", e);
        }
        std::process::exit(1);
    }

    // 4. Collect symbols
    let evaluator = match tasm::Evaluator::new(&ast) {
        Ok(s) => s,
        Err(e) => {
            eprintln!("  {:?}", e);
            std::process::exit(1);
        }
    };

    // 5. Generate code from functions and assembly blocks
    let funcs = tasm::func2code(&evaluator)?;
    let asms = tasm::asm2code(&evaluator)?;
    let codes: IndexMap<_, _> = funcs.into_iter().chain(asms).collect();

    // 6. Dead code elimination
    let deps = tasm::dependency(&codes);
    let used = tasm::search(deps.clone(), vec!["reset", "irq", "main"]);
    if args.verbose {
        tasm::print_deps(&deps, &used);
    }

    // 7. Collect global objects
    let iitems = {
        let mut iitems = IndexMap::new();
        for (name, (address, _)) in evaluator.asms() {
            // Only include if reachable
            if used.contains(name) {
                if let Some(code) = codes.get(name) {
                    iitems.insert(name.to_string(), (code.0.len(), address));
                }
            }
        }
        for (&name, code) in &codes {
            // Only include if reachable
            if used.contains(name) && !evaluator.asms().contains_key(name) {
                iitems.insert(name.to_string(), (code.0.len(), None));
            }
        }
        iitems
    };
    let ditems = {
        let mut ditems = IndexMap::new();
        for (name, (norm_type, address, _)) in evaluator.statics() {
            // Only include if reachable (data items referenced from reachable code)
            if used.contains(name) {
                ditems.insert(name.to_string(), (norm_type.sizeof(), address));
            }
        }
        for (name, (norm_type, _, address, _)) in evaluator.consts() {
            // Only include if reachable
            if used.contains(name) && !evaluator.statics().contains_key(name) {
                ditems.insert(name.to_string(), (norm_type.sizeof(), address));
            }
        }
        ditems
    };

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

    // Allocate items with fixed addresses first (for both instruction and data)
    for (name, (size, addr)) in &iitems {
        if let Some(addr) = addr {
            ialoc.allocate(*addr, *size, name)?;
        }
    }
    for (name, (size, addr)) in &ditems {
        if let Some(addr) = addr {
            daloc.allocate(*addr, *size, name)?;
        }
    }

    // Allocate instruction items without fixed addresses
    for (name, (size, addr)) in &iitems {
        if addr.is_none() {
            ialoc.section(imem.get("code")?, *size, name)?;
        }
    }

    // Allocate data items without fixed addresses to appropriate sections
    // First, allocate consts to const section
    for (name, (_, _, _addr, _)) in evaluator.consts() {
        if let Some((size, ditem_addr)) = ditems.get(name) {
            if ditem_addr.is_none() {
                daloc.section(dmem.get("const")?, *size, name)?;
            }
        }
    }

    for (name, (_, address, _)) in evaluator.statics() {
        if address.is_none() {
            if let Some((size, _)) = ditems.get(name) {
                daloc.section(dmem.get("static")?, *size, name)?;
            }
        }
    }

    let imap: IndexMap<String, usize> = ialoc.allocations().into_iter().collect();
    let dmap: IndexMap<String, usize> = daloc.allocations().into_iter().collect();

    // 9. Resolve symbols
    let resolved = tasm::resolve_symbols(&codes, &imap, &dmap, &evaluator);
    if args.verbose {
        tasm::binprint(&imap, &dmap, &codes, &evaluator);
    }

    // 10. Generate binary
    let main_bin = tasm::genibin(&resolved, &imap)?;
    let const_bin = tasm::gencbin(&evaluator, &dmap)?;
    let symbol_map = tasm::SymbolMap::generate(&evaluator, &imap, &dmap);

    // 12. Write output files
    fs::write(&args.out, main_bin)?;
    fs::write(&args.rom, const_bin)?;
    if let Some(ref file) = args.map {
        fs::write(&file, symbol_map.to_yaml())?;
    }
    Ok(())
}
