use std::collections::HashMap;
use std::fs;

use clap::Parser;
use indexmap::IndexMap;

use tasm::error::Error;
use tasm::gen::maps::{generate_data_map, generate_function_map, generate_static_map};
use tasm::grammer::lexer::Lexer;
use tasm::grammer::parser::Parser as TasmParser;
use tasm::linker::allocator::Allocator;
use tasm::linker::binary::{generate_data_binary, generate_program_binary, resolve_symbols};
use tasm::linker::deps::{dependency, filter};
use tasm::linker::memory::Memory;
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

fn main() -> Result<(), Error> {
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
    let symbols = Symbols::collect(&ast)?;

    // 5. Generate code from functions and assembly blocks
    let func_codes = tasm::convert::func2code(&symbols)?;
    let asm_codes = tasm::convert::asm2code(&symbols)?;
    let codes: HashMap<_, _> = func_codes.into_iter().chain(asm_codes).collect();

    // 6. Perform dead code elimination
    let deps = dependency(&codes);
    let used = filter(deps.clone(), vec!["reset", "irq", "main"]);

    if args.verbose {
        eprintln!("=== Dead Code Elimination Report ===");
        eprintln!();

        // Calculate the maximum width for each column
        let max_symbol_len = deps.keys().map(|s| s.len()).max().unwrap_or(6).max(6);
        let max_deps_len = deps
            .values()
            .map(|v| {
                if v.is_empty() {
                    4 // "None"
                } else {
                    v.iter().map(|s| s.len()).sum::<usize>() + (v.len() - 1) * 2
                    // +2 for ", "
                }
            })
            .max()
            .unwrap_or(12)
            .max(12);

        // Print header
        eprintln!(
            "{:<width$} | {:^6} | {:<deps_width$}",
            "Symbol",
            "Kept",
            "Dependencies",
            width = max_symbol_len,
            deps_width = max_deps_len
        );
        eprintln!(
            "{:-<width$}-+-{:-<6}-+-{:-<deps_width$}",
            "",
            "",
            "",
            width = max_symbol_len,
            deps_width = max_deps_len
        );

        // Sort symbols for consistent output
        let mut symbols: Vec<_> = deps.keys().collect();
        symbols.sort();

        // Print each symbol's status
        for &symbol in &symbols {
            let is_kept = used.contains(symbol);
            let deps_list = deps.get(symbol).unwrap();
            let deps_str = if deps_list.is_empty() {
                "None".to_string()
            } else {
                deps_list.join(", ")
            };

            eprintln!(
                "{:<width$} | {:^6} | {:<deps_width$}",
                symbol,
                if is_kept { "✓" } else { " " },
                deps_str,
                width = max_symbol_len,
                deps_width = max_deps_len
            );
        }

        eprintln!();
        eprintln!(
            "Total: {} symbols, {} kept, {} eliminated",
            deps.len(),
            used.len(),
            deps.len() - used.len()
        );
        eprintln!();
    }

    // 7. Collect global objects (with DCE applied)
    let iitems = {
        let mut iitems = IndexMap::new();
        for (&name, (addr, _)) in &symbols.asms.0 {
            // Only include if reachable
            if used.contains(name) {
                if let Some(code) = codes.get(name) {
                    iitems.insert(name.to_string(), (code.0.len(), *addr));
                }
            }
        }
        for (&name, code) in &codes {
            // Only include if reachable
            if used.contains(name) && !symbols.asms.0.contains_key(name) {
                iitems.insert(name.to_string(), (code.0.len(), None));
            }
        }
        iitems
    };
    let ditems = {
        let mut ditems = IndexMap::new();
        for (&name, (ty, addr, _)) in symbols.statics.0.iter() {
            // Only include if reachable (data items referenced from reachable code)
            if used.contains(name) {
                ditems.insert(name.to_string(), (ty.sizeof(), *addr));
            }
        }
        for (&name, (ty, _, addr, _)) in symbols.consts.0.iter() {
            // Only include if reachable
            if used.contains(name) && !symbols.statics.0.contains_key(name) {
                ditems.insert(name.to_string(), (ty.sizeof(), *addr));
            }
        }
        ditems
    };

    // 8. Allocate objects
    let imem = Memory::new(0, 0x10000)
        .section("reset", 0x0000, 0x0004)
        .section("irq", 0x0004, 0x0008)
        .section("code", 0x0008, 0x10000);
    let dmem = Memory::new(0, 0x10000)
        .section("reg", 0x0000, 0x0010)
        .section("csr", 0x0010, 0x0100)
        .section("ioreg", 0x0100, 0x1000)
        .section("vram", 0x1000, 0x3000)
        .section("const", 0x3000, 0x5000)
        .section("static", 0x5000, 0x10000);
    let mut ialoc = Allocator::new(0, 0x10000);
    let mut daloc = Allocator::new(0, 0x10000);

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
    for (&name, _) in symbols.consts.0.iter() {
        if let Some((size, addr)) = ditems.get(name) {
            if addr.is_none() {
                daloc.section(dmem.get("const")?, *size, name)?;
            }
        }
    }

    for (&name, (_, addr, _)) in symbols.statics.0.iter() {
        if addr.is_none() {
            if let Some((size, _)) = ditems.get(name) {
                daloc.section(dmem.get("static")?, *size, name)?;
            }
        }
    }

    let imap: IndexMap<String, usize> = ialoc.allocations().into_iter().collect();
    let dmap: IndexMap<String, usize> = daloc.allocations().into_iter().collect();

    // 9. Resolve symbols
    let resolved = resolve_symbols(&codes, &imap, &dmap, &symbols);
    if args.verbose {
        binprint(&imap, &dmap, &codes, &symbols);
    }

    // 10. Generate binary
    let ibin = generate_program_binary(&resolved, &imap)?; // Instruction binary
    let cbin = generate_data_binary(&symbols, &dmap)?; // Constant binary

    // 11. Generate map files
    let fmap = generate_function_map(&symbols, &imap);
    let smap = generate_static_map(&symbols, &dmap);
    let dmap_content = generate_data_map(&dmap);

    // 12. Write output to file
    if !std::path::Path::new(&args.out).exists() {
        fs::create_dir(&args.out)?;
    }
    fs::write(&format!("{}/i.bin", args.out), ibin)?;
    fs::write(&format!("{}/c.bin", args.out), cbin)?;
    fs::write(&format!("{}/f.yaml", args.out), fmap)?;
    fs::write(&format!("{}/s.yaml", args.out), smap)?;
    fs::write(&format!("{}/d.yaml", args.out), dmap_content)?;

    Ok(())
}
