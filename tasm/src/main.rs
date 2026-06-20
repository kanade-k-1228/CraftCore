use std::collections::{HashMap, HashSet};
use std::fs;
use std::path::Path;

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

    /// Module include directories (repeatable). The directory's basename becomes
    /// the module root name; `<DIR>/a/b.tasm` is module `<basename>::a::b`.
    #[clap(short = 'I', long = "include", value_name = "DIR")]
    include: Vec<String>,

    /// Enable verbose output
    #[clap(short, long)]
    verbose: bool,
}

fn main() -> Result<(), tasm::Error> {
    let args = Args::parse();

    // 1. Read source files (+ module files discovered under each `-I <DIR>`).
    //    Build a path → module-prefix map; root sources have no prefix.
    let mut modmap: HashMap<String, String> = HashMap::new();
    let sources = {
        let mut seen: HashSet<String> = HashSet::new();
        let mut sources: Vec<(String, String)> = vec![];
        // Root sources (explicit src args): no module prefix.
        for path in &args.src {
            if seen.insert(path.clone()) {
                let content = fs::read_to_string(path)?;
                sources.push((path.clone(), content));
            }
        }
        // Module files from `-I`: basename(DIR) is the module root name.
        let mut module_files: Vec<(String, String)> = vec![];
        for inc in &args.include {
            let dir = Path::new(inc);
            let root = dir
                .file_name()
                .map(|n| n.to_string_lossy().into_owned())
                .unwrap_or_default();
            collect_module_files(dir, &root, &mut module_files)?;
        }
        for (path, prefix) in module_files {
            if seen.insert(path.clone()) {
                let content = fs::read_to_string(&path)?;
                modmap.insert(path.clone(), prefix);
                sources.push((path, content));
            }
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
    let (mut ast, errors) = tasm::Parser::new(tokens.into_iter()).parse();
    if !errors.is_empty() {
        for e in &errors {
            eprintln!("error: {}", e);
        }
        std::process::exit(-1);
    }

    // 3.5. Apply module prefixes to top-level definition names (FQN).
    tasm::apply_module_prefixes(&mut ast, &modmap);

    // 4. Evaluator Database
    let global = tasm::Global::new(&ast, modmap)?;

    // 5. Resolve dependencies from entry points (resolve names to FQN first).
    let entries: Vec<String> = ["reset", "irq", "main"]
        .into_iter()
        .filter_map(|e| global.resolve_entry(e))
        .collect();
    let entry_refs: Vec<&str> = entries.iter().map(|s| s.as_str()).collect();
    let (labels, symbols) = global.deps(&entry_refs, IndexSet::new(), IndexSet::new())?;

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

/// `-I <DIR>` 配下の .tasm を再帰収集し、(path, module_prefix) を out へ積む。
/// prefix は親までのモジュールパス (`rtos`, `rtos::sub` など)。
fn collect_module_files(
    dir: &Path,
    prefix: &str,
    out: &mut Vec<(String, String)>,
) -> std::io::Result<()> {
    let mut entries: Vec<_> = fs::read_dir(dir)?.collect::<Result<_, _>>()?;
    entries.sort_by_key(|e| e.path());
    for e in entries {
        let p = e.path();
        if p.is_dir() {
            let name = p
                .file_name()
                .map(|n| n.to_string_lossy().into_owned())
                .unwrap_or_default();
            let sub = format!("{}::{}", prefix, name);
            collect_module_files(&p, &sub, out)?;
        } else if p.extension().and_then(|s| s.to_str()) == Some("tasm") {
            let stem = p
                .file_stem()
                .map(|n| n.to_string_lossy().into_owned())
                .unwrap_or_default();
            let mod_prefix = format!("{}::{}", prefix, stem);
            out.push((p.to_string_lossy().into_owned(), mod_prefix));
        }
    }
    Ok(())
}
