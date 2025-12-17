use clap::Parser;
use std::io::BufRead;

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

    // Parse the input file
    let mut tokens = vec![];
    for (file_idx, input) in args.input.iter().enumerate() {
        let file = std::fs::File::open(&input).expect(&format!("Failed to open file: {}", input));
        let reader = std::io::BufReader::new(file);
        for (line_idx, line) in reader.lines().enumerate() {
            let line = line.expect("Failed to read line");
            let toks = tasm::grammer::lexer::LineLexer::new(&line, file_idx, line_idx).parse();
            tokens.extend(toks);
        }
    }

    // Parse tokens into AST
    let (ast, errors) = tasm::grammer::parser::Parser::new(tokens.into_iter()).parse();

    if !errors.is_empty() {
        eprintln!("Parser errors:");
        for error in &errors {
            eprintln!("  {:?}", error);
        }
        std::process::exit(1);
    }

    // Collect the globals in dependency order
    let consts = match tasm::collect::table::consts::collect(&ast) {
        Ok(c) => c,
        Err(e) => {
            eprintln!("Constants collection error: {:?}", e);
            std::process::exit(1);
        }
    };

    let types = match tasm::collect::table::types::collect(&ast, &consts) {
        Ok(t) => t,
        Err(e) => {
            eprintln!("Types collection error: {:?}", e);
            std::process::exit(1);
        }
    };

    let statics = match tasm::collect::table::statics::collect(&ast, &consts, &types) {
        Ok(s) => s,
        Err(e) => {
            eprintln!("Statics collection error: {:?}", e);
            std::process::exit(1);
        }
    };

    let asms = match tasm::collect::table::asms::collect(&ast) {
        Ok(a) => a,
        Err(e) => {
            eprintln!("Assembly blocks collection error: {:?}", e);
            std::process::exit(1);
        }
    };

    let funcs = match tasm::collect::table::funcs::collect(&ast, &consts, &types, &statics) {
        Ok(f) => f,
        Err(e) => {
            eprintln!("Functions collection error: {:?}", e);
            std::process::exit(1);
        }
    };

    // Display collected globals if verbose
    if args.verbose {
        println!("\n=== Collected Information ===\n");
        tasm::collect::table::consts::print(&consts);
        tasm::collect::table::types::print(&types);
        tasm::collect::table::statics::print(&statics);
        tasm::collect::table::asms::print(&asms);
        tasm::collect::table::funcs::print(&funcs);
    }

    // Generate binary
    let asm_code = match tasm::convert::CodeGen::generate(&consts, &types, &statics, &asms, &funcs)
    {
        Ok(code) => code,
        Err(e) => {
            eprintln!("Code generation error: {}", e);
            std::process::exit(1);
        }
    };

    // Allocate Globals
    let allocated = match tasm::link::link(&asm_code) {
        Ok(alloc) => alloc,
        Err(e) => {
            eprintln!("Link error: {}", e);
            std::process::exit(1);
        }
    };

    // Prepare allocation information for verbose output
    let mut sorted_symbols: Vec<_> = allocated.symbols.iter().collect();
    sorted_symbols.sort_by_key(|(_, addr)| *addr);

    let mut sorted_sections = allocated.sections.clone();
    sorted_sections.sort_by_key(|s| s.addr);

    if args.verbose {
        println!("=== Phase 1: Object Allocation ===");
        println!("\nAllocated Symbols:");
        for (name, addr) in &sorted_symbols {
            println!("  0x{:04X} : {}", addr, name);
        }

        println!("\nAllocated Sections:");
        for section in &sorted_sections {
            match section.data {
                tasm::link::SectionData::Data(size) => {
                    println!(
                        "  0x{:04X}-0x{:04X} : {} (data, {} bytes)",
                        section.addr,
                        section.addr + size - 1,
                        section.name,
                        size
                    );
                }
                tasm::link::SectionData::Code(ref code) => {
                    let size = code.len();
                    println!(
                        "  0x{:04X}-0x{:04X} : {} (code, {} instructions)",
                        section.addr,
                        section.addr + size - 1,
                        section.name,
                        size
                    );
                }
            }
        }
        println!();
    }

    // Phase 2: Resolve - resolve symbols and fill unresolved operands
    let resolved = match tasm::link::resolve(&allocated) {
        Ok(resolved) => resolved,
        Err(e) => {
            eprintln!("Resolve error: {}", e);
            std::process::exit(1);
        }
    };

    if args.verbose {
        println!("=== Phase 2: Symbol Resolution ===");
        println!("Symbols resolved successfully");
        println!();
    }

    // Phase 3: Generate binary from resolved allocation
    let binary = match tasm::link::generate_binary(&resolved) {
        Ok(bin) => bin,
        Err(e) => {
            eprintln!("Binary generation error: {}", e);
            std::process::exit(1);
        }
    };

    let binary_size = binary.len();

    if args.verbose {
        println!("=== Phase 3: Binary Generation ===");
        println!("Binary size: {} bytes", binary_size);
        println!();
    }

    // Write output to file
    std::fs::write(&args.output, binary)
        .expect(&format!("Failed to write to file: {}", args.output));

    println!(
        "Successfully compiled {} to {}",
        args.input.join(", "),
        args.output
    );
}
