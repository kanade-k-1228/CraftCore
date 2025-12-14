use clap::Parser;
use std::io::BufRead;

const HELP_TEMPLATE: &str = "\
{before-help}{bin} {version}
  {author}
  {about}

{usage-heading}
{tab}{usage}

{all-args}{after-help}";

#[derive(Debug, clap::Parser)]
#[clap(author, version, about,help_template = HELP_TEMPLATE)]
struct Args {
    /// Input files
    #[clap(default_value = "main.tasm")]
    input: Vec<String>,

    /// Output file
    #[clap(short, long, default_value = "main.rk")]
    output: String,
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

    // Collect the globals
    let globals = match tasm::collect::collect(ast) {
        Ok(g) => g,
        Err(e) => {
            eprintln!("Collection error: {:?}", e);
            std::process::exit(1);
        }
    };

    // Generate code
    let asm_code = match tasm::codegen::CodeGen::generate(&globals) {
        Ok(code) => code,
        Err(e) => {
            eprintln!("Code generation error: {}", e);
            std::process::exit(1);
        }
    };

    // Link and generate binary
    let binary = match tasm::linker::link(&asm_code) {
        Ok(bin) => bin,
        Err(e) => {
            eprintln!("Linker error: {}", e);
            std::process::exit(1);
        }
    };

    // Write output to file
    std::fs::write(&args.output, binary)
        .expect(&format!("Failed to write to file: {}", args.output));

    println!(
        "Successfully compiled {} to {}",
        args.input.join(", "),
        args.output
    );
}
