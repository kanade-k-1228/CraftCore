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
            let toks = tasm::lexer::LineLexer::new(&line, file_idx, line_idx).parse();
            tokens.extend(toks);
        }
    }

    // Print the tokens for debugging
    let (ast, err) = tasm::parser::Parser::new(tokens.into_iter()).parse();

    println!("Parsed AST: {:#?}", ast);
    println!("Parser error: {:#?}", err);

    // Collect the globals
    let globals = tasm::collect::collect(ast);
    println!("Collected globals: {:#?}", globals);
}
