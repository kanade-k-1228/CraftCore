use tasm::lexer::LineLexer;
use tasm::parser::Parser;
use tasm::token::{Token, TokenKind};

fn case(code: &str) {
    let mut tokens: Vec<Token> = Vec::new();
    for (line_idx, line) in code.lines().enumerate() {
        println!("{:>2}: {}", line_idx, line);
        let toks = LineLexer::new(code, 0, 0).parse();
        for tok in &toks {
            print!("\r\x1b[{}C^", tok.pos.row + 4);
        }
        println!();
        for (i, tok) in toks.iter().enumerate() {
            println!("{:>2}: {:?}", i, tok.kind);
        }
        tokens.extend(
            toks.into_iter()
                .filter(|t| !matches!(t.kind, TokenKind::Comment(_))),
        );
    }

    let (ast, errors) = Parser::new(tokens.into_iter()).parse();
    println!("{:#?}", ast);
    for error in &errors {
        println!("{:?}", error);
    }
    assert!(errors.is_empty(), "Errors found: {:?}", errors);
}

#[test]
fn test() {
    case("static@100 heap: {ptr: int, buf: [100]int}; fn main() -> int { a; return;}");
    case("type t: int; fn main() -> [3]int { return \"ABC\"; } type c: int;");
    case("fn main () -> int { if(0) { return 1; } else { return 2; } }");
}
