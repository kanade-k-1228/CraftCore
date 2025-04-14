use tasm::lexer::LineLexer;
use tasm::parser::Parser;

#[test]
fn test() {
    let code = "fn main() -> int { a; return 0;}";
    let tokens = LineLexer::new(code, 0, 0).parse();
    for token in &tokens {
        println!("{:?}", token.kind);
    }
    let ast = Parser::new(tokens.into_iter()).parse();
    println!("{:#?}", ast);
}
