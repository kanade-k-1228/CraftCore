use tasm::token::{Kind, Token};

fn test_case(code: &str, expects: Vec<Kind>) {
    use tasm::lexer::LineLexer;
    let toks = LineLexer::new(code, 0, 0).parse();

    println!("{:?}", code);
    for Token(_, pos) in &toks {
        print!("\r\x1b[{}C^", pos.row + 1);
    }
    println!();
    for (i, Token(kind, _)) in toks.iter().enumerate() {
        println!("{:>2}: {:?}", i, kind);
    }

    assert_eq!(toks.len(), expects.len());
    for (i, expect) in expects.iter().enumerate() {
        assert_eq!(toks[i].0, *expect);
    }
}

#[test]
fn test_lexer() {
    use Kind::*;
    test_case(
        "type t: int; fn main() { return 0; } // sample comment",
        vec![
            KwType,
            Ident(format!("t")),
            Colon,
            KwInt,
            Semicolon,
            KwFunc,
            Ident(format!("main")),
            LParen,
            RParen,
            LBrace,
            KwReturn,
            NumberLit(0),
            Semicolon,
            RBrace,
            Comment(format!("sample comment")),
        ],
    );
}
