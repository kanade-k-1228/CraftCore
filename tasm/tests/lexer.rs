use tasm::token::TokenKind;

fn test_case(code: &str, expects: Vec<TokenKind>) {
    use tasm::lexer::LineLexer;
    let toks = LineLexer::new(code, 0, 0).parse();

    println!(" {code}");
    for tok in &toks {
        print!("\r\x1b[{}C^", tok.pos.row + 1);
    }
    println!();
    for (i, tok) in toks.iter().enumerate() {
        println!("{:>2}: {:?}", i, tok.kind);
    }

    assert_eq!(toks.len(), expects.len());
    for (i, expect) in expects.iter().enumerate() {
        assert_eq!(toks[i].kind, *expect);
    }
}

#[test]
fn test_lexer() {
    use TokenKind::*;
    test_case(
        "type t: int; fn main() { return \"ABC\"; } // sample comment",
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
            LCurly,
            KwReturn,
            LitString(format!("ABC")),
            Semicolon,
            RCurly,
            Comment(format!("sample comment")),
        ],
    );
}
