use tasm::token::{TokenKind, TokenKind::*};

fn case(code: &str, expects: Vec<TokenKind>) {
    use tasm::lexer::LineLexer;
    let tokens = LineLexer::new(code, 0, 0).parse();

    println!(" {code}");
    for token in &tokens {
        print!("\r\x1b[{}C^", token.pos.row + 1);
    }
    println!();
    for (idx, token) in tokens.iter().enumerate() {
        println!("{:>2}: {:?}", idx, token.kind);
    }

    assert_eq!(tokens.len(), expects.len());
    for (idx, expect) in expects.iter().enumerate() {
        assert_eq!(tokens[idx].kind, *expect);
    }
}

#[test]
fn tests() {
    case(
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
            Text(format!("ABC")),
            Semicolon,
            RCurly,
            Comment(format!("sample comment")),
        ],
    );
}

#[test]
fn test_number() {
    case("0", vec![Number(format!("0"), 0)]);
    case("123", vec![Number(format!("123"), 123)]);
    case("0x01", vec![Number(format!("0x01"), 1)]);
}

#[test]
fn test_text() {
    case("\"foo-42\"", vec![Text(format!("foo-42"))]);
    case("\"\\n\"", vec![Text(format!("\n"))]); // \n
    case("\"\\\\\"", vec![Text(format!("\\"))]); // \\ -> \
}
