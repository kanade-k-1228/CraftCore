use tasm::token::{TokenKind, TokenKind::*};

fn assert(code: &str, expect: TokenKind) {
    use tasm::lexer::LineLexer;
    let tokens = LineLexer::new(code, 0, 0).parse();
    assert_eq!(tokens[0].kind, expect);
}

macro_rules! case {
    ($name:ident, $code:expr, $expect:expr) => {
        #[test]
        fn $name() {
            assert($code, $expect);
        }
    };
}

// ---- Double-char operators ----
case!(equal_equal, "==", EqualEqual);
case!(excl_equal, "!=", ExclEqual);
case!(langle_equal, "<=", LAngleEqual);
case!(rangle_equal, ">=", RAngleEqual);
case!(langle_langle, "<<", LAngleLAngle);
case!(rangle_rangle, ">>", RAngleRAngle);
case!(arrow, "->", Arrow);

// ---- Single-char operators ----
case!(equal, "=", Equal);
case!(plus, "+", Plus);
case!(minus, "-", Minus);
case!(star, "*", Star);
case!(atmark, "@", Atmark);
case!(slash, "/", Slash);
case!(percent, "%", Percent);
case!(ampasand, "&", Ampasand);
case!(pipe, "|", Pipe);
case!(caret, "^", Caret);
case!(excl, "!", Excl);
case!(question, "?", Question);
case!(colon, ":", Colon);
case!(semicolon, ";", Semicolon);
case!(comma, ",", Comma);
case!(period, ".", Period);
case!(lparen, "(", LParen);
case!(rparen, ")", RParen);
case!(lbracket, "[", LBracket);
case!(rbracket, "]", RBracket);
case!(lcurly, "{", LCurly);
case!(rcurly, "}", RCurly);
case!(langle, "<", LAngle);
case!(rangle, ">", RAngle);

// ---- Keywords ----
case!(kw_fn, "fn", KwFunc);
case!(kw_asm, "asm", KwAsm);
case!(kw_return, "return", KwReturn);
case!(kw_var, "var", KwVar);
case!(kw_static, "static", KwStatic);
case!(kw_const, "const", KwConst);
case!(kw_int, "int", KwInt);
case!(kw_type, "type", KwType);
case!(kw_if, "if", KwIf);
case!(kw_else, "else", KwElse);
case!(kw_while, "while", KwWhile);
case!(kw_break, "break", KwBreak);
case!(kw_continue, "continue", KwContinue);

// ---- Identifiers ----
case!(ident_simple, "abc", Ident(format!("abc")));
case!(ident_with_underscores, "foo_bar", Ident(format!("foo_bar")));

// ---- Literals ----
case!(number_dec, "123", Number(format!("123"), 123));
case!(number_hex, "0x1f", Number(format!("0x1f"), 0x1f));
case!(text, "\"hello\"", Text(format!("hello")));
case!(text_escape_n, "\"a\\nb\"", Text(format!("a\nb")));
case!(text_escape_escape, "\"\\\\\"", Text(format!("\\")));
case!(char_literal, "'a'", Char('a'));

// ---- Special ----
case!(comment_0, "// aaa", Comment(format!("aaa")));
case!(comment_1, "// aaa bbb", Comment(format!("aaa bbb")));
case!(comment_2, "// aaa bbb ccc", Comment(format!("aaa bbb ccc")));
case!(error_unknown, "$", Error(format!("$")));
