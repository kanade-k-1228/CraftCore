// token.rs

#[derive(Debug, Clone)]
pub struct Token {
    pub kind: TokenKind,
    pub pos: Pos,
}

impl Token {
    pub fn new(kind: TokenKind, pos: Pos) -> Self {
        Token { kind, pos }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum TokenKind {
    // Double character tokens
    EqualEqual,   // '=='
    ExclEqual,    // '!='
    RAngleEqual,  // '>='
    LAngleEqual,  // '<='
    LAngleLAngle, // '<<'
    RAngleRAngle, // '>>'
    Arrow,        // '->'

    // Single character tokens
    Equal,     // '='
    Plus,      // '+'
    Minus,     // '-'
    Star,      // '*'
    Atmark,    // '@'
    Slash,     // '/'
    Percent,   // '%'
    Ampasand,  // '&'
    Pipe,      // '|'
    Caret,     // '^'
    Excl,      // '!'
    Question,  // '?'
    Colon,     // ':'
    Semicolon, // ';'
    Comma,     // ','
    Period,    // '.'
    LParen,    // '('
    RParen,    // ')'
    LBracket,  // '['
    RBracket,  // ']'
    LCurly,    // '{'
    RCurly,    // '}'
    LAngle,    // '<'
    RAngle,    // '>'

    // Keywords
    KwFunc,     // "fn"
    KwReturn,   // "return"
    KwVar,      // "var"
    KwLet,      // "let"
    KwStatic,   // "static"
    KwInt,      // "int"
    KwType,     // "type"
    KwIf,       // "if"
    KwElse,     // "else"
    KwWhile,    // "while"
    KwBreak,    // "break"
    KwContinue, // "continue"
    KwSizeof,   // "sizeof"

    // Identifier
    Ident(String),

    // Literals
    LitNumber(i64),
    LitString(String),

    // Special
    Comment(String), // Comment
    Error(String),   // Error
}

#[derive(Debug, Clone, PartialEq)]
pub struct Pos {
    pub file: usize,
    pub col: usize,
    pub row: usize,
}
