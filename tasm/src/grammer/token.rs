#[derive(Debug, Clone)]
pub struct Token<'a> {
    pub kind: TokenKind,
    pub pos: Pos<'a>,
}

impl<'a> Token<'a> {
    pub fn new(kind: TokenKind, pos: Pos<'a>) -> Self {
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
    KwAsm,      // "asm"
    KwReturn,   // "return"
    KwVar,      // "var"
    KwStatic,   // "static"
    KwConst,    // "const"
    KwInt,      // "int"
    KwVoid,     // "void"
    KwType,     // "type"
    KwIf,       // "if"
    KwElse,     // "else"
    KwWhile,    // "while"
    KwBreak,    // "break"
    KwContinue, // "continue"
    KwAs,       // "as"

    // Identifier
    Ident(String),

    // Literals
    Number(String, usize),
    Text(String),
    Char(char),

    // Special
    Comment(String), // Comment
    Error(String),   // Error
}

#[derive(Debug, Clone, PartialEq)]
pub struct Pos<'a> {
    pub file: &'a str,
    pub col: usize,
    pub row: usize,
}
