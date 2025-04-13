// token.rs

#[derive(Debug, Clone)]
pub struct Token(pub Kind, pub Pos);

#[derive(Debug, Clone, PartialEq)]
pub enum Kind {
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
    Colon,     // ':'
    Semicolon, // ';'
    Comma,     // ','
    Period,    // '.'
    LParen,    // '('
    RParen,    // ')'
    LBracket,  // '['
    RBracket,  // ']'
    LBrace,    // '{'
    RBrace,    // '}'
    LAngle,    // '<'
    RAngle,    // '>'

    // Keywords
    KwFunc,   // "fn"
    KwVar,    // "var"
    KwType,   // "type"
    KwReturn, // "return"
    KwIf,     // "if"
    KwElse,   // "else"
    KwWhile,  // "while"
    KwStruct, // "struct"
    KwInt,    // "int"

    // Identifier
    Ident(String),

    // Literals
    NumberLit(i64),
    StringLit(String),

    // Special
    Comment(String), // Comment
    Error(String),   // Error
}

#[derive(Debug, Clone, PartialEq)]
pub struct Pos {
    pub file: usize,
    pub line: usize,
    pub col: usize,
}
