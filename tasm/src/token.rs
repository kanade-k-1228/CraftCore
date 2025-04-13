// token.rs

#[derive(Debug, Clone)]
pub struct Token(pub Kind, pub Pos); // (line, column)

#[derive(Debug, Clone, PartialEq)]
pub enum Kind {
    // Keywords
    Func,   // "func"
    Var,    // "var"
    Type,   // "type"
    Return, // "return"
    If,     // "if"
    Else,   // "else"
    While,  // "while"
    Struct, // "struct"
    Int,    // "int"

    // Operators
    Assign,    // '='
    Plus,      // '+'
    Minus,     // '-'
    Asterisk,  // '*'
    AtSign,    // '@'
    Slash,     // '/'
    Percent,   // '%'
    Ampersand, // '&'
    Pipe,      // '|'
    Caret,     // '^'

    Equal,        // '=='
    NotEqual,     // '!='
    Less,         // '<'
    LessEqual,    // '<='
    Greater,      // '>'
    GreaterEqual, // '>='
    ShiftLeft,    // '<<'
    ShiftRight,   // '>>'

    // Delimiters and punctuation
    Colon,     // ':'
    Semicolon, // ';'
    Comma,     // ','
    Dot,       // '.'
    LParen,    // '('
    RParen,    // ')'
    LBracket,  // '['
    RBracket,  // ']'
    LBrace,    // '{'
    RBrace,    // '}'

    // Identifiers
    Ident(String),
    Number(i64),

    // Special
    Comment(String), // Comment
    EOF,             // End-of-file (no more input)
    Error(String),   // Error
}

#[derive(Debug, Clone, PartialEq)]
pub struct Pos(pub usize, pub usize, pub usize); // (file, line, column)
