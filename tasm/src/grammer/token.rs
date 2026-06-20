use std::fmt;
use std::rc::Rc;

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

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?} at {}", self.kind, self.pos)
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
    ColonColon,   // '::'

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
    KwSizeof,   // "sizeof"

    Ident(String), // Identifier
    Scope(String), // Scope name

    // Literals
    Number(String, usize),
    Text(String),
    Char(char),

    // Special
    Comment(String), // Comment
    Error(String),   // Error
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Pos(Rc<str>, usize, usize);

impl Pos {
    pub fn new(file: Rc<str>, row: usize, col: usize) -> Self {
        Pos(file, row, col)
    }

    /// 参照元ファイルのパス文字列 (Lexer に渡した path)。モジュール解決に使う。
    pub fn file(&self) -> &str {
        &self.0
    }
}

impl Default for Pos {
    fn default() -> Self {
        Pos(Rc::from(""), 0, 0)
    }
}

impl fmt::Display for Pos {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}:{}:{}", self.0, self.1, self.2)
    }
}
