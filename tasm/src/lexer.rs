// lexer.rs

use crate::token::{Pos, Token, TokenKind};
use std::iter::Peekable;
use std::str::CharIndices;

pub struct Lexer {
    file_idx: usize,
    lines: Vec<String>,
}

impl Lexer {
    pub fn new(file_idx: usize, code: String) -> Self {
        Self {
            file_idx,
            lines: code.lines().map(|line| line.to_string()).collect(),
        }
    }

    pub fn parse(self) -> Vec<Token> {
        let mut tokens = Vec::new();
        for (line_idx, line) in self.lines.iter().enumerate() {
            let line_lexer = LineLexer::new(line, self.file_idx, line_idx);
            tokens.extend(line_lexer.parse());
        }
        tokens
    }
}

pub struct LineLexer<'a> {
    iter: Peekable<CharIndices<'a>>,
    file: usize,
    col: usize,
}

impl<'a> LineLexer<'a> {
    pub fn new(line: &'a str, file: usize, col: usize) -> Self {
        Self {
            iter: line.char_indices().peekable(),
            file,
            col,
        }
    }
}

// ----------------------------------------------------------------------------
// Helpers
// ----------------------------------------------------------------------------

impl<'a> LineLexer<'a> {
    fn peek_nth(&self, n: usize) -> Option<(usize, char)> {
        self.iter.clone().nth(n)
    }
    fn consume(&mut self) -> Option<(usize, char)> {
        self.iter.next()
    }
}

// ----------------------------------------------------------------------------
// Parser
// ----------------------------------------------------------------------------

impl<'a> LineLexer<'a> {
    pub fn parse(mut self) -> Vec<Token> {
        let mut tokens = Vec::new();
        while let Some((idx, ch0)) = self.peek_nth(0) {
            // 0. Skip whitespaces
            if ch0.is_whitespace() {
                self.consume();
                continue;
            }

            let pos = Pos {
                file: self.file,
                col: self.col,
                row: idx,
            };

            // 1. Double character token
            if let Some((_, ch1)) = self.peek_nth(1) {
                // Comment
                if ch0 == '/' && ch1 == '/' {
                    self.consume(); // consume '/'
                    self.consume(); // consume '/'
                    while let Some(_) = self.iter.next_if(|(_, c)| c.is_whitespace()) {}
                    let comment = self.iter.map(|(_, ch)| ch).collect::<String>();
                    tokens.push(Token::new(TokenKind::Comment(comment), pos));
                    break;
                }

                if let Some(kind) = double_char_token(ch0, ch1) {
                    self.consume(); // consume
                    self.consume(); // consume second char
                    tokens.push(Token::new(kind, pos));
                    continue;
                }
            }

            // 2. Single character token
            if let Some(kind) = single_char_token(ch0) {
                self.consume();
                tokens.push(Token::new(kind, pos));
                continue;
            }

            // 3. Number literal
            if ch0.is_ascii_digit() {
                tokens.push(Token::new(self.parse_number(), pos));
                continue;
            }

            // 4. Char literal
            if ch0 == '\'' {
                self.consume();
                let (_, ch1) = self.consume().unwrap();
                let (_, ch2) = self.consume().unwrap();
                assert!(ch2 == '\'');
                tokens.push(Token::new(TokenKind::Char(ch1), pos));
                continue;
            }

            // 5. String literal
            if ch0 == '"' {
                tokens.push(Token::new(self.parse_text(), pos));
                continue;
            }

            // 6. Identifier or keyword
            if ch0.is_ascii_alphabetic() || ch0 == '_' {
                tokens.push(Token::new(self.parse_string(ch0), pos));
                continue;
            }

            // Error
            self.iter.next();
            tokens.push(Token::new(TokenKind::Error(format!("{ch0}")), pos));
        }
        tokens
    }

    fn parse_string(&mut self, ch: char) -> TokenKind {
        self.iter.next();
        let mut lexeme = vec![ch];
        while let Some((_, ch)) = self
            .iter
            .next_if(|(_, ch)| matches!(ch, '_' | '0'..='9' | 'a'..='z' | 'A'..='Z' ))
        {
            lexeme.push(ch);
        }
        let lexeme = lexeme.into_iter().collect::<String>();
        match keyword(&lexeme) {
            Some(kind) => kind,
            None => TokenKind::Ident(lexeme.to_string()),
        }
    }

    // Text: "hoge\nfuga"
    fn parse_text(&mut self) -> TokenKind {
        self.consume();

        let mut lexeme = vec![];
        let mut escape = false;
        while let Some((_, ch)) = self.consume() {
            if escape {
                match ch {
                    '\\' => lexeme.push('\\'),
                    'n' => lexeme.push('\n'),
                    ch => panic!("Invalid Escape :{ch}"),
                }
                escape = false;
            } else {
                match ch {
                    '"' => break,
                    '\\' => escape = true,
                    ch => lexeme.push(ch),
                }
            }
        }
        let lexeme = lexeme.into_iter().collect::<String>();
        TokenKind::Text(lexeme.to_string())
    }

    fn parse_number(&mut self) -> TokenKind {
        let (_, ch0) = self.consume().unwrap();
        if ch0 == '0' {
            if let Some(&(_, ch1)) = self.iter.peek() {
                if ch1 == 'x' || ch1 == 'X' {
                    self.consume();
                    return self.parse_number_hex(ch0, ch1);
                }
            }
        }
        return self.parse_number_dec(ch0);
    }

    fn parse_number_hex(&mut self, ch0: char, ch1: char) -> TokenKind {
        let mut lexeme = vec![ch0, ch1];
        while let Some((_, ch)) = self
            .iter
            .next_if(|(_, ch)| matches!(ch, '_' | '0'..='9' | 'a'..='f' | 'A'..='F' ))
        {
            lexeme.push(ch);
        }
        let lexeme = lexeme.into_iter().collect::<String>();
        match usize::from_str_radix(&lexeme[2..].replace("_", ""), 16) {
            Ok(num) => TokenKind::Number(lexeme.to_string(), num),
            Err(_) => TokenKind::Error(lexeme.to_string()),
        }
    }

    fn parse_number_dec(&mut self, ch: char) -> TokenKind {
        let mut lexeme = vec![ch];
        while let Some((_, ch)) = self.iter.next_if(|(_, ch)| matches!(ch, '_' | '0'..='9')) {
            lexeme.push(ch);
        }
        let lexeme = lexeme.into_iter().collect::<String>();
        match usize::from_str_radix(&lexeme.replace("_", ""), 10) {
            Ok(num) => TokenKind::Number(lexeme.to_string(), num),
            Err(_) => TokenKind::Error(lexeme.to_string()),
        }
    }
}

fn double_char_token(ch0: char, ch1: char) -> Option<TokenKind> {
    match (ch0, ch1) {
        ('=', '=') => Some(TokenKind::EqualEqual),
        ('!', '=') => Some(TokenKind::ExclEqual),
        ('<', '=') => Some(TokenKind::LAngleEqual),
        ('>', '=') => Some(TokenKind::RAngleEqual),
        ('<', '<') => Some(TokenKind::LAngleLAngle),
        ('>', '>') => Some(TokenKind::RAngleRAngle),
        ('-', '>') => Some(TokenKind::Arrow),
        _ => None,
    }
}

fn single_char_token(ch: char) -> Option<TokenKind> {
    match ch {
        '=' => Some(TokenKind::Equal),
        '+' => Some(TokenKind::Plus),
        '-' => Some(TokenKind::Minus),
        '*' => Some(TokenKind::Star),
        '@' => Some(TokenKind::Atmark),
        '/' => Some(TokenKind::Slash),
        '%' => Some(TokenKind::Percent),
        '&' => Some(TokenKind::Ampasand),
        '|' => Some(TokenKind::Pipe),
        '^' => Some(TokenKind::Caret),
        '!' => Some(TokenKind::Excl),
        '?' => Some(TokenKind::Question),
        ':' => Some(TokenKind::Colon),
        ';' => Some(TokenKind::Semicolon),
        ',' => Some(TokenKind::Comma),
        '.' => Some(TokenKind::Period),
        '(' => Some(TokenKind::LParen),
        ')' => Some(TokenKind::RParen),
        '[' => Some(TokenKind::LBracket),
        ']' => Some(TokenKind::RBracket),
        '{' => Some(TokenKind::LCurly),
        '}' => Some(TokenKind::RCurly),
        '<' => Some(TokenKind::LAngle),
        '>' => Some(TokenKind::RAngle),
        _ => None,
    }
}

fn keyword(s: &str) -> Option<TokenKind> {
    match s {
        "fn" => Some(TokenKind::KwFunc),
        "var" => Some(TokenKind::KwVar),
        "type" => Some(TokenKind::KwType),
        "const" => Some(TokenKind::KwConst),
        "static" => Some(TokenKind::KwStatic),
        "if" => Some(TokenKind::KwIf),
        "else" => Some(TokenKind::KwElse),
        "while" => Some(TokenKind::KwWhile),
        "asm" => Some(TokenKind::KwAsm),
        "break" => Some(TokenKind::KwBreak),
        "continue" => Some(TokenKind::KwContinue),
        "int" => Some(TokenKind::KwInt),
        "return" => Some(TokenKind::KwReturn),
        _ => None,
    }
}
