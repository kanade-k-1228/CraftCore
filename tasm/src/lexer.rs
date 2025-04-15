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
    line: &'a str,
    iter: Peekable<CharIndices<'a>>,
    file_idx: usize,
    line_idx: usize,
}

impl<'a> LineLexer<'a> {
    pub fn new(line: &'a str, file_idx: usize, line_idx: usize) -> Self {
        Self {
            line,
            iter: line.char_indices().peekable(),
            file_idx,
            line_idx,
        }
    }
}

impl<'a> LineLexer<'a> {
    pub fn check_if<F: FnOnce(char) -> bool>(&mut self, cond: F) -> bool {
        if let Some(&(_, ch)) = self.iter.peek() {
            cond(ch)
        } else {
            false
        }
    }

    pub fn check_if2<F: FnOnce(char, char) -> bool>(&mut self, cond: F) -> bool {
        let mut clone = self.iter.clone();
        if let Some((_, first)) = clone.next() {
            if let Some((_, second)) = clone.next() {
                return cond(first, second);
            }
        }
        false
    }

    pub fn consume_if<F: FnOnce(char) -> bool>(&mut self, cond: F) -> Option<char> {
        if let Some(&(_, ch)) = self.iter.peek() {
            if cond(ch) {
                self.iter.next();
                return Some(ch);
            }
        }
        None
    }
}

impl<'a> LineLexer<'a> {
    pub fn parse(mut self) -> Vec<Token> {
        let mut tokens = Vec::new();
        while let Some((idx, ch0)) = self.iter.next() {
            // 0. Skip whitespaces
            if ch0.is_whitespace() {
                continue;
            }

            let pos = Pos {
                file: self.file_idx,
                col: self.line_idx,
                row: idx,
            };

            // 1. Double character token
            if let Some(&(_, ch1)) = self.iter.peek() {
                // Comment
                if ch0 == '/' && ch1 == '/' {
                    self.iter.next(); // consume second '/'
                    while let Some(_) = self.iter.next_if(|(_, c)| c.is_whitespace()) {}
                    let comment = self.iter.map(|(_, ch)| ch).collect::<String>();
                    tokens.push(Token::new(TokenKind::Comment(comment), pos));
                    break;
                }

                if let Some(kind) = double_char_token(ch0, ch1) {
                    self.iter.next(); // consume second char
                    tokens.push(Token::new(kind, pos));
                    continue;
                }
            }

            // 2. Single character token
            if let Some(kind) = single_char_token(ch0) {
                tokens.push(Token::new(kind, pos));
                continue;
            }

            // 3. Number literal
            if ch0.is_ascii_digit() {
                tokens.push(Token::new(self.parse_number(ch0), pos));
                continue;
            }

            // 4. String literal
            if ch0 == '"' {
                tokens.push(Token::new(self.parse_text(), pos));
                continue;
            }

            // 5. Identifier or keyword
            if ch0.is_ascii_alphabetic() || ch0 == '_' {
                tokens.push(Token::new(self.parse_string(ch0), pos));
                continue;
            }

            // Error
            tokens.push(Token::new(TokenKind::Error(format!("{ch0}")), pos));
        }
        tokens
    }

    fn parse_string(&mut self, ch: char) -> TokenKind {
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

    fn parse_text(&mut self) -> TokenKind {
        let mut lexeme = vec![];
        let mut escape = false;
        while let Some((_, ch)) = self.iter.next() {
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

    fn parse_number(&mut self, ch0: char) -> TokenKind {
        if ch0 == '0' {
            if let Some(&(_, ch1)) = self.iter.peek() {
                if ch1 == 'x' || ch1 == 'X' {
                    self.iter.next();
                    if let Some((_, ch0)) = self.iter.next() {
                        return self.parse_number_hex(ch0);
                    }
                }
            }
        }
        return self.parse_number_dec(ch0);
    }

    fn parse_number_hex(&mut self, ch: char) -> TokenKind {
        let mut lexeme = vec![ch];
        while let Some((_, ch)) = self
            .iter
            .next_if(|(_, ch)| matches!(ch, '_' | '0'..='9' | 'a'..='f' | 'A'..='F' ))
        {
            lexeme.push(ch);
        }
        let lexeme = lexeme.into_iter().collect::<String>();
        match usize::from_str_radix(&lexeme.replace("_", ""), 16) {
            Ok(num) => TokenKind::Number(lexeme.to_string(), num),
            Err(_) => TokenKind::Error(lexeme.to_string()),
        }
    }

    fn parse_number_dec(&mut self, ch: char) -> TokenKind {
        let mut lexeme = vec![ch];
        while let Some((_, ch)) = self.iter.next_if(|(_, ch)| matches!(ch, '0'..='9' | '_')) {
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
        "int" => Some(TokenKind::KwInt),
        "return" => Some(TokenKind::KwReturn),
        _ => None,
    }
}
