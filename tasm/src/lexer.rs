// lexer.rs

use crate::token::{Kind, Pos, Token};
use std::iter::Peekable;
use std::str::CharIndices;

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

    pub fn parse(mut self) -> Vec<Token> {
        let mut tokens = Vec::new();
        while let Some((idx, ch)) = self.iter.next() {
            let pos = Pos {
                file: self.file_idx,
                col: self.line_idx,
                row: idx,
            };

            // 0. Skip whitespaces
            if ch.is_whitespace() {
                continue;
            }

            // 2. Comment
            if ch == '/' {
                if let Some(_) = self.iter.next_if(|&(_, ch2)| ch2 == '/') {
                    while let Some(_) = self.iter.next_if(|(_, c)| c.is_whitespace()) {}
                    let comment = self.iter.map(|(_, ch)| ch).collect::<String>();
                    tokens.push(Token(Kind::Comment(comment), pos));
                    break;
                }
            }

            // 3. Double character token
            if let Some(&(_, ch2)) = self.iter.peek() {
                if let Some(kind) = double_char_token(ch, ch2) {
                    self.iter.next();
                    tokens.push(Token(kind, pos));
                    continue;
                }
            }

            // 4. Single character token
            if let Some(kind) = single_char_token(ch) {
                tokens.push(Token(kind, pos));
                continue;
            }

            // 5. Identifier or keyword
            if ch.is_ascii_alphabetic() || ch == '_' {
                tokens.push(Token(self.parse_ident(ch, idx), pos));
                continue;
            }

            // 6. String literal
            if ch == '"' {
                tokens.push(Token(self.parse_string(ch, idx), pos));
                continue;
            }

            // 6. Number literal
            if ch.is_ascii_digit() {
                tokens.push(Token(self.parse_number(ch, idx), pos));
                continue;
            }

            // 7. Error
            tokens.push(Token(Kind::Error(format!("{ch}")), pos));
        }
        tokens
    }

    fn parse_ident(&mut self, ch: char, idx: usize) -> Kind {
        let mut end = idx + ch.len_utf8();
        while let Some(&(ptr, next_ch)) = self.iter.peek() {
            if next_ch.is_ascii_alphanumeric() || next_ch == '_' {
                self.iter.next();
            } else {
                end = ptr;
                break;
            }
        }
        let lexeme = &self.line[idx..end];
        match keyword(lexeme) {
            Some(kind) => kind,
            None => Kind::Ident(lexeme.to_string()),
        }
    }

    fn parse_string(&mut self, ch: char, idx: usize) -> Kind {
        let start = idx + ch.len_utf8();
        let mut end = start;
        while let Some(&(ptr, next_ch)) = self.iter.peek() {
            if next_ch != '"' {
                self.iter.next();
            } else {
                self.iter.next();
                end = ptr;
                break;
            }
        }
        let lexeme = &self.line[start..end];
        Kind::StringLit(lexeme.to_string())
    }

    fn parse_number(&mut self, ch: char, idx: usize) -> Kind {
        let mut end = idx + ch.len_utf8();
        while let Some(&(ptr, next_ch)) = self.iter.peek() {
            if next_ch.is_ascii_digit() || next_ch == '_' {
                self.iter.next();
            } else {
                end = ptr + next_ch.len_utf8();
                break;
            }
        }
        let lexeme = &self.line[idx..end];
        match lexeme.replace("_", "").parse::<i64>() {
            Ok(value) => Kind::NumberLit(value),
            Err(_) => Kind::Error(lexeme.to_string()),
        }
    }
}

fn double_char_token(c1: char, c2: char) -> Option<Kind> {
    match (c1, c2) {
        ('=', '=') => Some(Kind::EqualEqual),
        ('!', '=') => Some(Kind::ExclEqual),
        ('<', '=') => Some(Kind::LAngleEqual),
        ('>', '=') => Some(Kind::RAngleEqual),
        ('<', '<') => Some(Kind::LAngleLAngle),
        ('>', '>') => Some(Kind::RAngleRAngle),
        ('-', '>') => Some(Kind::Arrow),
        _ => None,
    }
}

fn single_char_token(c: char) -> Option<Kind> {
    match c {
        '=' => Some(Kind::Equal),
        '+' => Some(Kind::Plus),
        '-' => Some(Kind::Minus),
        '*' => Some(Kind::Star),
        '@' => Some(Kind::Atmark),
        '/' => Some(Kind::Slash),
        '%' => Some(Kind::Percent),
        '&' => Some(Kind::Ampasand),
        '|' => Some(Kind::Pipe),
        '^' => Some(Kind::Caret),
        '!' => Some(Kind::Excl),
        ':' => Some(Kind::Colon),
        ';' => Some(Kind::Semicolon),
        ',' => Some(Kind::Comma),
        '.' => Some(Kind::Period),
        '(' => Some(Kind::LParen),
        ')' => Some(Kind::RParen),
        '[' => Some(Kind::LBracket),
        ']' => Some(Kind::RBracket),
        '{' => Some(Kind::LBrace),
        '}' => Some(Kind::RBrace),
        '<' => Some(Kind::LAngle),
        '>' => Some(Kind::RAngle),
        _ => None,
    }
}

fn keyword(s: &str) -> Option<Kind> {
    match s {
        "fn" => Some(Kind::KwFunc),
        "var" => Some(Kind::KwVar),
        "type" => Some(Kind::KwType),
        "return" => Some(Kind::KwReturn),
        "if" => Some(Kind::KwIf),
        "else" => Some(Kind::KwElse),
        "while" => Some(Kind::KwWhile),
        "struct" => Some(Kind::KwStruct),
        "int" => Some(Kind::KwInt),
        _ => None,
    }
}
