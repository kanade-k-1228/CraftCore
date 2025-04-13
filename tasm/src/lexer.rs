// lexer.rs

use crate::token::{Kind, Pos, Token};
use std::iter::{Enumerate, Peekable};
use std::str::CharIndices;

pub struct LineLexer<'a> {
    line: &'a str,
    iter: Peekable<Enumerate<CharIndices<'a>>>,
    file_idx: usize,
    line_idx: usize,
}

impl<'a> LineLexer<'a> {
    pub fn new(line: &'a str, file_idx: usize, line_idx: usize) -> Self {
        Self {
            line,
            iter: line.char_indices().enumerate().peekable(),
            file_idx,
            line_idx,
        }
    }

    pub fn parse(mut self) -> Vec<Token> {
        let mut tokens = Vec::new();
        while let Some(token) = self.next_token() {
            tokens.push(token);
        }
        tokens
    }

    fn next_token(&mut self) -> Option<Token> {
        // 0. Skip whitespaces
        while self
            .iter
            .next_if(|(_, (_, ch))| ch.is_whitespace())
            .is_some()
        {}

        // 1. End of line
        let (idx, (start, c)) = self.iter.next()?;

        // 2. Comment
        if c == '/' {
            if let Some(_) = self.iter.next_if(|(_, (_, ch))| *ch == '/') {
                let comment = self.line[start..].to_string();
                return self.token(Kind::Comment(comment), idx);
            }
        }

        // 3. Double character token
        if let Some(&(_, (_, c2))) = self.iter.peek() {
            if let Some(kind) = double_char_token(c, c2) {
                self.iter.next();
                return self.token(kind, idx);
            }
        }

        // 4. Single character token
        if let Some(kind) = single_char_token(c) {
            return self.token(kind, idx);
        }

        // 5. Identifier or keyword
        if c.is_ascii_alphabetic() || c == '_' {
            let mut end = start + c.len_utf8();
            while let Some(&(_, (ptr, next_ch))) = self.iter.peek() {
                if next_ch.is_ascii_alphanumeric() || next_ch == '_' {
                    self.iter.next();
                    end = ptr + next_ch.len_utf8();
                } else {
                    end = ptr;
                    break;
                }
            }
            let lexeme = &self.line[start..end];
            if let Some(kind) = keyword(lexeme) {
                return self.token(kind, idx);
            } else {
                return self.token(Kind::Ident(lexeme.to_string()), idx);
            }
        }

        // 6. Number literal
        if c.is_ascii_digit() {
            let mut end = start + c.len_utf8();
            while let Some(&(_, (ptr, next_ch))) = self.iter.peek() {
                if next_ch.is_ascii_digit() || next_ch == '_' {
                    self.iter.next();
                } else {
                    end = ptr;
                    break;
                }
            }
            let lexeme = &self.line[start..end];
            let value = lexeme.replace("_", "").parse::<i64>().unwrap_or(0);
            return self.token(Kind::NumberLit(value), idx);
        }

        // 7. Error
        return self.token(Kind::Error(c.to_string()), idx);
    }

    fn token(&self, kind: Kind, idx: usize) -> Option<Token> {
        Some(Token(
            kind,
            Pos {
                file: self.file_idx,
                col: self.line_idx,
                row: idx,
            },
        ))
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
