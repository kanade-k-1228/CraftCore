// lexer.rs

use crate::token::{Pos, Token, TokenKind};
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
                    tokens.push(Token::new(TokenKind::Comment(comment), pos));
                    break;
                }
            }

            // 3. Double character token
            if let Some(&(_, ch2)) = self.iter.peek() {
                if let Some(kind) = double_char_token(ch, ch2) {
                    self.iter.next();
                    tokens.push(Token::new(kind, pos));
                    continue;
                }
            }

            // 4. Single character token
            if let Some(kind) = single_char_token(ch) {
                tokens.push(Token::new(kind, pos));
                continue;
            }

            // 5. Identifier or keyword
            if ch.is_ascii_alphabetic() || ch == '_' {
                tokens.push(Token {
                    kind: self.parse_ident(ch, idx),
                    pos,
                });
                continue;
            }

            // 6. String literal
            if ch == '"' {
                tokens.push(Token::new(self.parse_string(ch, idx), pos));
                continue;
            }

            // 7. Number literal
            if ch.is_ascii_digit() {
                tokens.push(Token::new(self.parse_number(ch, idx), pos));
                continue;
            }

            // 8. Error
            tokens.push(Token::new(TokenKind::Error(format!("{ch}")), pos));
        }
        tokens
    }

    fn parse_ident(&mut self, ch: char, idx: usize) -> TokenKind {
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
            None => TokenKind::Ident(lexeme.to_string()),
        }
    }

    fn parse_string(&mut self, ch: char, idx: usize) -> TokenKind {
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
        TokenKind::LitString(lexeme.to_string())
    }

    fn parse_number(&mut self, ch: char, idx: usize) -> TokenKind {
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
            Ok(value) => TokenKind::LitNumber(value),
            Err(_) => TokenKind::Error(lexeme.to_string()),
        }
    }
}

fn double_char_token(c1: char, c2: char) -> Option<TokenKind> {
    match (c1, c2) {
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

fn single_char_token(c: char) -> Option<TokenKind> {
    match c {
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
        ':' => Some(TokenKind::Colon),
        ';' => Some(TokenKind::Semicolon),
        ',' => Some(TokenKind::Comma),
        '.' => Some(TokenKind::Period),
        '(' => Some(TokenKind::LParen),
        ')' => Some(TokenKind::RParen),
        '[' => Some(TokenKind::LBracket),
        ']' => Some(TokenKind::RBracket),
        '{' => Some(TokenKind::LBrace),
        '}' => Some(TokenKind::RBrace),
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
        "return" => Some(TokenKind::KwReturn),
        "if" => Some(TokenKind::KwIf),
        "else" => Some(TokenKind::KwElse),
        "while" => Some(TokenKind::KwWhile),
        "struct" => Some(TokenKind::KwStruct),
        "int" => Some(TokenKind::KwInt),
        _ => None,
    }
}
