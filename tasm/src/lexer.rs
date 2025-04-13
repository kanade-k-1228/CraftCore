// lexer.rs

use crate::token::{Kind, Pos, Token};

pub struct LineLexer {
    line: String, // ソースコードの1行
    pos: Pos,     // 現在位置 (file_id, line_number, column_index)
}

impl LineLexer {
    pub fn new(line: String, pos: Pos) -> Self {
        LineLexer { line, pos }
    }

    pub fn parse(mut self) -> Vec<Token> {
        let mut tokens = Vec::new();
        while let Some(token) = self.next_token() {
            tokens.push(token);
        }
        tokens
    }

    pub fn next_token(&mut self) -> Option<Token> {
        // 0. Consume whitespace
        loop {
            match self.line[self.pos.col..].chars().nth(0) {
                Some(ch) if ch.is_whitespace() => {
                    self.increment(1);
                }
                _ => break,
            }
        }

        let slice = &self.line[self.pos.col..];

        // 1. End of line
        let ch = match slice.chars().nth(0) {
            None => return None,
            Some(ch) => ch,
        };

        // 2. Double character tokens
        if let Some(ch1) = slice.chars().nth(1) {
            // Comment line
            if ch == '/' && ch1 == '/' {
                self.pos.col += slice.len();
                return Some(self.token(Kind::Comment("".to_string())));
            }

            // 2. Double character tokens
            if let Some(kind) = double_char_token(ch, ch1) {
                self.increment(2);
                return Some(self.token(kind));
            }
        }

        // 3. Single character tokens
        if let Some(kind) = single_char_token(ch) {
            self.increment(1);
            return Some(self.token(kind));
        }

        // 4. Identifier or Keyword
        if ch.is_ascii_alphabetic() || ch == '_' {
            let start_idx = self.pos.col;
            while self.pos.col < self.line.len() {
                let c = self.line.as_bytes()[self.pos.col] as char;
                if c.is_ascii_alphanumeric() || c == '_' {
                    self.pos.col += 1;
                } else {
                    break;
                }
            }
            let lexeme = &self.line[start_idx..self.pos.col];
            match keyword(lexeme) {
                Some(kind) => return Some(self.token(kind)),
                None => return Some(self.token(Kind::Ident(lexeme.to_string()))),
            }
        }

        // 5. Number literal
        if ch.is_ascii_digit() {
            let start_idx = self.pos.col;
            while self.pos.col < self.line.len() {
                let c = self.line.as_bytes()[self.pos.col] as char;
                if c.is_ascii_digit() || c == '_' {
                    self.increment(1);
                } else {
                    break;
                }
            }
            let lexeme = &self.line[start_idx..self.pos.col];
            let value = lexeme.parse::<i64>().unwrap_or(0);
            return Some(self.token(Kind::NumberLit(value)));
        }

        // 6. If none of the above matched, mark as error
        self.increment(1); // consume the unknown character
        let err_text = ch.to_string();
        return Some(self.token(Kind::Error(err_text)));
    }

    fn token(&self, kind: Kind) -> Token {
        Token(kind, self.pos.clone())
    }

    fn increment(&mut self, n: usize) {
        self.pos.col += n;
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
