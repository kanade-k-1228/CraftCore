// lexer.rs

use crate::token::{Kind, Token};

pub struct Lexer {
    input: Vec<char>,
    pos: usize,
    line: usize,
    col: usize,
}

impl Lexer {
    pub fn new(source: &str) -> Self {
        Lexer {
            input: source.chars().collect(),
            pos: 0,
            line: 1,
            col: 0,
        }
    }

    fn current_char(&self) -> char {
        self.input.get(self.pos).copied().unwrap_or('\0')
    }

    fn advance(&mut self) {
        if self.pos < self.input.len() {
            let ch = self.input[self.pos];
            self.pos += 1;
            if ch == '\n' {
                // new line: increment line counter and reset column
                self.line += 1;
                self.col = 0;
            } else {
                self.col += 1;
            }
        }
    }

    pub fn next_token(&mut self) -> Token {
        // Skip whitespace and comments
        self.skip_whitespace();
        let ch = self.current_char();

        if ch == '\0' {
            return Token(Kind::EOF, (self.line, self.col));
        }

        match ch {
            '=' => {
                if self.current_char() == '=' {
                    Token(Kind::Equal, (self.line, self.col))
                } else {
                    Token(Kind::Assign, (self.line, self.col))
                }
            }
            '!' => {
                if self.current_char() == '=' {
                    Token(Kind::NotEqual, (self.line, self.col))
                } else {
                    Token(Kind::Error("!".to_string()), (self.line, self.col))
                }
            }
            '<' => {
                let next = self.current_char();
                if next == '=' {
                    Token(Kind::LessEqual, (self.line, self.col))
                } else if next == '<' {
                    Token(Kind::ShiftLeft, (self.line, self.col))
                } else {
                    Token(Kind::Less, (self.line, self.col))
                }
            }
            '>' => {
                let next = self.current_char();
                if next == '=' {
                    Token(Kind::GreaterEqual, (self.line, self.col))
                } else if next == '>' {
                    Token(Kind::ShiftRight, (self.line, self.col))
                } else {
                    Token(Kind::Greater, (self.line, self.col))
                }
            }
            '+' => Token(Kind::Plus, (self.line, self.col)),
            '-' => Token(Kind::Minus, (self.line, self.col)),
            '*' => Token(Kind::Asterisk, (self.line, self.col)),
            '/' => Token(Kind::Slash, (self.line, self.col)),
            '%' => Token(Kind::Percent, (self.line, self.col)),
            '&' => Token(Kind::Ampersand, (self.line, self.col)),
            '|' => Token(Kind::Pipe, (self.line, self.col)),
            '^' => Token(Kind::Caret, (self.line, self.col)),
            ':' => Token(Kind::Colon, (self.line, self.col)),
            ';' => Token(Kind::Semicolon, (self.line, self.col)),
            ',' => Token(Kind::Comma, (self.line, self.col)),
            '.' => Token(Kind::Dot, (self.line, self.col)),
            '@' => Token(Kind::AtSign, (self.line, self.col)),
            '(' => Token(Kind::LParen, (self.line, self.col)),
            ')' => Token(Kind::RParen, (self.line, self.col)),
            '[' => Token(Kind::LBracket, (self.line, self.col)),
            ']' => Token(Kind::RBracket, (self.line, self.col)),
            '{' => Token(Kind::LBrace, (self.line, self.col)),
            '}' => Token(Kind::RBrace, (self.line, self.col)),

            // Identifier or keyword
            c if c.is_ascii_alphabetic() || c == '_' => {
                let ident = self.read_identifier();
                match ident.as_str() {
                    "func" => Token(Kind::Func, (self.line, self.col)),
                    "var" => Token(Kind::Var, (self.line, self.col)),
                    "type" => Token(Kind::Type, (self.line, self.col)),
                    "return" => Token(Kind::Return, (self.line, self.col)),
                    "if" => Token(Kind::If, (self.line, self.col)),
                    "else" => Token(Kind::Else, (self.line, self.col)),
                    "while" => Token(Kind::While, (self.line, self.col)),
                    _ => Token(Kind::Ident(ident), (self.line, self.col)),
                }
            }

            // Number literal
            c if c.is_ascii_digit() => {
                let number_text = self.read_number();
                if let Ok(value) = number_text.parse::<i64>() {
                    Token(Kind::Number(value), (self.line, self.col))
                } else {
                    Token(
                        Kind::Error(format!("Invalid number: {}", number_text)),
                        (self.line, self.col),
                    )
                }
            }

            // Anything else is illegal
            other => Token(Kind::Error(other.to_string()), (self.line, self.col)),
        }
    }

    /// Helper: skip whitespace and comments.
    fn skip_whitespace(&mut self) {
        loop {
            match self.current_char() {
                // whitespace characters to skip
                ' ' | '\t' | '\r' | '\n' => {
                    self.advance();
                }
                // handle '//' comments: skip until newline or EOF
                '/' => {
                    // Peek next char to see if it's a comment
                    if self.input.get(self.pos + 1).copied().unwrap_or('\0') == '/' {
                        // skip the '//' and everything until end of line
                        self.advance();
                        self.advance();
                        while self.current_char() != '\n' && self.current_char() != '\0' {
                            self.advance();
                        }
                        continue; // after newline, loop will handle skipping it
                    } else {
                        // not a comment, just a division operator or something, break (don't skip)
                        break;
                    }
                }
                _ => break,
            }
        }
    }

    /// Helper: read a sequence of identifier characters (letters, digits, underscores).
    fn read_identifier(&mut self) -> String {
        let start = self.pos;
        while self.current_char().is_ascii_alphanumeric() || self.current_char() == '_' {
            self.advance();
        }
        self.input[start..self.pos].iter().collect()
    }

    /// Helper: read a sequence of digits (for integer literals).
    fn read_number(&mut self) -> String {
        let start = self.pos;
        while self.current_char().is_ascii_digit() {
            self.advance();
        }
        self.input[start..self.pos].iter().collect()
    }
}
