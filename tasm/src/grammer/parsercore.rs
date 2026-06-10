use super::token::{Pos, Token, TokenKind};
use crate::error::Error;
use std::iter::Peekable;

pub struct Parser<I: Iterator<Item = Token>> {
    tokens: Peekable<I>,
    errors: Vec<Error>,
}

impl<I: Iterator<Item = Token>> Parser<I> {
    pub fn new(tokens: I) -> Self {
        Parser {
            tokens: tokens.peekable(),
            errors: Vec::new(),
        }
    }

    pub fn error(&mut self, e: Error) {
        self.errors.push(e);
    }

    pub fn geterrors(self) -> Vec<Error> {
        self.errors
    }
}

impl<I: Iterator<Item = Token>> Parser<I> {
    /// Skip comments and report invalid tokens from the lexer
    fn skip(&mut self) {
        while let Some(token) = self.tokens.peek() {
            match &token.kind {
                TokenKind::Comment(_) => {
                    self.tokens.next();
                }
                TokenKind::Error(lexeme) => {
                    let err = Error::InvalidToken(token.pos.clone(), lexeme.clone());
                    self.tokens.next();
                    self.errors.push(err);
                }
                _ => break,
            }
        }
    }

    /// Peek : Watch next token without consuming it
    pub fn peek(&mut self) -> Option<&Token> {
        self.skip();
        self.tokens.peek()
    }

    /// Next : Consume next token and return it
    pub fn next(&mut self) -> Option<Token> {
        self.skip();
        self.tokens.next()
    }

    /// Peek and check next token is match with condition
    pub fn check_if<F: Fn(&Token) -> bool>(&mut self, cond: F) -> bool {
        self.skip();
        if let Some(token) = self.tokens.peek() {
            if cond(token) {
                return true;
            }
        }
        return false;
    }

    /// Consume if next token is match with condition
    pub fn consume_if<F: Fn(&Token) -> bool>(&mut self, cond: F) -> Option<Token> {
        self.skip();
        self.tokens.next_if(|token| cond(token))
    }

    /// Consume until next token is match with condition
    pub fn consume_until<F: Fn(&Token) -> bool>(&mut self, cond: F) {
        while let Some(tok) = self.tokens.peek() {
            if cond(tok) {
                return;
            }
            self.tokens.next();
        }
    }

    /// Next token must be match with condition (skipping comments)
    pub fn expect_tobe<F: Fn(&Token) -> bool>(&mut self, cond: F) -> Result<Token, Error> {
        self.skip();
        if let Some(token) = self.tokens.peek().cloned() {
            if cond(&token) {
                self.tokens.next();
                Ok(token)
            } else {
                Err(Error::UnexpectedToken(token.pos.clone(), token.clone()))
            }
        } else {
            Err(Error::UnexpectedEOF(Pos::default()))
        }
    }
}

#[macro_export]
macro_rules! check {
    ($parser:expr, $kind:pat) => {
        $parser.check_if(|token| matches!(&token.kind, $kind))
    };
}

#[macro_export]
macro_rules! expect {
    ($parser:expr, $kind:pat) => {
        $parser.expect_tobe(|token| matches!(&token.kind, $kind))
    };
}

#[macro_export]
macro_rules! optional {
    ($parser:expr, $trigger:pat, $following:expr) => {
        if check!($parser, $trigger) {
            expect!($parser, $trigger)?;
            Some($following)
        } else {
            None
        }
    };
    ($parser:expr, $trigger:pat) => {
        $parser.consume_if(|token| matches!(&token.kind, $trigger))
    };
}

/// Parse repeated elements with optional delimiters
/// 3 args: { element } terminal (no delimiter)
/// 4 args: [ element { delimiter element } ] terminal (with delimiter)
#[macro_export]
macro_rules! repeat {
    // Without delimiter: { element } terminal
    ($parser:expr, $elem:expr, $terminal:pat) => {{
        let mut items = Vec::new();
        while !check!($parser, $terminal) {
            items.push($elem?);
        }
        items
    }};

    // With delimiter: [ element { delimiter element } [ delimiter ] ] terminal
    ($parser:expr, $elem:expr, $delimiter:pat, $terminal:pat) => {{
        let mut items = Vec::new();
        if !check!($parser, $terminal) {
            items.push($elem?);
            while check!($parser, $delimiter) {
                expect!($parser, $delimiter)?;
                if check!($parser, $terminal) {
                    break;
                }
                items.push($elem?);
            }
        }
        items
    }};
}
