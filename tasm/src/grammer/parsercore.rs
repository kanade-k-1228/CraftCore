use super::token::{Token, TokenKind};
use crate::error::ParseError;
use std::iter::Peekable;

pub struct Parser<'a, I: Iterator<Item = Token<'a>>> {
    tokens: Peekable<I>,
    errors: Vec<ParseError>,
}

impl<'a, I: Iterator<Item = Token<'a>>> Parser<'a, I> {
    pub fn new(tokens: I) -> Self {
        Parser {
            tokens: tokens.peekable(),
            errors: Vec::new(),
        }
    }

    pub fn error(&mut self, e: ParseError) {
        self.errors.push(e);
    }

    pub fn geterrors(self) -> Vec<ParseError> {
        self.errors
    }
}

impl<'a, I: Iterator<Item = Token<'a>>> Parser<'a, I> {
    /// Skip all invalid tokens
    fn skip(&mut self) {
        while let Some(token) = self.tokens.peek() {
            match &token.kind {
                TokenKind::Comment(_) | TokenKind::Error(_) => self.tokens.next(),
                _ => break,
            };
        }
    }

    /// Peek : Watch next token without consuming it
    pub fn peek(&mut self) -> Option<&Token<'a>> {
        self.skip();
        self.tokens.peek()
    }

    /// Next : Consume next token and return it
    pub fn next(&mut self) -> Option<Token<'a>> {
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
    pub fn consume_if<F: Fn(&Token) -> bool>(&mut self, cond: F) -> Option<Token<'a>> {
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
    pub fn expect_tobe<F: Fn(&Token) -> bool>(&mut self, cond: F) -> Result<Token<'a>, ParseError> {
        self.skip();
        if let Some(token) = self.tokens.peek().cloned() {
            if cond(&token) {
                self.tokens.next();
                Ok(token)
            } else {
                Err(ParseError::UnexpectedToken(token.into()))
            }
        } else {
            Err(ParseError::UnexpectedEOF)
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

    // With delimiter: [ element { delimiter element } ] terminal
    ($parser:expr, $elem:expr, $delimiter:pat, $terminal:pat) => {{
        let mut items = Vec::new();
        if !check!($parser, $terminal) {
            items.push($elem?);
            while check!($parser, $delimiter) {
                expect!($parser, $delimiter)?;
                items.push($elem?);
            }
        }
        items
    }};
}
