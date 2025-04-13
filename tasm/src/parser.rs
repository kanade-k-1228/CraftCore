// parser.rs

use crate::ast::{Def, Expr, Program, Stmt, Type};
use crate::token::{Pos, Token, TokenKind};
use std::iter::Peekable;

struct ParseError {
    token: Token,
    msg: String,
}

pub struct Parser<I: Iterator<Item = Token>> {
    tokens: Peekable<I>,
    errors: Vec<ParseError>,
}

impl<I: Iterator<Item = Token>> Parser<I> {
    pub fn new(tokens: I) -> Self {
        Parser {
            tokens: tokens.peekable(),
            errors: Vec::new(),
        }
    }

    fn recover_to<F: Fn(&Token) -> bool>(&mut self, cond: F) {
        while let Some(tok) = self.tokens.peek() {
            if cond(tok) {
                return;
            }
            self.tokens.next();
        }
    }

    fn parse_program(&mut self) -> Program {
        let mut program = Vec::new();
        while let Some(token) = self.tokens.peek() {
            match token.kind {
                TokenKind::KwType => match self.parse_type_def() {
                    Ok(def) => program.push(def),
                    Err(err) => {
                        self.errors.push(err);
                        self.recover_to(|t| {
                            matches!(
                                t.kind,
                                TokenKind::Semicolon
                                    | TokenKind::KwFunc
                                    | TokenKind::KwType
                                    | TokenKind::KwVar
                            )
                        });
                    }
                },

                TokenKind::KwFunc => match self.parse_function_def() {
                    Ok(func) => program.push(func),
                    Err(err) => {
                        self.errors.push(err);
                        self.recover_to(|t| {
                            matches!(
                                t.kind,
                                TokenKind::Semicolon
                                    | TokenKind::KwFunc
                                    | TokenKind::KwType
                                    | TokenKind::KwVar
                            )
                        });
                    }
                },

                TokenKind::KwVar => match self.parse_global_var() {
                    Ok(var) => program.push(var),
                    Err(err) => {
                        self.errors.push(err);
                        self.recover_to(|t| {
                            matches!(
                                t.kind,
                                TokenKind::Semicolon
                                    | TokenKind::KwFunc
                                    | TokenKind::KwType
                                    | TokenKind::KwVar
                            )
                        });
                    }
                },

                _ => {
                    let token = self.tokens.next().unwrap();
                    self.errors.push(ParseError {
                        token,
                        msg: format!("Unexpected token"),
                    });
                    continue;
                }
            }
        }
        Program(program)
    }

    fn parse_type_def(&mut self) -> Result<Def, ParseError> {
        let type_token = self.tokens.next().unwrap();
        debug_assert_eq!(type_token.kind, TokenKind::KwType);

        // 1. Expect ident
        let mut tname = String::new();
        if let Some(token) = self.tokens.next() {
            if let TokenKind::Ident(name) = token.kind {
                tname = name;
            }
        }

        // 2. Expect ":"
        if let Some(token) = self.tokens.next() {
            if token.kind != TokenKind::Colon {
                return Err(ParseError {
                    token,
                    msg: format!("Expect :"),
                });
            }
        }

        // 3. Expect type
        let mut ttyp = Type::Error;
        if let Ok(typ) = self.parse_type() {
            ttyp = typ;
        } else {
            return Err(ParseError {
                token: type_token,
                msg: format!("Expect type"),
            });
        }

        Ok(Def::Type(tname, ttyp))
    }

    fn parse_function_def(&mut self) -> Result<Def, ParseError> {
        todo!()
    }

    fn parse_global_var(&mut self) -> Result<Def, ParseError> {
        todo!()
    }

    fn parse_block(&mut self) -> Result<Stmt, ParseError> {
        todo!()
    }

    fn parse_stmt(&mut self) -> Result<Stmt, ParseError> {
        todo!()
    }

    fn parse_expr(&mut self) -> Result<Expr, ParseError> {
        self.parse_precedence(0)
    }

    fn parse_precedence(&mut self, min_prec: u8) -> Result<Expr, ParseError> {
        todo!()
    }

    fn parse_arg_list(&mut self) -> Result<Vec<Expr>, ParseError> {
        todo!()
    }

    fn parse_type(&mut self) -> Result<Type, ParseError> {
        todo!()
    }

    fn parse_param_list(&mut self) -> Result<Vec<(String, Type)>, ParseError> {
        todo!()
    }
}
