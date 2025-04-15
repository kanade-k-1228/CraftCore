// parser.rs

use crate::ast::{BinOp, Def, Defs, Expr, Stmt, Type, UnaryOp};
use crate::token::{Token, TokenKind};
use std::iter::Peekable;

pub struct Parser<I: Iterator<Item = Token>> {
    tokens: Peekable<I>,
    errors: Vec<ParseError>,
}

#[derive(Debug, Clone)]
pub enum ParseError {
    TODO,
    UnexpectedEOF,
    UnexpectedToken(Token),
    InvalidType(Token),
    InvalidFunction(Token),
    InvalidVariable(Token),
}

impl<I: Iterator<Item = Token>> Parser<I> {
    pub fn new(tokens: I) -> Self {
        Parser {
            tokens: tokens.peekable(),
            errors: Vec::new(),
        }
    }

    pub fn parse(mut self) -> (Defs, Vec<ParseError>) {
        let program = self.parse_program();
        return (program, self.errors);
    }
}

// ------------------------------------------------------------------------
// Helpers
// ------------------------------------------------------------------------

#[allow(dead_code)]
impl<I: Iterator<Item = Token>> Parser<I> {
    /// Check next token is match with condition
    fn check_if<F: Fn(&Token) -> bool>(&mut self, cond: F) -> bool {
        if let Some(token) = self.tokens.peek() {
            if cond(token) {
                return true;
            }
        }
        return false;
    }

    /// Consume if next token is match with condition
    fn consume_if<F: Fn(&Token) -> bool>(&mut self, cond: F) -> Option<Token> {
        self.tokens.next_if(|token| cond(token))
    }

    /// Consume until next token is match with condition
    fn consume_until<F: Fn(&Token) -> bool>(&mut self, cond: F) {
        while let Some(tok) = self.tokens.peek() {
            if cond(tok) {
                return;
            }
            self.tokens.next();
        }
    }

    /// Next token must be match with condition
    fn expect_tobe<F: Fn(&Token) -> bool>(&mut self, cond: F) -> Result<Token, ParseError> {
        if let Some(token) = self.tokens.peek().cloned() {
            if cond(&token) {
                self.tokens.next();
                Ok(token)
            } else {
                Err(ParseError::UnexpectedToken(token))
            }
        } else {
            Err(ParseError::UnexpectedEOF)
        }
    }
}

macro_rules! check {
    ($parser:expr, $kind:pat) => {
        $parser.check_if(|token| matches!(&token.kind, $kind))
    };
}

macro_rules! expect {
    ($parser:expr, $kind:pat) => {
        $parser.expect_tobe(|token| matches!(&token.kind, $kind))
    };
}

#[allow(unused_macros)]
macro_rules! optional {
    ($parser:expr, $kind:pat) => {
        $parser.consume_if(|token| matches!(&token.kind, $kind))
    };
}

macro_rules! recover {
    ($parser:expr, $kind:pat) => {
        $parser.consume_until(|token| matches!(&token.kind, $kind))
    };
}

// ------------------------------------------------------------------------
// Parsers
// ------------------------------------------------------------------------

impl<I: Iterator<Item = Token>> Parser<I> {
    fn parse_program(&mut self) -> Defs {
        let mut program = Vec::new();
        while let Some(token) = self.tokens.peek() {
            match token.kind {
                TokenKind::KwType => match self.parse_typedef() {
                    Ok(def) => program.push(def),
                    Err(err) => {
                        self.errors.push(err);
                        recover!(
                            self,
                            TokenKind::Semicolon
                                | TokenKind::KwFunc
                                | TokenKind::KwType
                                | TokenKind::KwVar
                        );
                    }
                },

                TokenKind::KwFunc => match self.parse_func() {
                    Ok((name, args, ret, body)) => program.push(Def::Func(name, args, ret, body)),
                    Err(err) => {
                        self.errors.push(err);
                        self.consume_until(|t| {
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

                TokenKind::KwVar => match self.parse_var_def() {
                    Ok((name, typ, init)) => program.push(Def::Var(name, typ, init)),
                    Err(err) => {
                        self.errors.push(err);
                        self.consume_until(|t| {
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
                    self.errors.push(ParseError::UnexpectedToken(token));
                    continue;
                }
            }
        }
        Defs(program)
    }

    /// Type definition
    /// `type <ident> : <type> ;`
    fn parse_typedef(&mut self) -> Result<Def, ParseError> {
        expect!(self, TokenKind::KwType)?;
        let name = self.parse_ident()?;
        expect!(self, TokenKind::Colon)?;
        let typ = self.parse_type()?;
        expect!(self, TokenKind::Semicolon)?;
        Ok(Def::Type(name, typ))
    }

    /// Type
    /// `int` | `<ident>` | `*<type>` | `<type>[<expr>]` | `{ <ident> : <type> , ... }` | `<args> -> <type>`
    fn parse_type(&mut self) -> Result<Type, ParseError> {
        if let Some(token) = self.tokens.peek() {
            match token.kind {
                // Int
                TokenKind::KwInt => {
                    expect!(self, TokenKind::KwInt)?;
                    Ok(Type::Word)
                }

                // Custom
                TokenKind::Ident(_) => {
                    let name = self.parse_ident()?;
                    Ok(Type::Custom(name))
                }

                // Pointer
                TokenKind::Star => {
                    expect!(self, TokenKind::Star)?;
                    let dest_type = self.parse_type()?;
                    Ok(Type::Addr(Box::new(dest_type)))
                }

                // Array
                TokenKind::LBracket => {
                    expect!(self, TokenKind::LBracket)?;
                    let expr = self.parse_expr()?;
                    expect!(self, TokenKind::RBracket)?;
                    let elem_type = self.parse_type()?;
                    Ok(Type::Array(expr, Box::new(elem_type)))
                }

                // Struct
                TokenKind::LCurly => {
                    let fields = self.parse_struct()?;
                    Ok(Type::Struct(fields))
                }

                // Function
                TokenKind::LParen => {
                    expect!(self, TokenKind::LParen)?;
                    let args = self.parse_args()?;
                    expect!(self, TokenKind::RParen)?;
                    expect!(self, TokenKind::Arrow)?;
                    let ret = self.parse_type()?;
                    Ok(Type::Func(args, Box::new(ret)))
                }

                _ => Err(ParseError::UnexpectedToken(token.clone())),
            }
        } else {
            Err(ParseError::UnexpectedEOF)
        }
    }

    /// List of expressions
    /// `( <expr> , <expr> , ... )`
    fn parse_list(&mut self) -> Result<Vec<Expr>, ParseError> {
        let mut exprs = Vec::new();
        expect!(self, TokenKind::LParen)?;
        while let Some(token) = self.tokens.peek() {
            match token.kind {
                TokenKind::RParen => {
                    expect!(self, TokenKind::RParen)?;
                    break;
                }
                TokenKind::Comma => {
                    expect!(self, TokenKind::Comma)?;
                    continue;
                }
                _ => {
                    let expr = self.parse_expr()?;
                    exprs.push(expr);
                }
            }
        }
        Ok(exprs)
    }

    /// Function
    /// `fn <ident> <args> -> <type> <block>`
    fn parse_func(&mut self) -> Result<(String, Vec<(String, Type)>, Type, Stmt), ParseError> {
        expect!(self, TokenKind::KwFunc)?;
        let name = self.parse_ident()?;
        let args = self.parse_args()?;
        expect!(self, TokenKind::Arrow)?;
        let ret = self.parse_type()?;
        let body = self.parse_block()?;
        Ok((name, args, ret, body))
    }

    /// Arguments
    /// `( <ident> : <type> , <ident> : <type> , ... )`
    fn parse_args(&mut self) -> Result<Vec<(String, Type)>, ParseError> {
        let mut args = Vec::new();
        expect!(self, TokenKind::LParen)?;
        while let Some(token) = self.tokens.peek() {
            match token.kind {
                TokenKind::Ident(_) => {
                    let name = self.parse_ident()?;
                    expect!(self, TokenKind::Colon)?;
                    let typ = self.parse_type()?;
                    args.push((name.clone(), typ));
                    continue;
                }
                TokenKind::Comma => {
                    expect!(self, TokenKind::Comma)?;
                    continue;
                }
                TokenKind::RParen => {
                    expect!(self, TokenKind::RParen)?;
                    return Ok(args);
                }
                _ => {
                    return Err(ParseError::UnexpectedToken(token.clone()))?;
                }
            }
        }
        Err(ParseError::UnexpectedEOF)
    }

    /// Struct type
    /// `{ <ident> : <type> , ... }`
    fn parse_struct(&mut self) -> Result<Vec<(String, Type)>, ParseError> {
        let mut fields = Vec::new();
        expect!(self, TokenKind::LCurly)?;
        while let Some(token) = self.tokens.peek() {
            match token.kind {
                TokenKind::Ident(_) => {
                    let name = self.parse_ident()?;
                    expect!(self, TokenKind::Colon)?;
                    let typ = self.parse_type()?;
                    fields.push((name.clone(), typ));
                    continue;
                }
                TokenKind::Comma => {
                    expect!(self, TokenKind::Comma)?;
                    continue;
                }
                TokenKind::RParen => {
                    expect!(self, TokenKind::RCurly)?;
                    return Ok(fields);
                }
                _ => {
                    return Err(ParseError::UnexpectedToken(token.clone()))?;
                }
            }
        }
        Err(ParseError::UnexpectedEOF)
    }

    /// Variable definition
    /// `var <ident> : <type> = <expr> ;`
    fn parse_var_def(&mut self) -> Result<(String, Type, Option<Expr>), ParseError> {
        expect!(self, TokenKind::KwVar)?;
        let name = self.parse_ident()?;
        expect!(self, TokenKind::Colon)?;
        let typ = self.parse_type()?;
        let init = if check!(self, TokenKind::Equal) {
            expect!(self, TokenKind::Equal)?;
            let expr = self.parse_expr()?;
            Some(expr)
        } else {
            None
        };
        expect!(self, TokenKind::Semicolon)?;
        Ok((name, typ, init))
    }

    /// Block statement
    /// `{ <stmt> <stmt> ... }`
    fn parse_block(&mut self) -> Result<Stmt, ParseError> {
        expect!(self, TokenKind::LCurly)?;
        let mut stmts = Vec::new();
        while !check!(self, TokenKind::RCurly) {
            let stmt = self.parse_stmt()?;
            stmts.push(stmt);
        }
        expect!(self, TokenKind::RCurly)?;
        Ok(Stmt::Block(stmts))
    }

    /// Statement
    /// `;` | `var <ident> : <type> = <expr> ;` | `if ( <expr> ) <stmt> [ else <stmt> ]` |
    /// `while ( <expr> ) <stmt>` | `return [ <expr> ] ;` | `{ <stmt> ... }` | `<expr> ;`
    fn parse_stmt(&mut self) -> Result<Stmt, ParseError> {
        if let Some(token) = &self.tokens.peek() {
            match &token.kind {
                // Empty statement
                TokenKind::Semicolon => {
                    expect!(self, TokenKind::Semicolon)?;
                    return Ok(Stmt::Expr(Expr::Error));
                }

                // Local variable definition
                TokenKind::KwVar => {
                    let (name, typ, init) = self.parse_var_def()?;
                    return Ok(Stmt::Var(name, typ, init));
                }

                // If Statement: 'if' '(' expr ')' stmt ?('else' stmt)
                TokenKind::KwIf => {
                    expect!(self, TokenKind::KwIf)?;
                    expect!(self, TokenKind::LParen)?;
                    let cond = self.parse_expr()?;
                    expect!(self, TokenKind::RParen)?;
                    let then_stmt = Box::new(self.parse_stmt()?);
                    let else_stmt = if check!(self, TokenKind::KwElse) {
                        expect!(self, TokenKind::KwElse)?;
                        Some(Box::new(self.parse_stmt()?))
                    } else {
                        None
                    };
                    return Ok(Stmt::Cond(cond, then_stmt, else_stmt));
                }

                // While Statement: 'while' '(' expr ')' stmt
                TokenKind::KwWhile => {
                    expect!(self, TokenKind::KwWhile)?;
                    expect!(self, TokenKind::LParen)?;
                    let cond = self.parse_expr()?;
                    expect!(self, TokenKind::RParen)?;
                    let body = Box::new(self.parse_stmt()?);
                    return Ok(Stmt::Loop(cond, body));
                }

                TokenKind::KwReturn => {
                    // return 文: 'return' ?( expr ) ';'
                    expect!(self, TokenKind::KwReturn)?;
                    let expr = if !check!(self, TokenKind::Semicolon) {
                        Some(self.parse_expr()?)
                    } else {
                        None
                    };
                    expect!(self, TokenKind::Semicolon)?;
                    return Ok(Stmt::Return(expr));
                }

                // Block Statements: '{' stmt* '}'
                TokenKind::LCurly => {
                    return self.parse_block();
                }

                _ => {
                    let expr = self.parse_expr()?;
                    if check!(self, TokenKind::Equal) {
                        // 代入文: expr '=' expr ';'
                        expect!(self, TokenKind::Equal)?;
                        let rhs = self.parse_expr()?;
                        expect!(self, TokenKind::Semicolon)?;
                        return Ok(Stmt::Assign(expr, rhs));
                    } else {
                        // 式文: expr ';'
                        expect!(self, TokenKind::Semicolon)?;
                        return Ok(Stmt::Expr(expr));
                    }
                }
            }
        }
        return Err(ParseError::TODO);
    }

    /// Expression
    fn parse_expr(&mut self) -> Result<Expr, ParseError> {
        self.parse_or()
    }

    /// Logical OR expression
    /// `<expr> | <expr> | ...`
    fn parse_or(&mut self) -> Result<Expr, ParseError> {
        let mut lhs = self.parse_xor()?;
        while check!(self, TokenKind::Pipe) {
            expect!(self, TokenKind::Pipe)?;
            let rhs = self.parse_xor()?;
            lhs = Expr::Binary(Box::new(lhs), BinOp::Or, Box::new(rhs))
        }
        Ok(lhs)
    }

    /// Logical XOR expression
    /// `<expr> ^ <expr> ^ ...`
    fn parse_xor(&mut self) -> Result<Expr, ParseError> {
        let mut lhs = self.parse_and()?;
        while check!(self, TokenKind::Caret) {
            expect!(self, TokenKind::Caret)?;
            let rhs = self.parse_and()?;
            lhs = Expr::Binary(Box::new(lhs), BinOp::Xor, Box::new(rhs))
        }
        Ok(lhs)
    }

    /// Logical AND expression
    /// `<expr> & <expr> & ...`
    fn parse_and(&mut self) -> Result<Expr, ParseError> {
        let mut lhs = self.parse_eq()?;
        while check!(self, TokenKind::Ampasand) {
            expect!(self, TokenKind::Ampasand)?;
            let rhs = self.parse_eq()?;
            lhs = Expr::Binary(Box::new(lhs), BinOp::And, Box::new(rhs))
        }
        Ok(lhs)
    }

    /// Equality expression
    /// `<expr> == <expr>` | `<expr> != <expr>`
    fn parse_eq(&mut self) -> Result<Expr, ParseError> {
        let mut lhs = self.parse_relat()?;
        loop {
            if check!(self, TokenKind::EqualEqual) {
                expect!(self, TokenKind::EqualEqual)?;
                let rhs = self.parse_relat()?;
                lhs = Expr::Binary(Box::new(lhs), BinOp::Eq, Box::new(rhs));
            } else if check!(self, TokenKind::ExclEqual) {
                expect!(self, TokenKind::ExclEqual)?;
                let rhs = self.parse_relat()?;
                lhs = Expr::Binary(Box::new(lhs), BinOp::Ne, Box::new(rhs));
            } else {
                break;
            }
        }
        Ok(lhs)
    }

    /// Relational expression
    /// `<expr> < <expr>` | `<expr> <= <expr>` | `<expr> > <expr>` | `<expr> >= <expr>`
    fn parse_relat(&mut self) -> Result<Expr, ParseError> {
        let lhs = self.parse_shift()?;
        if let Some(token) = self.tokens.peek() {
            match token.kind {
                TokenKind::LAngleEqual => {
                    expect!(self, TokenKind::LAngleEqual)?;
                    let rhs = self.parse_shift()?;
                    Ok(Expr::Binary(Box::new(lhs), BinOp::Le, Box::new(rhs)))
                }
                TokenKind::LAngle => {
                    expect!(self, TokenKind::LAngle)?;
                    let rhs = self.parse_shift()?;
                    Ok(Expr::Binary(Box::new(lhs), BinOp::Lt, Box::new(rhs)))
                }
                TokenKind::RAngleEqual => {
                    expect!(self, TokenKind::RAngleEqual)?;
                    let rhs = self.parse_shift()?;
                    Ok(Expr::Binary(Box::new(lhs), BinOp::Ge, Box::new(rhs)))
                }
                TokenKind::RAngle => {
                    expect!(self, TokenKind::RAngle)?;
                    let rhs = self.parse_shift()?;
                    Ok(Expr::Binary(Box::new(lhs), BinOp::Gt, Box::new(rhs)))
                }
                _ => Ok(lhs),
            }
        } else {
            Err(ParseError::UnexpectedEOF)
        }
    }

    /// Shift expression
    /// `<expr> << <expr>` | `<expr> >> <expr>`
    fn parse_shift(&mut self) -> Result<Expr, ParseError> {
        let lhs = self.parse_add()?;
        if let Some(token) = self.tokens.peek() {
            match token.kind {
                // TokenKind::LAngleLAngle => {
                //     expect!(self,TokenKind::LAngleLAngle)?;
                //     let rhs = self.parse_add()?;
                //     Ok(Expr::Binary(Box::new(lhs), BinOp::LShift, Box::new(rhs)))
                // }
                // TokenKind::RAngleRAngle => {
                //     expect!(self,TokenKind::RAngleRAngle)?;
                //     let rhs = self.parse_add()?;
                //     Ok(Expr::Binary(Box::new(lhs), BinOp::RShift, Box::new(rhs)))
                // }
                _ => Ok(lhs),
            }
        } else {
            Err(ParseError::UnexpectedEOF)
        }
    }

    /// Additive expression
    /// `<expr> + <expr>` | `<expr> - <expr>`
    fn parse_add(&mut self) -> Result<Expr, ParseError> {
        let lhs = self.parse_mul()?;
        if let Some(token) = self.tokens.peek() {
            match token.kind {
                TokenKind::Plus => {
                    expect!(self, TokenKind::Plus)?;
                    let rhs = self.parse_mul()?;
                    Ok(Expr::Binary(Box::new(lhs), BinOp::Add, Box::new(rhs)))
                }
                TokenKind::Minus => {
                    expect!(self, TokenKind::Minus)?;
                    let rhs = self.parse_mul()?;
                    Ok(Expr::Binary(Box::new(lhs), BinOp::Sub, Box::new(rhs)))
                }
                _ => Ok(lhs),
            }
        } else {
            Err(ParseError::UnexpectedEOF)
        }
    }

    /// Multiplicative expression
    /// `<expr> * <expr>` | `<expr> / <expr>` | `<expr> % <expr>`
    fn parse_mul(&mut self) -> Result<Expr, ParseError> {
        let lhs = self.parse_cast()?;
        if let Some(token) = self.tokens.peek() {
            match token.kind {
                TokenKind::Star => {
                    expect!(self, TokenKind::Star)?;
                    let rhs = self.parse_cast()?;
                    Ok(Expr::Binary(Box::new(lhs), BinOp::Mul, Box::new(rhs)))
                }
                TokenKind::Slash => {
                    expect!(self, TokenKind::Slash)?;
                    let rhs = self.parse_cast()?;
                    Ok(Expr::Binary(Box::new(lhs), BinOp::Div, Box::new(rhs)))
                }
                TokenKind::Percent => {
                    expect!(self, TokenKind::Percent)?;
                    let rhs = self.parse_cast()?;
                    Ok(Expr::Binary(Box::new(lhs), BinOp::Mod, Box::new(rhs)))
                }
                _ => Ok(lhs),
            }
        } else {
            Err(ParseError::UnexpectedEOF)
        }
    }

    /// Cast expression
    /// `<expr> : <type>`
    fn parse_cast(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.parse_unary()?;
        if check!(self, TokenKind::Colon) {
            expect!(self, TokenKind::Colon)?;
            let typ = self.parse_type()?;
            expr = Expr::Cast(Box::new(expr), Box::new(typ));
        }
        Ok(expr)
    }

    /// Unary expression
    /// `+ <expr>` | `- <expr>` | `* <expr>` | `& <expr>` | `@ <expr>` | `! <expr>`
    fn parse_unary(&mut self) -> Result<Expr, ParseError> {
        if let Some(token) = self.tokens.peek() {
            match token.kind {
                TokenKind::Plus => {
                    expect!(self, TokenKind::Plus)?;
                    let expr = self.parse_unary()?;
                    return Ok(Expr::Unary(UnaryOp::Pos, Box::new(expr)));
                }
                TokenKind::Minus => {
                    expect!(self, TokenKind::Minus)?;
                    let expr = self.parse_unary()?;
                    return Ok(Expr::Unary(UnaryOp::Neg, Box::new(expr)));
                }
                TokenKind::Star => {
                    expect!(self, TokenKind::Star)?;
                    let expr = self.parse_unary()?;
                    return Ok(Expr::Unary(UnaryOp::Deref, Box::new(expr)));
                }
                TokenKind::Atmark => {
                    expect!(self, TokenKind::Atmark)?;
                    let expr = self.parse_unary()?;
                    return Ok(Expr::Unary(UnaryOp::Ref, Box::new(expr)));
                }
                TokenKind::Excl => {
                    expect!(self, TokenKind::Excl)?;
                    let expr = self.parse_unary()?;
                    return Ok(Expr::Unary(UnaryOp::Not, Box::new(expr)));
                }
                _ => {}
            }
        }
        self.parse_post()
    }

    /// Postfix expression
    /// `<expr> ( <expr> , <expr> , ... )` | `<expr> [ <expr> ]` | `<expr> . <ident>`
    fn parse_post(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.parse_prim()?;
        loop {
            // Function call
            if check!(self, TokenKind::LParen) {
                let args = self.parse_list()?;
                expr = Expr::Call(Box::new(expr), args);
            }

            // Array access
            if check!(self, TokenKind::LBracket) {
                expect!(self, TokenKind::LBracket)?;
                let index = self.parse_expr()?;
                expect!(self, TokenKind::RBracket)?;
                expr = Expr::ArrayAccess(Box::new(expr), Box::new(index));
            }

            // Member access
            if check!(self, TokenKind::Period) {
                expect!(self, TokenKind::Period)?;
                let field = self.parse_ident()?;
                expr = Expr::Member(Box::new(expr), field);
            } else {
                break;
            }
        }
        Ok(expr)
    }

    /// Primary expression
    /// `( <expr> )` | `<ident>` | `<number>` | `<string>`
    fn parse_prim(&mut self) -> Result<Expr, ParseError> {
        if let Some(token) = self.tokens.peek() {
            match token.kind {
                // Nested expression:
                TokenKind::LParen => {
                    expect!(self, TokenKind::LParen)?;
                    let inner = self.parse_expr()?;
                    expect!(self, TokenKind::RParen)?;
                    Ok(inner)
                }

                // Identifier
                TokenKind::Ident(_) => {
                    let name = self.parse_ident()?;
                    Ok(Expr::Ident(name.clone()))
                }

                TokenKind::LitNumber(val) => {
                    expect!(self, TokenKind::LitNumber(_))?;
                    Ok(Expr::IntLit(val))
                }

                TokenKind::LitString(_) => {
                    let s = self.parse_string()?;
                    return Ok(Expr::StringLit(s));
                }
                _ => Err(ParseError::UnexpectedToken(token.clone())),
            }
        } else {
            Err(ParseError::UnexpectedEOF)
        }
    }

    /// Identifier
    /// `<ident>`
    fn parse_ident(&mut self) -> Result<String, ParseError> {
        if let Some(Token {
            kind: TokenKind::Ident(s),
            pos: _,
        }) = &self.tokens.peek().cloned()
        {
            self.tokens.next();
            return Ok(s.clone());
        }
        return Err(ParseError::TODO);
    }

    /// String literal
    /// `"abc"`
    fn parse_string(&mut self) -> Result<String, ParseError> {
        if let Some(Token {
            kind: TokenKind::LitString(s),
            pos: _,
        }) = &self.tokens.peek().cloned()
        {
            self.tokens.next();
            return Ok(s.clone());
        }
        return Err(ParseError::TODO);
    }
}
