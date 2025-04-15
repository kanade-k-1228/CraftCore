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
    ExpectedToken(TokenKind),
    UnexpectedToken(Token),
    ExpectedBut(TokenKind, Token),
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

    // ------------------------------------------------------------------------
    // Parsers
    // ------------------------------------------------------------------------

    fn parse_program(&mut self) -> Defs {
        let mut program = Vec::new();
        while let Some(token) = self.tokens.peek() {
            match token.kind {
                TokenKind::KwType => match self.parse_typedef() {
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

                TokenKind::KwFunc => match self.parse_func() {
                    Ok((name, args, ret, body)) => program.push(Def::Func(name, args, ret, body)),
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

                TokenKind::KwVar => match self.parse_var_def() {
                    Ok((name, typ, init)) => program.push(Def::Var(name, typ, init)),
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
        self.expect(TokenKind::KwType)?;
        let name = self.parse_ident()?;
        self.expect(TokenKind::Colon)?;
        let typ = self.parse_type()?;
        self.optional(TokenKind::Semicolon);
        Ok(Def::Type(name, typ))
    }

    /// Type
    /// `int` | `<ident>` | `*<type>` | `<type>[<expr>]` | `{ <ident> : <type> , ... }` | `<args> -> <type>`
    fn parse_type(&mut self) -> Result<Type, ParseError> {
        if let Some(token) = &self.tokens.peek() {
            match &token.kind {
                // Int
                TokenKind::KwInt => {
                    self.expect(TokenKind::KwInt)?;
                    Ok(Type::Word)
                }

                // Custom
                TokenKind::Ident(_) => {
                    let name = self.parse_ident()?;
                    Ok(Type::Custom(name))
                }

                // Pointer
                TokenKind::Star => {
                    self.expect(TokenKind::Star)?;
                    let dest_type = self.parse_type()?;
                    Ok(Type::Addr(Box::new(dest_type)))
                }

                // Array
                TokenKind::LBracket => {
                    self.expect(TokenKind::LBracket)?;
                    let expr = self.parse_expr()?;
                    self.expect(TokenKind::RBracket)?;
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
                    self.expect(TokenKind::LParen)?;
                    let args = self.parse_args()?;
                    self.expect(TokenKind::RParen)?;
                    self.expect(TokenKind::Arrow)?;
                    let ret = self.parse_type()?;
                    Ok(Type::Func(args, Box::new(ret)))
                }

                _ => Err(ParseError::UnexpectedToken(token.clone().clone())),
            }
        } else {
            Err(ParseError::UnexpectedEOF)
        }
    }

    /// List of expressions
    /// `( <expr> , <expr> , ... )`
    fn parse_list(&mut self) -> Result<Vec<Expr>, ParseError> {
        let mut exprs = Vec::new();
        self.expect(TokenKind::LParen)?;
        while let Some(token) = self.tokens.peek() {
            match token.kind {
                TokenKind::RParen => {
                    self.expect(TokenKind::RParen)?;
                    break;
                }
                TokenKind::Comma => {
                    self.expect(TokenKind::Comma)?;
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
        self.expect(TokenKind::KwFunc)?;
        let name = self.parse_ident()?;
        let args = self.parse_args()?;
        self.expect(TokenKind::Arrow)?;
        let ret = self.parse_type()?;
        let body = self.parse_block()?;
        Ok((name, args, ret, body))
    }

    /// Arguments
    /// `( <ident> : <type> , <ident> : <type> , ... )`
    fn parse_args(&mut self) -> Result<Vec<(String, Type)>, ParseError> {
        let mut args = Vec::new();
        self.expect(TokenKind::LParen)?;
        while let Some(token) = self.tokens.peek() {
            match token.kind {
                TokenKind::Ident(_) => {
                    let name = self.parse_ident()?;
                    self.expect(TokenKind::Colon)?;
                    let typ = self.parse_type()?;
                    args.push((name.clone(), typ));
                    continue;
                }
                TokenKind::Comma => {
                    self.expect(TokenKind::Comma)?;
                    continue;
                }
                TokenKind::RParen => {
                    self.expect(TokenKind::RParen)?;
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
        self.expect(TokenKind::LCurly)?;
        while let Some(token) = self.tokens.peek() {
            match token.kind {
                TokenKind::Ident(_) => {
                    let name = self.parse_ident()?;
                    self.expect(TokenKind::Colon)?;
                    let typ = self.parse_type()?;
                    fields.push((name.clone(), typ));
                    continue;
                }
                TokenKind::Comma => {
                    self.expect(TokenKind::Comma)?;
                    continue;
                }
                TokenKind::RParen => {
                    self.expect(TokenKind::RCurly)?;
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
        self.expect(TokenKind::KwVar)?;
        let name = self.parse_ident()?;
        self.expect(TokenKind::Colon)?;
        let typ = self.parse_type()?;
        let init = if self.check(TokenKind::Equal) {
            self.expect(TokenKind::Equal)?;
            let expr = self.parse_expr()?;
            Some(expr)
        } else {
            None
        };
        self.expect(TokenKind::Semicolon)?;
        Ok((name, typ, init))
    }

    /// Block statement
    /// `{ <stmt> <stmt> ... }`
    fn parse_block(&mut self) -> Result<Stmt, ParseError> {
        self.expect(TokenKind::LCurly)?;
        let mut stmts = Vec::new();
        while !self.check(TokenKind::RCurly) {
            let stmt = self.parse_stmt()?;
            stmts.push(stmt);
        }
        self.expect(TokenKind::RCurly)?;
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
                    self.expect(TokenKind::Semicolon)?;
                    return Ok(Stmt::Expr(Expr::Error));
                }

                // Local variable definition
                TokenKind::KwVar => {
                    let (name, typ, init) = self.parse_var_def()?;
                    return Ok(Stmt::Var(name, typ, init));
                }

                // If Statement: 'if' '(' expr ')' stmt ?('else' stmt)
                TokenKind::KwIf => {
                    self.expect(TokenKind::KwIf)?;
                    self.expect(TokenKind::LParen)?;
                    let cond = self.parse_expr()?;
                    self.expect(TokenKind::RParen)?;
                    let then_stmt = Box::new(self.parse_stmt()?);
                    let else_stmt = if self.check(TokenKind::KwElse) {
                        self.expect(TokenKind::KwElse)?;
                        Some(Box::new(self.parse_stmt()?))
                    } else {
                        None
                    };
                    return Ok(Stmt::Cond(cond, then_stmt, else_stmt));
                }

                // While Statement: 'while' '(' expr ')' stmt
                TokenKind::KwWhile => {
                    self.expect(TokenKind::KwWhile)?;
                    self.expect(TokenKind::LParen)?;
                    let cond = self.parse_expr()?;
                    self.expect(TokenKind::RParen)?;
                    let body = Box::new(self.parse_stmt()?);
                    return Ok(Stmt::Loop(cond, body));
                }

                TokenKind::KwReturn => {
                    // return 文: 'return' ?( expr ) ';'
                    self.expect(TokenKind::KwReturn)?;
                    let expr = if !self.check(TokenKind::Semicolon) {
                        Some(self.parse_expr()?)
                    } else {
                        None
                    };
                    self.expect(TokenKind::Semicolon)?;
                    return Ok(Stmt::Return(expr));
                }

                // Block Statements: '{' stmt* '}'
                TokenKind::LCurly => {
                    return self.parse_block();
                }

                _ => {
                    let expr = self.parse_expr()?;
                    if self.check(TokenKind::Equal) {
                        // 代入文: expr '=' expr ';'
                        self.expect(TokenKind::Equal)?;
                        let rhs = self.parse_expr()?;
                        self.expect(TokenKind::Semicolon)?;
                        return Ok(Stmt::Assign(expr, rhs));
                    } else {
                        // 式文: expr ';'
                        self.expect(TokenKind::Semicolon)?;
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
        while self.check(TokenKind::Pipe) {
            self.expect(TokenKind::Pipe)?;
            let rhs = self.parse_xor()?;
            lhs = Expr::Binary(Box::new(lhs), BinOp::Or, Box::new(rhs))
        }
        Ok(lhs)
    }

    /// Logical XOR expression
    /// `<expr> ^ <expr> ^ ...`
    fn parse_xor(&mut self) -> Result<Expr, ParseError> {
        let mut lhs = self.parse_and()?;
        while self.check(TokenKind::Caret) {
            self.expect(TokenKind::Caret)?;
            let rhs = self.parse_and()?;
            lhs = Expr::Binary(Box::new(lhs), BinOp::Xor, Box::new(rhs))
        }
        Ok(lhs)
    }

    /// Logical AND expression
    /// `<expr> & <expr> & ...`
    fn parse_and(&mut self) -> Result<Expr, ParseError> {
        let mut lhs = self.parse_eq()?;
        while self.check(TokenKind::Ampasand) {
            self.expect(TokenKind::Ampasand)?;
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
            if self.check(TokenKind::EqualEqual) {
                self.expect(TokenKind::EqualEqual)?;
                let rhs = self.parse_relat()?;
                lhs = Expr::Binary(Box::new(lhs), BinOp::Eq, Box::new(rhs));
            } else if self.check(TokenKind::ExclEqual) {
                self.expect(TokenKind::ExclEqual)?;
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
                    self.expect(TokenKind::LAngleEqual)?;
                    let rhs = self.parse_shift()?;
                    Ok(Expr::Binary(Box::new(lhs), BinOp::Le, Box::new(rhs)))
                }
                TokenKind::LAngle => {
                    self.expect(TokenKind::LAngle)?;
                    let rhs = self.parse_shift()?;
                    Ok(Expr::Binary(Box::new(lhs), BinOp::Lt, Box::new(rhs)))
                }
                TokenKind::RAngleEqual => {
                    self.expect(TokenKind::RAngleEqual)?;
                    let rhs = self.parse_shift()?;
                    Ok(Expr::Binary(Box::new(lhs), BinOp::Ge, Box::new(rhs)))
                }
                TokenKind::RAngle => {
                    self.expect(TokenKind::RAngle)?;
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
                //     self.expect(TokenKind::LAngleLAngle)?;
                //     let rhs = self.parse_add()?;
                //     Ok(Expr::Binary(Box::new(lhs), BinOp::LShift, Box::new(rhs)))
                // }
                // TokenKind::RAngleRAngle => {
                //     self.expect(TokenKind::RAngleRAngle)?;
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
                    self.expect(TokenKind::Plus)?;
                    let rhs = self.parse_mul()?;
                    Ok(Expr::Binary(Box::new(lhs), BinOp::Add, Box::new(rhs)))
                }
                TokenKind::Minus => {
                    self.expect(TokenKind::Minus)?;
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
                    self.expect(TokenKind::Star)?;
                    let rhs = self.parse_cast()?;
                    Ok(Expr::Binary(Box::new(lhs), BinOp::Mul, Box::new(rhs)))
                }
                TokenKind::Slash => {
                    self.expect(TokenKind::Slash)?;
                    let rhs = self.parse_cast()?;
                    Ok(Expr::Binary(Box::new(lhs), BinOp::Div, Box::new(rhs)))
                }
                TokenKind::Percent => {
                    self.expect(TokenKind::Percent)?;
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
        if self.check(TokenKind::Colon) {
            self.expect(TokenKind::Colon)?;
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
                    self.expect(TokenKind::Plus)?;
                    let expr = self.parse_unary()?;
                    return Ok(Expr::Unary(UnaryOp::Pos, Box::new(expr)));
                }
                TokenKind::Minus => {
                    self.expect(TokenKind::Minus)?;
                    let expr = self.parse_unary()?;
                    return Ok(Expr::Unary(UnaryOp::Neg, Box::new(expr)));
                }
                TokenKind::Star => {
                    self.expect(TokenKind::Star)?;
                    let expr = self.parse_unary()?;
                    return Ok(Expr::Unary(UnaryOp::Deref, Box::new(expr)));
                }
                TokenKind::Atmark => {
                    self.expect(TokenKind::Atmark)?;
                    let expr = self.parse_unary()?;
                    return Ok(Expr::Unary(UnaryOp::Ref, Box::new(expr)));
                }
                TokenKind::Excl => {
                    self.expect(TokenKind::Excl)?;
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
            if self.check(TokenKind::LParen) {
                let args = self.parse_list()?;
                expr = Expr::Call(Box::new(expr), args);
            }

            // Array access
            if self.check(TokenKind::LBracket) {
                self.expect(TokenKind::LBracket)?;
                let index = self.parse_expr()?;
                self.expect(TokenKind::RBracket)?;
                expr = Expr::ArrayAccess(Box::new(expr), Box::new(index));
            }

            // Member access
            if self.check(TokenKind::Period) {
                self.expect(TokenKind::Period)?;
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
                    self.expect(TokenKind::LParen)?;
                    let inner = self.parse_expr()?;
                    self.expect(TokenKind::RParen)?;
                    Ok(inner)
                }

                // Identifier
                TokenKind::Ident(_) => {
                    let name = self.parse_ident()?;
                    Ok(Expr::Ident(name.clone()))
                }

                TokenKind::LitNumber(val) => {
                    self.expect(TokenKind::LitNumber(val))?;
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

    // ------------------------------------------------------------------------
    // Helpers
    // ------------------------------------------------------------------------

    /// Recover from an error by skipping tokens until a token that matches the condition is found.
    fn recover_to<F: Fn(&Token) -> bool>(&mut self, cond: F) {
        while let Some(tok) = self.tokens.peek() {
            if cond(tok) {
                return;
            }
            self.tokens.next();
        }
    }

    /// Consume if next token is match with kind
    fn expect(&mut self, kind: TokenKind) -> Result<Token, ParseError> {
        if let Some(token) = self.tokens.peek().cloned() {
            if std::mem::discriminant(&token.kind) == std::mem::discriminant(&kind) {
                self.tokens.next();
                return Ok(token);
            } else {
                return Err(ParseError::ExpectedBut(kind, token.clone()));
            }
        }
        return Err(ParseError::UnexpectedEOF);
    }

    fn optional(&mut self, kind: TokenKind) -> Option<Token> {
        self.tokens
            .next_if(|token| std::mem::discriminant(&token.kind) == std::mem::discriminant(&kind))
    }

    /// Check next token is match with kind
    fn check(&mut self, kind: TokenKind) -> bool {
        if let Some(token) = self.tokens.peek() {
            if std::mem::discriminant(&token.kind) == std::mem::discriminant(&kind) {
                return true;
            }
        }
        return false;
    }
}
