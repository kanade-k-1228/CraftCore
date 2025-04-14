// parser.rs

use crate::ast::{BinOp, Def, Expr, Program, Stmt, Type, UnaryOp};
use crate::token::{self, Token, TokenKind};
use std::iter::Peekable;

pub struct Parser<I: Iterator<Item = Token>> {
    tokens: Peekable<I>,
    errors: Vec<ParseError>,
}

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

    pub fn parse(mut self) -> Result<Program, Vec<ParseError>> {
        let program = self.parse_program();
        if self.errors.is_empty() {
            Ok(program)
        } else {
            Err(self.errors)
        }
    }

    // ------------------------------------------------------------------------
    // Parsers
    // ------------------------------------------------------------------------

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

                TokenKind::KwFunc => match self.parse_func_def() {
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
        Program(program)
    }

    // Type definition
    // 'type' ident ':' type ?(';')
    fn parse_type_def(&mut self) -> Result<Def, ParseError> {
        self.expect(TokenKind::KwType)?;
        let name = self.parse_ident()?;
        self.expect(TokenKind::Colon)?;
        let typ = self.parse_type()?;
        self.optional(TokenKind::Semicolon);
        Ok(Def::Type(name, typ))
    }

    // Function definition
    // 'fn' ident args '->' type block
    fn parse_func_def(&mut self) -> Result<(String, Vec<(String, Type)>, Type, Stmt), ParseError> {
        self.expect(TokenKind::KwFunc)?;
        let name = self.parse_ident()?;
        let args = self.parse_args()?;
        self.expect(TokenKind::RParen)?;
        self.expect(TokenKind::Arrow)?;
        let ret = self.parse_type()?;
        let body = self.parse_block()?;
        Ok((name, args, ret, body))
    }

    // Function arguments
    // '(' (ident ':' type) % ',' ')'
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

    // Variable definition
    // 'var' ident ':' type ?('=' expr) ';'
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

    fn parse_block(&mut self) -> Result<Stmt, ParseError> {
        self.expect(TokenKind::LCurly)?; // '{'
        let mut stmts = Vec::new();
        while !self.check(TokenKind::RCurly) {
            let stmt = self.parse_stmt()?;
            stmts.push(stmt);
        }
        self.expect(TokenKind::RCurly)?; // '}'
        Ok(Stmt::Block(stmts))
    }

    fn parse_stmt(&mut self) -> Result<Stmt, ParseError> {
        if let Some(token) = &self.tokens.peek() {
            match &token.kind {
                TokenKind::Semicolon => {
                    // 空文
                    self.expect(TokenKind::Semicolon)?;
                    return Ok(Stmt::Expr(Expr::Error));
                }
                TokenKind::KwVar => {
                    // ローカル変数定義 'var' ident ':' type ?('=' expr) ';'
                    self.expect(TokenKind::KwVar)?;
                    let name = self.parse_ident()?;
                    self.expect(TokenKind::Colon)?;
                    let typ = self.parse_type()?;
                    let init = if self.check(TokenKind::Equal) {
                        self.expect(TokenKind::Equal)?;
                        Some(self.parse_expr()?)
                    } else {
                        None
                    };
                    self.expect(TokenKind::Semicolon)?;
                    return Ok(Stmt::Var(name, typ, init));
                }
                TokenKind::KwIf => {
                    // If 文 'if' '(' expr ')' stmt ?('else' stmt)
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
                TokenKind::KwWhile => {
                    // While 文: 'while' '(' expr ')' stmt
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
                TokenKind::LCurly => {
                    // 複文: '{' stmt* '}'
                    return self.parse_block();
                }
                _ => {
                    let expr = self.parse_expr()?;
                    if self.check(TokenKind::Equal) {
                        // 代入文: expr '=' expr ';'
                        self.expect(TokenKind::Equal)?;
                        let rhs = self.parse_expr()?;
                        self.expect(TokenKind::Semicolon)?;
                        return Ok(Stmt::Expr(Expr::Assign(Box::new(expr), Box::new(rhs))));
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

    pub fn parse_expr(&mut self) -> Result<Expr, ParseError> {
        self.parse_conditional()
    }

    fn parse_conditional(&mut self) -> Result<Expr, ParseError> {
        let cond = self.parse_or()?;
        if self.check(TokenKind::Question) {
            self.expect(TokenKind::Question)?;
            let lhs = self.parse_or()?;
            self.expect(TokenKind::Colon)?;
            let rhs = self.parse_conditional()?;
            Ok(Expr::Cond(Box::new(cond), Box::new(lhs), Box::new(rhs)))
        } else {
            Ok(cond)
        }
    }

    fn parse_or(&mut self) -> Result<Expr, ParseError> {
        let mut lhs = self.parse_xor()?;
        while self.check(TokenKind::Pipe) {
            self.expect(TokenKind::Pipe)?;
            let rhs = self.parse_xor()?;
            lhs = Expr::Binary(Box::new(lhs), BinOp::Or, Box::new(rhs))
        }
        Ok(lhs)
    }

    fn parse_xor(&mut self) -> Result<Expr, ParseError> {
        let mut lhs = self.parse_and()?;
        while self.check(TokenKind::Caret) {
            self.expect(TokenKind::Caret)?;
            let rhs = self.parse_and()?;
            lhs = Expr::Binary(Box::new(lhs), BinOp::Xor, Box::new(rhs))
        }
        Ok(lhs)
    }

    fn parse_and(&mut self) -> Result<Expr, ParseError> {
        let mut lhs = self.parse_eq()?;
        while self.check(TokenKind::Ampasand) {
            self.expect(TokenKind::Ampasand)?;
            let rhs = self.parse_eq()?;
            lhs = Expr::Binary(Box::new(lhs), BinOp::And, Box::new(rhs))
        }
        Ok(lhs)
    }

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

    fn parse_relat(&mut self) -> Result<Expr, ParseError> {
        let mut lhs = self.parse_shift()?;
        loop {
            let op_kind = match self.peek_kind() {
                Some(TokenKind::Lt) => BinOp::Lt, // '<'
                Some(TokenKind::Le) => BinOp::Le, // '<='
                Some(TokenKind::Gt) => BinOp::Gt, // '>'
                Some(TokenKind::Ge) => BinOp::Ge, // '>='
                _ => break,
            };
            self.next_token()?; // consume the operator token
            let right = self.parse_shift()?;
            lhs = Expr::Binary {
                op: op_kind,
                left: Box::new(lhs),
                right: Box::new(right),
            };
        }
        Ok(lhs)
    }

    fn parse_shift(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.parse_add()?;
        loop {
            let op_kind = match self.peek_kind() {
                Some(TokenKind::LShift) => BinOp::LShift, // '<<'
                Some(TokenKind::RShift) => BinOp::RShift, // '>>'
                _ => break,
            };
            self.next_token()?; // consume the shift operator
            let right = self.parse_add()?;
            expr = Expr::Binary {
                op: op_kind,
                left: Box::new(expr),
                right: Box::new(right),
            };
        }
        Ok(expr)
    }

    fn parse_add(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.parse_mul()?;
        loop {
            let op_kind = match self.peek_kind() {
                Some(TokenKind::Plus) => BinOp::Add,
                Some(TokenKind::Minus) => BinOp::Sub,
                _ => break,
            };
            self.next_token()?; // consume '+' or '-'
            let right = self.parse_mul()?;
            expr = Expr::Binary {
                op: op_kind,
                left: Box::new(expr),
                right: Box::new(right),
            };
        }
        Ok(expr)
    }

    fn parse_mul(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.parse_cast()?;
        loop {
            let op_kind = match self.peek_kind() {
                Some(TokenKind::Star) => BinOp::Mul,
                Some(TokenKind::Slash) => BinOp::Div,
                Some(TokenKind::Percent) => BinOp::Mod,
                _ => break,
            };
            self.next_token()?; // consume the operator
            let right = self.parse_cast()?;
            expr = Expr::Binary {
                op: op_kind,
                left: Box::new(expr),
                right: Box::new(right),
            };
        }
        Ok(expr)
    }

    fn parse_cast(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.parse_unary()?;
        while let Some(TokenKind::Colon) = self.peek_kind() {
            self.next_token()?; // consume ':'
            let ty = self.parse_type()?; // parse a type after colon
            expr = Expr::Cast {
                expr: Box::new(expr),
                ty,
            };
        }
        Ok(expr)
    }

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
        self.parse_factor()
    }

    fn parse_factor(&mut self) -> Result<Expr, ParseError> {
        // Parse primary expression

        if let Some(token) = self.tokens.peek() {
            match token.kind {
                TokenKind::LParen => {
                    self.expect(TokenKind::LParen)?;
                    let inner = self.parse_expr()?; // parse sub-expression inside '(' ... ')'
                    self.expect(TokenKind::RParen)?; // expect matching ')'
                    return Ok(inner); // result of parenthesized expression is the inner expression
                }

                TokenKind::LitNumber(val) => {
                    self.expect(TokenKind::LitNumber(val))?;
                    return Ok(Expr::IntLit(val));
                }
                // TokenKind::LitString(ref s) => {
                //     self.expect(TokenKind::LitString(s))?;
                //     return Ok(Expr::ArrayLit(s.clone()));
                // }
                TokenKind::Ident(ref name) => {
                    self.expect(TokenKind::Ident(String::new()))?;
                    return Ok(Expr::Ident(name.clone()));
                }

                _ => {
                    return Err(ParseError::UnexpectedToken(token.clone()));
                }
            };
        }

        // Parse postfix
        loop {
            if self.consume_if(TokenKind::LParen)? {
                // Function call: parse argument list inside ()
                let mut args = Vec::new();
                if !self.consume_if(TokenKind::RParen)? {
                    // Parse first argument
                    args.push(self.parse_expr()?);
                    // Parse additional arguments separated by commas
                    while self.consume_if(TokenKind::Comma)? {
                        args.push(self.parse_expr()?);
                    }
                    self.expect(TokenKind::RParen)?;
                }
                expr = Expr::Call {
                    func: Box::new(expr),
                    args,
                };
                continue; // continue loop to handle more postfix ops after call
            }

            if self.consume_if(TokenKind::LBracket)? {
                // Array indexing: parse the index expression inside []
                let index_expr = self.parse_expr()?;
                self.expect(TokenKind::RBracket)?;
                expr = Expr::Index {
                    base: Box::new(expr),
                    index: Box::new(index_expr),
                };
                continue;
            }

            if self.consume_if(TokenKind::Dot)? {
                // Member access: expect an identifier after the dot
                let field_token = self.next_token()?;
                if let TokenKind::Ident(field_name) = field_token.kind {
                    expr = Expr::Member {
                        base: Box::new(expr),
                        field: field_name,
                    };
                } else {
                    return Err(ParseError::new(format!(
                        "Expected field name after '.', found {:?}",
                        field_token.kind
                    )));
                }
                continue;
            }

            // (If the language had '->' for pointers, it would be handled similarly here.)
            break;
        }

        Ok(expr)
    }

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
                    self.expect(TokenKind::LCurly)?;
                    let fields = self.parse_args()?;
                    self.expect(TokenKind::RCurly)?;
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

    fn parse_list(&mut self) -> Result<Vec<Expr>, ParseError> {
        let mut exprs = Vec::new();
        self.expect(TokenKind::LParen)?; // '('
        while let Some(token) = self.tokens.peek() {
            match token.kind {
                TokenKind::RParen => {
                    self.expect(TokenKind::RParen)?; // ')'
                    break;
                }
                TokenKind::Comma => {
                    self.expect(TokenKind::Comma)?; // ','
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

    fn parse_ident(&mut self) -> Result<String, ParseError> {
        if let Some(token) = self.tokens.peek() {
            if let TokenKind::Ident(s) = &token.kind {
                self.tokens.next();
                return Ok(s.clone());
            }
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
        use std::mem::discriminant;
        if let Some(token) = self
            .tokens
            .next_if(|token| discriminant(&token.kind) == discriminant(&kind))
        {
            return Ok(token.clone());
        }
        return Err(ParseError::TODO);
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
