// parser.rs

use crate::ast::{BinaryOp, Def, Defs, Expr, Stmt, Type, UnaryOp};
use crate::token::{Token, TokenKind::*};
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

macro_rules! optional {
    ($parser:expr, $trigger:pat, $following:expr) => {{
        if check!($parser, $trigger) {
            expect!($parser, $trigger)?;
            Some($following)
        } else {
            None
        }
    }};
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
                KwType => match self.parse_typedef() {
                    Ok((name, typ)) => {
                        program.push(Def::Type(name, typ));
                    }
                    Err(err) => {
                        self.errors.push(err);
                        recover!(self, KwType | KwConst | KwStatic | KwAsm | KwFunc);
                    }
                },

                KwConst => match self.parse_const() {
                    Ok((name, typ, expr)) => {
                        program.push(Def::Const(name, typ, expr));
                    }
                    Err(err) => {
                        self.errors.push(err);
                        recover!(self, KwType | KwConst | KwStatic | KwAsm | KwFunc);
                    }
                },

                KwStatic => match self.parse_static() {
                    Ok((name, addr, typ)) => {
                        program.push(Def::Static(name, addr, typ));
                    }
                    Err(err) => {
                        self.errors.push(err);
                        recover!(self, KwType | KwConst | KwStatic | KwAsm | KwFunc);
                    }
                },

                KwAsm => match self.parse_asm() {
                    Ok((name, addr, body)) => {
                        program.push(Def::Asm(name, addr, body));
                    }
                    Err(err) => {
                        self.errors.push(err);
                        recover!(self, KwType | KwConst | KwStatic | KwAsm | KwFunc);
                    }
                },

                KwFunc => match self.parse_func() {
                    Ok((name, args, ret, body)) => {
                        program.push(Def::Func(name, args, ret, body));
                    }
                    Err(err) => {
                        self.errors.push(err);
                        recover!(self, KwType | KwConst | KwStatic | KwAsm | KwFunc);
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
    /// `type <ident> = <type> ;`
    fn parse_typedef(&mut self) -> Result<(String, Type), ParseError> {
        expect!(self, KwType)?;
        let name = self.parse_ident()?;
        expect!(self, Equal)?;
        let typ = self.parse_type()?;
        expect!(self, Semicolon)?;
        Ok((name, typ))
    }

    /// Const
    /// `const <ident> ?(: <type>) = <expr> ;`
    fn parse_const(&mut self) -> Result<(String, Option<Type>, Expr), ParseError> {
        expect!(self, KwConst)?;
        let name = self.parse_ident()?;
        let typ = optional!(self, Colon, self.parse_type()?);
        expect!(self, Equal)?;
        let expr = self.parse_expr()?;
        expect!(self, Semicolon)?;
        Ok((name, typ, expr))
    }

    /// Type
    /// `int` | `<ident>` | `*<type>` | `<type>[<expr>]` | `{ <ident> : <type> , ... }` | `<args> -> <type>`
    fn parse_type(&mut self) -> Result<Type, ParseError> {
        if let Some(token) = self.tokens.peek() {
            match token.kind {
                // Int
                KwInt => {
                    expect!(self, KwInt)?;
                    Ok(Type::Int)
                }

                // Custom
                Ident(_) => {
                    let name = self.parse_ident()?;
                    Ok(Type::Custom(name))
                }

                // Pointer
                Star => {
                    expect!(self, Star)?;
                    let dest_type = self.parse_type()?;
                    Ok(Type::Addr(Box::new(dest_type)))
                }

                // Array
                LBracket => {
                    expect!(self, LBracket)?;
                    let expr = self.parse_expr()?;
                    expect!(self, RBracket)?;
                    let elem_type = self.parse_type()?;
                    Ok(Type::Array(expr, Box::new(elem_type)))
                }

                // Struct
                LCurly => {
                    let fields = self.parse_struct()?;
                    Ok(Type::Struct(fields))
                }

                // Function
                LParen => {
                    println!("FUNCTION");
                    let args = self.parse_args()?;
                    expect!(self, Arrow)?;
                    let ret = self.parse_type()?;
                    Ok(Type::Func(args, Box::new(ret)))
                }

                _ => Err(ParseError::UnexpectedToken(token.clone())),
            }
        } else {
            Err(ParseError::UnexpectedEOF)
        }
    }

    /// Function call
    /// `( <expr> , <expr> , ... )`
    fn parse_call(&mut self) -> Result<Vec<Expr>, ParseError> {
        let mut exprs = Vec::new();
        expect!(self, LParen)?;
        while let Some(token) = self.tokens.peek() {
            match token.kind {
                RParen => {
                    expect!(self, RParen)?;
                    break;
                }
                Comma => {
                    expect!(self, Comma)?;
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

    /// Assembly
    /// `asm <ident> ?(@ <expr>) { <stmt> ... }`
    fn parse_asm(&mut self) -> Result<(String, Option<Expr>, Stmt), ParseError> {
        expect!(self, KwAsm)?;
        let name = self.parse_ident()?;
        let addr = optional!(self, Atmark, self.parse_expr()?);
        let body = self.parse_block()?;
        Ok((name, addr, body))
    }

    /// Function
    /// `fn <ident> <args> -> <type> <block>`
    fn parse_func(&mut self) -> Result<(String, Vec<(String, Type)>, Type, Stmt), ParseError> {
        expect!(self, KwFunc)?;
        let name = self.parse_ident()?;
        let args = self.parse_args()?;
        expect!(self, Arrow)?;
        let ret = self.parse_type()?;
        let body = self.parse_block()?;
        Ok((name, args, ret, body))
    }

    /// Arguments
    /// `( <ident> : <type> , <ident> : <type> , ... )`
    fn parse_args(&mut self) -> Result<Vec<(String, Type)>, ParseError> {
        let mut args = Vec::new();
        expect!(self, LParen)?;

        while let Some(token) = self.tokens.peek() {
            match token.kind {
                Ident(_) => {
                    let name = self.parse_ident()?;
                    expect!(self, Colon)?;
                    let typ = self.parse_type()?;
                    args.push((name.clone(), typ));
                    continue;
                }
                Comma => {
                    expect!(self, Comma)?;
                    continue;
                }
                RParen => {
                    expect!(self, RParen)?;
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
        expect!(self, LCurly)?;
        while let Some(token) = self.tokens.peek() {
            match token.kind {
                Ident(_) => {
                    let name = self.parse_ident()?;
                    expect!(self, Colon)?;
                    let typ = self.parse_type()?;
                    fields.push((name.clone(), typ));
                    continue;
                }
                Comma => {
                    expect!(self, Comma)?;
                    continue;
                }
                RCurly => {
                    expect!(self, RCurly)?;
                    return Ok(fields);
                }
                _ => {
                    return Err(ParseError::UnexpectedToken(token.clone()))?;
                }
            }
        }
        Err(ParseError::UnexpectedEOF)
    }

    /// Static variable definition
    /// `static ?(@ <expr>) <ident> : <type>;`
    fn parse_static(&mut self) -> Result<(String, Option<Expr>, Type), ParseError> {
        expect!(self, KwStatic)?;
        let addr = optional!(self, Atmark, self.parse_expr()?);
        let name = self.parse_ident()?;
        expect!(self, Colon)?;
        let typ = self.parse_type()?;
        expect!(self, Semicolon)?;
        Ok((name, addr, typ))
    }

    /// Block statement
    /// `{ <stmt> <stmt> ... }`
    fn parse_block(&mut self) -> Result<Stmt, ParseError> {
        expect!(self, LCurly)?;
        let mut stmts = Vec::new();
        while !check!(self, RCurly) {
            let stmt = self.parse_stmt()?;
            stmts.push(stmt);
        }
        expect!(self, RCurly)?;
        Ok(Stmt::Block(stmts))
    }

    /// Statement
    /// `;` | `var <ident> : <type> = <expr> ;` | `if ( <expr> ) <stmt> [ else <stmt> ]` |
    /// `while ( <expr> ) <stmt>` | `return [ <expr> ] ;` | `{ <stmt> ... }` | `<expr> ;`
    fn parse_stmt(&mut self) -> Result<Stmt, ParseError> {
        if let Some(token) = &self.tokens.peek() {
            match &token.kind {
                // Empty statement
                Semicolon => {
                    expect!(self, Semicolon)?;
                    return Ok(Stmt::Expr(Expr::Error));
                }

                // Local variable definition
                KwVar => {
                    let (name, typ, init) = self.parse_var()?;
                    return Ok(Stmt::Var(name, typ, init));
                }

                // If Statement: 'if' '(' expr ')' stmt ?('else' stmt)
                KwIf => {
                    expect!(self, KwIf)?;
                    expect!(self, LParen)?;
                    let cond = self.parse_expr()?;
                    expect!(self, RParen)?;
                    let tstmt = Box::new(self.parse_stmt()?);
                    let fstmt = optional!(self, KwElse, Box::new(self.parse_stmt()?));
                    return Ok(Stmt::Cond(cond, tstmt, fstmt));
                }

                // While Statement: 'while' '(' expr ')' stmt
                KwWhile => {
                    expect!(self, KwWhile)?;
                    expect!(self, LParen)?;
                    let cond = self.parse_expr()?;
                    expect!(self, RParen)?;
                    let body = Box::new(self.parse_stmt()?);
                    return Ok(Stmt::Loop(cond, body));
                }

                KwReturn => {
                    // return 文: 'return' ?( expr ) ';'
                    expect!(self, KwReturn)?;
                    let expr = if !check!(self, Semicolon) {
                        Some(self.parse_expr()?)
                    } else {
                        None
                    };
                    expect!(self, Semicolon)?;
                    return Ok(Stmt::Return(expr));
                }

                // Block Statements: '{' stmt* '}'
                LCurly => {
                    return self.parse_block();
                }

                _ => {
                    let expr = self.parse_expr()?;
                    if check!(self, Equal) {
                        // 代入文: expr '=' expr ';'
                        expect!(self, Equal)?;
                        let rhs = self.parse_expr()?;
                        expect!(self, Semicolon)?;
                        return Ok(Stmt::Assign(expr, rhs));
                    } else {
                        // 式文: expr ';'
                        expect!(self, Semicolon)?;
                        return Ok(Stmt::Expr(expr));
                    }
                }
            }
        }
        return Err(ParseError::TODO);
    }

    /// Variable definition
    /// `var <ident> : <type> ?( = <expr> ) ;`
    fn parse_var(&mut self) -> Result<(String, Type, Option<Expr>), ParseError> {
        expect!(self, KwVar)?;
        let name = self.parse_ident()?;
        expect!(self, Colon)?;
        let typ = self.parse_type()?;
        let init = optional!(self, Equal, self.parse_expr()?);
        expect!(self, Semicolon)?;
        Ok((name, typ, init))
    }

    /// Expression
    fn parse_expr(&mut self) -> Result<Expr, ParseError> {
        self.parse_or()
    }

    /// Logical OR expression
    /// `<expr> | <expr> | ...`
    fn parse_or(&mut self) -> Result<Expr, ParseError> {
        let mut lhs = self.parse_xor()?;
        while check!(self, Pipe) {
            expect!(self, Pipe)?;
            let rhs = self.parse_xor()?;
            lhs = Expr::Binary(BinaryOp::Or, Box::new(lhs), Box::new(rhs))
        }
        Ok(lhs)
    }

    /// Logical XOR expression
    /// `<expr> ^ <expr> ^ ...`
    fn parse_xor(&mut self) -> Result<Expr, ParseError> {
        let mut lhs = self.parse_and()?;
        while check!(self, Caret) {
            expect!(self, Caret)?;
            let rhs = self.parse_and()?;
            lhs = Expr::Binary(BinaryOp::Xor, Box::new(lhs), Box::new(rhs))
        }
        Ok(lhs)
    }

    /// Logical AND expression
    /// `<expr> & <expr> & ...`
    fn parse_and(&mut self) -> Result<Expr, ParseError> {
        let mut lhs = self.parse_eq()?;
        while check!(self, Ampasand) {
            expect!(self, Ampasand)?;
            let rhs = self.parse_eq()?;
            lhs = Expr::Binary(BinaryOp::And, Box::new(lhs), Box::new(rhs))
        }
        Ok(lhs)
    }

    /// Equality expression
    /// `<expr> == <expr>` | `<expr> != <expr>`
    fn parse_eq(&mut self) -> Result<Expr, ParseError> {
        let mut lhs = self.parse_relat()?;
        loop {
            if check!(self, EqualEqual) {
                expect!(self, EqualEqual)?;
                let rhs = self.parse_relat()?;
                lhs = Expr::Binary(BinaryOp::Eq, Box::new(lhs), Box::new(rhs));
            } else if check!(self, ExclEqual) {
                expect!(self, ExclEqual)?;
                let rhs = self.parse_relat()?;
                lhs = Expr::Binary(BinaryOp::Ne, Box::new(lhs), Box::new(rhs));
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
                LAngleEqual => {
                    expect!(self, LAngleEqual)?;
                    let rhs = self.parse_shift()?;
                    Ok(Expr::Binary(BinaryOp::Le, Box::new(lhs), Box::new(rhs)))
                }
                LAngle => {
                    expect!(self, LAngle)?;
                    let rhs = self.parse_shift()?;
                    Ok(Expr::Binary(BinaryOp::Lt, Box::new(lhs), Box::new(rhs)))
                }
                RAngleEqual => {
                    expect!(self, RAngleEqual)?;
                    let rhs = self.parse_shift()?;
                    Ok(Expr::Binary(BinaryOp::Ge, Box::new(lhs), Box::new(rhs)))
                }
                RAngle => {
                    expect!(self, RAngle)?;
                    let rhs = self.parse_shift()?;
                    Ok(Expr::Binary(BinaryOp::Gt, Box::new(lhs), Box::new(rhs)))
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
                LAngleLAngle => {
                    expect!(self, LAngleLAngle)?;
                    let rhs = self.parse_add()?;
                    Ok(Expr::Binary(BinaryOp::Shl, Box::new(lhs), Box::new(rhs)))
                }
                RAngleRAngle => {
                    expect!(self, RAngleRAngle)?;
                    let rhs = self.parse_add()?;
                    Ok(Expr::Binary(BinaryOp::Shr, Box::new(lhs), Box::new(rhs)))
                }
                _ => Ok(lhs),
            }
        } else {
            Err(ParseError::UnexpectedEOF)
        }
    }

    /// Additive expression
    /// `<expr> + <expr> + ...` | `<expr> - <expr> - ...`
    fn parse_add(&mut self) -> Result<Expr, ParseError> {
        let mut lhs = self.parse_mul()?;
        while let Some(token) = self.tokens.peek() {
            match token.kind {
                Plus => {
                    expect!(self, Plus)?;
                    let rhs = self.parse_mul()?;
                    lhs = Expr::Binary(BinaryOp::Add, Box::new(lhs), Box::new(rhs));
                }
                Minus => {
                    expect!(self, Minus)?;
                    let rhs = self.parse_mul()?;
                    lhs = Expr::Binary(BinaryOp::Sub, Box::new(lhs), Box::new(rhs));
                }
                _ => {
                    return Ok(lhs);
                }
            }
        }
        return Err(ParseError::UnexpectedEOF);
    }

    /// Multiplicative expression
    /// `<expr> * <expr>` | `<expr> / <expr>` | `<expr> % <expr>`
    fn parse_mul(&mut self) -> Result<Expr, ParseError> {
        let lhs = self.parse_cast()?;
        if let Some(token) = self.tokens.peek() {
            match token.kind {
                Star => {
                    expect!(self, Star)?;
                    let rhs = self.parse_cast()?;
                    Ok(Expr::Binary(BinaryOp::Mul, Box::new(lhs), Box::new(rhs)))
                }
                Slash => {
                    expect!(self, Slash)?;
                    let rhs = self.parse_cast()?;
                    Ok(Expr::Binary(BinaryOp::Div, Box::new(lhs), Box::new(rhs)))
                }
                Percent => {
                    expect!(self, Percent)?;
                    let rhs = self.parse_cast()?;
                    Ok(Expr::Binary(BinaryOp::Mod, Box::new(lhs), Box::new(rhs)))
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
        if check!(self, Colon) {
            expect!(self, Colon)?;
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
                Plus => {
                    expect!(self, Plus)?;
                    let expr = self.parse_unary()?;
                    return Ok(Expr::Unary(UnaryOp::Pos, Box::new(expr)));
                }
                Minus => {
                    expect!(self, Minus)?;
                    let expr = self.parse_unary()?;
                    return Ok(Expr::Unary(UnaryOp::Neg, Box::new(expr)));
                }
                Star => {
                    expect!(self, Star)?;
                    let expr = self.parse_unary()?;
                    return Ok(Expr::Unary(UnaryOp::Deref, Box::new(expr)));
                }
                Atmark => {
                    expect!(self, Atmark)?;
                    let expr = self.parse_unary()?;
                    return Ok(Expr::Unary(UnaryOp::Ref, Box::new(expr)));
                }
                Excl => {
                    expect!(self, Excl)?;
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
            if check!(self, LParen) {
                let args = self.parse_call()?;
                expr = Expr::Call(Box::new(expr), args);
                continue;
            }

            // Array access
            if check!(self, LBracket) {
                expect!(self, LBracket)?;
                let index = self.parse_expr()?;
                expect!(self, RBracket)?;
                expr = Expr::ArrayAccess(Box::new(expr), Box::new(index));
                continue;
            }

            // Member access
            if check!(self, Period) {
                expect!(self, Period)?;
                let field = self.parse_ident()?;
                expr = Expr::Member(Box::new(expr), field);
                continue;
            }

            break;
        }
        Ok(expr)
    }

    /// Primary expression
    /// `( <expr> )` | `<ident>` | `<number>` | `<string>`
    fn parse_prim(&mut self) -> Result<Expr, ParseError> {
        if let Some(token) = self.tokens.peek() {
            match token.kind {
                // Nested expression:
                LParen => {
                    expect!(self, LParen)?;
                    let inner = self.parse_expr()?;
                    expect!(self, RParen)?;
                    Ok(inner)
                }

                // Identifier
                Ident(_) => {
                    let name = self.parse_ident()?;
                    Ok(Expr::Ident(name.clone()))
                }

                // Number literal
                Number(_, val) => {
                    expect!(self, Number(_, _))?;
                    Ok(Expr::NumberLit(val))
                }

                // Struct literal
                LCurly => {
                    let fields = self.parse_struct_literal()?;
                    return Ok(Expr::StructLit(fields));
                }

                // Array literal
                LBracket => {
                    let arr = self.parse_array_literal()?;
                    return Ok(Expr::ArrayLit(arr));
                }

                // String literal
                Text(_) => {
                    let s = self.parse_string_literal()?;
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
        if let Some(Token { kind: Ident(s), .. }) = &self.tokens.peek().cloned() {
            self.tokens.next();
            return Ok(s.clone());
        }
        return Err(ParseError::TODO);
    }

    /// Struct literal
    /// `{ <ident> = <expr> , ... }`
    fn parse_struct_literal(&mut self) -> Result<Vec<(String, Expr)>, ParseError> {
        expect!(self, LCurly)?;
        let mut fields = Vec::new();
        while !check!(self, RCurly) {
            let name = self.parse_ident()?;
            expect!(self, Equal)?;
            let expr = self.parse_expr()?;
            fields.push((name.clone(), expr));
            if check!(self, Comma) {
                expect!(self, Comma)?;
                continue;
            }
        }
        expect!(self, RCurly)?;
        Ok(fields)
    }

    /// Array literal
    /// `[ <expr> , ... ]`
    fn parse_array_literal(&mut self) -> Result<Vec<Expr>, ParseError> {
        expect!(self, LBracket)?;
        let mut items = Vec::new();
        while !check!(self, RBracket) {
            let expr = self.parse_expr()?;
            items.push(expr);
            if check!(self, Comma) {
                expect!(self, Comma)?;
                continue;
            }
        }
        expect!(self, RBracket)?;
        Ok(items)
    }

    /// String literal
    /// `"abc"`
    fn parse_string_literal(&mut self) -> Result<String, ParseError> {
        if let Some(Token { kind: Text(s), .. }) = &self.tokens.peek().cloned() {
            self.tokens.next();
            return Ok(s.clone());
        }
        return Err(ParseError::TODO);
    }
}
