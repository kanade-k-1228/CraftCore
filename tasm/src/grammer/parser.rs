use super::ast::{AsmStmt, BinaryOp, Def, Expr, Stmt, Type, UnaryOp, AST};
use super::parsercore::Parser;
use super::token::{Token, TokenKind::*};
use crate::error::ParseError;
use crate::{check, expect, optional, recover, repeat};

impl<'a, I: Iterator<Item = Token<'a>>> Parser<'a, I> {
    pub fn parse(mut self) -> (AST, Vec<ParseError>) {
        let program = self.parse_program();
        return (program, self.geterrors());
    }
}

impl<'a, I: Iterator<Item = Token<'a>>> Parser<'a, I> {
    fn parse_program(&mut self) -> AST {
        let mut defs = Vec::new();
        while self.peek().is_some() {
            match self.parse_def() {
                Ok(def) => {
                    defs.push(def);
                }
                Err(err) => {
                    self.error(err);
                    recover!(self, KwType | KwConst | KwStatic | KwAsm | KwFunc);
                }
            }
        }
        AST(defs)
    }

    /// type = "int" | "void" | ident | "*" type | "[" expr "]" type | "{" fields "}" | "(" fields ")" "->" type
    fn parse_type(&mut self) -> Result<Type, ParseError> {
        if let Some(token) = self.peek() {
            match token.kind {
                // Integer type: "int"
                KwInt => {
                    expect!(self, KwInt)?;
                    Ok(Type::Int)
                }

                // Void type: "void"
                KwVoid => {
                    expect!(self, KwVoid)?;
                    Ok(Type::Void)
                }

                // Custom type: ident
                Ident(_) => {
                    let name = self.parse_ident()?;
                    Ok(Type::Custom(name))
                }

                // Pointer type: "*" type
                Star => {
                    expect!(self, Star)?;
                    let ty = self.parse_type()?;
                    Ok(Type::Addr(Box::new(ty)))
                }

                // Array type: "[" expr "]" type
                LBracket => {
                    expect!(self, LBracket)?;
                    let expr = self.parse_expr()?;
                    expect!(self, RBracket)?;
                    let elem_type = self.parse_type()?;
                    Ok(Type::Array(expr, Box::new(elem_type)))
                }

                // Struct type: "{" [ ident ":" type { "," ident ":" type } ] "}"
                LCurly => {
                    expect!(self, LCurly)?;
                    let fields = repeat!(self, self.parse_field_type(), Comma, RCurly);
                    expect!(self, RCurly)?;
                    Ok(Type::Struct(fields))
                }

                // Function type: "(" [ ident ":" type { "," ident ":" type } ] ")" "->" type
                LParen => {
                    expect!(self, LParen)?;
                    let args = repeat!(self, self.parse_field_type(), Comma, RParen);
                    expect!(self, RParen)?;
                    expect!(self, Arrow)?;
                    let ret = self.parse_type()?;
                    Ok(Type::Func(args, Box::new(ret)))
                }

                _ => Err(ParseError::UnexpectedToken(token.into())),
            }
        } else {
            Err(ParseError::UnexpectedEOF)
        }
    }

    /// def = type-def | const-def | static-def | asm-def | func-def
    fn parse_def(&mut self) -> Result<Def, ParseError> {
        if let Some(token) = self.peek() {
            match token.kind {
                // type-def = "type" ident "=" type ";"
                KwType => {
                    expect!(self, KwType)?;
                    let name = self.parse_ident()?;
                    expect!(self, Equal)?;
                    let typ = self.parse_type()?;
                    expect!(self, Semicolon)?;
                    Ok(Def::Type(name, typ))
                }

                // const-def = "const" [ "@" expr ] ident "=" expr ";"
                KwConst => {
                    expect!(self, KwConst)?;
                    let addr = optional!(self, Atmark, self.parse_expr()?);
                    let name = self.parse_ident()?;
                    expect!(self, Equal)?;
                    let expr = self.parse_expr()?;
                    expect!(self, Semicolon)?;
                    Ok(Def::Const(name, addr, expr))
                }

                // static-def = "static" [ "@" expr ] ident ":" type ";"
                KwStatic => {
                    expect!(self, KwStatic)?;
                    let addr = optional!(self, Atmark, self.parse_expr()?);
                    let name = self.parse_ident()?;
                    expect!(self, Colon)?;
                    let ty = self.parse_type()?;
                    expect!(self, Semicolon)?;
                    Ok(Def::Static(name, addr, ty))
                }

                // asm-def = "asm" [ "@" expr ] ident "{" { asm-stmt } "}"
                KwAsm => {
                    expect!(self, KwAsm)?;
                    let addr = optional!(self, Atmark, self.parse_expr()?);
                    let name = self.parse_ident()?;
                    expect!(self, LCurly)?;
                    let body = repeat!(self, self.parse_asm_stmt(), RCurly);
                    expect!(self, RCurly)?;
                    Ok(Def::Asm(name, addr, body))
                }

                // func-def = "fn" ident "(" fields ")" [ "->" type ] block
                KwFunc => {
                    expect!(self, KwFunc)?;
                    let name = self.parse_ident()?;
                    expect!(self, LParen)?;
                    let args = repeat!(self, self.parse_field_type(), Comma, RParen);
                    expect!(self, RParen)?;
                    let ret = optional!(self, Arrow, self.parse_type()?).unwrap_or(Type::Void);
                    expect!(self, LCurly)?;
                    let stmts = repeat!(self, self.parse_stmt(), RCurly);
                    expect!(self, RCurly)?;
                    Ok(Def::Func(name, args, ret, stmts))
                }
                _ => Err(ParseError::UnexpectedToken(token.into())),
            }
        } else {
            Err(ParseError::UnexpectedEOF)
        }
    }

    /// asm-stmt = { ident ":" } ident "(" [ expr { "," expr } ] ")" ";"
    fn parse_asm_stmt(&mut self) -> Result<AsmStmt, ParseError> {
        let mut labels = Vec::new();

        // Parse labels (ident ":")
        while let Some(Token { kind: Ident(s), .. }) = self.peek() {
            let ident = s.clone();
            self.next();

            if check!(self, Colon) {
                expect!(self, Colon)?;
                labels.push(ident);
            } else {
                // This is the instruction name
                expect!(self, LParen)?;
                let args = repeat!(self, self.parse_expr(), Comma, RParen);
                expect!(self, RParen)?;
                expect!(self, Semicolon)?;
                return Ok(AsmStmt(ident, args, labels));
            }
        }

        Err(ParseError::UnexpectedEOF)
    }

    /// stmt = block | var-stmt | if-stmt | while-stmt | return-stmt | assign-stmt | expr-stmt
    fn parse_stmt(&mut self) -> Result<Stmt, ParseError> {
        if let Some(token) = &self.peek() {
            match &token.kind {
                // Block statement: "{" { stmt } "}"
                LCurly => {
                    expect!(self, LCurly)?;
                    let stmts = repeat!(self, self.parse_stmt(), RCurly);
                    expect!(self, RCurly)?;
                    return Ok(Stmt::Block(stmts));
                }

                // Variable statement: "var" ident ":" type [ "=" expr ] ";"
                KwVar => {
                    expect!(self, KwVar)?;
                    let name = self.parse_ident()?;
                    expect!(self, Colon)?;
                    let ty = self.parse_type()?;
                    let init = optional!(self, Equal, self.parse_expr()?);
                    expect!(self, Semicolon)?;
                    return Ok(Stmt::Var(name, ty, init));
                }

                // Conditional statement: "if" "(" expr ")" stmt [ "else" stmt ]
                KwIf => {
                    expect!(self, KwIf)?;
                    expect!(self, LParen)?;
                    let cond = self.parse_expr()?;
                    expect!(self, RParen)?;
                    let tstmt = Box::new(self.parse_stmt()?);
                    let fstmt = optional!(self, KwElse, Box::new(self.parse_stmt()?));
                    return Ok(Stmt::Cond(cond, tstmt, fstmt));
                }

                // Loop statement: "while" "(" expr ")" stmt
                KwWhile => {
                    expect!(self, KwWhile)?;
                    expect!(self, LParen)?;
                    let cond = self.parse_expr()?;
                    expect!(self, RParen)?;
                    let body = Box::new(self.parse_stmt()?);
                    return Ok(Stmt::Loop(cond, body));
                }

                // Return statement: "return" [ expr ] ";"
                KwReturn => {
                    expect!(self, KwReturn)?;
                    let expr = if !check!(self, Semicolon) {
                        Some(self.parse_expr()?)
                    } else {
                        None
                    };
                    expect!(self, Semicolon)?;
                    return Ok(Stmt::Return(expr));
                }

                _ => {
                    // Expression-based statements
                    let expr = self.parse_expr()?;
                    if check!(self, Equal) {
                        // Assignment statement: expr "=" expr ";"
                        expect!(self, Equal)?;
                        let rhs = self.parse_expr()?;
                        expect!(self, Semicolon)?;
                        return Ok(Stmt::Assign(expr, rhs));
                    } else {
                        // Expression statement: expr ";"
                        expect!(self, Semicolon)?;
                        return Ok(Stmt::Expr(expr));
                    }
                }
            }
        }
        return Err(ParseError::TODO);
    }

    /// expr = or-expr
    fn parse_expr(&mut self) -> Result<Expr, ParseError> {
        self.parse_or_expr()
    }

    /// or-expr = xor-expr { "|" xor-expr }
    fn parse_or_expr(&mut self) -> Result<Expr, ParseError> {
        let mut lhs = self.parse_xor_expr()?;
        while check!(self, Pipe) {
            // Bitwise OR: expr "|" xor-expr
            expect!(self, Pipe)?;
            let rhs = self.parse_xor_expr()?;
            lhs = Expr::Binary(BinaryOp::Or, Box::new(lhs), Box::new(rhs))
        }
        Ok(lhs)
    }

    /// xor-expr = and-expr { "^" and-expr }
    fn parse_xor_expr(&mut self) -> Result<Expr, ParseError> {
        let mut lhs = self.parse_and_expr()?;
        while check!(self, Caret) {
            // Bitwise XOR: expr "^" and-expr
            expect!(self, Caret)?;
            let rhs = self.parse_and_expr()?;
            lhs = Expr::Binary(BinaryOp::Xor, Box::new(lhs), Box::new(rhs))
        }
        Ok(lhs)
    }

    /// and-expr = eq-expr { "&" eq-expr }
    fn parse_and_expr(&mut self) -> Result<Expr, ParseError> {
        let mut lhs = self.parse_eq_expr()?;
        while check!(self, Ampasand) {
            // Bitwise AND: expr "&" eq-expr
            expect!(self, Ampasand)?;
            let rhs = self.parse_eq_expr()?;
            lhs = Expr::Binary(BinaryOp::And, Box::new(lhs), Box::new(rhs))
        }
        Ok(lhs)
    }

    /// eq-expr = relat-expr { ( "==" | "!=" ) relat-expr }
    fn parse_eq_expr(&mut self) -> Result<Expr, ParseError> {
        let mut lhs = self.parse_relat_expr()?;
        loop {
            // Equality: expr "==" relat-expr
            if check!(self, EqualEqual) {
                expect!(self, EqualEqual)?;
                let rhs = self.parse_relat_expr()?;
                lhs = Expr::Binary(BinaryOp::Eq, Box::new(lhs), Box::new(rhs));
            }
            // Inequality: expr "!=" relat-expr
            else if check!(self, ExclEqual) {
                expect!(self, ExclEqual)?;
                let rhs = self.parse_relat_expr()?;
                lhs = Expr::Binary(BinaryOp::Ne, Box::new(lhs), Box::new(rhs));
            } else {
                break;
            }
        }
        Ok(lhs)
    }

    /// relat-expr = shift-expr [ ( "<" | "<=" | ">" | ">=" ) shift-expr ]
    fn parse_relat_expr(&mut self) -> Result<Expr, ParseError> {
        let lhs = self.parse_shift_expr()?;
        if let Some(token) = self.peek() {
            match token.kind {
                // Less than or equal: shift-expr "<=" shift-expr
                LAngleEqual => {
                    expect!(self, LAngleEqual)?;
                    let rhs = self.parse_shift_expr()?;
                    Ok(Expr::Binary(BinaryOp::Le, Box::new(lhs), Box::new(rhs)))
                }
                // Less than: shift-expr "<" shift-expr
                LAngle => {
                    expect!(self, LAngle)?;
                    let rhs = self.parse_shift_expr()?;
                    Ok(Expr::Binary(BinaryOp::Lt, Box::new(lhs), Box::new(rhs)))
                }
                // Greater than or equal: shift-expr ">=" shift-expr
                RAngleEqual => {
                    expect!(self, RAngleEqual)?;
                    let rhs = self.parse_shift_expr()?;
                    Ok(Expr::Binary(BinaryOp::Ge, Box::new(lhs), Box::new(rhs)))
                }
                // Greater than: shift-expr ">" shift-expr
                RAngle => {
                    expect!(self, RAngle)?;
                    let rhs = self.parse_shift_expr()?;
                    Ok(Expr::Binary(BinaryOp::Gt, Box::new(lhs), Box::new(rhs)))
                }
                _ => Ok(lhs),
            }
        } else {
            Err(ParseError::UnexpectedEOF)
        }
    }

    /// shift-expr = add-expr [ ( "<<" | ">>" ) add-expr ]
    fn parse_shift_expr(&mut self) -> Result<Expr, ParseError> {
        let lhs = self.parse_add_expr()?;
        if let Some(token) = self.peek() {
            match token.kind {
                // Left shift: add-expr "<<" add-expr
                LAngleLAngle => {
                    expect!(self, LAngleLAngle)?;
                    let rhs = self.parse_add_expr()?;
                    Ok(Expr::Binary(BinaryOp::Shl, Box::new(lhs), Box::new(rhs)))
                }
                // Right shift: add-expr ">>" add-expr
                RAngleRAngle => {
                    expect!(self, RAngleRAngle)?;
                    let rhs = self.parse_add_expr()?;
                    Ok(Expr::Binary(BinaryOp::Shr, Box::new(lhs), Box::new(rhs)))
                }
                _ => Ok(lhs),
            }
        } else {
            Err(ParseError::UnexpectedEOF)
        }
    }

    /// add-expr = mul-expr { ( "+" | "-" ) mul-expr }
    fn parse_add_expr(&mut self) -> Result<Expr, ParseError> {
        let mut lhs = self.parse_mul_expr()?;
        while let Some(token) = self.peek() {
            match token.kind {
                // Addition: expr "+" mul-expr
                Plus => {
                    expect!(self, Plus)?;
                    let rhs = self.parse_mul_expr()?;
                    lhs = Expr::Binary(BinaryOp::Add, Box::new(lhs), Box::new(rhs));
                }
                // Subtraction: expr "-" mul-expr
                Minus => {
                    expect!(self, Minus)?;
                    let rhs = self.parse_mul_expr()?;
                    lhs = Expr::Binary(BinaryOp::Sub, Box::new(lhs), Box::new(rhs));
                }
                _ => {
                    return Ok(lhs);
                }
            }
        }
        return Err(ParseError::UnexpectedEOF);
    }

    /// mul-expr = unary-expr [ ( "*" | "/" | "%" ) unary-expr ]
    fn parse_mul_expr(&mut self) -> Result<Expr, ParseError> {
        let lhs = self.parse_unary_expr()?;
        if let Some(token) = self.peek() {
            match token.kind {
                // Multiplication: unary-expr "*" unary-expr
                Star => {
                    expect!(self, Star)?;
                    let rhs = self.parse_unary_expr()?;
                    Ok(Expr::Binary(BinaryOp::Mul, Box::new(lhs), Box::new(rhs)))
                }
                // Division: unary-expr "/" unary-expr
                Slash => {
                    expect!(self, Slash)?;
                    let rhs = self.parse_unary_expr()?;
                    Ok(Expr::Binary(BinaryOp::Div, Box::new(lhs), Box::new(rhs)))
                }
                // Modulo: unary-expr "%" unary-expr
                Percent => {
                    expect!(self, Percent)?;
                    let rhs = self.parse_unary_expr()?;
                    Ok(Expr::Binary(BinaryOp::Mod, Box::new(lhs), Box::new(rhs)))
                }
                _ => Ok(lhs),
            }
        } else {
            Err(ParseError::UnexpectedEOF)
        }
    }

    /// unary-expr = ( "+" | "-" | "!" ) unary-expr | postfix-expr
    fn parse_unary_expr(&mut self) -> Result<Expr, ParseError> {
        if let Some(token) = self.peek() {
            match token.kind {
                // Unary plus: "+" unary-expr
                Plus => {
                    expect!(self, Plus)?;
                    let expr = self.parse_unary_expr()?;
                    return Ok(Expr::Unary(UnaryOp::Pos, Box::new(expr)));
                }
                // Unary minus: "-" unary-expr
                Minus => {
                    expect!(self, Minus)?;
                    let expr = self.parse_unary_expr()?;
                    return Ok(Expr::Unary(UnaryOp::Neg, Box::new(expr)));
                }
                // Logical not: "!" unary-expr
                Excl => {
                    expect!(self, Excl)?;
                    let expr = self.parse_unary_expr()?;
                    return Ok(Expr::Unary(UnaryOp::Not, Box::new(expr)));
                }
                _ => {}
            }
        }
        // Fall through to postfix-expr
        self.parse_postfix_expr()
    }

    /// postfix-expr = prim-expr { postfix-op }
    /// postfix-op = "(" [ expr { "," expr } ] ")" | "[" expr "]" | "." ident | "*" | "@" | "as" type
    fn parse_postfix_expr(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.parse_prim_expr()?;
        loop {
            // Function call: expr "(" [ expr { "," expr } ] ")"
            if check!(self, LParen) {
                expect!(self, LParen)?;
                let args = repeat!(self, self.parse_expr(), Comma, RParen);
                expect!(self, RParen)?;
                expr = Expr::Call(Box::new(expr), args);
                continue;
            }

            // Array indexing: expr "[" expr "]"
            if check!(self, LBracket) {
                expect!(self, LBracket)?;
                let index = self.parse_expr()?;
                expect!(self, RBracket)?;
                expr = Expr::Index(Box::new(expr), Box::new(index));
                continue;
            }

            // Member access: expr "." ident
            if check!(self, Period) {
                expect!(self, Period)?;
                let field = self.parse_ident()?;
                expr = Expr::Member(Box::new(expr), field);
                continue;
            }

            // Dereference (postfix): expr "*"
            if check!(self, Star) {
                expect!(self, Star)?;
                expr = Expr::Deref(Box::new(expr));
                continue;
            }

            // Address-of (postfix): expr "@"
            if check!(self, Atmark) {
                expect!(self, Atmark)?;
                expr = Expr::Addr(Box::new(expr));
                continue;
            }

            // Type cast: expr "as" type
            if check!(self, KwAs) {
                expect!(self, KwAs)?;
                let target_type = self.parse_type()?;
                expr = Expr::Cast(Box::new(expr), Box::new(target_type));
                continue;
            }

            break;
        }
        Ok(expr)
    }

    /// prim-expr = "(" expr ")" | ident | num-lit | char-lit | string-lit | struct-lit | array-lit | sizeof-expr
    fn parse_prim_expr(&mut self) -> Result<Expr, ParseError> {
        if let Some(token) = self.peek() {
            match &token.kind {
                // Sizeof expression: "<" type ">"
                LAngle => {
                    expect!(self, LAngle)?;
                    let typ = self.parse_type()?;
                    expect!(self, RAngle)?;
                    Ok(Expr::Sizeof(Box::new(typ)))
                }

                // Parenthesized expression: "(" expr ")"
                LParen => {
                    expect!(self, LParen)?;
                    let inner = self.parse_expr()?;
                    expect!(self, RParen)?;
                    Ok(inner)
                }

                // Identifier: ident
                Ident(_) => {
                    let name = self.parse_ident()?;
                    Ok(Expr::Ident(name.clone()))
                }

                // Number literal: num-lit
                Number(_, val) => {
                    let val = *val;
                    expect!(self, Number(_, _))?;
                    Ok(Expr::NumberLit(val))
                }

                // Character literal: char-lit
                Char(ch) => {
                    let ch = *ch;
                    expect!(self, Char(_))?;
                    Ok(Expr::CharLit(ch))
                }

                // Struct literal: "{" [ ident ":" expr { "," ident ":" expr } ] "}"
                LCurly => {
                    expect!(self, LCurly)?;
                    let fields = repeat!(self, self.parse_field_expr(), Comma, RCurly);
                    expect!(self, RCurly)?;
                    return Ok(Expr::StructLit(fields));
                }

                // Array literal: "[" [ expr { "," expr } ] "]"
                LBracket => {
                    expect!(self, LBracket)?;
                    let items = repeat!(self, self.parse_expr(), Comma, RBracket);
                    expect!(self, RBracket)?;
                    return Ok(Expr::ArrayLit(items));
                }

                // String literal: string-lit
                Text(s) => {
                    let s = s.clone();
                    expect!(self, Text(_))?;
                    return Ok(Expr::StringLit(s));
                }

                _ => Err(ParseError::UnexpectedToken(token.into())),
            }
        } else {
            Err(ParseError::UnexpectedEOF)
        }
    }

    /// ident = ( "A".."Z" | "a".."z" | "_" ) { "0".."9" | "A".."Z" | "a".."z" | "_" }
    fn parse_ident(&mut self) -> Result<String, ParseError> {
        if let Some(Token { kind: Ident(s), .. }) = &self.peek().cloned() {
            self.next();
            return Ok(s.clone());
        }
        return Err(ParseError::TODO);
    }

    /// ident ":" type
    fn parse_field_type(&mut self) -> Result<(String, Type), ParseError> {
        let name = self.parse_ident()?;
        expect!(self, Colon)?;
        let typ = self.parse_type()?;
        Ok((name, typ))
    }

    /// ident ":" expr
    fn parse_field_expr(&mut self) -> Result<(String, Expr), ParseError> {
        let name = self.parse_ident()?;
        expect!(self, Colon)?;
        let expr = self.parse_expr()?;
        Ok((name, expr))
    }
}
