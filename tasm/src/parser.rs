// parser.rs

use crate::ast::{BinOpKind, Def, Expr, Program, Stmt, Type};
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

                TokenKind::KwVar => match self.parse_var() {
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
                    self.errors.push(ParseError::UnexpectedToken(token));
                    continue;
                }
            }
        }
        Program(program)
    }

    fn parse_type_def(&mut self) -> Result<Def, ParseError> {
        self.expect(TokenKind::KwType)?; // 'type'
        let name = self.parse_ident()?; // ident
        self.expect(TokenKind::Colon)?; // ':'
        let typ = self.parse_type()?; // type
        self.optional(TokenKind::Semicolon); // ';'
        Ok(Def::Type(name, typ))
    }

    fn parse_function_def(&mut self) -> Result<Def, ParseError> {
        self.expect(TokenKind::KwFunc)?; // 'fn'
        let name = self.parse_ident()?; // name
        self.expect(TokenKind::Colon)?; // '('
        let args = self.parse_arg_def()?; // args
        self.expect(TokenKind::RParen)?; // ')'
        self.expect(TokenKind::Arrow)?; // '->'
        let ret = self.parse_type()?; // type
        let body = self.parse_block()?; // body
        Ok(Def::Func(name, args, ret, body))
    }

    fn parse_var(&mut self) -> Result<Def, ParseError> {
        self.expect(TokenKind::KwVar)?; // 'var'
        let name = self.parse_ident()?; // ident
        self.expect(TokenKind::Colon)?; // ':'
        let typ = self.parse_type()?; // type
        let init = if self.check(TokenKind::Equal) {
            self.expect(TokenKind::Equal)?; // '='
            let expr = self.parse_expr()?; // expr
            Some(expr)
        } else {
            None
        };
        self.expect(TokenKind::Semicolon)?; // ';'
        Ok(Def::Var(name, typ, init))
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
                    return Ok(Stmt::ExprStmt(Expr::Error));
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
                    return Ok(Stmt::VarDecl(name, typ, init));
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
                    return Ok(Stmt::If(cond, then_stmt, else_stmt));
                }
                TokenKind::KwWhile => {
                    // While 文: 'while' '(' expr ')' stmt
                    self.expect(TokenKind::KwWhile)?;
                    self.expect(TokenKind::LParen)?;
                    let cond = self.parse_expr()?;
                    self.expect(TokenKind::RParen)?;
                    let body = Box::new(self.parse_stmt()?);
                    return Ok(Stmt::While(cond, body));
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
                        return Ok(Stmt::ExprStmt(Expr::Assign(Box::new(expr), Box::new(rhs))));
                    } else {
                        // 式文: expr ';'
                        self.expect(TokenKind::Semicolon)?;
                        return Ok(Stmt::ExprStmt(expr));
                    }
                }
            }
        }
        return Err(ParseError::TODO);
    }

    fn parse_expr(&mut self) -> Result<Expr, ParseError> {
        self.parse_precedence(0)
    }

    fn parse_precedence(&mut self, min_prec: u8) -> Result<Expr, ParseError> {
        let mut lhs = self.parse_postfix()?;
        while let Some(token) = self.tokens.peek() {
            if let Some((bin_op, prec, _right_assoc)) = token_to_op(&token.kind) {
                if prec < min_prec {
                    break;
                }
                self.expect(token.kind.clone())?;
                // 左結合とするため右側は (prec + 1) を下限とする
                let rhs = self.parse_precedence(prec + 1)?;
                lhs = Expr::BinaryOp(Box::new(lhs), bin_op, Box::new(rhs));
            } else {
                break;
            }
        }
        Ok(lhs)
    }

    fn parse_postfix(&mut self) -> Result<Expr, ParseError> {
        let expr = self.parse_expr()?;
        while let Some(token) = self.tokens.peek() {
            match token.kind {
                TokenKind::Colon => {
                    // キャスト: expr ":" type
                    self.expect(TokenKind::Colon)?;
                    let typ = self.parse_type()?;
                    return Ok(Expr::UnaryOp(
                        crate::ast::UnaryOpKind::Cast(typ),
                        Box::new(expr),
                    ));
                }
                TokenKind::Star => {
                    // 後置の '*' （デリファレンス）
                    self.expect(TokenKind::Star)?;
                    return Ok(Expr::UnaryOp(
                        crate::ast::UnaryOpKind::Deref,
                        Box::new(expr),
                    ));
                }
                TokenKind::Atmark => {
                    // 後置の '@' （アドレス演算子）
                    self.expect(TokenKind::Atmark)?;
                    return Ok(Expr::UnaryOp(crate::ast::UnaryOpKind::Ref, Box::new(expr)));
                }
                TokenKind::LBracket => {
                    // 配列添字: expr "[" expr "]"
                    self.expect(TokenKind::LBracket)?;
                    let index_expr = self.parse_expr()?;
                    self.expect(TokenKind::RBracket)?;
                    return Ok(Expr::ArrayAccess(Box::new(expr), Box::new(index_expr)));
                }
                TokenKind::Period => {
                    // メンバアクセス: expr "." ident
                    self.expect(TokenKind::Period)?;
                    let member_name = self.parse_ident()?;
                    return Ok(Expr::Member(Box::new(expr), member_name));
                }
                TokenKind::LParen => {
                    // 関数呼び出し: expr "(" expr* ")"
                    self.expect(TokenKind::LParen)?;
                    let args = self.parse_list()?;
                    self.expect(TokenKind::RParen)?;
                    return Ok(Expr::Call(Box::new(expr), args));
                }
                _ => break,
            }
        }
        Ok(expr)
    }

    fn parse_type(&mut self) -> Result<Type, ParseError> {
        if let Some(token) = &self.tokens.peek() {
            match &token.kind {
                // Int
                TokenKind::KwInt => {
                    self.expect(TokenKind::KwInt)?; // 'int'
                    return Ok(Type::Word);
                }

                // Custom
                TokenKind::Ident(_) => {
                    let name = self.parse_ident()?; // ident
                    return Ok(Type::Custom(name));
                }

                // Pointer
                TokenKind::Star => {
                    self.expect(TokenKind::Star)?; // '*'
                    let dest_type = self.parse_type()?;
                    return Ok(Type::Addr(Box::new(dest_type)));
                }

                // Array
                TokenKind::LBracket => {
                    self.expect(TokenKind::LBracket)?; // '['
                    let expr = self.parse_expr()?;
                    self.expect(TokenKind::RBracket)?; // ']'
                    let elem_type = self.parse_type()?;
                    return Ok(Type::Array(expr, Box::new(elem_type)));
                }

                // Struct
                TokenKind::LCurly => {
                    self.expect(TokenKind::LCurly)?; // '{'
                    let fields = self.parse_arg_def()?;
                    self.expect(TokenKind::RCurly)?; // '}'
                    return Ok(Type::Struct(fields));
                }

                // Function
                TokenKind::LParen => {
                    self.expect(TokenKind::LParen)?; // '('
                    let args = self.parse_arg_def()?;
                    self.expect(TokenKind::RParen)?; // ')'
                    self.expect(TokenKind::Arrow)?; // '->'
                    let ret = self.parse_type()?;
                    return Ok(Type::Func(args, Box::new(ret)));
                }

                _ => {
                    return Err(ParseError::TODO);
                }
            }
        } else {
            return Err(ParseError::UnexpectedEOF);
        }
    }

    fn parse_arg_def(&mut self) -> Result<Vec<(String, Type)>, ParseError> {
        let mut fields = Vec::new();
        while let Some(token) = self.tokens.peek() {
            match token.kind {
                TokenKind::Ident(_) => {
                    let name = self.parse_ident()?; // ident
                    self.expect(TokenKind::Colon)?; // ':'
                    let typ = self.parse_type()?; // type
                    fields.push((name.clone(), typ));
                }
                _ => break,
            }
        }
        Ok(fields)
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

    fn optional(&mut self, kind: TokenKind) {
        self.tokens
            .next_if(|token| std::mem::discriminant(&token.kind) == std::mem::discriminant(&kind));
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

fn token_to_op(kind: &TokenKind) -> Option<(BinOpKind, u8, bool)> {
    match kind {
        // 乗除算
        TokenKind::Star => Some((BinOpKind::Mul, 7, false)),
        TokenKind::Slash => Some((BinOpKind::Div, 7, false)),
        TokenKind::Percent => Some((BinOpKind::Mod, 7, false)),
        // 加減算
        TokenKind::Plus => Some((BinOpKind::Add, 6, false)),
        TokenKind::Minus => Some((BinOpKind::Sub, 6, false)),
        // 比較演算
        TokenKind::LAngle => Some((BinOpKind::Lt, 5, false)),
        TokenKind::LAngleEqual => Some((BinOpKind::Le, 5, false)),
        TokenKind::RAngle => Some((BinOpKind::Gt, 5, false)),
        TokenKind::RAngleEqual => Some((BinOpKind::Ge, 5, false)),
        // 等価演算
        TokenKind::EqualEqual => Some((BinOpKind::Eq, 4, false)),
        TokenKind::ExclEqual => Some((BinOpKind::Ne, 4, false)),
        // 論理演算
        TokenKind::Ampasand => Some((BinOpKind::And, 3, false)),
        TokenKind::Caret => Some((BinOpKind::Xor, 2, false)),
        TokenKind::Pipe => Some((BinOpKind::Or, 1, false)),
        _ => None,
    }
}
