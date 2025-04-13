// parser.rs

use crate::ast::*;
use crate::lexer::Lexer;
use crate::token::Token;

/// An error that can occur during parsing.
#[derive(Debug)]
pub struct ParseError {
    pub message: String,
    pub line: usize,
    pub col: usize,
}

/// The Parser struct holds the token list and a current index for parsing.
pub struct Parser {
    tokens: Vec<Token>,
    pos: usize,
    // We might keep track of current token's line/col by extending Token enum to carry metadata.
    // For simplicity, we omit position tracking per token here, and use Lexer info only for errors.
}

impl Parser {
    /// Create a new parser from a lexer (lexing the entire input upfront).
    pub fn from_lexer(mut lexer: Lexer) -> Result<Self, ParseError> {
        let mut tokens = Vec::new();
        loop {
            let tok = lexer.next_token();
            if let Token::Illegal(ch) = tok {
                return Err(ParseError {
                    message: format!("Illegal character '{}' in input", ch),
                    line: lexer.line,
                    col: lexer.col,
                });
            }
            tokens.push(tok.clone());
            if tok == Token::EOF {
                break;
            }
        }
        Ok(Parser { tokens, pos: 0 })
    }

    /// Peek the current token without consuming it.
    fn current_token(&self) -> &Token {
        self.tokens.get(self.pos).unwrap_or(&Token::EOF)
    }

    /// Advance to the next token.
    fn advance(&mut self) {
        if self.pos < self.tokens.len() {
            self.pos += 1;
        }
    }

    /// Expect the current token to be of a specific kind, and advance.
    /// If it doesn't match, return a parse error.
    fn expect(&mut self, expected: Token) -> Result<(), ParseError> {
        if std::mem::discriminant(self.current_token()) == std::mem::discriminant(&expected) {
            // (We use discriminant to compare variant type irrespective of associated data)
            self.advance();
            Ok(())
        } else {
            Err(self.error(format!(
                "Expected token {:?}, found {:?}",
                expected,
                self.current_token()
            )))
        }
    }

    /// Create a ParseError at the current position with a message.
    fn error(&self, msg: String) -> ParseError {
        ParseError {
            message: msg,
            line: 0, // line/col tracking could be added if tokens carried positions
            col: 0,
        }
    }

    /// Parse an entire program.
    pub fn parse_program(&mut self) -> Result<Program, ParseError> {
        let mut items = Vec::new();
        while *self.current_token() != Token::EOF {
            // Determine the kind of item by looking at the current token.
            match self.current_token() {
                Token::Func => {
                    let func = self.parse_func_def()?;
                    items.push(Item::Func(func));
                }
                Token::Var => {
                    let var = self.parse_global_var_def()?;
                    items.push(Item::GlobalVar(var));
                }
                Token::Type => {
                    let td = self.parse_type_def()?;
                    items.push(Item::TypeDef(td));
                }
                Token::EOF => break,
                other => {
                    return Err(self.error(format!("Unexpected token {:?} at top-level", other)));
                }
            }
        }
        Ok(Program { items })
    }

    /// Parse a function definition: "func <name>(<params>) : <return_type> { <body> }"
    fn parse_func_def(&mut self) -> Result<FuncDef, ParseError> {
        self.expect(Token::Func)?; // consume "func"
                                   // Current token should be an identifier for the function name
        let name = if let Token::Ident(name) = self.current_token().clone() {
            name
        } else {
            return Err(self.error("Expected function name after 'func'".to_string()));
        };
        self.advance(); // consume function name

        // Parse parameter list
        self.expect(Token::LParen)?; // consume '('
        let mut params = Vec::new();
        if *self.current_token() != Token::RParen {
            // Parse first param (and potentially more)
            loop {
                // Each param: name : type
                let param_name = if let Token::Ident(name) = self.current_token().clone() {
                    name
                } else {
                    return Err(self.error("Expected parameter name".to_string()));
                };
                self.advance();
                self.expect(Token::Colon)?; // consume ':'
                let param_type = self.parse_type_ref()?; // parse a TypeRef
                params.push(Param {
                    name: param_name,
                    ty: param_type,
                });
                if *self.current_token() == Token::Comma {
                    self.advance(); // consume ',' and continue to next param
                    continue;
                } else {
                    break;
                }
            }
        }
        self.expect(Token::RParen)?; // consume ')'

        // Return type (optional): could be " : Type" or omitted (meaning no return type)
        let return_type = if *self.current_token() == Token::Colon {
            self.advance(); // consume ':'
            Some(self.parse_type_ref()?)
        } else {
            None
        };

        // Function body
        self.expect(Token::LBrace)?; // consume '{'
        let body = self.parse_block()?;
        self.expect(Token::RBrace)?; // consume '}'

        Ok(FuncDef {
            name,
            params,
            return_type,
            body,
        })
    }

    /// Parse a global variable definition: "var <name> : <type> = <expr> ;"
    fn parse_global_var_def(&mut self) -> Result<VarDef, ParseError> {
        self.expect(Token::Var)?; // consume "var"
        let var_name = if let Token::Ident(name) = self.current_token().clone() {
            name
        } else {
            return Err(self.error("Expected variable name after 'var'".to_string()));
        };
        self.advance();
        self.expect(Token::Colon)?; // consume ':'
        let var_type = self.parse_type_ref()?;
        let mut init_expr = None;
        if *self.current_token() == Token::Assign {
            self.advance(); // consume '='
            let expr = self.parse_expression()?;
            init_expr = Some(expr);
        }
        self.expect(Token::Semicolon)?; // consume ';'
        Ok(VarDef {
            name: var_name,
            ty: var_type,
            init: init_expr,
            is_global: true,
        })
    }

    /// Parse a type definition: "type <Name> = <TypeRef> ;"
    fn parse_type_def(&mut self) -> Result<TypeDef, ParseError> {
        self.expect(Token::Type)?; // consume "type"
        let type_name = if let Token::Ident(name) = self.current_token().clone() {
            name
        } else {
            return Err(self.error("Expected type name after 'type'".to_string()));
        };
        self.advance();
        self.expect(Token::Assign)?; // consume '=' (assuming type alias uses '=')
        let alias_type = self.parse_type_ref()?;
        self.expect(Token::Semicolon)?; // consume ';'
        Ok(TypeDef {
            name: type_name,
            ty: alias_type,
        })
    }

    /// Parse a block of statements: " { ... } ". (Here we assume '{' is already consumed)
    fn parse_block(&mut self) -> Result<Block, ParseError> {
        let mut stmts = Vec::new();
        while *self.current_token() != Token::RBrace && *self.current_token() != Token::EOF {
            let stmt = self.parse_statement()?;
            stmts.push(stmt);
        }
        Ok(stmts)
    }

    /// Parse a single statement.
    fn parse_statement(&mut self) -> Result<Stmt, ParseError> {
        match self.current_token() {
            Token::Var => {
                // Local variable declaration (very similar to global, but without is_global)
                self.expect(Token::Var)?;
                let name = if let Token::Ident(name) = self.current_token().clone() {
                    name
                } else {
                    return Err(self.error("Expected variable name after 'var'".to_string()));
                };
                self.advance();
                self.expect(Token::Colon)?;
                let ty = self.parse_type_ref()?;
                let mut init_expr = None;
                if *self.current_token() == Token::Assign {
                    self.advance();
                    init_expr = Some(self.parse_expression()?);
                }
                self.expect(Token::Semicolon)?;
                Ok(Stmt::LocalVar(VarDef {
                    name,
                    ty,
                    init: init_expr,
                    is_global: false,
                }))
            }
            Token::Return => {
                self.expect(Token::Return)?;
                let expr = if *self.current_token() != Token::Semicolon {
                    // return expression
                    Some(self.parse_expression()?)
                } else {
                    None
                };
                self.expect(Token::Semicolon)?;
                Ok(Stmt::Return(expr))
            }
            Token::If => {
                self.expect(Token::If)?;
                self.expect(Token::LParen)?;
                let cond_expr = self.parse_expression()?;
                self.expect(Token::RParen)?;
                self.expect(Token::LBrace)?;
                let then_block = self.parse_block()?;
                self.expect(Token::RBrace)?;
                let mut else_block = None;
                if *self.current_token() == Token::Else {
                    self.expect(Token::Else)?;
                    if *self.current_token() == Token::LBrace {
                        // else { ... }
                        self.expect(Token::LBrace)?;
                        else_block = Some(self.parse_block()?);
                        self.expect(Token::RBrace)?;
                    } else if *self.current_token() == Token::If {
                        // else if: handle by recursion for simplicity (parse as if, then wrap into else_block)
                        let nested_if = self.parse_statement()?;
                        // The nested if will produce a Stmt::IfElse; we need to extract its block
                        if let Stmt::IfElse {
                            cond,
                            then_block: then,
                            else_block: els,
                        } = nested_if
                        {
                            // We embed the nested if-else as the else_block of the original if
                            else_block = Some(vec![Stmt::IfElse {
                                cond,
                                then_block: then,
                                else_block: els,
                            }]);
                        } else {
                            return Err(self.error("Error parsing else-if".to_string()));
                        }
                    } else {
                        return Err(self.error("Expected '{' or 'if' after 'else'".to_string()));
                    }
                }
                Ok(Stmt::IfElse {
                    cond: cond_expr,
                    then_block,
                    else_block,
                })
            }
            Token::While => {
                self.expect(Token::While)?;
                self.expect(Token::LParen)?;
                let cond_expr = self.parse_expression()?;
                self.expect(Token::RParen)?;
                self.expect(Token::LBrace)?;
                let body = self.parse_block()?;
                self.expect(Token::RBrace)?;
                Ok(Stmt::While {
                    cond: cond_expr,
                    body,
                })
            }
            Token::LBrace => {
                // A bare block (statement block as a statement)
                self.expect(Token::LBrace)?;
                let block = self.parse_block()?;
                self.expect(Token::RBrace)?;
                // We can wrap it as a single statement (or inline the block if allowed).
                // Here, we'll just return a block as sequence of statements (could be treated as one Stmt variant if needed).
                // For simplicity, let's treat a bare block as just executing its statements sequentially.
                // We return a Stmt::ExprStmt containing a dummy Unit, but better is to incorporate block in AST if needed.
                Ok(Stmt::ExprStmt(Expr::IntConst(0))) // (Placeholder: in a full AST, we'd have a Stmt::Block variant)
            }
            _ => {
                // Any other case should be an expression statement or assignment
                let expr = self.parse_expression()?;
                self.expect(Token::Semicolon)?;
                Ok(Stmt::ExprStmt(expr))
            }
        }
    }

    /// Parse an expression (handles binary operator precedence and unary operators).
    fn parse_expression(&mut self) -> Result<Expr, ParseError> {
        self.parse_assignment()
    }

    /// Parse an assignment expression, which is the lowest-precedence binary operation.
    /// We treat assignment ("=") right-associative (like a = b = c parses as a = (b = c)).
    fn parse_assignment(&mut self) -> Result<Expr, ParseError> {
        // Parse a comparison expression (next level up)
        let mut expr = self.parse_binary_op_level0()?; // we'll define parse_binary_op_level0 for the next level (e.g., logical OR or bitwise OR)
                                                       // In *tasm* spec, '=' is likely not an expression but a statement, but we include it for completeness.
        if *self.current_token() == Token::Assign {
            self.advance();
            let value = self.parse_assignment()?; // allow right-associative chaining
                                                  // `expr` should be something assignable (like VarRef or field or array index).
                                                  // We won't deeply validate here assignability, just build the AST node:
            expr = Expr::BinaryOp {
                op: BinaryOp::Assign,
                left: Box::new(expr),
                right: Box::new(value),
            };
        }
        Ok(expr)
    }

    /// The next functions parse binary operators from lowest to highest precedence.
    /// We can create separate functions for each precedence level or group some.
    /// For brevity, we'll group a bit:
    /// Level 0: bitwise OR (|)
    /// Level 1: bitwise XOR (^)
    /// Level 2: bitwise AND (&)
    /// Level 3: equality (==, !=)
    /// Level 4: comparison (<, <=, >, >=)
    /// Level 5: shifts (<<, >>)
    /// Level 6: addition/subtraction (+, -)
    /// Level 7: multiplication/division/modulo (*, /, %)
    /// Higher precedence (level 8) will be unary operators and primary expressions.

    fn parse_binary_op_level0(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.parse_binary_op_level1()?;
        while *self.current_token() == Token::Pipe {
            self.advance();
            let right = self.parse_binary_op_level1()?;
            expr = Expr::BinaryOp {
                op: BinaryOp::BitOr,
                left: Box::new(expr),
                right: Box::new(right),
            };
        }
        Ok(expr)
    }

    fn parse_binary_op_level1(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.parse_binary_op_level2()?;
        while *self.current_token() == Token::Caret {
            self.advance();
            let right = self.parse_binary_op_level2()?;
            expr = Expr::BinaryOp {
                op: BinaryOp::BitXor,
                left: Box::new(expr),
                right: Box::new(right),
            };
        }
        Ok(expr)
    }

    fn parse_binary_op_level2(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.parse_binary_op_level3()?;
        while *self.current_token() == Token::Ampersand {
            self.advance();
            let right = self.parse_binary_op_level3()?;
            expr = Expr::BinaryOp {
                op: BinaryOp::BitAnd,
                left: Box::new(expr),
                right: Box::new(right),
            };
        }
        Ok(expr)
    }

    fn parse_binary_op_level3(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.parse_binary_op_level4()?;
        while *self.current_token() == Token::Equal || *self.current_token() == Token::NotEqual {
            let op_token = self.current_token().clone();
            self.advance();
            let right = self.parse_binary_op_level4()?;
            let op = if op_token == Token::Equal {
                BinaryOp::Eq
            } else {
                BinaryOp::NotEq
            };
            expr = Expr::BinaryOp {
                op,
                left: Box::new(expr),
                right: Box::new(right),
            };
        }
        Ok(expr)
    }

    fn parse_binary_op_level4(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.parse_binary_op_level5()?;
        while matches!(
            self.current_token(),
            Token::Less | Token::LessEqual | Token::Greater | Token::GreaterEqual
        ) {
            let op_token = self.current_token().clone();
            self.advance();
            let right = self.parse_binary_op_level5()?;
            let op = match op_token {
                Token::Less => BinaryOp::Lt,
                Token::LessEqual => BinaryOp::LtEq,
                Token::Greater => BinaryOp::Gt,
                Token::GreaterEqual => BinaryOp::GtEq,
                _ => unreachable!(),
            };
            expr = Expr::BinaryOp {
                op,
                left: Box::new(expr),
                right: Box::new(right),
            };
        }
        Ok(expr)
    }

    fn parse_binary_op_level5(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.parse_binary_op_level6()?;
        while *self.current_token() == Token::ShiftLeft
            || *self.current_token() == Token::ShiftRight
        {
            let op_token = self.current_token().clone();
            self.advance();
            let right = self.parse_binary_op_level6()?;
            let op = if op_token == Token::ShiftLeft {
                BinaryOp::Shl
            } else {
                BinaryOp::Shr
            };
            expr = Expr::BinaryOp {
                op,
                left: Box::new(expr),
                right: Box::new(right),
            };
        }
        Ok(expr)
    }

    fn parse_binary_op_level6(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.parse_binary_op_level7()?;
        while *self.current_token() == Token::Plus || *self.current_token() == Token::Minus {
            let op_token = self.current_token().clone();
            self.advance();
            let right = self.parse_binary_op_level7()?;
            let op = if op_token == Token::Plus {
                BinaryOp::Add
            } else {
                BinaryOp::Sub
            };
            expr = Expr::BinaryOp {
                op,
                left: Box::new(expr),
                right: Box::new(right),
            };
        }
        Ok(expr)
    }

    fn parse_binary_op_level7(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.parse_unary()?;
        while matches!(
            self.current_token(),
            Token::Asterisk | Token::Slash | Token::Percent
        ) {
            let op_token = self.current_token().clone();
            self.advance();
            let right = self.parse_unary()?;
            let op = match op_token {
                Token::Asterisk => BinaryOp::Mul,
                Token::Slash => BinaryOp::Div,
                Token::Percent => BinaryOp::Mod,
                _ => unreachable!(),
            };
            expr = Expr::BinaryOp {
                op,
                left: Box::new(expr),
                right: Box::new(right),
            };
        }
        Ok(expr)
    }

    /// Parse unary operators and primary expressions.
    fn parse_unary(&mut self) -> Result<Expr, ParseError> {
        match self.current_token() {
            Token::Minus => {
                self.advance();
                // Unary minus (negation)
                let expr = self.parse_unary()?; // parse next factor after '-'
                Ok(Expr::UnaryOp {
                    op: UnaryOp::Neg,
                    expr: Box::new(expr),
                })
            }
            Token::Asterisk => {
                self.advance();
                // Unary dereference
                let expr = self.parse_unary()?;
                Ok(Expr::UnaryOp {
                    op: UnaryOp::Deref,
                    expr: Box::new(expr),
                })
            }
            Token::AtSign => {
                self.advance();
                // Address-of operator
                let expr = self.parse_unary()?;
                Ok(Expr::UnaryOp {
                    op: UnaryOp::AddressOf,
                    expr: Box::new(expr),
                })
            }
            _ => self.parse_primary(),
        }
    }

    /// Parse primary expressions: integer literals, identifiers (variable ref or function call), parenthesized expression, etc.
    fn parse_primary(&mut self) -> Result<Expr, ParseError> {
        match self.current_token().clone() {
            Token::IntLiteral(value) => {
                self.advance();
                Ok(Expr::IntConst(value))
            }
            Token::Ident(name) => {
                self.advance();
                // After an identifier, we need to see if it's a function call or something else.
                if *self.current_token() == Token::LParen {
                    // Function call
                    self.advance(); // consume '('
                    let mut args = Vec::new();
                    if *self.current_token() != Token::RParen {
                        // parse arguments if any
                        loop {
                            let arg = self.parse_expression()?;
                            args.push(arg);
                            if *self.current_token() == Token::Comma {
                                self.advance();
                                continue;
                            } else {
                                break;
                            }
                        }
                    }
                    self.expect(Token::RParen)?;
                    Ok(Expr::FuncCall {
                        func_name: name,
                        args,
                    })
                } else if *self.current_token() == Token::LBracket {
                    // Possibly an array indexing: ident [ expr ]
                    // The identifier is likely an array variable.
                    self.advance(); // consume '['
                    let index_expr = self.parse_expression()?;
                    self.expect(Token::RBracket)?;
                    Ok(Expr::ArrayIndex {
                        array: Box::new(Expr::VarRef(name)),
                        index: Box::new(index_expr),
                    })
                } else {
                    // Plain variable reference
                    Ok(Expr::VarRef(name))
                }
            }
            Token::LParen => {
                // Parenthesized expression
                self.advance();
                let expr = self.parse_expression()?;
                self.expect(Token::RParen)?;
                Ok(expr)
            }
            _ => Err(self.error(format!(
                "Unexpected token in expression: {:?}",
                self.current_token()
            ))),
        }
    }

    /// Parse a type reference (used in variable declarations, function signatures, and type definitions).
    fn parse_type_ref(&mut self) -> Result<TypeRef, ParseError> {
        // We support Named types, pointer types (*), and array types.
        // Pointer type in declaration might be indicated by a prefix '*' or maybe by some keyword.
        // We will allow a leading '*' to indicate pointer.
        if *self.current_token() == Token::Asterisk {
            self.advance();
            // Pointer type: "*" <type>
            let inner = self.parse_type_ref()?;
            return Ok(TypeRef::Pointer(Box::new(inner)));
        }

        // Array type: we expect something like "<Type> [ <size> ]"
        // To differentiate array vs just a named type, we look ahead for LBracket.
        if let Token::Ident(type_name) = self.current_token().clone() {
            // Save name and advance tentatively
            let name = type_name;
            self.advance();
            if *self.current_token() == Token::LBracket {
                // This is an array type of form Name[Size]
                self.advance(); // consume '['
                if let Token::IntLiteral(len) = self.current_token().clone() {
                    self.advance();
                    // expecting closing bracket
                    self.expect(Token::RBracket)?;
                    // The element type could itself be complex (in general, e.g., int[4][5] etc., but let's assume single dimension for simplicity).
                    // We interpret `name` as the base type name.
                    let base_type = TypeRef::Named(name);
                    return Ok(TypeRef::Array(Box::new(base_type), len as usize));
                } else {
                    return Err(self.error("Expected array size integer inside []".to_string()));
                }
            } else {
                // Not an array, just a named type.
                return Ok(TypeRef::Named(name));
            }
        } else {
            return Err(self.error("Expected a type name".to_string()));
        }
    }
}
