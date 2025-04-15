// ast.rs

#[derive(Debug, Clone)]
pub struct Defs(pub Vec<Def>); // Program is collection of definitions

#[derive(Debug, Clone)]
pub enum Def {
    Type(String, Type),                            // name, type
    Const(String, Option<Type>, Expr),             // name, type, value
    Static(String, Option<Expr>, Type),            // name, addr, type
    Asm(String, Option<Expr>, Stmt),               // name, addr, body
    Func(String, Vec<(String, Type)>, Type, Stmt), // name, arg, ret, body
}

#[derive(Debug, Clone)]
pub enum Type {
    Word,                                 // data         | int
    Custom(String),                       // user-defined | Type
    Addr(Box<Type>),                      // address      | *Type
    Array(Expr, Box<Type>),               // array        | Type[10]
    Struct(Vec<(String, Type)>),          // struct       | {a: int, b: Type}
    Func(Vec<(String, Type)>, Box<Type>), // function     | (a: int, b: Type) -> Type
    Error,                                // placeholder for error
}

#[derive(Debug, Clone)]
pub enum Stmt {
    Block(Vec<Stmt>),                         // block       | { ... }
    Expr(Expr),                               // expression  | expr
    Assign(Expr, Expr),                       // assignment  | left-expr = expr ;
    Cond(Expr, Box<Stmt>, Option<Box<Stmt>>), // conditional | 'if' '(' expr ')' stmt ?('else' stmt)
    Loop(Expr, Box<Stmt>),                    // loop        | 'while' '(' expr ')' stmt
    Return(Option<Expr>),                     // return      | 'return' ?( expr ) ';'
    Var(String, Type, Option<Expr>),          // variable    | 'var' name ':' type ?('=' init) ';'
    Error,                                    // placeholder for a statement that failed to parse
}

#[derive(Debug, Clone)]
pub enum Expr {
    IntLit(i64),                           // integer literal | 42
    ArrayLit(Vec<Expr>),                   // array literal   | [expr1, expr2, ...]
    StringLit(String),                     // string literal  | "ABC"
    StructLit(Vec<(String, Expr)>),        // struct literal  | {a: expr1, b: expr2}
    Ident(String),                         // variable        | var name: Type [= init]
    Cond(Box<Expr>, Box<Expr>, Box<Expr>), // conditional     | expr ? expr : expr
    Unary(UnaryOp, Box<Expr>),             // unary op        | -expr, !expr, *expr, &expr
    Binary(Box<Expr>, BinOp, Box<Expr>),   // bin op          | expr1 + expr2
    Call(Box<Expr>, Vec<Expr>),            // function call   | func(expr1, expr2, ...)
    Cast(Box<Expr>, Box<Type>),            // cast            | expr : Type
    ArrayAccess(Box<Expr>, Box<Expr>),     // array access    | expr[expr]
    Member(Box<Expr>, String),             // member access   | expr.field
    Error,                                 // placeholder for an expression that failed to parse
}

#[derive(Debug, Clone)]
pub enum UnaryOp {
    Pos,   // unary plus (+expr)
    Neg,   // unary minus (-expr)
    Not,   // logical not (!expr)
    Deref, // pointer dereference (*expr)
    Ref,   // address-of (&expr)
}

#[derive(Debug, Clone)]
pub enum BinOp {
    Add, // +  arithmetic addition
    Sub, // -  arithmetic subtraction
    Mul, // *  arithmetic multiplication
    Div, // /  arithmetic division
    Mod, // %  arithmetic modulus
    And, // &  bitwise and
    Or,  // |  bitwise or
    Xor, // ^  bitwise xor
    Eq,  // == equal
    Ne,  // != non-equal
    Lt,  // <  less than
    Le,  // <= less than or equal
    Gt,  // >  greater than
    Ge,  // >= greater than or equal
}
