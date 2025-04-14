// ast.rs

#[derive(Debug, Clone)]
pub struct Program(pub Vec<Def>); // Program is collection of definitions

#[derive(Debug, Clone)]
pub enum Def {
    Type(String, Type),                            // name, type
    Var(String, Type, Option<Expr>),               // name, type, initializer
    Func(String, Vec<(String, Type)>, Type, Stmt), // name, args, return type, body
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
    If(Expr, Box<Stmt>, Option<Box<Stmt>>), // if (condition) then_stmt [else else_stmt]
    While(Expr, Box<Stmt>),                 // while (condition) body_stmt
    Return(Option<Expr>),                   // return [expression] ;
    ExprStmt(Expr),                         // expression statement (expr;)
    VarDecl(String, Type, Option<Expr>),    // local variable declaration: var name: Type [= init] ;
    Block(Vec<Stmt>),                       // block of statements { ... }
    Error,                                  // placeholder for a statement that failed to parse
}

#[derive(Debug, Clone)]
pub enum Expr {
    IntLit(i64),                               // integer literal | 42
    ArrayLit(Vec<Expr>),                       // array literal   | [expr1, expr2, ...]
    Var(String, Box<Type>, Option<Box<Expr>>), // variable        | var name: Type [= init]
    UnaryOp(UnaryOpKind, Box<Expr>),           // unary op        | -expr, !expr, *expr, &expr
    BinaryOp(Box<Expr>, BinOpKind, Box<Expr>), // bin op          | expr1 + expr2, expr1 - expr2, ...
    Assign(Box<Expr>, Box<Expr>),              // assignment      | lhs = rhs
    Call(Box<Expr>, Vec<Expr>),                // function call   | func(expr1, expr2, ...)
    Cast(Box<Expr>, Box<Type>),                // cast            | expr : Type
    ArrayAccess(Box<Expr>, Box<Expr>),         // array access    | expr[expr]
    Member(Box<Expr>, String),                 // member access   | expr.field
    Error,                                     // placeholder for an expression that failed to parse
}

#[derive(Debug, Clone)]
pub enum UnaryOpKind {
    Neg,   // unary minus (-expr)
    Not,   // logical not (!expr)
    Deref, // pointer dereference (*expr)
    Ref,   // address-of (&expr)
}

#[derive(Debug, Clone)]
pub enum BinOpKind {
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
