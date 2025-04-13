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
    Word,                        // 16-bit data    | type sample: int;
    Addr(Box<Type>),             // 16-bit address | type sample: *Type;
    Name(String),                // user-defined   | type sample: Type;
    Array(usize, Box<Type>),     // array          | type sample: Type[10];
    Struct(Vec<(String, Type)>), // struct         | type sample: { field1: Type1, field2: Type2 };
    Error,                       // placeholder for error
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
    IntLit(i64),                               // integer literal
    Var(String),                               // variable (identifier) reference
    UnaryOp(UnaryOpKind, Box<Expr>),           // unary operation
    BinaryOp(Box<Expr>, BinOpKind, Box<Expr>), // binary operations
    Assign(Box<Expr>, Box<Expr>),              // assignment expression (lhs = rhs)
    Call(String, Vec<Expr>),                   // function call: name(expr1, expr2, ...)
    Member(Box<Expr>, String),                 // struct member access: expr.field
    Arrow(Box<Expr>, String),                  // pointer-to-struct member access: expr->field
    Error,                                     // placeholder for an expression that failed to parse
}

#[derive(Debug, Clone)]
enum UnaryOpKind {
    Neg,   // unary minus (-expr)
    Not,   // logical not (!expr)
    Deref, // pointer dereference (*expr)
    Ref,   // address-of (&expr)
}

#[derive(Debug, Clone)]
enum BinOpKind {
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
