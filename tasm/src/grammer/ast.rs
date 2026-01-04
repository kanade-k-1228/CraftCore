#[derive(Debug, Clone)]
pub struct AST(pub Vec<Def>); // defs = { def }

#[derive(Debug, Clone)]
pub enum Type {
    Int,                                  // "int"
    Void,                                 // "void"
    Custom(String),                       // ident
    Addr(Box<Type>),                      // "*" type
    Array(Expr, Box<Type>),               // "[" expr "]" type
    Struct(Vec<(String, Type)>),          // "{" [ ident ":" type { "," ident ":" type } ] "}"
    Func(Vec<(String, Type)>, Box<Type>), // "(" [ ident ":" type { "," ident ":" type } ] ")" "->" type
}

#[derive(Debug, Clone)]
pub enum Def {
    Type(String, Type),                                 // "type" ident "=" type ";"
    Const(String, Option<Expr>, Expr),                  // "const" [ "@" expr ] ident "=" expr ";"
    Static(String, Option<Expr>, Type),                 // "static" [ "@" expr ] ident ":" type ";"
    Asm(String, Option<Expr>, Vec<AsmStmt>), // "asm" [ "@" expr ] ident "{" { asm-stmt } "}"
    Func(String, Vec<(String, Type)>, Type, Vec<Stmt>), // "fn" ident "(" [ ident ":" type { "," ident ":" type } ] ")" [ "->" type ] "{" { stmt } "}"
}

#[derive(Debug, Clone)]
pub enum Stmt {
    Block(Vec<Stmt>),                         // "{" { stmt } "}"
    Expr(Expr),                               // expr ";"
    Assign(Expr, Expr),                       // expr "=" expr ";"
    Cond(Expr, Box<Stmt>, Option<Box<Stmt>>), // "if" "(" expr ")" stmt [ "else" stmt ]
    Loop(Expr, Box<Stmt>),                    // "while" "(" expr ")" stmt
    Return(Option<Expr>),                     // "return" [ expr ] ";"
    Var(String, Type, Option<Expr>),          // "var" ident ":" type [ "=" expr ] ";"
}

#[derive(Debug, Clone)]
pub struct AsmStmt(pub String, pub Vec<Expr>, pub Vec<String>); // { ident ":" } ident "(" [ expr { "," expr } ] ")" ";"

#[derive(Debug, Clone)]
pub enum Expr {
    Cond(Box<Expr>, Box<Expr>, Box<Expr>), // (not in current EBNF - ternary conditional)
    Binary(BinaryOp, Box<Expr>, Box<Expr>), // expr (binop) expr
    Unary(UnaryOp, Box<Expr>),             // ( "+" | "-" | "!" ) expr
    Call(Box<Expr>, Vec<Expr>),            // expr "(" [ expr { "," expr } ] ")"
    Index(Box<Expr>, Box<Expr>),           // expr "[" expr "]"
    Member(Box<Expr>, String),             // expr "." ident
    Addr(Box<Expr>),                       // expr "*"
    Deref(Box<Expr>),                      // expr "@"
    Cast(Box<Expr>, Box<Type>),            // expr "as" type
    Ident(String),                         // ident
    NumberLit(usize),                      // num-lit
    CharLit(char),                         // char-lit
    StringLit(String),                     // string-lit
    ArrayLit(Vec<Expr>),                   // "[" [ expr { "," expr } ] "]"
    StructLit(Vec<(String, Expr)>),        // "{" [ ident ":" expr { "," ident ":" expr } ] "}"
    Sizeof(Box<Type>),                     // "<" type ">"
}

#[derive(Debug, Clone)]
pub enum UnaryOp {
    Pos, // "+"
    Neg, // "-"
    Not, // "!"
}

#[derive(Debug, Clone)]
pub enum BinaryOp {
    Add, // "+"
    Sub, // "-"
    Mul, // "*"
    Div, // "/"
    Mod, // "%"
    And, // "&"
    Or,  // "|"
    Xor, // "^"
    Shl, // "<<"
    Shr, // ">>"
    Eq,  // "=="
    Ne,  // "!="
    Lt,  // "<"
    Le,  // "<="
    Gt,  // ">"
    Ge,  // ">="
}
