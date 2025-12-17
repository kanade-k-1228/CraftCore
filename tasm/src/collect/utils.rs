use crate::grammer::ast;

#[derive(Debug, Clone)]
pub enum CollectError {
    TODO,
    Duplicate(String),
    MissingTypeAnnotation(String),
    NonLiteralArrayLength(Box<ast::Expr>),
    UnsupportedConstExpr(Box<ast::Expr>),
    CannotInferTypeOfEmptyArray,
}
