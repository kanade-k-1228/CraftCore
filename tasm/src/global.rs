// global.rs

use std::collections::HashMap;

use crate::ast::Stmt;

#[derive(Debug, Clone)]
pub struct Globals(pub HashMap<String, Global>);

#[derive(Debug, Clone)]
pub enum Global {
    Type(Type),
    Const(Literal, Type),
    Static(Type, Option<usize>),
    Asm(Stmt, Option<usize>),
    Func(Vec<(String, Type)>, Type, Stmt),
}

#[derive(Debug, Clone)]
pub enum Type {
    Int,                                  // data         | int
    Custom(String),                       // user-defined | Type
    Addr(Box<Type>),                      // address      | *Type
    Array(usize, Box<Type>),              // array        | Type[10]
    Struct(Vec<(String, Type)>),          // struct       | {a: int, b: Type}
    Func(Vec<(String, Type)>, Box<Type>), // function     | (a: int, b: Type) -> Type
    Error,                                // placeholder for error
}

#[derive(Debug, Clone)]
pub enum Literal {
    Number(usize),                  // integer literal | 42
    Struct(Vec<(String, Literal)>), // struct literal  | {a: expr1, b: expr2}
    Array(Vec<Literal>),            // array literal   | [expr1, expr2, ...]
    Char(char),                     // char literal    | 'A'
    String(String),                 // string literal  | "ABC"
}
