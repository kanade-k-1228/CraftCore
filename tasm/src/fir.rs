// fir.rs

use std::collections::HashMap;

#[derive(Debug, Clone)]
pub struct FIR(pub Vec<Func>);

#[derive(Debug, Clone)]
pub struct Func {
    pub name: String,
    pub nodes: HashMap<String, Node>,
}

#[derive(Debug, Clone)]
pub struct Node {
    pub assigns: Vec<(String, String)>, // (var, expr)
    pub cond: String,                   // conditional expr
    pub next: (String, String),         // True/False branch
}
