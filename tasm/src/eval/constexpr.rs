use crate::{error::Error, eval::normtype::NormType};

#[derive(Debug, Clone, PartialEq)]
pub enum ConstExpr {
    Number(usize),                    // 42
    Char(char),                       // 'A'
    String(String),                   // "ABC"
    Array(Vec<ConstExpr>),            // [expr1, expr2, ...]
    Struct(Vec<(String, ConstExpr)>), // {a: expr1, b: expr2}
}

impl ConstExpr {
    pub fn to_usize(&self) -> usize {
        match self {
            ConstExpr::Number(n) => *n,
            ConstExpr::Char(c) => *c as usize,
            ConstExpr::String(_) => 0,
            ConstExpr::Array(_) => 0,
            ConstExpr::Struct(_) => 0,
        }
    }

    pub fn typeinfer(&self) -> Result<NormType, Error> {
        Ok(match self {
            ConstExpr::Number(_) => NormType::Int,
            ConstExpr::Char(_) => NormType::Int,
            ConstExpr::String(s) => NormType::Array(s.len(), Box::new(NormType::Int)),
            ConstExpr::Array(elems) => {
                let base = match elems.get(0) {
                    Some(e) => e.typeinfer()?,
                    None => NormType::Void,
                };
                NormType::Array(elems.len(), Box::new(base))
            }
            ConstExpr::Struct(fields) => {
                let mut list = Vec::new();
                for (name, expr) in fields {
                    list.push((name.clone(), expr.typeinfer()?));
                }
                NormType::Struct(list)
            }
        })
    }

    /// Serialize a ConstExpr to word (u16) array.
    pub fn bin(&self) -> Vec<u16> {
        match self {
            ConstExpr::Number(n) => vec![*n as u16],
            ConstExpr::Char(c) => vec![*c as u16],
            ConstExpr::String(s) => s.chars().map(|c| c as u16).collect(),
            ConstExpr::Array(a) => a.iter().flat_map(|e| e.bin()).collect(),
            ConstExpr::Struct(f) => f.iter().flat_map(|(_, e)| e.bin()).collect(),
        }
    }
}

impl std::fmt::Display for ConstExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ConstExpr::Number(n) => write!(f, "{}", n),
            ConstExpr::Char(c) => write!(f, "'{}'", c),
            ConstExpr::String(s) => write!(f, "\"{}\"", s),
            ConstExpr::Array(a) => {
                let elems: Vec<String> = a.iter().map(|e| e.to_string()).collect();
                write!(f, "[{}]", elems.join(", "))
            }
            ConstExpr::Struct(fields) => {
                let elems: Vec<String> = fields
                    .iter()
                    .map(|(n, v)| format!("{}: {}", n, v))
                    .collect();
                write!(f, "{{{}}}", elems.join(", "))
            }
        }
    }
}
