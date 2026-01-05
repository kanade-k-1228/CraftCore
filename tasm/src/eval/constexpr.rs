use crate::{error::CollectError, eval::normtype::NormType};

#[derive(Debug, Clone, PartialEq)]
pub enum ConstExpr {
    Number(usize),                    // integer literal | 42
    Struct(Vec<(String, ConstExpr)>), // struct literal  | {a: expr1, b: expr2}
    Array(Vec<ConstExpr>),            // array literal   | [expr1, expr2, ...]
    Char(char),                       // char literal    | 'A'
    String(String),                   // string literal  | "ABC"
}

impl ConstExpr {
    pub fn typeinfer(&self) -> Result<NormType, CollectError> {
        Ok(match self {
            ConstExpr::Number(_) => NormType::Int,
            ConstExpr::Char(_) => NormType::Int, // char is represented as int
            ConstExpr::String(s) => NormType::Array(s.len(), Box::new(NormType::Int)),
            ConstExpr::Struct(fields) => {
                let mut list = Vec::new();
                for (name, field_lit) in fields {
                    list.push((name.clone(), field_lit.typeinfer()?));
                }
                NormType::Struct(list)
            }
            ConstExpr::Array(elems) => {
                if elems.is_empty() {
                    return Err(CollectError::CannotInferTypeOfEmptyArray);
                }
                NormType::Array(elems.len(), Box::new(elems[0].typeinfer()?))
            }
        })
    }

    pub fn pprint(&self) {
        println!("{}", self.format_pretty(0));
    }

    pub fn format_pretty(&self, indent: usize) -> String {
        let indent_str = "  ".repeat(indent);
        match self {
            ConstExpr::Number(n) => format!("{}{}", indent_str, n),
            ConstExpr::Char(c) => format!("{}'{}'", indent_str, c),
            ConstExpr::String(s) => format!("{}\"{}\"", indent_str, s),
            ConstExpr::Array(elems) => {
                if elems.is_empty() {
                    format!("{}[]", indent_str)
                } else if elems.len() <= 5 {
                    let elem_strs: Vec<String> = elems.iter().map(|e| e.format_inline()).collect();
                    format!("{}[{}]", indent_str, elem_strs.join(", "))
                } else {
                    let mut result = format!("{}[\n", indent_str);
                    for (i, elem) in elems.iter().enumerate() {
                        result.push_str(&format!("{},", elem.format_pretty(indent + 1)));
                        if i < elems.len() - 1 {
                            result.push_str("\n");
                        }
                    }
                    result.push_str(&format!("\n{}]", indent_str));
                    result
                }
            }
            ConstExpr::Struct(fields) => {
                if fields.is_empty() {
                    format!("{}{{}}", indent_str)
                } else {
                    let mut result = format!("{}{{\n", indent_str);
                    for (i, (name, value)) in fields.iter().enumerate() {
                        if value.is_complex() {
                            result.push_str(&format!(
                                "{}  {}: \n{},",
                                indent_str,
                                name,
                                value.format_pretty(indent + 2)
                            ));
                        } else {
                            result.push_str(&format!(
                                "{}  {}: {},",
                                indent_str,
                                name,
                                value.format_inline()
                            ));
                        }
                        if i < fields.len() - 1 {
                            result.push_str("\n");
                        }
                    }
                    result.push_str(&format!("\n{}}}", indent_str));
                    result
                }
            }
        }
    }

    pub fn format_inline(&self) -> String {
        match self {
            ConstExpr::Number(n) => format!("{}", n),
            ConstExpr::Char(c) => format!("'{}'", c),
            ConstExpr::String(s) => {
                if s.len() <= 20 {
                    format!("\"{}\"", s)
                } else {
                    format!("\"{}...\"", &s[..17])
                }
            }
            ConstExpr::Array(elems) => {
                if elems.is_empty() {
                    "[]".to_string()
                } else if elems.len() <= 3 {
                    let elem_strs: Vec<String> = elems.iter().map(|e| e.format_inline()).collect();
                    format!("[{}]", elem_strs.join(", "))
                } else {
                    format!("[...{} items]", elems.len())
                }
            }
            ConstExpr::Struct(fields) => {
                if fields.is_empty() {
                    "{}".to_string()
                } else if fields.len() <= 2 {
                    let field_strs: Vec<String> = fields
                        .iter()
                        .map(|(name, val)| format!("{}: {}", name, val.format_inline()))
                        .collect();
                    format!("{{{}}}", field_strs.join(", "))
                } else {
                    format!("{{...{} fields}}", fields.len())
                }
            }
        }
    }

    fn is_complex(&self) -> bool {
        match self {
            ConstExpr::Number(_) | ConstExpr::Char(_) => false,
            ConstExpr::String(s) => s.len() > 20,
            ConstExpr::Array(elems) => elems.len() > 3,
            ConstExpr::Struct(fields) => fields.len() > 2,
        }
    }

    /// Serialize a ConstExpr to bytes for binary output
    pub fn serialize(&self) -> Vec<u8> {
        match self {
            ConstExpr::Number(n) => (*n as u32).to_le_bytes().to_vec(),
            ConstExpr::Char(c) => {
                vec![*c as u8]
            }
            ConstExpr::String(s) => {
                let mut bytes = s.as_bytes().to_vec();
                // Add null terminator
                bytes.push(0);
                bytes
            }
            ConstExpr::Array(exprs) => {
                let mut result = Vec::new();
                for expr in exprs {
                    result.extend(expr.serialize());
                }
                result
            }
            ConstExpr::Struct(items) => {
                let mut result = Vec::new();
                for (_, expr) in items {
                    result.extend(expr.serialize());
                }
                result
            }
        }
    }
}
