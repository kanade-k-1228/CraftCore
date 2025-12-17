use crate::{
    collect::{flattype::FlatType, utils::CollectError, ConstMap},
    grammer::ast,
};

#[derive(Debug, Clone)]
pub enum ConstExpr {
    Number(usize),                    // integer literal | 42
    Struct(Vec<(String, ConstExpr)>), // struct literal  | {a: expr1, b: expr2}
    Array(Vec<ConstExpr>),            // array literal   | [expr1, expr2, ...]
    Char(char),                       // char literal    | 'A'
    String(String),                   // string literal  | "ABC"
}

pub fn eval(expr: &ast::Expr, consts: &ConstMap) -> Result<ConstExpr, CollectError> {
    Ok(match expr {
        ast::Expr::NumberLit(n) => ConstExpr::Number(*n),
        ast::Expr::CharLit(c) => ConstExpr::Char(*c),
        ast::Expr::StringLit(s) => ConstExpr::String(s.clone()),
        ast::Expr::Ident(name) => match consts.get(name) {
            Some((_, lit, _)) => lit.clone(),
            None => return Err(CollectError::UnsupportedConstExpr(Box::new(expr.clone()))),
        },
        ast::Expr::StructLit(fields) => ConstExpr::Struct(
            fields
                .iter()
                .map(|(name, expr)| Ok((name.clone(), eval(expr, consts)?)))
                .collect::<Result<_, _>>()?,
        ),
        ast::Expr::ArrayLit(elems) => ConstExpr::Array(
            elems
                .iter()
                .map(|expr| eval(expr, consts))
                .collect::<Result<_, _>>()?,
        ),
        _ => return Err(CollectError::UnsupportedConstExpr(Box::new(expr.clone()))),
    })
}

impl ConstExpr {
    pub fn totype(&self) -> Result<FlatType, CollectError> {
        Ok(match self {
            ConstExpr::Number(_) => FlatType::Int,
            ConstExpr::Char(_) => FlatType::Int, // char is represented as int
            ConstExpr::String(s) => FlatType::Array(s.len(), Box::new(FlatType::Int)),
            ConstExpr::Struct(fields) => {
                let mut list = Vec::new();
                for (name, field_lit) in fields {
                    list.push((name.clone(), field_lit.totype()?));
                }
                FlatType::Struct(list)
            }
            ConstExpr::Array(elems) => {
                if elems.is_empty() {
                    return Err(CollectError::CannotInferTypeOfEmptyArray);
                }
                FlatType::Array(elems.len(), Box::new(elems[0].totype()?))
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
                    let elem_strs: Vec<String> = elems.iter()
                        .map(|e| e.format_inline())
                        .collect();
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
                            result.push_str(&format!("{}  {}: \n{},",
                                indent_str, name, value.format_pretty(indent + 2)));
                        } else {
                            result.push_str(&format!("{}  {}: {},",
                                indent_str, name, value.format_inline()));
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
                    let elem_strs: Vec<String> = elems.iter()
                        .map(|e| e.format_inline())
                        .collect();
                    format!("[{}]", elem_strs.join(", "))
                } else {
                    format!("[...{} items]", elems.len())
                }
            }
            ConstExpr::Struct(fields) => {
                if fields.is_empty() {
                    "{}".to_string()
                } else if fields.len() <= 2 {
                    let field_strs: Vec<String> = fields.iter()
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
}
