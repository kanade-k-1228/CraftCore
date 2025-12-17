use crate::{
    collect::{utils::CollectError, ConstMap, TypeMap},
    eval::constexpr::ConstExpr,
    grammer::ast,
};

#[derive(Debug, Clone)]
pub enum NormType {
    Int,                                          // data         | int
    Addr(Box<NormType>),                          // address      | *Type
    Array(usize, Box<NormType>),                  // array        | Type[10]
    Struct(Vec<(String, NormType)>),              // struct       | {a: int, b: Type}
    Func(Vec<(String, NormType)>, Box<NormType>), // function     | (a: int, b: Type) -> Type
    Error,                                        // placeholder for error
}

impl NormType {
    pub fn sizeof(&self) -> usize {
        match self {
            NormType::Int => 1,     // 1 byte for int in this architecture
            NormType::Addr(_) => 1, // 1 byte for address
            NormType::Array(len, elem) => len * elem.sizeof(),
            NormType::Struct(fields) => fields.iter().map(|(_, elem)| elem.sizeof()).sum(),
            NormType::Func(_, _) => 0, // Funtion has no space on RAM
            NormType::Error => 0,
        }
    }
}

impl NormType {
    pub fn pprint(&self) {
        println!("{}", self.format_pretty(0));
    }

    pub fn format_pretty(&self, indent: usize) -> String {
        let indent_str = "  ".repeat(indent);
        match self {
            NormType::Int => format!("{}int", indent_str),
            NormType::Addr(inner) => {
                format!("{}*{}", indent_str, inner.format_inline())
            }
            NormType::Array(len, elem) => {
                format!("{}[{}]{}", indent_str, len, elem.format_inline())
            }
            NormType::Struct(fields) => {
                if fields.is_empty() {
                    format!("{}{{}}", indent_str)
                } else {
                    let mut result = format!("{}{{\n", indent_str);
                    for (name, field_type) in fields {
                        result.push_str(&format!(
                            "{}  {}: {},\n",
                            indent_str,
                            name,
                            field_type.format_inline()
                        ));
                    }
                    result.push_str(&format!("{}}}", indent_str));
                    result
                }
            }
            NormType::Func(params, ret) => {
                if params.is_empty() {
                    format!("{}() -> {}", indent_str, ret.format_inline())
                } else {
                    let mut result = format!("{}(\n", indent_str);
                    for (name, param_type) in params {
                        result.push_str(&format!(
                            "{}  {}: {},\n",
                            indent_str,
                            name,
                            param_type.format_inline()
                        ));
                    }
                    result.push_str(&format!("{}) -> {}", indent_str, ret.format_inline()));
                    result
                }
            }
            NormType::Error => format!("{}error", indent_str),
        }
    }

    pub fn format_inline(&self) -> String {
        match self {
            NormType::Int => "int".to_string(),
            NormType::Addr(inner) => format!("*{}", inner.format_inline()),
            NormType::Array(len, elem) => format!("{}[{}]", elem.format_inline(), len),
            NormType::Struct(fields) => {
                if fields.is_empty() {
                    "{}".to_string()
                } else if fields.len() <= 3 {
                    let field_strs: Vec<String> = fields
                        .iter()
                        .map(|(name, ty)| format!("{}: {}", name, ty.format_inline()))
                        .collect();
                    format!("{{{}}}", field_strs.join(", "))
                } else {
                    format!("{{...{} fields}}", fields.len())
                }
            }
            NormType::Func(params, ret) => {
                format!("fn({}) -> {}", params.len(), ret.format_inline())
            }
            NormType::Error => "error".to_string(),
        }
    }
}

pub fn collect_type(
    ty: &ast::Type,
    consts: &ConstMap,
    types: &TypeMap,
) -> Result<NormType, CollectError> {
    match ty {
        ast::Type::Int => Ok(NormType::Int),
        ast::Type::Custom(name) => match types.0.get(name) {
            Some((flat, _)) => Ok(flat.clone()),
            None => Err(CollectError::TODO),
        },
        ast::Type::Addr(inner) => Ok(NormType::Addr(Box::new(collect_type(
            inner, consts, types,
        )?))),
        ast::Type::Array(len, ty) => {
            let len = match len {
                ast::Expr::NumberLit(n) => *n,
                ast::Expr::Ident(name) => match consts.0.get(name) {
                    Some((_, expr, _)) => match expr {
                        ConstExpr::Number(n) => *n,
                        _ => return Err(CollectError::TODO),
                    },
                    None => return Err(CollectError::TODO),
                },
                _ => return Err(CollectError::NonLiteralArrayLength(Box::new(len.clone()))),
            };
            let ty = collect_type(ty, consts, types)?;
            Ok(NormType::Array(len, Box::new(ty)))
        }
        ast::Type::Struct(fields) => {
            let mut rets = Vec::<(String, NormType)>::new();
            for (name, ty) in fields {
                let ty = collect_type(&ty, consts, types)?;
                rets.push((name.clone(), ty));
            }
            Ok(NormType::Struct(rets))
        }
        ast::Type::Func(params, ret) => {
            let ty = collect_type(ret, consts, types)?;
            let mut rets = Vec::<(String, NormType)>::new();
            for (name, ty) in params {
                let ty = collect_type(&ty, consts, types)?;
                rets.push((name.clone(), ty));
            }
            Ok(NormType::Func(rets, Box::new(ty)))
        }
        ast::Type::Error => Ok(NormType::Error),
    }
}
