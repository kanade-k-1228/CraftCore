use crate::{
    collect::{constexpr::ConstExpr, utils::CollectError, ConstMap, TypeMap},
    grammer::ast,
};

#[derive(Debug, Clone)]
pub enum FlatType {
    Int,                                          // data         | int
    Addr(Box<FlatType>),                          // address      | *Type
    Array(usize, Box<FlatType>),                  // array        | Type[10]
    Struct(Vec<(String, FlatType)>),              // struct       | {a: int, b: Type}
    Func(Vec<(String, FlatType)>, Box<FlatType>), // function     | (a: int, b: Type) -> Type
    Error,                                        // placeholder for error
}

impl FlatType {
    pub fn sizeof(&self) -> usize {
        match self {
            FlatType::Int => 1,     // 1 byte for int in this architecture
            FlatType::Addr(_) => 1, // 1 byte for address
            FlatType::Array(len, elem) => len * elem.sizeof(),
            FlatType::Struct(fields) => fields.iter().map(|(_, elem)| elem.sizeof()).sum(),
            FlatType::Func(_, _) => 0, // Funtion has no space on RAM
            FlatType::Error => 0,
        }
    }
}

impl FlatType {
    pub fn pprint(&self) {
        println!("{}", self.format_pretty(0));
    }

    pub fn format_pretty(&self, indent: usize) -> String {
        let indent_str = "  ".repeat(indent);
        match self {
            FlatType::Int => format!("{}int", indent_str),
            FlatType::Addr(inner) => {
                format!("{}*{}", indent_str, inner.format_inline())
            }
            FlatType::Array(len, elem) => {
                format!("{}[{}]{}", indent_str, len, elem.format_inline())
            }
            FlatType::Struct(fields) => {
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
            FlatType::Func(params, ret) => {
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
            FlatType::Error => format!("{}error", indent_str),
        }
    }

    pub fn format_inline(&self) -> String {
        match self {
            FlatType::Int => "int".to_string(),
            FlatType::Addr(inner) => format!("*{}", inner.format_inline()),
            FlatType::Array(len, elem) => format!("{}[{}]", elem.format_inline(), len),
            FlatType::Struct(fields) => {
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
            FlatType::Func(params, ret) => {
                format!("fn({}) -> {}", params.len(), ret.format_inline())
            }
            FlatType::Error => "error".to_string(),
        }
    }
}

pub fn collect_type(
    ty: &ast::Type,
    consts: &ConstMap,
    types: &TypeMap,
) -> Result<FlatType, CollectError> {
    match ty {
        ast::Type::Int => Ok(FlatType::Int),
        ast::Type::Custom(name) => match types.get(name) {
            Some((flat, _)) => Ok(flat.clone()),
            None => Err(CollectError::TODO),
        },
        ast::Type::Addr(inner) => Ok(FlatType::Addr(Box::new(collect_type(
            inner, consts, types,
        )?))),
        ast::Type::Array(len, ty) => {
            let len = match len {
                ast::Expr::NumberLit(n) => *n,
                ast::Expr::Ident(name) => match consts.get(name) {
                    Some((_, expr, _)) => match expr {
                        ConstExpr::Number(n) => *n,
                        _ => return Err(CollectError::TODO),
                    },
                    None => return Err(CollectError::TODO),
                },
                _ => return Err(CollectError::NonLiteralArrayLength(Box::new(len.clone()))),
            };
            let ty = collect_type(ty, consts, types)?;
            Ok(FlatType::Array(len, Box::new(ty)))
        }
        ast::Type::Struct(fields) => {
            let mut rets = Vec::<(String, FlatType)>::new();
            for (name, ty) in fields {
                let ty = collect_type(&ty, consts, types)?;
                rets.push((name.clone(), ty));
            }
            Ok(FlatType::Struct(rets))
        }
        ast::Type::Func(params, ret) => {
            let ty = collect_type(ret, consts, types)?;
            let mut rets = Vec::<(String, FlatType)>::new();
            for (name, ty) in params {
                let ty = collect_type(&ty, consts, types)?;
                rets.push((name.clone(), ty));
            }
            Ok(FlatType::Func(rets, Box::new(ty)))
        }
        ast::Type::Error => Ok(FlatType::Error),
    }
}
