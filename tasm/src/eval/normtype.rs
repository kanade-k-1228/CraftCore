use crate::{
    error::CollectError,
    eval::constexpr::ConstExpr,
    grammer::ast,
    symbols::{ConstMap, TypeMap},
};

#[derive(Debug, Clone)]
pub enum NormType {
    Int,                                          // data         | int
    Void,                                         // void type    | void
    Addr(Box<NormType>),                          // address      | *Type
    Array(usize, Box<NormType>),                  // array        | Type[10]
    Struct(Vec<(String, NormType)>),              // struct       | {a: int, b: Type}
    Func(Vec<(String, NormType)>, Box<NormType>), // function     | (a: int, b: Type) -> Type
}

impl NormType {
    pub fn sizeof(&self) -> usize {
        match self {
            NormType::Int => 1,     // 1 byte for int in this architecture
            NormType::Void => 0,    // void has no size
            NormType::Addr(_) => 1, // 1 byte for address
            NormType::Array(len, elem) => len * elem.sizeof(),
            NormType::Struct(fields) => fields.iter().map(|(_, elem)| elem.sizeof()).sum(),
            NormType::Func(_, _) => 0, // Funtion has no space on RAM
        }
    }
}

impl NormType {
    pub fn fmt(&self) -> String {
        match self {
            NormType::Int => "int".to_string(),
            NormType::Void => "void".to_string(),
            NormType::Addr(inner) => format!("*{}", inner.fmt()),
            NormType::Array(len, elem) => format!("{}[{}]", elem.fmt(), len),
            NormType::Struct(fields) => {
                let mut strs: Vec<String> = Vec::new();
                for (name, ty) in fields {
                    strs.push(format!("{}: {}", name, ty.fmt()));
                }
                format!("{{{}}}", strs.join(", "))
            }
            NormType::Func(args, ret) => {
                let mut strs: Vec<String> = Vec::new();
                for (name, ty) in args {
                    strs.push(format!("{}: {}", name, ty.fmt()));
                }
                format!("fn({}) -> {}", strs.join(", "), ret.fmt())
            }
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
        ast::Type::Void => Ok(NormType::Void),
        ast::Type::Custom(name) => match types.0.get(name.as_str()) {
            Some((flat, _, _)) => Ok(flat.clone()),
            None => Err(CollectError::TODO),
        },
        ast::Type::Addr(inner) => Ok(NormType::Addr(Box::new(collect_type(
            inner, consts, types,
        )?))),
        ast::Type::Array(len, ty) => {
            let len = match len {
                ast::Expr::NumberLit(n) => *n,
                ast::Expr::Ident(name) => match consts.0.get(name.as_str()) {
                    Some((_, expr, _, _)) => match expr {
                        ConstExpr::Number(n) => *n,
                        _ => return Err(CollectError::TODO),
                    },
                    None => return Err(CollectError::TODO),
                },
                _ => return Err(CollectError::NonLiteralArrayLength(format!("{:?}", len))),
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
    }
}
