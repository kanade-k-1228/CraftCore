use crate::{
    collect::{utils::CollectError, ConstMap, TypeMap},
    eval::{
        constexpr::{eval, ConstExpr},
        normtype::{collect_type, NormType},
    },
    grammer::ast,
};
use std::collections::HashMap;

#[derive(Debug, Clone)]
pub struct StaticMap(pub HashMap<String, (NormType, Option<usize>)>);

impl StaticMap {
    pub fn collect(
        ast: &ast::AST,
        consts: &ConstMap,
        types: &TypeMap,
    ) -> Result<Self, CollectError> {
        let mut result = HashMap::new();
        for def in &ast.0 {
            if let ast::Def::Static(name, addr, ty) = def {
                let resolved_ty = collect_type(&ty, consts, types)?;
                let addr = addr.as_ref().and_then(|e| match eval(e, consts) {
                    Ok(expr) => match expr {
                        ConstExpr::Number(n) => Some(n),
                        _ => None,
                    },
                    Err(_) => todo!(),
                });
                result.insert(name.clone(), (resolved_ty, addr));
            }
        }
        Ok(StaticMap(result))
    }
}
