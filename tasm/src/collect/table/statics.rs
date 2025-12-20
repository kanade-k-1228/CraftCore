use crate::{
    collect::{utils::CollectError, ConstMap, TypeMap},
    eval::{
        constexpr::ConstExpr,
        eval::eval,
        normtype::{collect_type, NormType},
    },
    grammer::ast,
};
use std::collections::HashMap;

#[derive(Debug)]
pub struct StaticMap<'a>(pub HashMap<String, (NormType, Option<usize>, &'a ast::Def)>);

impl<'a> StaticMap<'a> {
    pub fn collect(
        ast: &'a ast::AST,
        consts: &ConstMap,
        types: &TypeMap,
    ) -> Result<Self, CollectError> {
        let mut result = HashMap::new();
        for def in &ast.0 {
            if let ast::Def::Static(name, addr, ty) = def {
                let resolved_ty = collect_type(&ty, consts, types)?;
                // Create closure for eval
                let env = |name: &str| -> Option<ConstExpr> {
                    consts.0.get(name).map(|(_, lit, _, _)| lit.clone())
                };
                let addr = addr.as_ref().and_then(|e| match eval(e, &env) {
                    Ok(expr) => match expr {
                        ConstExpr::Number(n) => Some(n),
                        _ => None,
                    },
                    Err(_) => todo!(),
                });
                result.insert(name.clone(), (resolved_ty, addr, def));
            }
        }
        Ok(StaticMap(result))
    }
}
