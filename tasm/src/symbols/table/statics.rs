use crate::{
    error::CollectError,
    eval::{
        constexpr::ConstExpr,
        eval::eval,
        normtype::{collect_type, NormType},
    },
    grammer::ast,
    symbols::table::{consts::ConstMap, types::TypeMap},
};
use std::collections::HashMap;

pub type StaticEntry<'a> = (NormType, Option<usize>, &'a ast::Def);

#[derive(Debug)]
pub struct StaticMap<'a>(pub HashMap<&'a str, StaticEntry<'a>>);

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
                let env = |lookup_name: &str| -> Option<ConstExpr> {
                    consts.0.get(lookup_name).map(|(_, lit, _, _)| lit.clone())
                };
                let addr = addr.as_ref().and_then(|e| match eval(e, &env) {
                    Ok(expr) => match expr {
                        ConstExpr::Number(n) => Some(n),
                        _ => None,
                    },
                    Err(_) => todo!(),
                });
                result.insert(name.as_str(), (resolved_ty, addr, def));
            }
        }
        Ok(StaticMap(result))
    }
}
