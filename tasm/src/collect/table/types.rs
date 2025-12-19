use crate::{
    collect::{utils::CollectError, ConstMap},
    eval::normtype::{collect_type, NormType},
    grammer::ast,
};
use std::collections::HashMap;

#[derive(Debug, Clone)]
pub struct TypeMap(pub HashMap<String, (NormType, usize)>);

impl TypeMap {
    pub fn collect(ast: &ast::AST, consts: &ConstMap) -> Result<Self, CollectError> {
        let ast::AST(defs) = ast;
        let mut result = HashMap::new();

        for def in defs {
            if let ast::Def::Type(name, ty) = def {
                if result.contains_key(name) {
                    return Err(CollectError::Duplicate(name.clone()));
                }
                let resolved_ty = collect_type(&ty, consts, &TypeMap(result.clone()))?;
                let size = resolved_ty.sizeof();
                result.insert(name.clone(), (resolved_ty, size));
            }
        }
        Ok(TypeMap(result))
    }
}
