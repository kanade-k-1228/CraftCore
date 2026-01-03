use crate::{
    error::CollectError,
    eval::normtype::{collect_type, NormType},
    grammer::ast,
    symbols::table::consts::ConstMap,
};
use indexmap::IndexMap;

pub type TypeEntry<'a> = (NormType, usize, &'a ast::Def);

#[derive(Debug)]
pub struct TypeMap<'a>(pub IndexMap<&'a str, TypeEntry<'a>>);

impl<'a> TypeMap<'a> {
    pub fn collect(ast: &'a ast::AST, consts: &ConstMap) -> Result<Self, CollectError> {
        let ast::AST(defs) = ast;
        let mut result = IndexMap::new();

        for def in defs {
            if let ast::Def::Type(name, ty) = def {
                if result.contains_key(name.as_str()) {
                    return Err(CollectError::Duplicate(name.clone()));
                }
                let resolved_ty = collect_type(&ty, consts, &TypeMap(result.clone()))?;
                let size = resolved_ty.sizeof();
                result.insert(name.as_str(), (resolved_ty, size, def));
            }
        }
        Ok(TypeMap(result))
    }
}
