use crate::{
    error::CollectError,
    eval::normtype::{collect_type, NormType},
    grammer::ast,
    symbols::table::{consts::ConstMap, statics::StaticMap, types::TypeMap},
};
use indexmap::IndexMap;

pub type FuncEntry<'a> = (NormType, Option<usize>, &'a ast::Def);

#[derive(Debug)]
pub struct FuncMap<'a>(pub IndexMap<&'a str, FuncEntry<'a>>);

impl<'a> FuncMap<'a> {
    pub fn collect(
        ast: &'a ast::AST,
        consts: &ConstMap,
        types: &TypeMap,
        _statics: &StaticMap,
    ) -> Result<Self, CollectError> {
        let ast::AST(defs) = ast;
        let mut result = IndexMap::new();

        for def in defs {
            if let ast::Def::Func(name, args, ty, _) = def {
                if result.contains_key(name.as_str()) {
                    return Err(CollectError::Duplicate(name.clone()));
                }

                // Resolve argument and return types
                let resolved_args = args
                    .iter()
                    .map(|(arg_name, arg_ty)| {
                        Ok((arg_name.clone(), collect_type(&arg_ty, consts, types)?))
                    })
                    .collect::<Result<Vec<_>, CollectError>>()?;
                let resolved_ret = collect_type(&ty, consts, types)?;

                // Create FlatType for the function
                let func_type = NormType::Func(resolved_args, Box::new(resolved_ret));

                // For now, addr is None (will be determined in link phase)
                result.insert(name.as_str(), (func_type, None, def));
            }
        }
        Ok(FuncMap(result))
    }
}
