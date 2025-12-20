use crate::{
    collect::{utils::CollectError, ConstMap, StaticMap, TypeMap},
    eval::normtype::{collect_type, NormType},
    grammer::ast,
};
use std::collections::HashMap;

#[derive(Debug)]
pub struct FuncMap<'a>(pub HashMap<String, (NormType, Option<usize>, &'a ast::Def)>);

impl<'a> FuncMap<'a> {
    pub fn collect(
        ast: &'a ast::AST,
        consts: &ConstMap,
        types: &TypeMap,
        _statics: &StaticMap,
    ) -> Result<Self, CollectError> {
        let ast::AST(defs) = ast;
        let mut result = HashMap::new();

        for def in defs {
            if let ast::Def::Func(name, args, ty, _) = def {
                if result.contains_key(name) {
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
                result.insert(name.clone(), (func_type, None, def));
            }
        }
        Ok(FuncMap(result))
    }
}
