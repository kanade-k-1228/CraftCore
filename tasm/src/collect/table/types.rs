use crate::{
    collect::{
        flattype::{self, FlatType},
        utils::CollectError,
        ConstMap,
    },
    grammer::ast,
};
use std::collections::HashMap;

// Type/Size
pub type TypeMap = HashMap<String, (FlatType, usize)>;

pub fn print(types: &TypeMap) {
    println!("=== Types Table ===");
    println!("{:<20} {:<40} {:<6}", "Name", "Type Definition", "Size");
    println!("{:-<66}", "");

    let mut sorted_types: Vec<_> = types.iter().collect();
    sorted_types.sort_by_key(|(name, _)| name.as_str());

    for (name, (flat_ty, size)) in sorted_types {
        let type_str = flat_ty.format_inline();
        println!("{:<20} {:<40} {:<6}", name, type_str, size);
    }
    println!();
}

pub fn collect(ast: &ast::AST, consts: &ConstMap) -> Result<TypeMap, CollectError> {
    let ast::AST(defs) = ast;
    let mut result = HashMap::new();

    for def in defs {
        if let ast::Def::Type(name, ty) = def {
            if result.contains_key(name) {
                return Err(CollectError::Duplicate(name.clone()));
            }
            let resolved_ty = flattype::collect_type(&ty, consts, &result)?;
            let size = resolved_ty.sizeof();
            result.insert(name.clone(), (resolved_ty, size));
        }
    }
    Ok(result)
}
