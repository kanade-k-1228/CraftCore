use crate::{
    collect::{
        flattype::{self, FlatType},
        utils::CollectError,
        ConstMap, StaticMap, TypeMap,
    },
    grammer::ast,
};
use std::collections::HashMap;

// Type/Addr
pub type FuncMap = HashMap<String, (FlatType, Option<usize>)>;

pub fn print(funcs: &FuncMap) {
    println!("=== Functions Table ===");
    println!(
        "{:<20} {:<10} {:<40}",
        "Name", "Address", "Signature"
    );
    println!("{:-<70}", "");

    let mut sorted_funcs: Vec<_> = funcs.iter().collect();
    sorted_funcs.sort_by_key(|(name, _)| name.as_str());

    for (name, (flat_type, addr)) in sorted_funcs {
        let addr_str = addr.map_or("auto".to_string(), |a| format!("0x{:04X}", a));
        let sig_str = flat_type.format_inline();

        println!(
            "{:<20} {:<10} {:<40}",
            name, addr_str, sig_str
        );
    }
    println!();
}


pub fn collect(
    ast: &ast::AST,
    consts: &ConstMap,
    types: &TypeMap,
    _statics: &StaticMap,
) -> Result<FuncMap, CollectError> {
    let ast::AST(defs) = ast;
    let mut result = HashMap::new();

    for def in defs {
        if let ast::Def::Func(name, args, ty, _) = def.clone() {
            if result.contains_key(&name) {
                return Err(CollectError::Duplicate(name.clone()));
            }

            // Resolve argument and return types
            let resolved_args = args
                .iter()
                .map(|(arg_name, arg_ty)| {
                    Ok((
                        arg_name.clone(),
                        flattype::collect_type(&arg_ty, consts, types)?,
                    ))
                })
                .collect::<Result<Vec<_>, CollectError>>()?;
            let resolved_ret = flattype::collect_type(&ty, consts, types)?;

            // Create FlatType for the function
            let func_type = FlatType::Func(resolved_args, Box::new(resolved_ret));

            // For now, addr is None (will be determined in link phase)
            result.insert(name, (func_type, None));
        }
    }
    Ok(result)
}
