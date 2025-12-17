use crate::{
    collect::{
        constexpr::{eval, ConstExpr},
        flattype::{collect_type, FlatType},
        utils::CollectError,
        ConstMap, TypeMap,
    },
    grammer::ast,
};
use std::collections::HashMap;

// Type/Addr
pub type StaticMap = HashMap<String, (FlatType, Option<usize>)>;

pub fn print(statics: &StaticMap) {
    println!("=== Statics Table ===");
    println!(
        "{:<20} {:<10} {:<35}",
        "Name", "Address", "Type"
    );
    println!("{:-<65}", "");

    let mut sorted_statics: Vec<_> = statics.iter().collect();
    sorted_statics.sort_by_key(|(name, _)| name.as_str());

    for (name, (flat_ty, addr)) in sorted_statics {
        let addr_str = addr.map_or("auto".to_string(), |a| format!("0x{:04X}", a));
        let type_str = flat_ty.format_inline();
        println!("{:<20} {:<10} {:<35}", name, addr_str, type_str);
    }
    println!();
}

pub fn collect(
    ast: &ast::AST,
    consts: &ConstMap,
    types: &TypeMap,
) -> Result<StaticMap, CollectError> {
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
    Ok(result)
}
