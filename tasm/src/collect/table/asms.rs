use crate::collect::utils::CollectError;
use crate::grammer::ast;
use std::collections::HashMap;

// Addr
pub type AsmMap = HashMap<String, Option<usize>>;

pub fn print(asms: &AsmMap) {
    println!("=== Assembly Blocks Table ===");
    println!(
        "{:<20} {:<10}",
        "Name", "Address"
    );
    println!("{:-<30}", "");

    let mut sorted_asms: Vec<_> = asms.iter().collect();
    sorted_asms.sort_by_key(|(name, _)| name.as_str());

    for (name, addr) in sorted_asms {
        let addr_str = addr.map_or("auto".to_string(), |a| format!("0x{:04X}", a));
        println!(
            "{:<20} {:<10}",
            name, addr_str
        );
    }
    println!();
}

pub fn collect(ast: &ast::AST) -> Result<AsmMap, CollectError> {
    let ast::AST(defs) = ast;
    let mut result = HashMap::new();

    for def in defs {
        if let ast::Def::Asm(name, maybe_addr, _body) = def {
            if result.contains_key(name) {
                return Err(CollectError::Duplicate(name.clone()));
            }
            let addr = maybe_addr.as_ref().and_then(|e| {
                match e {
                    ast::Expr::NumberLit(n) => Some(*n),
                    // For now, we don't resolve const references here
                    // This can be done in the link phase
                    _ => None,
                }
            });
            result.insert(name.clone(), addr);
        }
    }
    Ok(result)
}
