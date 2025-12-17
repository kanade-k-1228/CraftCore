use crate::{
    collect::{
        constexpr::{self, ConstExpr},
        flattype::{self, FlatType},
        utils::CollectError,
    },
    grammer::ast,
};
use std::collections::HashMap;

// Type/Value/Addr
pub type ConstMap = HashMap<String, (FlatType, ConstExpr, Option<usize>)>;

pub fn print(consts: &ConstMap) {
    println!("=== Constants Table ===");
    println!(
        "{:<20} {:<10} {:<30} {:<6} {:<30}",
        "Name", "Address", "Type", "Size", "Value"
    );
    println!("{:-<96}", "");

    let mut sorted_consts: Vec<_> = consts.iter().collect();
    sorted_consts.sort_by_key(|(name, _)| name.as_str());

    for (name, (ty, value, addr)) in sorted_consts {
        let addr_str = addr.map_or("auto".to_string(), |a| format!("0x{:04X}", a));
        let type_str = ty.format_inline();
        let size = ty.sizeof();
        let value_str = value.format_inline();
        println!(
            "{:<20} {:<10} {:<30} {:<6} {:<30}",
            name, addr_str, type_str, size, value_str
        );
    }
    println!();
}

pub fn collect(ast: &ast::AST) -> Result<ConstMap, CollectError> {
    let mut unresolved: Vec<_> = ast
        .0
        .iter()
        .filter_map(|def| match def {
            ast::Def::Const(name, addr, ty, expr) => {
                Some((name.clone(), addr.clone(), ty.clone(), expr.clone()))
            }
            _ => None,
        })
        .collect();

    let mut result = HashMap::new();
    let mut made_progress = true;

    // Process consts in dependency order
    while !unresolved.is_empty() && made_progress {
        made_progress = false;
        unresolved.retain(|(name, addr, ty, expr)| {
            // Check for duplicates
            if result.contains_key(name) {
                // Already processed or duplicate - skip
                return false;
            }

            // Try to collect this const
            match constexpr::eval(&expr, &result) {
                Ok(lit) => {
                    // If type is provided, use it; otherwise infer from literal
                    let ty_result = if let Some(t) = ty {
                        // We need empty TypeMap for now since consts shouldn't depend on types
                        let empty_types = HashMap::new();
                        flattype::collect_type(&t, &result, &empty_types)
                    } else {
                        lit.totype()
                    };

                    if let Ok(resolved_ty) = ty_result {
                        // Store in result with full information
                        // Note: addr is Option<Expr> from AST, we need to evaluate it
                        let addr_value = addr.as_ref().and_then(|e| match e {
                            ast::Expr::NumberLit(n) => Some(*n),
                            _ => None,
                        });
                        result.insert(name.clone(), (resolved_ty, lit, addr_value));
                        made_progress = true;
                        return false; // Remove from pending list
                    }
                }
                Err(_) => {
                    // Can't process yet, might depend on another const
                }
            }
            true // Keep in pending list
        });
    }

    // Check if we have unresolved const dependencies
    if !unresolved.is_empty() {
        // Return error for the first unresolved const
        if let Some((_name, _, _, expr)) = unresolved.first() {
            return Err(CollectError::UnsupportedConstExpr(Box::new(expr.clone())));
        }
    }

    Ok(result)
}
