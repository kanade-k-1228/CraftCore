use crate::{
    collect::{utils::CollectError, TypeMap},
    eval::{
        constexpr::{eval, ConstExpr},
        normtype::{collect_type, NormType},
    },
    grammer::ast,
};
use std::collections::HashMap;

#[derive(Debug, Clone)]
pub struct ConstMap(pub HashMap<String, (NormType, ConstExpr, Option<usize>)>);

impl ConstMap {
    pub fn print(&self) {
        println!("\n┌─── Constants ──────────────────────────────────────────────────────────────────────────────────┐");
        println!(
            "│ {:<20} │ {:<10} │ {:<30} │ {:<6} │ {:<30} │",
            "Name", "Address", "Type", "Size", "Value"
        );
        println!("├──────────────────────┼────────────┼────────────────────────────────┼────────┼────────────────────────────────┤");

        let mut sorted_consts: Vec<_> = self.0.iter().collect();
        sorted_consts.sort_by_key(|(name, _)| name.as_str());

        for (name, (ty, value, addr)) in sorted_consts {
            let addr_str = addr.map_or("auto".to_string(), |a| format!("0x{:04X}", a));
            let type_str = ty.format_inline();
            let size = ty.sizeof();
            let value_str = value.format_inline();

            // Truncate long strings if needed
            let type_str = if type_str.len() > 30 {
                format!("{}...", &type_str[..27])
            } else {
                type_str
            };

            let value_str = if value_str.len() > 30 {
                format!("{}...", &value_str[..27])
            } else {
                value_str
            };

            println!(
                "│ {:<20} │ {:<10} │ {:<30} │ {:<6} │ {:<30} │",
                name, addr_str, type_str, size, value_str
            );
        }
        println!("└──────────────────────┴────────────┴────────────────────────────────┴────────┴────────────────────────────────┘");
    }

    pub fn collect(ast: &ast::AST) -> Result<Self, CollectError> {
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

        let mut result = ConstMap(HashMap::new());
        let mut made_progress = true;

        // Process consts in dependency order
        while !unresolved.is_empty() && made_progress {
            made_progress = false;
            unresolved.retain(|(name, addr, ty, expr)| {
                // Check for duplicates
                if result.0.contains_key(name) {
                    // Already processed or duplicate - skip
                    return false;
                }

                // Try to collect this const
                match eval(&expr, &result) {
                    Ok(lit) => {
                        // If type is provided, use it; otherwise infer from literal
                        let ty_result = if let Some(t) = ty {
                            // We need empty TypeMap for now since consts shouldn't depend on types
                            let empty_types = TypeMap(HashMap::new());
                            collect_type(&t, &result, &empty_types)
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
                            result
                                .0
                                .insert(name.clone(), (resolved_ty, lit, addr_value));
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
}
