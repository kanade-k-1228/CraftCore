use crate::{
    error::CollectError,
    eval::{
        constexpr::ConstExpr,
        eval::eval,
        normtype::{collect_type, NormType},
    },
    grammer::ast,
    symbols::table::types::TypeMap,
};
use std::collections::HashMap;

pub type ConstEntry<'a> = (NormType, ConstExpr, Option<usize>, &'a ast::Def);

#[derive(Debug)]
pub struct ConstMap<'a>(pub HashMap<&'a str, ConstEntry<'a>>);

impl<'a> ConstMap<'a> {
    pub fn collect(ast: &'a ast::AST) -> Result<Self, CollectError> {
        let mut unresolved: Vec<_> = ast
            .0
            .iter()
            .filter_map(|def| match def {
                ast::Def::Const(name, addr, ty, expr) => Some((name.as_str(), addr, ty, expr, def)),
                _ => None,
            })
            .collect();

        let mut result = ConstMap(HashMap::new());
        let mut made_progress = true;

        // Process consts in dependency order
        while !unresolved.is_empty() && made_progress {
            made_progress = false;
            unresolved.retain(|(name, addr, ty, expr, def)| {
                // Check for duplicates
                if result.0.contains_key(name) {
                    // Already processed or duplicate - skip
                    return false;
                }

                // Try to collect this const
                // Create closure for eval
                let env = |lookup_name: &str| -> Option<ConstExpr> {
                    result.0.get(lookup_name).map(|(_, lit, _, _)| lit.clone())
                };
                match eval(&expr, &env) {
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
                            result.0.insert(name, (resolved_ty, lit, addr_value, *def));
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
            if let Some((_name, _, _, expr, _)) = unresolved.first() {
                return Err(CollectError::UnsupportedConstExpr(format!("{:?}", expr)));
            }
        }

        Ok(result)
    }
}
