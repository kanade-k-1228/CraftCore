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
use indexmap::IndexMap;

pub type ConstEntry<'a> = (NormType, ConstExpr, Option<usize>, &'a ast::Def);

#[derive(Debug)]
pub struct ConstMap<'a>(pub IndexMap<&'a str, ConstEntry<'a>>);

impl<'a> ConstMap<'a> {
    /// First pass: collect constants that don't depend on types
    pub fn collect(ast: &'a ast::AST) -> Result<Self, CollectError> {
        let mut unresolved: Vec<_> = ast
            .0
            .iter()
            .filter_map(|def| match def {
                ast::Def::Const(name, addr, ty, expr) => Some((name.as_str(), addr, ty, expr, def)),
                _ => None,
            })
            .collect();

        let mut result = ConstMap(IndexMap::new());
        let mut made_progress = true;

        // Process consts in dependency order (but skip those with Sizeof)
        while !unresolved.is_empty() && made_progress {
            made_progress = false;
            unresolved.retain(|(name, addr, ty, expr, def)| {
                // Check for duplicates
                if result.0.contains_key(name) {
                    // Already processed or duplicate - skip
                    return false;
                }

                // Skip if expression contains Sizeof (needs types)
                if contains_sizeof(expr) {
                    return true; // Keep for second pass
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
                            let empty_types = TypeMap(IndexMap::new());
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

        // Don't error on unresolved - they may need types (will be handled in second pass)
        Ok(result)
    }

    /// Second pass: collect remaining constants using type information
    pub fn collect_with_types(
        mut self,
        ast: &'a ast::AST,
        types: &TypeMap,
    ) -> Result<Self, CollectError> {
        let mut unresolved: Vec<_> = ast
            .0
            .iter()
            .filter_map(|def| match def {
                ast::Def::Const(name, addr, ty, expr) => {
                    // Only process if not already collected
                    if self.0.contains_key(name.as_str()) {
                        None
                    } else {
                        Some((name.as_str(), addr, ty, expr, def))
                    }
                }
                _ => None,
            })
            .collect();

        let mut made_progress = true;

        // Process remaining consts with type information
        while !unresolved.is_empty() && made_progress {
            made_progress = false;
            unresolved.retain(|(name, addr, ty, expr, def)| {
                // Try to collect this const with types available
                let env = |lookup_name: &str| -> Option<ConstExpr> {
                    self.0.get(lookup_name).map(|(_, lit, _, _)| lit.clone())
                };

                // Use eval_with_types for proper Sizeof support
                match crate::eval::eval::eval_with_types(&expr, &env, &self, types) {
                    Ok(lit) => {
                        // If type is provided, use it; otherwise infer from literal
                        let ty_result = if let Some(t) = ty {
                            collect_type(&t, &self, types)
                        } else {
                            lit.totype()
                        };

                        if let Ok(resolved_ty) = ty_result {
                            // Store in result with full information
                            let addr_value = addr.as_ref().and_then(|e| match e {
                                ast::Expr::NumberLit(n) => Some(*n),
                                _ => None,
                            });
                            self.0.insert(name, (resolved_ty, lit, addr_value, *def));
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

        // Now we can error on unresolved
        if !unresolved.is_empty() {
            if let Some((_name, _, _, expr, _)) = unresolved.first() {
                return Err(CollectError::UnsupportedConstExpr(format!("{:?}", expr)));
            }
        }

        Ok(self)
    }
}

/// Helper function to check if an expression contains Sizeof
fn contains_sizeof(expr: &ast::Expr) -> bool {
    match expr {
        ast::Expr::Sizeof(_) => true,
        ast::Expr::Unary(_, e) => contains_sizeof(e),
        ast::Expr::Binary(_, lhs, rhs) => contains_sizeof(lhs) || contains_sizeof(rhs),
        ast::Expr::Index(base, index) => contains_sizeof(base) || contains_sizeof(index),
        ast::Expr::Member(base, _) => contains_sizeof(base),
        ast::Expr::Call(func, args) => contains_sizeof(func) || args.iter().any(contains_sizeof),
        ast::Expr::Cond(cond, then_e, else_e) => {
            contains_sizeof(cond) || contains_sizeof(then_e) || contains_sizeof(else_e)
        }
        ast::Expr::Cast(e, _) => contains_sizeof(e),
        ast::Expr::StructLit(fields) => fields.iter().any(|(_, e)| contains_sizeof(e)),
        ast::Expr::ArrayLit(elems) => elems.iter().any(contains_sizeof),
        _ => false,
    }
}
