use crate::collect::utils::CollectError;
use crate::grammer::ast;
use std::collections::HashMap;

#[derive(Debug, Clone)]
pub struct AsmMap(pub HashMap<String, Option<usize>>);

impl AsmMap {
    pub fn collect(ast: &ast::AST) -> Result<Self, CollectError> {
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
        Ok(AsmMap(result))
    }
}
