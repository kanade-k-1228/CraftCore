use crate::collect::utils::CollectError;
use crate::collect::ConstMap;
use crate::eval::constexpr::ConstExpr;
use crate::eval::eval::eval;
use crate::grammer::ast;
use std::collections::HashMap;

#[derive(Debug)]
pub struct AsmMap<'a>(pub HashMap<String, (Option<usize>, &'a ast::Def)>);

impl<'a> AsmMap<'a> {
    pub fn collect(ast: &'a ast::AST, consts: &ConstMap) -> Result<Self, CollectError> {
        let ast::AST(defs) = ast;
        let mut result = HashMap::new();

        for def in defs {
            if let ast::Def::Asm(name, maybe_addr, _body) = def {
                if result.contains_key(name) {
                    return Err(CollectError::Duplicate(name.clone()));
                }

                // Evaluate address expression using constants
                let addr = if let Some(expr) = maybe_addr {
                    // Create environment closure that looks up constants
                    let env = |name: &str| -> Option<ConstExpr> {
                        consts
                            .0
                            .get(name)
                            .map(|(_, const_expr, _, _)| const_expr.clone())
                    };

                    // Evaluate the expression
                    match eval(expr, &env) {
                        Ok(ConstExpr::Number(n)) => Some(n),
                        Ok(_) => {
                            return Err(CollectError::UnsupportedConstExpr(format!("{:?}", expr)))
                        }
                        Err(e) => return Err(e),
                    }
                } else {
                    None
                };

                result.insert(name.clone(), (addr, def));
            }
        }
        Ok(AsmMap(result))
    }
}
