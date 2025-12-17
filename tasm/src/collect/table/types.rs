use crate::{
    collect::{utils::CollectError, ConstMap},
    eval::normtype::{collect_type, NormType},
    grammer::ast,
};
use std::collections::HashMap;

#[derive(Debug, Clone)]
pub struct TypeMap(pub HashMap<String, (NormType, usize)>);

impl TypeMap {
    pub fn print(&self) {
        println!("\n┌─── Type Definitions ──────────────────────────────────────────────────┐");
        println!(
            "│ {:<20} │ {:<40} │ {:<6} │",
            "Name", "Type Definition", "Size"
        );
        println!("├──────────────────────┼──────────────────────────────────────────┼────────┤");

        let mut sorted_types: Vec<_> = self.0.iter().collect();
        sorted_types.sort_by_key(|(name, _)| name.as_str());

        for (name, (flat_ty, size)) in sorted_types {
            let type_str = flat_ty.format_inline();

            // Truncate long type definitions if needed
            let type_str = if type_str.len() > 40 {
                format!("{}...", &type_str[..37])
            } else {
                type_str
            };

            println!("│ {:<20} │ {:<40} │ {:<6} │", name, type_str, size);
        }
        println!("└──────────────────────┴──────────────────────────────────────────┴────────┘");
    }

    pub fn collect(ast: &ast::AST, consts: &ConstMap) -> Result<Self, CollectError> {
        let ast::AST(defs) = ast;
        let mut result = HashMap::new();

        for def in defs {
            if let ast::Def::Type(name, ty) = def {
                if result.contains_key(name) {
                    return Err(CollectError::Duplicate(name.clone()));
                }
                let resolved_ty = collect_type(&ty, consts, &TypeMap(result.clone()))?;
                let size = resolved_ty.sizeof();
                result.insert(name.clone(), (resolved_ty, size));
            }
        }
        Ok(TypeMap(result))
    }
}
