use crate::{
    collect::{utils::CollectError, ConstMap, StaticMap, TypeMap},
    eval::normtype::{collect_type, NormType},
    grammer::ast,
};
use std::collections::HashMap;

#[derive(Debug, Clone)]
pub struct FuncMap(pub HashMap<String, (NormType, Option<usize>)>);

impl FuncMap {
    pub fn print(&self) {
        println!("\n┌─── Functions ──────────────────────────────────────────────────────────┐");
        println!(
            "│ {:<20} │ {:<10} │ {:<40} │",
            "Name", "Address", "Signature"
        );
        println!(
            "├──────────────────────┼────────────┼──────────────────────────────────────────┤"
        );

        let mut sorted_funcs: Vec<_> = self.0.iter().collect();
        sorted_funcs.sort_by_key(|(name, _)| name.as_str());

        for (name, (flat_type, addr)) in sorted_funcs {
            let addr_str = addr.map_or("auto".to_string(), |a| format!("0x{:04X}", a));
            let sig_str = flat_type.format_inline();

            // Truncate long signatures if needed
            let sig_str = if sig_str.len() > 40 {
                format!("{}...", &sig_str[..37])
            } else {
                sig_str
            };

            println!("│ {:<20} │ {:<10} │ {:<40} │", name, addr_str, sig_str);
        }
        println!(
            "└──────────────────────┴────────────┴──────────────────────────────────────────┘"
        );
    }

    pub fn collect(
        ast: &ast::AST,
        consts: &ConstMap,
        types: &TypeMap,
        _statics: &StaticMap,
    ) -> Result<Self, CollectError> {
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
                        Ok((arg_name.clone(), collect_type(&arg_ty, consts, types)?))
                    })
                    .collect::<Result<Vec<_>, CollectError>>()?;
                let resolved_ret = collect_type(&ty, consts, types)?;

                // Create FlatType for the function
                let func_type = NormType::Func(resolved_args, Box::new(resolved_ret));

                // For now, addr is None (will be determined in link phase)
                result.insert(name, (func_type, None));
            }
        }
        Ok(FuncMap(result))
    }
}
