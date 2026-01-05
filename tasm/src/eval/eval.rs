use indexmap::IndexMap;

use crate::{
    error::CollectError,
    eval::{constexpr::ConstExpr, normtype::NormType},
    grammer::ast::{self, BinaryOp, UnaryOp},
};

/// Entry types for each symbol category
#[derive(Debug, Clone)]
pub struct TypeEntry<'a> {
    pub norm_type: NormType,
    pub size: usize,
    pub def: &'a ast::Def,
}

#[derive(Debug, Clone)]
pub struct ConstEntry<'a> {
    pub norm_type: NormType,
    pub value: ConstExpr,
    pub address: Option<usize>,
    pub def: &'a ast::Def,
}

#[derive(Debug, Clone)]
pub struct StaticEntry<'a> {
    pub norm_type: NormType,
    pub address: Option<usize>,
    pub def: &'a ast::Def,
}

#[derive(Debug, Clone)]
pub struct AsmEntry<'a> {
    pub address: Option<usize>,
    pub def: &'a ast::Def,
}

#[derive(Debug, Clone)]
pub struct FuncEntry<'a> {
    pub norm_type: NormType,
    pub address: Option<usize>,
    pub def: &'a ast::Def,
}

/// Unified symbol type
#[derive(Debug, Clone)]
pub enum Symbol<'a> {
    Type(TypeEntry<'a>),
    Const(ConstEntry<'a>),
    Static(StaticEntry<'a>),
    Asm(AsmEntry<'a>),
    Func(FuncEntry<'a>),
}

/// The Evaluator struct that replaces the old Symbols struct
pub struct Evaluator<'a> {
    types: IndexMap<&'a str, TypeEntry<'a>>,
    consts: IndexMap<&'a str, ConstEntry<'a>>,
    statics: IndexMap<&'a str, StaticEntry<'a>>,
    asms: IndexMap<&'a str, AsmEntry<'a>>,
    funcs: IndexMap<&'a str, FuncEntry<'a>>,
}

impl<'a> Evaluator<'a> {
    /// Collect all symbols from the AST with proper dependency ordering
    pub fn collect(ast: &'a ast::AST) -> Result<Self, CollectError> {
        let mut evaluator = Evaluator {
            types: IndexMap::new(),
            consts: IndexMap::new(),
            statics: IndexMap::new(),
            asms: IndexMap::new(),
            funcs: IndexMap::new(),
        };

        // First pass: collect constants that don't depend on types
        evaluator.collect_consts_first_pass(ast)?;

        // Collect types using the first-pass constants
        evaluator.collect_types(ast)?;

        // Second pass: collect remaining constants that use sizeof
        evaluator.collect_consts_second_pass(ast)?;

        // Now collect the rest with complete const and type information
        evaluator.collect_statics(ast)?;
        evaluator.collect_asms(ast)?;
        evaluator.collect_funcs(ast)?;

        // Check for duplicate names across all symbol tables
        evaluator.check_duplicates()?;

        Ok(evaluator)
    }

    /// First pass: collect constants that don't depend on types
    fn collect_consts_first_pass(&mut self, ast: &'a ast::AST) -> Result<(), CollectError> {
        let mut unresolved: Vec<_> = ast
            .0
            .iter()
            .filter_map(|def| match def {
                ast::Def::Const(name, addr, expr) => Some((name.as_str(), addr, expr, def)),
                _ => None,
            })
            .collect();

        let mut made_progress = true;

        // Process consts in dependency order (but skip those with Sizeof)
        while !unresolved.is_empty() && made_progress {
            made_progress = false;
            unresolved.retain(|(name, addr, expr, def)| {
                // Check for duplicates
                if self.consts.contains_key(name) {
                    return false;
                }

                // Skip if expression contains Sizeof (needs types)
                if contains_sizeof(expr) {
                    return true; // Keep for second pass
                }

                // Try to evaluate this const
                match self.constexpr(expr) {
                    Ok(value) => {
                        // Infer type from literal
                        if let Ok(norm_type) = value.typeinfer() {
                            // Evaluate address if present
                            let address = addr.as_ref().and_then(|e| match self.constexpr(e) {
                                Ok(ConstExpr::Number(n)) => Some(n),
                                _ => None,
                            });

                            self.consts.insert(
                                name,
                                ConstEntry {
                                    norm_type,
                                    value,
                                    address,
                                    def: *def,
                                },
                            );
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

        Ok(())
    }

    /// Collect types using the first-pass constants
    fn collect_types(&mut self, ast: &'a ast::AST) -> Result<(), CollectError> {
        for def in &ast.0 {
            if let ast::Def::Type(name, ty) = def {
                if self.types.contains_key(name.as_str()) {
                    return Err(CollectError::Duplicate(name.clone()));
                }

                let norm_type = self
                    .normtype(ty)
                    .map_err(|e| CollectError::UnsupportedConstExpr(e))?;
                let size = norm_type.sizeof();

                self.types.insert(
                    name.as_str(),
                    TypeEntry {
                        norm_type,
                        size,
                        def,
                    },
                );
            }
        }
        Ok(())
    }

    /// Second pass: collect remaining constants that use sizeof
    fn collect_consts_second_pass(&mut self, ast: &'a ast::AST) -> Result<(), CollectError> {
        let mut unresolved: Vec<_> = ast
            .0
            .iter()
            .filter_map(|def| match def {
                ast::Def::Const(name, addr, expr) => {
                    if self.consts.contains_key(name.as_str()) {
                        None
                    } else {
                        Some((name.as_str(), addr, expr, def))
                    }
                }
                _ => None,
            })
            .collect();

        let mut made_progress = true;

        while !unresolved.is_empty() && made_progress {
            made_progress = false;
            unresolved.retain(|(name, addr, expr, def)| {
                // Try to evaluate this const with types available
                match self.constexpr(expr) {
                    Ok(value) => {
                        if let Ok(norm_type) = value.typeinfer() {
                            // Evaluate address if present
                            let address = addr.as_ref().and_then(|e| match self.constexpr(e) {
                                Ok(ConstExpr::Number(n)) => Some(n),
                                _ => None,
                            });

                            self.consts.insert(
                                name,
                                ConstEntry {
                                    norm_type,
                                    value,
                                    address,
                                    def: *def,
                                },
                            );
                            made_progress = true;
                            return false; // Remove from pending list
                        }
                    }
                    Err(_) => {
                        // Can't process yet
                    }
                }
                true // Keep in pending list
            });
        }

        // Error on unresolved
        if !unresolved.is_empty() {
            if let Some((_, _, expr, _)) = unresolved.first() {
                return Err(CollectError::UnsupportedConstExpr(format!("{:?}", expr)));
            }
        }

        Ok(())
    }

    /// Collect static variables
    fn collect_statics(&mut self, ast: &'a ast::AST) -> Result<(), CollectError> {
        for def in &ast.0 {
            if let ast::Def::Static(name, addr, ty) = def {
                if self.statics.contains_key(name.as_str()) {
                    return Err(CollectError::Duplicate(name.clone()));
                }

                let norm_type = self
                    .normtype(ty)
                    .map_err(|e| CollectError::UnsupportedConstExpr(e))?;

                // Evaluate address if present
                let address = addr.as_ref().and_then(|e| match self.constexpr(e) {
                    Ok(ConstExpr::Number(n)) => Some(n),
                    _ => None,
                });

                self.statics.insert(
                    name.as_str(),
                    StaticEntry {
                        norm_type,
                        address,
                        def,
                    },
                );
            }
        }
        Ok(())
    }

    /// Collect assembly blocks
    fn collect_asms(&mut self, ast: &'a ast::AST) -> Result<(), CollectError> {
        for def in &ast.0 {
            if let ast::Def::Asm(name, addr, _body) = def {
                if self.asms.contains_key(name.as_str()) {
                    return Err(CollectError::Duplicate(name.clone()));
                }

                // Evaluate address if present
                let address = addr.as_ref().and_then(|e| match self.constexpr(e) {
                    Ok(ConstExpr::Number(n)) => Some(n),
                    _ => None,
                });

                self.asms.insert(name.as_str(), AsmEntry { address, def });
            }
        }
        Ok(())
    }

    /// Collect functions
    fn collect_funcs(&mut self, ast: &'a ast::AST) -> Result<(), CollectError> {
        for def in &ast.0 {
            if let ast::Def::Func(name, params, ret_ty, _body) = def {
                if self.funcs.contains_key(name.as_str()) {
                    return Err(CollectError::Duplicate(name.clone()));
                }

                // Resolve parameter types
                let mut norm_params = Vec::new();
                for (param_name, param_ty) in params {
                    let norm_ty = self
                        .normtype(param_ty)
                        .map_err(|e| CollectError::UnsupportedConstExpr(e))?;
                    norm_params.push((param_name.clone(), norm_ty));
                }

                // Resolve return type
                let norm_ret = self
                    .normtype(ret_ty)
                    .map_err(|e| CollectError::UnsupportedConstExpr(e))?;

                // Create function type
                let norm_type = NormType::Func(norm_params, Box::new(norm_ret));

                self.funcs.insert(
                    name.as_str(),
                    FuncEntry {
                        norm_type,
                        address: None, // Will be determined in link phase
                        def,
                    },
                );
            }
        }
        Ok(())
    }

    /// Check for duplicate names across all symbol tables
    fn check_duplicates(&self) -> Result<(), CollectError> {
        let mut all_names = IndexMap::new();

        // Check types
        for &name in self.types.keys() {
            if let Some(existing) = all_names.insert(name, "Type") {
                return Err(CollectError::Duplicate(format!(
                    "{} (conflicts with {})",
                    name, existing
                )));
            }
        }

        // Check consts
        for &name in self.consts.keys() {
            if let Some(existing) = all_names.insert(name, "Const") {
                return Err(CollectError::Duplicate(format!(
                    "{} (conflicts with {})",
                    name, existing
                )));
            }
        }

        // Check statics
        for &name in self.statics.keys() {
            if let Some(existing) = all_names.insert(name, "Static") {
                return Err(CollectError::Duplicate(format!(
                    "{} (conflicts with {})",
                    name, existing
                )));
            }
        }

        // Check asms
        for &name in self.asms.keys() {
            if let Some(existing) = all_names.insert(name, "Asm") {
                return Err(CollectError::Duplicate(format!(
                    "{} (conflicts with {})",
                    name, existing
                )));
            }
        }

        // Check funcs
        for &name in self.funcs.keys() {
            if let Some(existing) = all_names.insert(name, "Func") {
                return Err(CollectError::Duplicate(format!(
                    "{} (conflicts with {})",
                    name, existing
                )));
            }
        }

        Ok(())
    }

    /// Get a symbol by name
    pub fn get(&self, name: &str) -> Option<Symbol<'_>> {
        if let Some(entry) = self.types.get(name) {
            return Some(Symbol::Type(entry.clone()));
        }
        if let Some(entry) = self.consts.get(name) {
            return Some(Symbol::Const(entry.clone()));
        }
        if let Some(entry) = self.statics.get(name) {
            return Some(Symbol::Static(entry.clone()));
        }
        if let Some(entry) = self.asms.get(name) {
            return Some(Symbol::Asm(entry.clone()));
        }
        if let Some(entry) = self.funcs.get(name) {
            return Some(Symbol::Func(entry.clone()));
        }
        None
    }

    /// Get the asms map
    pub fn asms(&self) -> &IndexMap<&'a str, AsmEntry<'a>> {
        &self.asms
    }

    /// Get the consts map
    pub fn consts(&self) -> &IndexMap<&'a str, ConstEntry<'a>> {
        &self.consts
    }

    /// Get the statics map
    pub fn statics(&self) -> &IndexMap<&'a str, StaticEntry<'a>> {
        &self.statics
    }

    /// Get the funcs map
    pub fn funcs(&self) -> &IndexMap<&'a str, FuncEntry<'a>> {
        &self.funcs
    }

    /// Get the types map
    pub fn types(&self) -> &IndexMap<&'a str, TypeEntry<'a>> {
        &self.types
    }

    pub fn normtype(&self, ty: &ast::Type) -> Result<NormType, String> {
        match ty {
            ast::Type::Int => Ok(NormType::Int),
            ast::Type::Void => Ok(NormType::Void),
            ast::Type::Custom(name) => self
                .types
                .get(name.as_str())
                .map(|entry| entry.norm_type.clone())
                .ok_or_else(|| format!("Unknown type: {}", name)),
            ast::Type::Addr(inner) => Ok(NormType::Addr(Box::new(self.normtype(inner)?))),
            ast::Type::Array(len_expr, elem_ty) => {
                // Evaluate array length expression
                let len = match self.constexpr(len_expr) {
                    Ok(ConstExpr::Number(n)) => n,
                    _ => return Err("Array length must be a constant integer".to_string()),
                };
                let elem = self.normtype(elem_ty)?;
                Ok(NormType::Array(len, Box::new(elem)))
            }
            ast::Type::Struct(fields) => {
                let mut norm_fields = Vec::new();
                for (name, field_ty) in fields {
                    let norm_ty = self.normtype(field_ty)?;
                    norm_fields.push((name.clone(), norm_ty));
                }
                Ok(NormType::Struct(norm_fields))
            }
            ast::Type::Func(params, ret_ty) => {
                let mut norm_params = Vec::new();
                for (name, param_ty) in params {
                    let norm_ty = self.normtype(param_ty)?;
                    norm_params.push((name.clone(), norm_ty));
                }
                let ret = self.normtype(ret_ty)?;
                Ok(NormType::Func(norm_params, Box::new(ret)))
            }
        }
    }

    pub fn constexpr(&self, expr: &ast::Expr) -> Result<ConstExpr, String> {
        match expr {
            ast::Expr::NumberLit(n) => Ok(ConstExpr::Number(*n)),
            ast::Expr::CharLit(c) => Ok(ConstExpr::Char(*c)),
            ast::Expr::StringLit(s) => Ok(ConstExpr::String(s.clone())),
            ast::Expr::ArrayLit(elems) => {
                let mut const_elems = Vec::new();
                for elem in elems {
                    const_elems.push(self.constexpr(elem)?);
                }
                Ok(ConstExpr::Array(const_elems))
            }
            ast::Expr::StructLit(fields) => {
                let mut const_fields = Vec::new();
                for (name, field_expr) in fields {
                    let const_val = self.constexpr(field_expr)?;
                    const_fields.push((name.clone(), const_val));
                }
                Ok(ConstExpr::Struct(const_fields))
            }
            ast::Expr::Ident(name) => {
                // Look up constant value
                self.consts
                    .get(name.as_str())
                    .map(|entry| entry.value.clone())
                    .ok_or_else(|| format!("Unknown constant: {}", name))
            }
            ast::Expr::Binary(op, left, right) => {
                // Evaluate binary operations on constants
                let left_val = self.constexpr(left)?;
                let right_val = self.constexpr(right)?;

                match (&left_val, &right_val) {
                    (ConstExpr::Number(l), ConstExpr::Number(r)) => match op {
                        BinaryOp::Add => Ok(ConstExpr::Number(l + r)),
                        BinaryOp::Sub => Ok(ConstExpr::Number(l.saturating_sub(*r))),
                        BinaryOp::Mul => Ok(ConstExpr::Number(l * r)),
                        BinaryOp::Div => {
                            if *r == 0 {
                                Err("Division by zero".to_string())
                            } else {
                                Ok(ConstExpr::Number(l / r))
                            }
                        }
                        BinaryOp::Mod => {
                            if *r == 0 {
                                Err("Modulo by zero".to_string())
                            } else {
                                Ok(ConstExpr::Number(l % r))
                            }
                        }
                        BinaryOp::And => Ok(ConstExpr::Number(l & r)),
                        BinaryOp::Or => Ok(ConstExpr::Number(l | r)),
                        BinaryOp::Xor => Ok(ConstExpr::Number(l ^ r)),
                        BinaryOp::Shl => Ok(ConstExpr::Number(l << r)),
                        BinaryOp::Shr => Ok(ConstExpr::Number(l >> r)),
                        _ => Err("Invalid operation for constant evaluation".to_string()),
                    },
                    _ => Err("Binary operation requires numeric operands".to_string()),
                }
            }
            ast::Expr::Unary(op, inner) => {
                let inner_val = self.constexpr(inner)?;
                match (&inner_val, op) {
                    (ConstExpr::Number(n), UnaryOp::Pos) => Ok(ConstExpr::Number(*n)),
                    (ConstExpr::Number(n), UnaryOp::Neg) => {
                        // Handle negative numbers with wrapping
                        Ok(ConstExpr::Number((-((*n) as isize)) as usize))
                    }
                    (ConstExpr::Number(n), UnaryOp::Not) => Ok(ConstExpr::Number(!n)),
                    _ => Err("Unary operation requires numeric operand".to_string()),
                }
            }
            ast::Expr::SizeofType(ty) => {
                // Calculate size of type
                let norm_ty = self.normtype(ty)?;
                Ok(ConstExpr::Number(norm_ty.sizeof()))
            }
            ast::Expr::SizeofExpr(inner) => {
                // Calculate size of expression's type
                let norm_ty = self.typeinfer(inner)?;
                Ok(ConstExpr::Number(norm_ty.sizeof()))
            }
            ast::Expr::Cast(inner, _target_ty) => {
                // For constant evaluation, just evaluate the inner expression
                // Type checking happens elsewhere
                self.constexpr(inner)
            }
            _ => Err("Expression cannot be evaluated at compile time".to_string()),
        }
    }

    pub fn typeinfer(&self, expr: &ast::Expr) -> Result<NormType, String> {
        match expr {
            ast::Expr::NumberLit(_) => Ok(NormType::Int),
            ast::Expr::CharLit(_) => Ok(NormType::Int),
            ast::Expr::StringLit(s) => {
                Ok(NormType::Array(s.len() + 1, Box::new(NormType::Int))) // +1 for null terminator
            }
            ast::Expr::ArrayLit(elems) => {
                if elems.is_empty() {
                    return Err("Cannot infer type of empty array".to_string());
                }
                let elem_ty = self.typeinfer(&elems[0])?;
                Ok(NormType::Array(elems.len(), Box::new(elem_ty)))
            }
            ast::Expr::StructLit(fields) => {
                let mut field_types = Vec::new();
                for (name, field_expr) in fields {
                    let field_ty = self.typeinfer(field_expr)?;
                    field_types.push((name.clone(), field_ty));
                }
                Ok(NormType::Struct(field_types))
            }
            ast::Expr::Ident(name) => {
                // Check statics
                if let Some(entry) = self.statics.get(name.as_str()) {
                    return Ok(entry.norm_type.clone());
                }

                // Check functions
                if let Some(entry) = self.funcs.get(name.as_str()) {
                    return Ok(entry.norm_type.clone());
                }

                // Check constants and get their type
                if let Some(entry) = self.consts.get(name.as_str()) {
                    return Ok(entry.norm_type.clone());
                }

                Err(format!("Unknown identifier: {}", name))
            }
            ast::Expr::Binary(op, left, right) => {
                let left_ty = self.typeinfer(left)?;
                let _right_ty = self.typeinfer(right)?;

                match op {
                    BinaryOp::Eq
                    | BinaryOp::Ne
                    | BinaryOp::Lt
                    | BinaryOp::Le
                    | BinaryOp::Gt
                    | BinaryOp::Ge => {
                        // Comparison operations return int (boolean as int)
                        Ok(NormType::Int)
                    }
                    _ => {
                        // Arithmetic and bitwise operations return the type of operands
                        // For now, assume they preserve the left operand type
                        Ok(left_ty)
                    }
                }
            }
            ast::Expr::Unary(op, inner) => {
                let inner_ty = self.typeinfer(inner)?;
                match op {
                    UnaryOp::Pos | UnaryOp::Neg | UnaryOp::Not => Ok(inner_ty),
                }
            }
            ast::Expr::Call(func_expr, _args) => {
                let func_ty = self.typeinfer(func_expr)?;
                match func_ty {
                    NormType::Func(_, ret_ty) => Ok(*ret_ty),
                    _ => Err("Expression is not callable".to_string()),
                }
            }
            ast::Expr::Index(arr_expr, _idx) => {
                let arr_ty = self.typeinfer(arr_expr)?;
                match arr_ty {
                    NormType::Array(_, elem_ty) => Ok(*elem_ty),
                    NormType::Addr(inner) => Ok(*inner),
                    _ => Err("Expression is not indexable".to_string()),
                }
            }
            ast::Expr::Member(struct_expr, field_name) => {
                let struct_ty = self.typeinfer(struct_expr)?;
                match struct_ty {
                    NormType::Struct(fields) => {
                        for (name, field_ty) in fields {
                            if name == *field_name {
                                return Ok(field_ty);
                            }
                        }
                        Err(format!("Struct has no field: {}", field_name))
                    }
                    _ => Err("Expression is not a struct".to_string()),
                }
            }
            ast::Expr::Addr(inner) => {
                let inner_ty = self.typeinfer(inner)?;
                Ok(NormType::Addr(Box::new(inner_ty)))
            }
            ast::Expr::Deref(ptr_expr) => {
                let ptr_ty = self.typeinfer(ptr_expr)?;
                match ptr_ty {
                    NormType::Addr(inner) => Ok(*inner),
                    _ => Err("Cannot dereference non-pointer type".to_string()),
                }
            }
            ast::Expr::Cast(_inner, target_ty) => self.normtype(target_ty),
            ast::Expr::SizeofType(_) | ast::Expr::SizeofExpr(_) => Ok(NormType::Int),
            _ => Err("Cannot infer type of expression".to_string()),
        }
    }
}

/// Helper function to check if an expression contains Sizeof or SizeofExpr
fn contains_sizeof(expr: &ast::Expr) -> bool {
    match expr {
        ast::Expr::SizeofType(_) | ast::Expr::SizeofExpr(_) => true,
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
        ast::Expr::Addr(e) | ast::Expr::Deref(e) => contains_sizeof(e),
        _ => false,
    }
}
