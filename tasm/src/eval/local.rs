use indexmap::IndexMap;

use crate::{
    error::Error,
    eval::{constexpr::ConstExpr, global::Global, normtype::NormType},
    grammer::ast,
};

pub struct Local<'a> {
    global: &'a Global<'a>,
    stack: IndexMap<&'a str, (NormType, isize)>,
}

impl<'a> Local<'a> {
    pub fn fork(global: &'a Global<'a>) -> Self {
        Self {
            global,
            stack: IndexMap::new(),
        }
    }

    /// Returns the base offset (lowest address relative to FP) for a new local
    /// of the given size. Locals live below FP, so this is negative.
    fn next_offset(&self, size: isize) -> isize {
        let lowest = self
            .stack
            .values()
            .filter(|(_, offset)| *offset < 0)
            .map(|(_, offset)| *offset)
            .min()
            .unwrap_or(0);
        lowest - size
    }

    pub fn args(&mut self, args: &'a [(ast::Ident, ast::Type)]) -> Result<isize, Error> {
        // Args are stored above FP, immediately after the saved RA (FP+0) and
        // saved FP (FP+1) slots. The first declared argument gets the lowest
        // offset (FP+2), matching the prologue which copies args in declaration
        // order.
        let mut offset = 2isize;
        for ((name, _), ty) in args.iter() {
            let ty = self.global.normtype(ty)?;
            let size = ty.sizeof() as isize;
            self.stack.insert(name.as_str(), (ty, offset));
            offset += size;
        }
        Ok(offset)
    }

    pub fn push(&mut self, ident: &'a ast::Ident, ty: &'a ast::Type) -> Result<isize, Error> {
        let (name, pos) = ident;
        if self.stack.contains_key(name.as_str()) {
            return Err(Error::DuplicateLocal(pos.clone(), name.clone()));
        }

        let norm_ty = self.global.normtype(ty)?;
        let size = norm_ty.sizeof() as isize;
        let offset = self.next_offset(size);

        self.stack.insert(name.as_str(), (norm_ty, offset));

        Ok(offset)
    }

    pub fn pop(&mut self) {}

    pub fn vartype(&self, name: &str) -> Option<&NormType> {
        self.stack.get(name).map(|(ty, _)| ty)
    }

    pub fn offset(&self, name: &str) -> Option<isize> {
        self.stack.get(name).map(|(_, offset)| *offset)
    }

    /// Normalize a type - simply delegates to global
    pub fn normtype(&self, ty: &'a ast::Type) -> Result<NormType, Error> {
        self.global.normtype(ty)
    }

    /// Evaluate a constant expression - delegates to global
    /// Local variables are not constant expressions
    pub fn constexpr(&self, expr: &'a ast::Expr) -> Result<ConstExpr, Error> {
        // Local variables cannot be used in constant expressions
        if let ast::Expr::Ident((name, pos)) = expr {
            if self.is_local(name) {
                return Err(Error::NonConstantExpression(pos.clone()));
            }
        }
        self.global.constexpr(expr)
    }

    /// Infer the type of an expression with local context
    pub fn typeinfer(&self, expr: &'a ast::Expr) -> Result<NormType, Error> {
        match expr {
            ast::Expr::Ident((name, _)) => {
                // Check local scope first
                if let Some(ty) = self.vartype(name) {
                    return Ok(ty.clone());
                }
                // Fall back to global scope
                self.global.typeinfer(expr)
            }
            // For other expressions, delegate to global
            _ => self.global.typeinfer(expr),
        }
    }

    /// Infer address of expr with unresolved symbol
    /// Local variables cannot have static addresses
    pub fn addrexpr(&self, expr: &'a ast::Expr) -> Result<(String, usize), Error> {
        match expr {
            ast::Expr::Ident((name, pos)) => {
                // Local variables don't have static addresses
                if self.is_local(name) {
                    return Err(Error::NotAddressable(pos.clone(), name.clone()));
                }
                // Delegate to global for static/const/func
                self.global.addrexpr(expr)
            }

            // For member access, check if base is local
            ast::Expr::Member(base, _field) => {
                if let ast::Expr::Ident((name, pos)) = base.as_ref() {
                    if self.is_local(name) {
                        return Err(Error::NotAddressable(
                            pos.clone(),
                            format!("local variable {}", name),
                        ));
                    }
                }
                self.global.addrexpr(expr)
            }

            // For index access, check if base is local
            ast::Expr::Index(base, _index) => {
                if let ast::Expr::Ident((name, pos)) = base.as_ref() {
                    if self.is_local(name) {
                        return Err(Error::NotAddressable(
                            pos.clone(),
                            format!("local variable {}", name),
                        ));
                    }
                }
                self.global.addrexpr(expr)
            }

            // For other expressions, delegate to global
            _ => self.global.addrexpr(expr),
        }
    }

    pub fn is_local(&self, name: &str) -> bool {
        self.stack.contains_key(name)
    }

    /// Look up a global definition by name (delegates to Global).
    pub fn global_def(&self, name: &str) -> Option<&'a ast::Def> {
        self.global.get(name)
    }

    /// All (name → FP-relative offset) entries, preserving insertion order.
    pub fn entries(&self) -> IndexMap<String, isize> {
        self.stack
            .iter()
            .map(|(name, (_, offset))| (name.to_string(), *offset))
            .collect()
    }

    /// Total stack space used by locals (positive; in 16-bit words).
    pub fn stack_size(&self) -> usize {
        self.stack
            .values()
            .filter(|(_, offset)| *offset < 0)
            .map(|(_, offset)| (-offset) as usize)
            .max()
            .unwrap_or(0)
    }
}
