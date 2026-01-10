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

    fn next_offset(&self) -> isize {
        self.stack
            .values()
            .filter(|(_, offset)| *offset < 0)
            .map(|(ty, offset)| offset - ty.sizeof() as isize)
            .min()
            .unwrap_or(-1)
    }

    pub fn args(&mut self, args: &'a [(String, ast::Type)]) -> Result<isize, Error> {
        let mut offset = 2isize;
        for (name, ty) in args.iter().rev() {
            let ty = self.global.normtype(ty)?;
            let size = ty.sizeof() as isize;
            self.stack.insert(name.as_str(), (ty, offset));
            offset += size;
        }
        Ok(offset)
    }

    pub fn push(&mut self, name: &'a str, ty: &'a ast::Type) -> Result<isize, Error> {
        if self.stack.contains_key(name) {
            return Err(Error::DuplicateLocal(name.to_string()));
        }

        let norm_ty = self.global.normtype(ty)?;
        let offset = self.next_offset();

        self.stack.insert(name, (norm_ty, offset));

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
        if let ast::Expr::Ident(name) = expr {
            if self.is_local(name) {
                return Err(Error::NonConstantExpression);
            }
        }
        self.global.constexpr(expr)
    }

    /// Infer the type of an expression with local context
    pub fn typeinfer(&self, expr: &'a ast::Expr) -> Result<NormType, Error> {
        match expr {
            ast::Expr::Ident(name) => {
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
            ast::Expr::Ident(name) => {
                // Local variables don't have static addresses
                if self.is_local(name) {
                    return Err(Error::NotAddressable(name.to_string()));
                }
                // Delegate to global for static/const/func
                self.global.addrexpr(expr)
            }

            // For member access, check if base is local
            ast::Expr::Member(base, _field) => {
                if let ast::Expr::Ident(name) = base.as_ref() {
                    if self.is_local(name) {
                        return Err(Error::NotAddressable(format!("local variable {}", name)));
                    }
                }
                self.global.addrexpr(expr)
            }

            // For index access, check if base is local
            ast::Expr::Index(base, _index) => {
                if let ast::Expr::Ident(name) = base.as_ref() {
                    if self.is_local(name) {
                        return Err(Error::NotAddressable(format!("local variable {}", name)));
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

    pub fn stack_size(&self) -> usize {
        (-self.next_offset() - 1) as usize
    }
}
