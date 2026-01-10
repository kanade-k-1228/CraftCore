//! Global evaluation context using Salsa for incremental computation

use crate::{
    error::Error,
    grammer::ast::{self},
};

use super::{
    constexpr::ConstExpr,
    db::{self, Database, Db, ExprId, TypeId},
    normtype::NormType,
};

/// Global evaluation context
pub struct Global {
    db: Database,
}

impl Global {
    pub fn new(ast: ast::AST) -> Result<Self, Error> {
        let db = Database::new(ast)?;
        Ok(Global { db })
    }
}

/// Iterator methods
impl Global {
    pub fn keys(&self) -> impl Iterator<Item = &str> {
        self.db.defs().keys().map(|s| s.as_str())
    }

    pub fn types(&self) -> impl Iterator<Item = &str> {
        self.db
            .defs()
            .iter()
            .filter(|(_, &idx)| matches!(&self.db.ast().0[idx], ast::Def::Type(..)))
            .map(|(name, _)| name.as_str())
    }

    pub fn consts(&self) -> impl Iterator<Item = &str> {
        self.db
            .defs()
            .iter()
            .filter(|(_, &idx)| matches!(&self.db.ast().0[idx], ast::Def::Const(..)))
            .map(|(name, _)| name.as_str())
    }

    pub fn statics(&self) -> impl Iterator<Item = &str> {
        self.db
            .defs()
            .iter()
            .filter(|(_, &idx)| matches!(&self.db.ast().0[idx], ast::Def::Static(..)))
            .map(|(name, _)| name.as_str())
    }

    pub fn asms(&self) -> impl Iterator<Item = &str> {
        self.db
            .defs()
            .iter()
            .filter(|(_, &idx)| matches!(&self.db.ast().0[idx], ast::Def::Asm(..)))
            .map(|(name, _)| name.as_str())
    }

    pub fn funcs(&self) -> impl Iterator<Item = &str> {
        self.db
            .defs()
            .iter()
            .filter(|(_, &idx)| matches!(&self.db.ast().0[idx], ast::Def::Func(..)))
            .map(|(name, _)| name.as_str())
    }
}

/// Getter methods
impl Global {
    pub fn get(&self, name: &str) -> Option<&ast::Def> {
        let idx = *self.db.defs().get(name)?;
        Some(&self.db.ast().0[idx])
    }
}

/// Evaluation methods using salsa
impl Global {
    /// Normalize a type by resolving custom types and computing array sizes
    pub fn normtype(&self, ty: &ast::Type) -> Result<NormType, Error> {
        let ty_id = TypeId::new(&self.db, ty.clone());
        db::normtype(&self.db, ty_id)
    }

    /// Evaluate a constant expression
    pub fn constexpr(&self, expr: &ast::Expr) -> Result<ConstExpr, Error> {
        let expr_id = ExprId::new(&self.db, expr.clone());
        db::constexpr(&self.db, expr_id)
    }

    /// Infer the type of an expression
    pub fn typeinfer(&self, expr: &ast::Expr) -> Result<NormType, Error> {
        let expr_id = ExprId::new(&self.db, expr.clone());
        db::typeinfer(&self.db, expr_id)
    }

    /// Infer address of expr with unresolved symbol (symbol, offset)
    pub fn addrexpr(&self, expr: &ast::Expr) -> Result<(String, usize), Error> {
        match expr {
            ast::Expr::Ident(name) => {
                if let Some(def) = self.get(name) {
                    match def {
                        ast::Def::Static(_, _, _)
                        | ast::Def::Const(_, _, _)
                        | ast::Def::Func(_, _, _, _)
                        | ast::Def::Asm(_, _, _) => Ok((name.clone(), 0)),
                        _ => Err(Error::NotAddressable(name.to_string())),
                    }
                } else {
                    Err(Error::UnknownIdentifier(name.to_string()))
                }
            }

            ast::Expr::Index(base, index) => {
                let (symbol, offset) = self.addrexpr(base)?;
                let index = match self.constexpr(index) {
                    Ok(ConstExpr::Number(idx)) => idx,
                    _ => return Err(Error::NonConstantArrayIndexInAddress),
                };
                let ty = self.typeinfer(base)?;
                let ofs = ty.get_array_offset(index).ok_or(Error::NotIndexable)?;
                Ok((symbol, offset + ofs))
            }

            ast::Expr::Member(base, field) => {
                let (symbol, offset) = self.addrexpr(base)?;
                let ty = self.typeinfer(base)?;
                let ofs = ty
                    .get_field_offset(field)
                    .ok_or(Error::NoSuchField(field.to_string()))?;
                Ok((symbol, offset + ofs))
            }

            ast::Expr::Cast(inner, _) => self.addrexpr(inner),

            _ => Err(Error::NotAddressable(format!("{:?}", expr))),
        }
    }
}

/// Resolved getter methods
impl Global {
    pub fn get_type_resolved(&self, name: &str) -> Option<(NormType, usize)> {
        let def = self.get(name)?;
        if let ast::Def::Type(_, ty) = def {
            let ty = self.normtype(ty).ok()?;
            let size = ty.sizeof();
            Some((ty, size))
        } else {
            None
        }
    }

    pub fn get_const_resolved(&self, name: &str) -> Option<(NormType, ConstExpr, Option<usize>)> {
        let def = self.get(name)?;
        if let ast::Def::Const(_, addr, expr) = def {
            let value = self.constexpr(expr).ok()?;
            let ty = value.typeinfer().ok()?;
            let addr = addr.as_ref().and_then(|e| match self.constexpr(e) {
                Ok(ConstExpr::Number(n)) => Some(n),
                _ => None,
            });
            Some((ty, value, addr))
        } else {
            None
        }
    }

    pub fn get_static_resolved(&self, name: &str) -> Option<(NormType, Option<usize>)> {
        let def = self.get(name)?;
        if let ast::Def::Static(_, addr, ty) = def {
            let ty = self.normtype(ty).ok()?;
            let addr = addr.as_ref().and_then(|e| match self.constexpr(e) {
                Ok(ConstExpr::Number(n)) => Some(n),
                _ => None,
            });
            Some((ty, addr))
        } else {
            None
        }
    }

    pub fn get_asm_resolved(&self, name: &str) -> Option<Option<usize>> {
        let def = self.get(name)?;
        if let ast::Def::Asm(_, addr, _) = def {
            let addr = addr.as_ref().and_then(|e| match self.constexpr(e) {
                Ok(ConstExpr::Number(n)) => Some(n),
                _ => None,
            });
            Some(addr)
        } else {
            None
        }
    }

    pub fn get_func_resolved(&self, name: &str) -> Option<NormType> {
        let def = self.get(name)?;
        if let ast::Def::Func(_, params, ret_ty, _) = def {
            let mut norm_params = Vec::new();
            for (param_name, param_ty) in params {
                let norm_ty = self.normtype(param_ty).ok()?;
                norm_params.push((param_name.clone(), norm_ty));
            }
            let norm_ret = self.normtype(ret_ty).ok()?;
            Some(NormType::Func(norm_params, Box::new(norm_ret)))
        } else {
            None
        }
    }
}

/// Object collection methods
impl Global {
    pub fn instobjs(&self) -> Result<(Vec<(&str, usize, usize)>, Vec<(&str, usize, &str)>), Error> {
        let mut fixed = vec![];
        let mut auto = vec![];

        for (name, &idx) in self.db.defs() {
            let def = &self.db.ast().0[idx];
            match def {
                ast::Def::Asm(_, addr, body) => {
                    let size = body.len();
                    match addr {
                        Some(addr) => {
                            let addr = match self.constexpr(addr)? {
                                ConstExpr::Number(n) => n,
                                _ => {
                                    return Err(Error::InvalidImmediate(
                                        "address must be numeric".to_string(),
                                    ))
                                }
                            };
                            fixed.push((name.as_str(), size, addr));
                        }
                        None => auto.push((name.as_str(), size, "asm")),
                    }
                }
                ast::Def::Func(_, _, _, body) => {
                    let size = body.len(); // placeholder
                    auto.push((name.as_str(), size, "func"));
                }
                _ => {}
            }
        }

        Ok((fixed, auto))
    }

    pub fn dataobjs(&self) -> Result<(Vec<(&str, usize, usize)>, Vec<(&str, usize, &str)>), Error> {
        let mut fixed = vec![];
        let mut auto = vec![];

        for (name, &idx) in self.db.defs() {
            let def = &self.db.ast().0[idx];
            match def {
                ast::Def::Const(_, addr, value) => {
                    let ty = self.typeinfer(value)?;
                    let size = ty.sizeof();
                    match addr {
                        Some(addr) => {
                            let addr = match self.constexpr(addr)? {
                                ConstExpr::Number(n) => n,
                                _ => {
                                    return Err(Error::InvalidImmediate(
                                        "address must be numeric".to_string(),
                                    ))
                                }
                            };
                            fixed.push((name.as_str(), size, addr));
                        }
                        None => auto.push((name.as_str(), size, "const")),
                    }
                }
                ast::Def::Static(_, addr, ty) => {
                    let size = self.normtype(ty)?.sizeof();
                    match addr {
                        Some(addr) => {
                            let addr = match self.constexpr(addr)? {
                                ConstExpr::Number(n) => n,
                                _ => {
                                    return Err(Error::InvalidImmediate(
                                        "address must be numeric".to_string(),
                                    ))
                                }
                            };
                            fixed.push((name.as_str(), size, addr));
                        }
                        None => auto.push((name.as_str(), size, "static")),
                    }
                }
                _ => {}
            }
        }

        Ok((fixed, auto))
    }
}
