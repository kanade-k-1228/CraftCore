use indexmap::IndexMap;
use std::collections::HashMap;
use std::sync::RwLock;

use crate::{
    error::Error,
    grammer::{
        ast::{self, BinaryOp, UnaryOp},
        token::Pos,
    },
};

use super::{code::Code, constexpr::ConstExpr, normtype::NormType};

pub struct Global<'a> {
    defs: IndexMap<&'a str, &'a ast::Def>,
    _normtype: RwLock<HashMap<&'a ast::Type, NormType>>,
    _constexpr: RwLock<HashMap<&'a ast::Expr, ConstExpr>>,
    _typeinfer: RwLock<HashMap<&'a ast::Expr, NormType>>,
    _code: RwLock<HashMap<&'a str, Code>>,
}

impl<'a> Global<'a> {
    pub fn new(ast: &'a ast::AST) -> Result<Self, Error> {
        let mut defs = IndexMap::new();
        for def in &ast.0 {
            let (name, pos) = match def {
                ast::Def::Type((name, pos), _) => (name.as_str(), pos),
                ast::Def::Const((name, pos), _, _) => (name.as_str(), pos),
                ast::Def::Static((name, pos), _, _) => (name.as_str(), pos),
                ast::Def::Asm((name, pos), _, _) => (name.as_str(), pos),
                ast::Def::Func((name, pos), _, _, _) => (name.as_str(), pos),
            };

            if defs.contains_key(name) {
                return Err(Error::Duplicate(pos.clone(), name.to_string()));
            }

            defs.insert(name, def);
        }

        Ok(Global {
            defs,
            _normtype: RwLock::new(HashMap::new()),
            _constexpr: RwLock::new(HashMap::new()),
            _typeinfer: RwLock::new(HashMap::new()),
            _code: RwLock::new(HashMap::new()),
        })
    }
}

impl<'a> Global<'a> {
    pub fn keys(&self) -> impl Iterator<Item = &'a str> + '_ {
        self.defs.keys().copied()
    }

    pub fn types(&self) -> impl Iterator<Item = &'a str> + '_ {
        self.defs
            .iter()
            .filter(|(_, &def)| matches!(def, ast::Def::Type(..)))
            .map(|(&name, _)| name)
    }

    pub fn consts(&self) -> impl Iterator<Item = &'a str> + '_ {
        self.defs
            .iter()
            .filter(|(_, &def)| matches!(def, ast::Def::Const(..)))
            .map(|(&name, _)| name)
    }

    pub fn statics(&self) -> impl Iterator<Item = &'a str> + '_ {
        self.defs
            .iter()
            .filter(|(_, &def)| matches!(def, ast::Def::Static(..)))
            .map(|(&name, _)| name)
    }

    pub fn asms(&self) -> impl Iterator<Item = &'a str> + '_ {
        self.defs
            .iter()
            .filter(|(_, &def)| matches!(def, ast::Def::Asm(..)))
            .map(|(&name, _)| name)
    }

    pub fn funcs(&self) -> impl Iterator<Item = &'a str> + '_ {
        self.defs
            .iter()
            .filter(|(_, &def)| matches!(def, ast::Def::Func(..)))
            .map(|(&name, _)| name)
    }
}

impl<'a> Global<'a> {
    pub fn get(&self, name: &str) -> Option<&'a ast::Def> {
        self.defs.get(name).copied()
    }
}

impl<'a> Global<'a> {
    /// Normalize a type by resolving custom types and computing array sizes
    pub fn normtype(&self, ty: &'a ast::Type) -> Result<NormType, Error> {
        {
            let cache = self._normtype.read().unwrap();
            if let Some(cached) = cache.get(ty) {
                return Ok(cached.clone());
            }
        }

        // Evaluate
        let result = match ty {
            ast::Type::Int => Ok(NormType::Int),
            ast::Type::Void => Ok(NormType::Void),
            ast::Type::Custom((name, pos)) => {
                if let Some(&def) = self.defs.get(name.as_str()) {
                    match def {
                        ast::Def::Type(_, type_def) => self.normtype(type_def),
                        ast::Def::Const((_, pos), _, _)
                        | ast::Def::Static((_, pos), _, _)
                        | ast::Def::Asm((_, pos), _, _)
                        | ast::Def::Func((_, pos), _, _, _) => {
                            Err(Error::NotAType(pos.clone(), name.clone()))
                        }
                    }
                } else {
                    Err(Error::UnknownType(pos.clone(), name.clone()))
                }
            }
            ast::Type::Addr(inner) => {
                let inner_type = self.normtype(inner)?;
                Ok(NormType::Addr(Box::new(inner_type)))
            }
            ast::Type::Array(len, ty) => {
                let len = match self.constexpr(len) {
                    Ok(ConstExpr::Number(n)) => n,
                    _ => return Err(Error::NonConstantArrayLength(len.pos_or_default())),
                };
                let ty = self.normtype(ty)?;
                Ok(NormType::Array(len, Box::new(ty)))
            }
            ast::Type::Struct(fields) => {
                let mut norm_fields = Vec::new();
                for ((name, _), ty) in fields {
                    let norm = self.normtype(ty)?;
                    norm_fields.push((name.clone(), norm));
                }
                Ok(NormType::Struct(norm_fields))
            }
            ast::Type::Func(params, ret_ty) => {
                let mut norm_params = Vec::new();
                for ((name, _), param_ty) in params {
                    let norm_ty = self.normtype(param_ty)?;
                    norm_params.push((name.clone(), norm_ty));
                }
                let ret = self.normtype(ret_ty)?;
                Ok(NormType::Func(norm_params, Box::new(ret)))
            }
        };

        // Store with write lock if successful
        if let Ok(ref norm_type) = result {
            let mut cache = self._normtype.write().unwrap();
            cache.insert(ty, norm_type.clone());
        }

        result
    }

    /// Evaluate a constant expression
    pub fn constexpr(&self, expr: &'a ast::Expr) -> Result<ConstExpr, Error> {
        // Try to restore from cache with read lock
        {
            let cache = self._constexpr.read().unwrap();
            if let Some(cached) = cache.get(expr) {
                return Ok(cached.clone());
            }
        }

        let result = match expr {
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
                for ((name, _), field_expr) in fields {
                    let const_val = self.constexpr(field_expr)?;
                    const_fields.push((name.clone(), const_val));
                }
                Ok(ConstExpr::Struct(const_fields))
            }
            ast::Expr::Ident((name, pos)) => {
                // Look up constant value
                if let Some(&def) = self.defs.get(name.as_str()) {
                    match def {
                        ast::Def::Const(_, _, const_expr) => self.constexpr(const_expr),
                        ast::Def::Type((_, pos), _)
                        | ast::Def::Static((_, pos), _, _)
                        | ast::Def::Asm((_, pos), _, _)
                        | ast::Def::Func((_, pos), _, _, _) => {
                            Err(Error::NotAConstant(pos.clone(), name.clone()))
                        }
                    }
                } else {
                    Err(Error::UnknownConstant(pos.clone(), name.clone()))
                }
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
                                Err(Error::DivisionByZero(right.pos_or_default()))
                            } else {
                                Ok(ConstExpr::Number(l / r))
                            }
                        }
                        BinaryOp::Mod => {
                            if *r == 0 {
                                Err(Error::ModuloByZero(right.pos_or_default()))
                            } else {
                                Ok(ConstExpr::Number(l % r))
                            }
                        }
                        BinaryOp::And => Ok(ConstExpr::Number(l & r)),
                        BinaryOp::Or => Ok(ConstExpr::Number(l | r)),
                        BinaryOp::Xor => Ok(ConstExpr::Number(l ^ r)),
                        BinaryOp::Shl => Ok(ConstExpr::Number(l << r)),
                        BinaryOp::Shr => Ok(ConstExpr::Number(l >> r)),
                        BinaryOp::Eq => Ok(ConstExpr::Number(if l == r { 1 } else { 0 })),
                        BinaryOp::Ne => Ok(ConstExpr::Number(if l != r { 1 } else { 0 })),
                        BinaryOp::Lt => Ok(ConstExpr::Number(if l < r { 1 } else { 0 })),
                        BinaryOp::Le => Ok(ConstExpr::Number(if l <= r { 1 } else { 0 })),
                        BinaryOp::Gt => Ok(ConstExpr::Number(if l > r { 1 } else { 0 })),
                        BinaryOp::Ge => Ok(ConstExpr::Number(if l >= r { 1 } else { 0 })),
                    },
                    _ => Err(Error::NonNumericBinaryOperands(left.pos_or_default())),
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
                    _ => Err(Error::NonNumericUnaryOperand(inner.pos_or_default())),
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
            _ => Err(Error::NonConstantExpression(expr.pos_or_default())),
        };

        // Store with write lock if successful
        if let Ok(ref const_expr) = result {
            let mut cache = self._constexpr.write().unwrap();
            cache.insert(expr, const_expr.clone());
        }

        result
    }

    /// Infer the type of an expression
    pub fn typeinfer(&self, expr: &'a ast::Expr) -> Result<NormType, Error> {
        // Try to restore from cache with read lock
        {
            let cache = self._typeinfer.read().unwrap();
            if let Some(cached) = cache.get(expr) {
                return Ok(cached.clone());
            }
        }

        let result = match expr {
            ast::Expr::NumberLit(_) => Ok(NormType::Int),
            ast::Expr::CharLit(_) => Ok(NormType::Int),
            ast::Expr::StringLit(s) => {
                Ok(NormType::Array(s.len() + 1, Box::new(NormType::Int))) // +1 for null terminator
            }
            ast::Expr::ArrayLit(elems) => {
                if elems.is_empty() {
                    return Err(Error::EmptyArrayTypeInference(expr.pos_or_default()));
                }
                let elem_ty = self.typeinfer(&elems[0])?;
                Ok(NormType::Array(elems.len(), Box::new(elem_ty)))
            }
            ast::Expr::StructLit(fields) => {
                let mut field_types = Vec::new();
                for ((name, _), field_expr) in fields {
                    let field_ty = self.typeinfer(field_expr)?;
                    field_types.push((name.clone(), field_ty));
                }
                Ok(NormType::Struct(field_types))
            }
            ast::Expr::Ident((name, pos)) => {
                // Look up the identifier in definitions
                if let Some(&def) = self.defs.get(name.as_str()) {
                    match def {
                        ast::Def::Static(_, _, ty) => self.normtype(ty),
                        ast::Def::Const((_, pos), _, expr) => {
                            // Infer type from constant expression
                            let const_val = self.constexpr(expr)?;
                            const_val
                                .typeinfer()
                                .map_err(|_| Error::NotAValue(pos.clone(), name.clone()))
                        }
                        ast::Def::Func(_, params, ret_ty, _) => {
                            // Build function type
                            let mut norm_params = Vec::new();
                            for ((param_name, _), param_ty) in params {
                                let norm_ty = self.normtype(param_ty)?;
                                norm_params.push((param_name.clone(), norm_ty));
                            }
                            let norm_ret = self.normtype(ret_ty)?;
                            Ok(NormType::Func(norm_params, Box::new(norm_ret)))
                        }
                        ast::Def::Type((_, pos), _) | ast::Def::Asm((_, pos), _, _) => {
                            Err(Error::NotAValue(pos.clone(), name.clone()))
                        }
                    }
                } else {
                    Err(Error::UnknownIdentifier(pos.clone(), name.clone()))
                }
            }
            ast::Expr::Binary(op, left, right) => {
                let left_ty = self.typeinfer(left)?;
                let right_ty = self.typeinfer(right)?;

                let is_addr = |t: &NormType| matches!(t, NormType::Addr(_));
                let is_int = |t: &NormType| matches!(t, NormType::Int);

                match op {
                    // Comparison operators always return int (as boolean).
                    // Both sides must have matching word size.
                    BinaryOp::Eq
                    | BinaryOp::Ne
                    | BinaryOp::Lt
                    | BinaryOp::Le
                    | BinaryOp::Gt
                    | BinaryOp::Ge => {
                        if left_ty.sizeof() != right_ty.sizeof() {
                            return Err(Error::InvalidCastSize(
                                left.pos_or_default(),
                                left_ty.sizeof(),
                                right_ty.sizeof(),
                            ));
                        }
                        Ok(NormType::Int)
                    }

                    // Pointer arithmetic: ptr +/- int → ptr; int + ptr → ptr;
                    // ptr - ptr → int. Otherwise both must be int.
                    BinaryOp::Add => match (&left_ty, &right_ty) {
                        (NormType::Addr(_), r) if is_int(r) => Ok(left_ty),
                        (l, NormType::Addr(_)) if is_int(l) => Ok(right_ty),
                        (l, r) if is_int(l) && is_int(r) => Ok(NormType::Int),
                        _ => Err(Error::NonNumericBinaryOperands(left.pos_or_default())),
                    },
                    BinaryOp::Sub => match (&left_ty, &right_ty) {
                        (NormType::Addr(_), r) if is_int(r) => Ok(left_ty),
                        (l, r) if is_addr(l) && is_addr(r) => Ok(NormType::Int),
                        (l, r) if is_int(l) && is_int(r) => Ok(NormType::Int),
                        _ => Err(Error::NonNumericBinaryOperands(left.pos_or_default())),
                    },

                    // Pure arithmetic / bitwise / shift: both sides must be int.
                    BinaryOp::Mul
                    | BinaryOp::Div
                    | BinaryOp::Mod
                    | BinaryOp::And
                    | BinaryOp::Or
                    | BinaryOp::Xor
                    | BinaryOp::Shl
                    | BinaryOp::Shr => {
                        if !(is_int(&left_ty) && is_int(&right_ty)) {
                            return Err(Error::NonNumericBinaryOperands(left.pos_or_default()));
                        }
                        Ok(NormType::Int)
                    }
                }
            }
            ast::Expr::Unary(op, expr) => {
                let ty = self.typeinfer(expr)?;
                match op {
                    UnaryOp::Pos | UnaryOp::Neg | UnaryOp::Not => Ok(ty),
                }
            }
            ast::Expr::Call(func_expr, _args) => {
                let func_ty = self.typeinfer(func_expr)?;
                match func_ty {
                    NormType::Func(_, ret_ty) => Ok(*ret_ty),
                    _ => Err(Error::NotCallable(func_expr.pos_or_default())),
                }
            }
            ast::Expr::Index(arr_expr, _idx) => {
                let arr_ty = self.typeinfer(arr_expr)?;
                match arr_ty {
                    NormType::Array(_, elem_ty) => Ok(*elem_ty),
                    NormType::Addr(inner) => Ok(*inner),
                    _ => Err(Error::NotIndexable(arr_expr.pos_or_default())),
                }
            }
            ast::Expr::Member(base_expr, (field, field_pos)) => match self.typeinfer(base_expr)? {
                NormType::Struct(fields) => match fields.iter().find(|(name, _)| name == field) {
                    Some((_, ty)) => return Ok(ty.clone()),
                    None => return Err(Error::NoSuchField(field_pos.clone(), field.clone())),
                },
                _ => Err(Error::NotAStruct(base_expr.pos_or_default())),
            },
            ast::Expr::Addr(inner) => {
                let ty = self.typeinfer(inner)?;
                Ok(NormType::Addr(Box::new(ty)))
            }
            ast::Expr::Deref(inner) => {
                let ty = self.typeinfer(inner)?;
                match ty {
                    NormType::Addr(inner) => Ok(*inner),
                    _ => Err(Error::CannotDereferenceNonPointer(inner.pos_or_default())),
                }
            }
            ast::Expr::Cast(inner, ty) => {
                let base = self.typeinfer(inner)?;
                let cast = self.normtype(ty)?;
                if base.sizeof() == cast.sizeof() {
                    Ok(cast)
                } else {
                    Err(Error::InvalidCastSize(
                        inner.pos_or_default(),
                        base.sizeof(),
                        cast.sizeof(),
                    ))
                }
            }
            ast::Expr::SizeofType(_) | ast::Expr::SizeofExpr(_) => Ok(NormType::Int),
        };

        // Store with write lock if successful
        if let Ok(ref norm_type) = result {
            let mut cache = self._typeinfer.write().unwrap();
            cache.insert(expr, norm_type.clone());
        }

        result
    }

    /// Infer address of expr with unresolved symbol (symbol, offset)
    pub fn addrexpr(&self, expr: &'a ast::Expr) -> Result<(String, usize), Error> {
        match expr {
            ast::Expr::Ident((name, pos)) => match self.defs.get(name.as_str()) {
                Some(&def) => match def {
                    ast::Def::Static(_, _, _)
                    | ast::Def::Const(_, _, _)
                    | ast::Def::Func(_, _, _, _)
                    | ast::Def::Asm(_, _, _) => Ok((name.clone(), 0)),
                    ast::Def::Type((_, pos), _) => {
                        Err(Error::NotAddressable(pos.clone(), name.clone()))
                    }
                },
                None => Err(Error::UnknownIdentifier(pos.clone(), name.clone())),
            },

            ast::Expr::Index(base, index) => {
                let (symbol, offset) = self.addrexpr(base)?;
                let idx = match self.constexpr(index) {
                    Ok(ConstExpr::Number(idx)) => idx,
                    _ => {
                        return Err(Error::NonConstantArrayIndexInAddress(
                            index.pos_or_default(),
                        ))
                    }
                };
                let ty = self.typeinfer(base)?;
                let ofs = ty
                    .get_array_offset(idx)
                    .ok_or(Error::NotIndexable(base.pos_or_default()))?;
                Ok((symbol, offset + ofs))
            }

            ast::Expr::Member(base, (field, field_pos)) => {
                let (symbol, offset) = self.addrexpr(base)?;
                let ty = self.typeinfer(base)?;
                let ofs = ty
                    .get_field_offset(field)
                    .ok_or(Error::NoSuchField(field_pos.clone(), field.clone()))?;
                Ok((symbol, offset + ofs))
            }

            ast::Expr::Cast(inner, _) => self.addrexpr(inner),

            _ => Err(Error::NotAddressable(
                expr.pos_or_default(),
                format!("{:?}", expr),
            )),
        }
    }
}

/// Resolved getter methods (returns computed NormType, evaluated addresses, etc.)
impl<'a> Global<'a> {
    pub fn get_type_resolved(&self, name: &str) -> Option<(NormType, usize)> {
        let def = self.defs.get(name).copied()?;
        if let ast::Def::Type(_, ty) = def {
            let ty = self.normtype(ty).ok()?;
            let size = ty.sizeof();
            Some((ty, size))
        } else {
            None
        }
    }

    pub fn get_const_resolved(&self, name: &str) -> Option<(NormType, ConstExpr, Option<usize>)> {
        let def = self.defs.get(name).copied()?;
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
        let def = self.defs.get(name).copied()?;
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
        let def = self.defs.get(name).copied()?;
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

    /// Walk a function body and produce a map of (local var name → SP-relative
    /// offset). 新 ABI (SP 下向き): args は SP+0..SP-(args_total-1)、locals はその直下の負方向。
    pub fn get_func_locals(&'a self, name: &str) -> Option<IndexMap<String, i32>> {
        let def = self.defs.get(name).copied()?;
        let (params, _ret_ty, body) = match def {
            ast::Def::Func(_, params, ret_ty, body) => (params, ret_ty, body),
            _ => return None,
        };
        let mut local = super::local::Local::fork(self);
        // 引数の総サイズを計算 (locals の開始位置を決めるため)
        let mut args_total: i32 = 0;
        for (_, ty) in params {
            args_total += self.normtype(ty).ok()?.sizeof() as i32;
        }
        local.insert_args(params).ok()?;
        // Walk statements to collect every Var declaration in source order.
        fn walk<'a>(
            global: &'a super::global::Global<'a>,
            local: &mut super::local::Local<'a>,
            next: &mut i32,
            stmt: &'a ast::Stmt,
        ) -> Result<(), crate::error::Error> {
            match stmt {
                ast::Stmt::Block(_, stmts) => {
                    for s in stmts {
                        walk(global, local, next, s)?;
                    }
                }
                ast::Stmt::Cond(_, t, f) => {
                    walk(global, local, next, t)?;
                    if let Some(e) = f {
                        walk(global, local, next, e)?;
                    }
                }
                ast::Stmt::Loop(_, body) => walk(global, local, next, body)?,
                ast::Stmt::Var(ident, ty, _) => {
                    let nty = global.normtype(ty)?;
                    let size = nty.sizeof() as i32;
                    let head = *next - size + 1;
                    *next -= size;
                    let _ = local.insert(ident, nty, head);
                }
                _ => {}
            }
            Ok(())
        }
        // 新 ABI: locals は args 直下 (= SP - args_total) から負方向に伸びる。
        let mut next: i32 = -args_total;
        for s in body {
            walk(self, &mut local, &mut next, s).ok()?;
        }
        Some(local.entries())
    }

    pub fn get_func_resolved(&self, name: &str) -> Option<NormType> {
        let def = self.defs.get(name).copied()?;
        if let ast::Def::Func(_, params, ret_ty, _) = def {
            let mut norm_params = Vec::new();
            for ((param_name, _), param_ty) in params {
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

impl<'a> Global<'a> {
    pub fn instobjs(&self) -> Result<(Vec<(&str, usize, usize)>, Vec<(&str, usize, &str)>), Error> {
        let mut fixed = vec![];
        let mut auto = vec![];

        for (&name, &def) in &self.defs {
            match def {
                ast::Def::Asm(_, addr, body) => {
                    let size = body.len();
                    match addr {
                        Some(addr_expr) => {
                            let addr = match self.constexpr(addr_expr)? {
                                ConstExpr::Number(n) => n,
                                _ => {
                                    return Err(Error::InvalidImmediate(
                                        addr_expr.pos_or_default(),
                                        "address must be numeric".to_string(),
                                    ))
                                }
                            };
                            fixed.push((name, size, addr));
                        }
                        None => auto.push((name, size, "asm")),
                    }
                }
                ast::Def::Func(_, _, _, body) => {
                    let size = body.len(); // placeholder
                    auto.push((name, size, "func"));
                }
                _ => {}
            }
        }

        Ok((fixed, auto))
    }

    pub fn dataobjs(&self) -> Result<(Vec<(&str, usize, usize)>, Vec<(&str, usize, &str)>), Error> {
        let mut fixed = vec![];
        let mut auto = vec![];

        for (name, def) in &self.defs {
            match def {
                ast::Def::Const(_, addr, value) => {
                    let ty = self.typeinfer(value)?;
                    let size = ty.sizeof();
                    match addr {
                        Some(addr_expr) => {
                            let addr = match self.constexpr(addr_expr)? {
                                ConstExpr::Number(n) => n,
                                _ => {
                                    return Err(Error::InvalidImmediate(
                                        addr_expr.pos_or_default(),
                                        "address must be numeric".to_string(),
                                    ))
                                }
                            };
                            fixed.push((*name, size, addr));
                        }
                        None => auto.push((*name, size, "const")),
                    }
                }
                ast::Def::Static(_, addr, ty) => {
                    let size = self.normtype(ty)?.sizeof();
                    match addr {
                        Some(addr_expr) => {
                            let addr = match self.constexpr(addr_expr)? {
                                ConstExpr::Number(n) => n,
                                _ => {
                                    return Err(Error::InvalidImmediate(
                                        addr_expr.pos_or_default(),
                                        "address must be numeric".to_string(),
                                    ))
                                }
                            };
                            fixed.push((*name, size, addr));
                        }
                        None => auto.push((*name, size, "static")),
                    }
                }
                _ => {}
            }
        }

        Ok((fixed, auto))
    }
}

impl<'a> Global<'a> {
    /// Generate code for an asm or func definition with caching
    pub fn code(&'a self, name: &str) -> Result<Code, Error> {
        // Check cache first
        {
            let cache = self._code.read().unwrap();
            if let Some(cached) = cache.get(name) {
                return Ok(cached.clone());
            }
        }

        // Generate code based on definition type
        let result = match self.get(name) {
            Some(ast::Def::Asm(..)) => self.asm2code(name),
            Some(ast::Def::Func(..)) => self.func2code(name),
            Some(
                ast::Def::Type((_, pos), _)
                | ast::Def::Const((_, pos), _, _)
                | ast::Def::Static((_, pos), _, _),
            ) => Err(Error::NotCodeGeneratable(pos.clone(), name.to_string())),
            None => Err(Error::UnknownIdentifier(Pos::default(), name.to_string())),
        };

        // Cache the result if successful
        if let Ok(ref code) = result {
            if let Some(&key) = self.defs.keys().find(|&&k| k == name) {
                let mut cache = self._code.write().unwrap();
                cache.insert(key, code.clone());
            }
        }

        result
    }
}
