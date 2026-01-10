//! Salsa database for incremental compilation

use crate::error::Error;
use crate::grammer::ast;

use super::constexpr::ConstExpr;
use super::normtype::NormType;

/// Salsa database trait
#[salsa::db]
pub trait Db: salsa::Database {
    fn ast(&self) -> &ast::AST;
    fn defs(&self) -> &indexmap::IndexMap<String, usize>;
}

/// Interned Type reference
#[salsa::interned]
pub struct TypeId<'db> {
    pub data: ast::Type,
}

/// Interned Expr reference
#[salsa::interned]
pub struct ExprId<'db> {
    pub data: ast::Expr,
}

/// Interned definition name
#[salsa::interned]
pub struct DefName<'db> {
    pub name: String,
}

/// Normalize a type by resolving custom types and computing array sizes
#[salsa::tracked]
pub fn normtype<'db>(db: &'db dyn Db, ty: TypeId<'db>) -> Result<NormType, Error> {
    let ty_data = ty.data(db);
    match &ty_data {
        ast::Type::Int => Ok(NormType::Int),
        ast::Type::Void => Ok(NormType::Void),
        ast::Type::Custom(name) => {
            let defs = db.defs();
            if let Some(&idx) = defs.get(name) {
                let ast = db.ast();
                match &ast.0[idx] {
                    ast::Def::Type(_, type_def) => {
                        let inner_id = TypeId::new(db, type_def.clone());
                        normtype(db, inner_id)
                    }
                    _ => Err(Error::NotAType(name.to_string())),
                }
            } else {
                Err(Error::UnknownType(name.to_string()))
            }
        }
        ast::Type::Addr(inner) => {
            let inner_id = TypeId::new(db, (**inner).clone());
            let inner_type = normtype(db, inner_id)?;
            Ok(NormType::Addr(Box::new(inner_type)))
        }
        ast::Type::Array(len, elem_ty) => {
            let len_id = ExprId::new(db, len.clone());
            let len = match constexpr(db, len_id) {
                Ok(ConstExpr::Number(n)) => n,
                _ => return Err(Error::NonConstantArrayLength),
            };
            let elem_id = TypeId::new(db, (**elem_ty).clone());
            let elem = normtype(db, elem_id)?;
            Ok(NormType::Array(len, Box::new(elem)))
        }
        ast::Type::Struct(fields) => {
            let mut norm_fields = Vec::new();
            for (name, ty) in fields {
                let ty_id = TypeId::new(db, ty.clone());
                let norm = normtype(db, ty_id)?;
                norm_fields.push((name.clone(), norm));
            }
            Ok(NormType::Struct(norm_fields))
        }
        ast::Type::Func(params, ret_ty) => {
            let mut norm_params = Vec::new();
            for (name, param_ty) in params {
                let param_id = TypeId::new(db, param_ty.clone());
                let norm_ty = normtype(db, param_id)?;
                norm_params.push((name.clone(), norm_ty));
            }
            let ret_id = TypeId::new(db, (**ret_ty).clone());
            let ret = normtype(db, ret_id)?;
            Ok(NormType::Func(norm_params, Box::new(ret)))
        }
    }
}

/// Evaluate a constant expression
#[salsa::tracked]
pub fn constexpr<'db>(db: &'db dyn Db, expr: ExprId<'db>) -> Result<ConstExpr, Error> {
    let expr_data = expr.data(db);
    match &expr_data {
        ast::Expr::NumberLit(n) => Ok(ConstExpr::Number(*n)),
        ast::Expr::CharLit(c) => Ok(ConstExpr::Char(*c)),
        ast::Expr::StringLit(s) => Ok(ConstExpr::String(s.clone())),
        ast::Expr::ArrayLit(elems) => {
            let mut const_elems = Vec::new();
            for elem in elems {
                let elem_id = ExprId::new(db, elem.clone());
                const_elems.push(constexpr(db, elem_id)?);
            }
            Ok(ConstExpr::Array(const_elems))
        }
        ast::Expr::StructLit(fields) => {
            let mut const_fields = Vec::new();
            for (name, field_expr) in fields {
                let field_id = ExprId::new(db, field_expr.clone());
                let const_val = constexpr(db, field_id)?;
                const_fields.push((name.clone(), const_val));
            }
            Ok(ConstExpr::Struct(const_fields))
        }
        ast::Expr::Ident(name) => {
            let defs = db.defs();
            if let Some(&idx) = defs.get(name) {
                let ast = db.ast();
                if let ast::Def::Const(_, _, const_expr) = &ast.0[idx] {
                    let const_id = ExprId::new(db, const_expr.clone());
                    constexpr(db, const_id)
                } else {
                    Err(Error::NotAConstant(name.to_string()))
                }
            } else {
                Err(Error::UnknownConstant(name.to_string()))
            }
        }
        ast::Expr::Binary(op, left, right) => {
            let left_id = ExprId::new(db, (**left).clone());
            let right_id = ExprId::new(db, (**right).clone());
            let left_val = constexpr(db, left_id)?;
            let right_val = constexpr(db, right_id)?;

            match (&left_val, &right_val) {
                (ConstExpr::Number(l), ConstExpr::Number(r)) => match op {
                    ast::BinaryOp::Add => Ok(ConstExpr::Number(l + r)),
                    ast::BinaryOp::Sub => Ok(ConstExpr::Number(l.saturating_sub(*r))),
                    ast::BinaryOp::Mul => Ok(ConstExpr::Number(l * r)),
                    ast::BinaryOp::Div => {
                        if *r == 0 {
                            Err(Error::DivisionByZero)
                        } else {
                            Ok(ConstExpr::Number(l / r))
                        }
                    }
                    ast::BinaryOp::Mod => {
                        if *r == 0 {
                            Err(Error::ModuloByZero)
                        } else {
                            Ok(ConstExpr::Number(l % r))
                        }
                    }
                    ast::BinaryOp::And => Ok(ConstExpr::Number(l & r)),
                    ast::BinaryOp::Or => Ok(ConstExpr::Number(l | r)),
                    ast::BinaryOp::Xor => Ok(ConstExpr::Number(l ^ r)),
                    ast::BinaryOp::Shl => Ok(ConstExpr::Number(l << r)),
                    ast::BinaryOp::Shr => Ok(ConstExpr::Number(l >> r)),
                    ast::BinaryOp::Eq => Ok(ConstExpr::Number(if l == r { 1 } else { 0 })),
                    ast::BinaryOp::Ne => Ok(ConstExpr::Number(if l != r { 1 } else { 0 })),
                    ast::BinaryOp::Lt => Ok(ConstExpr::Number(if l < r { 1 } else { 0 })),
                    ast::BinaryOp::Le => Ok(ConstExpr::Number(if l <= r { 1 } else { 0 })),
                    ast::BinaryOp::Gt => Ok(ConstExpr::Number(if l > r { 1 } else { 0 })),
                    ast::BinaryOp::Ge => Ok(ConstExpr::Number(if l >= r { 1 } else { 0 })),
                },
                _ => Err(Error::NonNumericBinaryOperands),
            }
        }
        ast::Expr::Unary(op, inner) => {
            let inner_id = ExprId::new(db, (**inner).clone());
            let inner_val = constexpr(db, inner_id)?;
            match (&inner_val, op) {
                (ConstExpr::Number(n), ast::UnaryOp::Pos) => Ok(ConstExpr::Number(*n)),
                (ConstExpr::Number(n), ast::UnaryOp::Neg) => {
                    Ok(ConstExpr::Number((-((*n) as isize)) as usize))
                }
                (ConstExpr::Number(n), ast::UnaryOp::Not) => Ok(ConstExpr::Number(!n)),
                _ => Err(Error::NonNumericUnaryOperand),
            }
        }
        ast::Expr::SizeofType(ty) => {
            let ty_id = TypeId::new(db, (**ty).clone());
            let norm_ty = normtype(db, ty_id)?;
            Ok(ConstExpr::Number(norm_ty.sizeof()))
        }
        ast::Expr::SizeofExpr(inner) => {
            let inner_id = ExprId::new(db, (**inner).clone());
            let norm_ty = typeinfer(db, inner_id)?;
            Ok(ConstExpr::Number(norm_ty.sizeof()))
        }
        ast::Expr::Cast(inner, _target_ty) => {
            let inner_id = ExprId::new(db, (**inner).clone());
            constexpr(db, inner_id)
        }
        _ => Err(Error::NonConstantExpression),
    }
}

/// Infer the type of an expression
#[salsa::tracked]
pub fn typeinfer<'db>(db: &'db dyn Db, expr: ExprId<'db>) -> Result<NormType, Error> {
    let expr_data = expr.data(db);
    match &expr_data {
        ast::Expr::NumberLit(_) => Ok(NormType::Int),
        ast::Expr::CharLit(_) => Ok(NormType::Int),
        ast::Expr::StringLit(s) => Ok(NormType::Array(s.len() + 1, Box::new(NormType::Int))),
        ast::Expr::ArrayLit(elems) => {
            if elems.is_empty() {
                return Err(Error::EmptyArrayTypeInference);
            }
            let elem_id = ExprId::new(db, elems[0].clone());
            let elem_ty = typeinfer(db, elem_id)?;
            Ok(NormType::Array(elems.len(), Box::new(elem_ty)))
        }
        ast::Expr::StructLit(fields) => {
            let mut field_types = Vec::new();
            for (name, field_expr) in fields {
                let field_id = ExprId::new(db, field_expr.clone());
                let field_ty = typeinfer(db, field_id)?;
                field_types.push((name.clone(), field_ty));
            }
            Ok(NormType::Struct(field_types))
        }
        ast::Expr::Ident(name) => {
            let defs = db.defs();
            if let Some(&idx) = defs.get(name) {
                let ast = db.ast();
                match &ast.0[idx] {
                    ast::Def::Static(_, _, ty) => {
                        let ty_id = TypeId::new(db, ty.clone());
                        normtype(db, ty_id)
                    }
                    ast::Def::Const(_, _, expr) => {
                        let expr_id = ExprId::new(db, expr.clone());
                        let const_val = constexpr(db, expr_id)?;
                        const_val
                            .typeinfer()
                            .map_err(|_| Error::NotAValue(name.to_string()))
                    }
                    ast::Def::Func(_, params, ret_ty, _) => {
                        let mut norm_params = Vec::new();
                        for (param_name, param_ty) in params {
                            let param_id = TypeId::new(db, param_ty.clone());
                            let norm_ty = normtype(db, param_id)?;
                            norm_params.push((param_name.clone(), norm_ty));
                        }
                        let ret_id = TypeId::new(db, ret_ty.clone());
                        let norm_ret = normtype(db, ret_id)?;
                        Ok(NormType::Func(norm_params, Box::new(norm_ret)))
                    }
                    _ => Err(Error::NotAValue(name.to_string())),
                }
            } else {
                Err(Error::UnknownIdentifier(name.to_string()))
            }
        }
        ast::Expr::Binary(op, left, right) => {
            let left_id = ExprId::new(db, (**left).clone());
            let left_ty = typeinfer(db, left_id)?;
            let _right_id = ExprId::new(db, (**right).clone());
            let _right_ty = typeinfer(db, _right_id)?;

            match op {
                ast::BinaryOp::Eq
                | ast::BinaryOp::Ne
                | ast::BinaryOp::Lt
                | ast::BinaryOp::Le
                | ast::BinaryOp::Gt
                | ast::BinaryOp::Ge => Ok(NormType::Int),
                _ => Ok(left_ty),
            }
        }
        ast::Expr::Unary(op, inner) => {
            let inner_id = ExprId::new(db, (**inner).clone());
            let ty = typeinfer(db, inner_id)?;
            match op {
                ast::UnaryOp::Pos | ast::UnaryOp::Neg | ast::UnaryOp::Not => Ok(ty),
            }
        }
        ast::Expr::Call(func_expr, _args) => {
            let func_id = ExprId::new(db, (**func_expr).clone());
            let func_ty = typeinfer(db, func_id)?;
            match func_ty {
                NormType::Func(_, ret_ty) => Ok(*ret_ty),
                _ => Err(Error::NotCallable),
            }
        }
        ast::Expr::Index(arr_expr, _idx) => {
            let arr_id = ExprId::new(db, (**arr_expr).clone());
            let arr_ty = typeinfer(db, arr_id)?;
            match arr_ty {
                NormType::Array(_, elem_ty) => Ok(*elem_ty),
                NormType::Addr(inner) => Ok(*inner),
                _ => Err(Error::NotIndexable),
            }
        }
        ast::Expr::Member(inner, field) => {
            let inner_id = ExprId::new(db, (**inner).clone());
            match typeinfer(db, inner_id)? {
                NormType::Struct(fields) => match fields.iter().find(|(name, _)| name == field) {
                    Some((_, ty)) => Ok(ty.clone()),
                    None => Err(Error::NoSuchField(field.to_string())),
                },
                _ => Err(Error::NotAStruct),
            }
        }
        ast::Expr::Addr(inner) => {
            let inner_id = ExprId::new(db, (**inner).clone());
            let ty = typeinfer(db, inner_id)?;
            Ok(NormType::Addr(Box::new(ty)))
        }
        ast::Expr::Deref(inner) => {
            let inner_id = ExprId::new(db, (**inner).clone());
            let ty = typeinfer(db, inner_id)?;
            match ty {
                NormType::Addr(inner) => Ok(*inner),
                _ => Err(Error::CannotDereferenceNonPointer),
            }
        }
        ast::Expr::Cast(inner, ty) => {
            let inner_id = ExprId::new(db, (**inner).clone());
            let base = typeinfer(db, inner_id)?;
            let ty_id = TypeId::new(db, (**ty).clone());
            let cast = normtype(db, ty_id)?;
            if base.sizeof() == cast.sizeof() {
                Ok(cast)
            } else {
                Err(Error::InvalidCastSize(base.sizeof(), cast.sizeof()))
            }
        }
        ast::Expr::SizeofType(_) | ast::Expr::SizeofExpr(_) => Ok(NormType::Int),
        ast::Expr::Cond(_, then_expr, _else_expr) => {
            let then_id = ExprId::new(db, (**then_expr).clone());
            typeinfer(db, then_id)
        }
    }
}

/// Database implementation
#[salsa::db]
#[derive(Default, Clone)]
pub struct Database {
    storage: salsa::Storage<Self>,
    ast: Option<ast::AST>,
    defs: indexmap::IndexMap<String, usize>,
}

#[salsa::db]
impl salsa::Database for Database {}

#[salsa::db]
impl Db for Database {
    fn ast(&self) -> &ast::AST {
        self.ast.as_ref().expect("AST not initialized")
    }

    fn defs(&self) -> &indexmap::IndexMap<String, usize> {
        &self.defs
    }
}

impl Database {
    pub fn new(ast: ast::AST) -> Result<Self, Error> {
        let mut defs = indexmap::IndexMap::new();
        for (idx, def) in ast.0.iter().enumerate() {
            let name = match def {
                ast::Def::Type(name, _) => name.clone(),
                ast::Def::Const(name, _, _) => name.clone(),
                ast::Def::Static(name, _, _) => name.clone(),
                ast::Def::Asm(name, _, _) => name.clone(),
                ast::Def::Func(name, _, _, _) => name.clone(),
            };

            if defs.contains_key(&name) {
                return Err(Error::Duplicate(name));
            }

            defs.insert(name, idx);
        }

        Ok(Database {
            storage: salsa::Storage::default(),
            ast: Some(ast),
            defs,
        })
    }
}
