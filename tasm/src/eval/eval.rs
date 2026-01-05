use indexmap::IndexMap;
use std::cell::RefCell;
use std::collections::HashMap;

use crate::{
    error::EvalError,
    eval::{constexpr::ConstExpr, normtype::NormType},
    grammer::ast::{self, BinaryOp, UnaryOp},
};

pub struct Evaluator<'a> {
    defs: IndexMap<&'a str, &'a ast::Def>,
    normtype_cache: RefCell<HashMap<&'a ast::Type, NormType>>,
    constexpr_cache: RefCell<HashMap<&'a ast::Expr, ConstExpr>>,
    typeinfer_cache: RefCell<HashMap<&'a ast::Expr, NormType>>,
}

impl<'a> Evaluator<'a> {
    /// Create a new evaluator by building an index of AST definitions
    pub fn new(ast: &'a ast::AST) -> Result<Self, EvalError> {
        let mut defs = IndexMap::new();

        // Build index of all definitions
        for def in &ast.0 {
            let name = match def {
                ast::Def::Type(name, _) => name.as_str(),
                ast::Def::Const(name, _, _) => name.as_str(),
                ast::Def::Static(name, _, _) => name.as_str(),
                ast::Def::Asm(name, _, _) => name.as_str(),
                ast::Def::Func(name, _, _, _) => name.as_str(),
            };

            // Check for duplicates
            if defs.contains_key(name) {
                return Err(EvalError::Duplicate(name.to_string()));
            }

            defs.insert(name, def);
        }

        Ok(Evaluator {
            defs,
            normtype_cache: RefCell::new(HashMap::new()),
            constexpr_cache: RefCell::new(HashMap::new()),
            typeinfer_cache: RefCell::new(HashMap::new()),
        })
    }

    /// Accessor methods for compatibility
    pub fn types(&self) -> IndexMap<&'a str, (NormType, usize, &'a ast::Def)> {
        let mut types = IndexMap::new();
        for (&name, &def) in &self.defs {
            if let ast::Def::Type(_, ty) = def {
                if let Ok(ty) = self.normtype(ty) {
                    let size = ty.sizeof();
                    types.insert(name, (ty, size, def));
                }
            }
        }
        types
    }

    pub fn consts(&self) -> IndexMap<&'a str, (NormType, ConstExpr, Option<usize>, &'a ast::Def)> {
        let mut consts = IndexMap::new();
        for (&name, &def) in &self.defs {
            if let ast::Def::Const(_, addr, expr) = def {
                if let Ok(value) = self.constexpr(expr) {
                    if let Ok(ty) = value.typeinfer() {
                        let addr = addr.as_ref().and_then(|e| match self.constexpr(e) {
                            Ok(ConstExpr::Number(n)) => Some(n),
                            _ => None,
                        });
                        consts.insert(name, (ty, value, addr, def));
                    }
                }
            }
        }
        consts
    }

    pub fn statics(&self) -> IndexMap<&'a str, (NormType, Option<usize>, &'a ast::Def)> {
        let mut statics = IndexMap::new();
        for (&name, &def) in &self.defs {
            if let ast::Def::Static(_, addr, ty) = def {
                if let Ok(ty) = self.normtype(ty) {
                    let addr = addr.as_ref().and_then(|e| match self.constexpr(e) {
                        Ok(ConstExpr::Number(n)) => Some(n),
                        _ => None,
                    });
                    statics.insert(name, (ty, addr, def));
                }
            }
        }
        statics
    }

    pub fn asms(&self) -> IndexMap<&'a str, (Option<usize>, &'a ast::Def)> {
        let mut asms = IndexMap::new();
        for (&name, &def) in &self.defs {
            if let ast::Def::Asm(_, addr, _) = def {
                let addr = addr.as_ref().and_then(|e| match self.constexpr(e) {
                    Ok(ConstExpr::Number(n)) => Some(n),
                    _ => None,
                });
                asms.insert(name, (addr, def));
            }
        }
        asms
    }

    pub fn funcs(&self) -> IndexMap<&'a str, (NormType, Option<usize>, &'a ast::Def)> {
        let mut funcs = IndexMap::new();
        for (&name, &def) in &self.defs {
            if let ast::Def::Func(_, params, ret_ty, _) = def {
                let mut norm_params = Vec::new();
                let mut success = true;
                for (param_name, param_ty) in params {
                    if let Ok(norm_ty) = self.normtype(param_ty) {
                        norm_params.push((param_name.clone(), norm_ty));
                    } else {
                        success = false;
                        break;
                    }
                }

                if success {
                    if let Ok(norm_ret) = self.normtype(ret_ty) {
                        let norm_type = NormType::Func(norm_params, Box::new(norm_ret));
                        funcs.insert(name, (norm_type, None, def));
                    }
                }
            }
        }
        funcs
    }

    /// Normalize a type by resolving custom types and computing array sizes
    pub fn normtype(&self, ty: &'a ast::Type) -> Result<NormType, EvalError> {
        // Restore from cache
        if let Some(cached) = self.normtype_cache.borrow().get(ty) {
            return Ok(cached.clone());
        }

        // Evaluate
        let result = match ty {
            ast::Type::Int => Ok(NormType::Int),
            ast::Type::Void => Ok(NormType::Void),
            ast::Type::Custom(name) => {
                if let Some(&def) = self.defs.get(name.as_str()) {
                    match def {
                        ast::Def::Type(_, type_def) => self.normtype(type_def),
                        _ => Err(EvalError::NotAType(name.to_string())),
                    }
                } else {
                    Err(EvalError::UnknownType(name.to_string()))
                }
            }
            ast::Type::Addr(inner) => {
                let inner_type = self.normtype(inner)?;
                Ok(NormType::Addr(Box::new(inner_type)))
            }
            ast::Type::Array(len, ty) => {
                let len = match self.constexpr(len) {
                    Ok(ConstExpr::Number(n)) => n,
                    _ => return Err(EvalError::NonConstantArrayLength),
                };
                let ty = self.normtype(ty)?;
                Ok(NormType::Array(len, Box::new(ty)))
            }
            ast::Type::Struct(fields) => {
                let mut norm_fields = Vec::new();
                for (name, ty) in fields {
                    let norm = self.normtype(ty)?;
                    norm_fields.push((name.clone(), norm));
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
        };

        // Store
        if let Ok(ref norm_type) = result {
            self.normtype_cache
                .borrow_mut()
                .insert(ty, norm_type.clone());
        }

        result
    }

    /// Evaluate a constant expression
    pub fn constexpr(&self, expr: &'a ast::Expr) -> Result<ConstExpr, EvalError> {
        // Restore from cache
        if let Some(cached) = self.constexpr_cache.borrow().get(expr) {
            return Ok(cached.clone());
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
                for (name, field_expr) in fields {
                    let const_val = self.constexpr(field_expr)?;
                    const_fields.push((name.clone(), const_val));
                }
                Ok(ConstExpr::Struct(const_fields))
            }
            ast::Expr::Ident(name) => {
                // Look up constant value
                if let Some(&def) = self.defs.get(name.as_str()) {
                    if let ast::Def::Const(_, _, const_expr) = def {
                        // Recursively evaluate the constant
                        self.constexpr(const_expr)
                    } else {
                        Err(EvalError::NotAConstant(name.to_string()))
                    }
                } else {
                    Err(EvalError::UnknownConstant(name.to_string()))
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
                                Err(EvalError::DivisionByZero)
                            } else {
                                Ok(ConstExpr::Number(l / r))
                            }
                        }
                        BinaryOp::Mod => {
                            if *r == 0 {
                                Err(EvalError::ModuloByZero)
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
                    _ => Err(EvalError::NonNumericBinaryOperands),
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
                    _ => Err(EvalError::NonNumericUnaryOperand),
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
            _ => Err(EvalError::NonConstantExpression),
        };

        // Cache the result if successful
        if let Ok(ref const_expr) = result {
            self.constexpr_cache
                .borrow_mut()
                .insert(expr, const_expr.clone());
        }

        result
    }

    /// Infer the type of an expression
    pub fn typeinfer(&self, expr: &'a ast::Expr) -> Result<NormType, EvalError> {
        // Check cache first
        if let Some(cached) = self.typeinfer_cache.borrow().get(expr) {
            return Ok(cached.clone());
        }

        let result = match expr {
            ast::Expr::NumberLit(_) => Ok(NormType::Int),
            ast::Expr::CharLit(_) => Ok(NormType::Int),
            ast::Expr::StringLit(s) => {
                Ok(NormType::Array(s.len() + 1, Box::new(NormType::Int))) // +1 for null terminator
            }
            ast::Expr::ArrayLit(elems) => {
                if elems.is_empty() {
                    return Err(EvalError::EmptyArrayTypeInference);
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
                // Look up the identifier in definitions
                if let Some(&def) = self.defs.get(name.as_str()) {
                    match def {
                        ast::Def::Static(_, _, ty) => self.normtype(ty),
                        ast::Def::Const(_, _, expr) => {
                            // Infer type from constant expression
                            let const_val = self.constexpr(expr)?;
                            const_val
                                .typeinfer()
                                .map_err(|_| EvalError::NotAValue(name.to_string()))
                        }
                        ast::Def::Func(_, params, ret_ty, _) => {
                            // Build function type
                            let mut norm_params = Vec::new();
                            for (param_name, param_ty) in params {
                                let norm_ty = self.normtype(param_ty)?;
                                norm_params.push((param_name.clone(), norm_ty));
                            }
                            let norm_ret = self.normtype(ret_ty)?;
                            Ok(NormType::Func(norm_params, Box::new(norm_ret)))
                        }
                        _ => Err(EvalError::NotAValue(name.to_string())),
                    }
                } else {
                    Err(EvalError::UnknownIdentifier(name.to_string()))
                }
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
                    _ => Err(EvalError::NotCallable),
                }
            }
            ast::Expr::Index(arr_expr, _idx) => {
                let arr_ty = self.typeinfer(arr_expr)?;
                match arr_ty {
                    NormType::Array(_, elem_ty) => Ok(*elem_ty),
                    NormType::Addr(inner) => Ok(*inner),
                    _ => Err(EvalError::NotIndexable),
                }
            }
            ast::Expr::Member(expr, field) => match self.typeinfer(expr)? {
                NormType::Struct(fields) => match fields.iter().find(|(name, _)| name == field) {
                    Some((_, ty)) => return Ok(ty.clone()),
                    None => return Err(EvalError::NoSuchField(field.to_string())),
                },
                _ => Err(EvalError::NotAStruct),
            },
            ast::Expr::Addr(expr) => {
                let ty = self.typeinfer(expr)?;
                Ok(NormType::Addr(Box::new(ty)))
            }
            ast::Expr::Deref(expr) => {
                let ty = self.typeinfer(expr)?;
                match ty {
                    NormType::Addr(inner) => Ok(*inner),
                    _ => Err(EvalError::CannotDereferenceNonPointer),
                }
            }
            ast::Expr::Cast(expr, ty) => {
                let base = self.typeinfer(expr)?;
                let cast = self.normtype(ty)?;
                if base.sizeof() == cast.sizeof() {
                    Ok(cast)
                } else {
                    Err(EvalError::InvalidCastSize(base.sizeof(), cast.sizeof()))
                }
            }
            ast::Expr::SizeofType(_) | ast::Expr::SizeofExpr(_) => Ok(NormType::Int),
            ast::Expr::Cond(_, then_expr, _else_expr) => {
                // Type of conditional is the type of then branch
                // (assuming then and else branches have same type)
                self.typeinfer(then_expr)
            }
        };

        // Cache the result if successful
        if let Ok(ref norm_type) = result {
            self.typeinfer_cache
                .borrow_mut()
                .insert(expr, norm_type.clone());
        }

        result
    }
}
