use indexmap::IndexMap;
use std::collections::HashMap;
use std::sync::RwLock;

use crate::{
    error::{Error, ErrorKind},
    grammer::{
        ast::{self, BinaryOp, UnaryOp},
        token::Pos,
        util::walk_stmt,
    },
};

use super::{code::Code, constexpr::ConstExpr, normtype::NormType};

/// RwLock キャッシュの共通形: read で照会し、miss なら評価して write で格納する。
/// 評価が失敗した場合はキャッシュしない。
fn cached<K: std::hash::Hash + Eq + Copy, V: Clone>(
    cache: &RwLock<HashMap<K, V>>,
    key: K,
    eval: impl FnOnce() -> Result<V, Error>,
) -> Result<V, Error> {
    if let Some(hit) = cache.read().unwrap().get(&key) {
        return Ok(hit.clone());
    }
    let value = eval()?;
    cache.write().unwrap().insert(key, value.clone());
    Ok(value)
}

pub struct Global<'a> {
    defs: IndexMap<&'a str, &'a ast::Def>,
    /// ファイルパス → モジュール prefix ("" or 未登録 = ルート)。素名解決のフォールバックに使う。
    modmap: HashMap<String, String>,
    _normtype: RwLock<HashMap<&'a ast::Type, NormType>>,
    _constexpr: RwLock<HashMap<&'a ast::Expr, ConstExpr>>,
    _typeinfer: RwLock<HashMap<&'a ast::Expr, NormType>>,
    _code: RwLock<HashMap<&'a str, Code>>,
}

impl<'a> Global<'a> {
    pub fn new(ast: &'a ast::AST, modmap: HashMap<String, String>) -> Result<Self, Error> {
        let mut defs = IndexMap::new();
        for def in &ast.0 {
            let (name, pos) = def.ident();
            let name = name.as_str();

            if defs.contains_key(name) {
                return Err(ErrorKind::Duplicate(name.to_string()).at(pos.clone()));
            }

            defs.insert(name, def);
        }

        Ok(Global {
            defs,
            modmap,
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

    /// 参照名 name を、参照元 pos のモジュール文脈で解決する。
    /// 返り値の String は解決後の完全修飾名 (Imm に格納する canonical name)。
    /// 1. `::` を含む → 絶対ルックアップ
    /// 2. 素名 → 参照元モジュール prefix を付けて解決 (同一ファイル内)
    /// 3. 無ければ素名のまま (ルート / グローバル層へフォールバック)
    pub fn resolve(&self, name: &str, pos: &Pos) -> Option<(&'a ast::Def, String)> {
        if name.contains("::") {
            return self.defs.get(name).map(|&d| (d, name.to_string()));
        }
        if let Some(prefix) = self.modmap.get(pos.file()) {
            if !prefix.is_empty() {
                let local = format!("{}::{}", prefix, name);
                if let Some(&d) = self.defs.get(local.as_str()) {
                    return Some((d, local));
                }
            }
        }
        self.defs.get(name).map(|&d| (d, name.to_string()))
    }

    /// エントリポイント名 (reset/irq/main) を FQN に解決する。
    /// ルートにあればそれ、無ければ末尾一致 (`::name`) の最初のものを使う。
    pub fn resolve_entry(&self, name: &str) -> Option<String> {
        if self.defs.contains_key(name) {
            return Some(name.to_string());
        }
        let suffix = format!("::{}", name);
        self.defs
            .keys()
            .copied()
            .find(|k| k.ends_with(suffix.as_str()))
            .map(|k| k.to_string())
    }
}

/// パース直後の AST に対し、各トップレベル定義名へモジュール prefix を付与する。
/// modmap: ファイルパス → モジュール prefix ("" or 未登録 = ルート)。
pub fn apply_module_prefixes(ast: &mut ast::AST, modmap: &HashMap<String, String>) {
    for def in &mut ast.0 {
        let ident = def.ident_mut();
        if let Some(prefix) = modmap.get(ident.1.file()) {
            if !prefix.is_empty() {
                ident.0 = format!("{}::{}", prefix, ident.0);
            }
        }
    }
}

impl<'a> Global<'a> {
    /// Normalize a type by resolving custom types and computing array sizes
    pub fn normtype(&self, ty: &'a ast::Type) -> Result<NormType, Error> {
        cached(&self._normtype, ty, || match ty {
            ast::Type::Int => Ok(NormType::Int),
            ast::Type::Void => Ok(NormType::Void),
            ast::Type::Custom((name, pos)) => {
                if let Some((def, _)) = self.resolve(name, pos) {
                    match def {
                        ast::Def::Type(_, type_def) => self.normtype(type_def),
                        ast::Def::Const((_, pos), _, _)
                        | ast::Def::Static((_, pos), _, _)
                        | ast::Def::Asm((_, pos), _, _)
                        | ast::Def::Func((_, pos), _, _, _) => {
                            Err(ErrorKind::NotAType(name.clone()).at(pos.clone()))
                        }
                    }
                } else {
                    Err(ErrorKind::UnknownType(name.clone()).at(pos.clone()))
                }
            }
            ast::Type::Addr(inner) => {
                let inner_type = self.normtype(inner)?;
                Ok(NormType::Addr(Box::new(inner_type)))
            }
            ast::Type::Array(len, ty) => {
                let len = match self.constexpr(len) {
                    Ok(ConstExpr::Number(n)) => n,
                    _ => return Err(ErrorKind::NonConstantArrayLength.at(len.pos_or_default())),
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
        })
    }

    /// Evaluate a constant expression
    pub fn constexpr(&self, expr: &'a ast::Expr) -> Result<ConstExpr, Error> {
        cached(&self._constexpr, expr, || match expr {
            ast::Expr::NumberLit(n, _) => Ok(ConstExpr::Number(*n)),
            ast::Expr::CharLit(c, _) => Ok(ConstExpr::Char(*c)),
            ast::Expr::StringLit(s, _) => Ok(ConstExpr::String(s.clone())),
            ast::Expr::ArrayLit(elems, _) => {
                let mut const_elems = Vec::new();
                for elem in elems {
                    const_elems.push(self.constexpr(elem)?);
                }
                Ok(ConstExpr::Array(const_elems))
            }
            ast::Expr::StructLit(fields, _) => {
                let mut const_fields = Vec::new();
                for ((name, _), field_expr) in fields {
                    let const_val = self.constexpr(field_expr)?;
                    const_fields.push((name.clone(), const_val));
                }
                Ok(ConstExpr::Struct(const_fields))
            }
            ast::Expr::Ident((name, pos)) => {
                // Look up constant value
                if let Some((def, _)) = self.resolve(name, pos) {
                    match def {
                        ast::Def::Const(_, _, const_expr) => self.constexpr(const_expr),
                        ast::Def::Type((_, pos), _)
                        | ast::Def::Static((_, pos), _, _)
                        | ast::Def::Asm((_, pos), _, _)
                        | ast::Def::Func((_, pos), _, _, _) => {
                            Err(ErrorKind::NotAConstant(name.clone()).at(pos.clone()))
                        }
                    }
                } else {
                    Err(ErrorKind::UnknownConstant(name.clone()).at(pos.clone()))
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
                                Err(ErrorKind::DivisionByZero.at(right.pos_or_default()))
                            } else {
                                Ok(ConstExpr::Number(l / r))
                            }
                        }
                        BinaryOp::Mod => {
                            if *r == 0 {
                                Err(ErrorKind::ModuloByZero.at(right.pos_or_default()))
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
                    _ => Err(ErrorKind::NonNumericBinaryOperands.at(left.pos_or_default())),
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
                    _ => Err(ErrorKind::NonNumericUnaryOperand.at(inner.pos_or_default())),
                }
            }
            ast::Expr::SizeofType(ty, _) => {
                // Calculate size of type
                let norm_ty = self.normtype(ty)?;
                Ok(ConstExpr::Number(norm_ty.sizeof()))
            }
            ast::Expr::SizeofExpr(inner, _) => {
                // Calculate size of expression's type
                let norm_ty = self.typeinfer(inner)?;
                Ok(ConstExpr::Number(norm_ty.sizeof()))
            }
            ast::Expr::Cast(inner, _target_ty) => {
                // For constant evaluation, just evaluate the inner expression
                // Type checking happens elsewhere
                self.constexpr(inner)
            }
            _ => Err(ErrorKind::NonConstantExpression.at(expr.pos_or_default())),
        })
    }

    /// Infer the type of an expression
    pub fn typeinfer(&self, expr: &'a ast::Expr) -> Result<NormType, Error> {
        cached(&self._typeinfer, expr, || match expr {
            ast::Expr::NumberLit(_, _) => Ok(NormType::Int),
            ast::Expr::CharLit(_, _) => Ok(NormType::Int),
            ast::Expr::StringLit(s, _) => {
                // 型サイズ = 文字数 (ConstExpr::typeinfer と同じ)。NUL 終端は bin() が付ける。
                Ok(NormType::Array(s.len(), Box::new(NormType::Int)))
            }
            ast::Expr::ArrayLit(elems, _) => {
                if elems.is_empty() {
                    return Err(ErrorKind::EmptyArrayTypeInference.at(expr.pos_or_default()));
                }
                let elem_ty = self.typeinfer(&elems[0])?;
                Ok(NormType::Array(elems.len(), Box::new(elem_ty)))
            }
            ast::Expr::StructLit(fields, _) => {
                let mut field_types = Vec::new();
                for ((name, _), field_expr) in fields {
                    let field_ty = self.typeinfer(field_expr)?;
                    field_types.push((name.clone(), field_ty));
                }
                Ok(NormType::Struct(field_types))
            }
            ast::Expr::Ident((name, pos)) => {
                // Look up the identifier in definitions
                if let Some((def, _)) = self.resolve(name, pos) {
                    match def {
                        ast::Def::Static(_, _, ty) => self.normtype(ty),
                        ast::Def::Const((_, pos), _, expr) => {
                            // Infer type from constant expression
                            let const_val = self.constexpr(expr)?;
                            const_val
                                .typeinfer()
                                .map_err(|_| ErrorKind::NotAValue(name.clone()).at(pos.clone()))
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
                            Err(ErrorKind::NotAValue(name.clone()).at(pos.clone()))
                        }
                    }
                } else {
                    Err(ErrorKind::UnknownIdentifier(name.clone()).at(pos.clone()))
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
                            return Err(ErrorKind::InvalidCastSize(
                                left_ty.sizeof(),
                                right_ty.sizeof(),
                            )
                            .at(left.pos_or_default()));
                        }
                        Ok(NormType::Int)
                    }

                    // Pointer arithmetic: ptr +/- int → ptr; int + ptr → ptr;
                    // ptr - ptr → int. Otherwise both must be int.
                    BinaryOp::Add => match (&left_ty, &right_ty) {
                        (NormType::Addr(_), r) if is_int(r) => Ok(left_ty),
                        (l, NormType::Addr(_)) if is_int(l) => Ok(right_ty),
                        (l, r) if is_int(l) && is_int(r) => Ok(NormType::Int),
                        _ => Err(ErrorKind::NonNumericBinaryOperands.at(left.pos_or_default())),
                    },
                    BinaryOp::Sub => match (&left_ty, &right_ty) {
                        (NormType::Addr(_), r) if is_int(r) => Ok(left_ty),
                        (l, r) if is_addr(l) && is_addr(r) => Ok(NormType::Int),
                        (l, r) if is_int(l) && is_int(r) => Ok(NormType::Int),
                        _ => Err(ErrorKind::NonNumericBinaryOperands.at(left.pos_or_default())),
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
                            return Err(
                                ErrorKind::NonNumericBinaryOperands.at(left.pos_or_default())
                            );
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
                    _ => Err(ErrorKind::NotCallable.at(func_expr.pos_or_default())),
                }
            }
            ast::Expr::Index(arr_expr, _idx) => {
                let arr_ty = self.typeinfer(arr_expr)?;
                match arr_ty {
                    NormType::Array(_, elem_ty) => Ok(*elem_ty),
                    NormType::Addr(inner) => Ok(*inner),
                    _ => Err(ErrorKind::NotIndexable.at(arr_expr.pos_or_default())),
                }
            }
            ast::Expr::Member(base_expr, (field, field_pos)) => match self.typeinfer(base_expr)? {
                NormType::Struct(fields) => match fields.iter().find(|(name, _)| name == field) {
                    Some((_, ty)) => Ok(ty.clone()),
                    None => Err(ErrorKind::NoSuchField(field.clone()).at(field_pos.clone())),
                },
                _ => Err(ErrorKind::NotAStruct.at(base_expr.pos_or_default())),
            },
            ast::Expr::Addr(inner) => {
                let ty = self.typeinfer(inner)?;
                Ok(NormType::Addr(Box::new(ty)))
            }
            ast::Expr::Deref(inner) => {
                let ty = self.typeinfer(inner)?;
                match ty {
                    NormType::Addr(inner) => Ok(*inner),
                    _ => Err(ErrorKind::CannotDereferenceNonPointer.at(inner.pos_or_default())),
                }
            }
            ast::Expr::Cast(inner, ty) => {
                let base = self.typeinfer(inner)?;
                let cast = self.normtype(ty)?;
                if base.sizeof() == cast.sizeof() {
                    Ok(cast)
                } else {
                    Err(ErrorKind::InvalidCastSize(base.sizeof(), cast.sizeof())
                        .at(inner.pos_or_default()))
                }
            }
            ast::Expr::SizeofType(_, _) | ast::Expr::SizeofExpr(_, _) => Ok(NormType::Int),
        })
    }

    /// Infer address of expr with unresolved symbol (symbol, offset)
    pub fn addrexpr(&self, expr: &'a ast::Expr) -> Result<(String, usize), Error> {
        match expr {
            ast::Expr::Ident((name, pos)) => match self.resolve(name, pos) {
                Some((def, fqn)) => match def {
                    ast::Def::Static(_, _, _)
                    | ast::Def::Const(_, _, _)
                    | ast::Def::Func(_, _, _, _)
                    | ast::Def::Asm(_, _, _) => Ok((fqn, 0)),
                    ast::Def::Type((_, pos), _) => {
                        Err(ErrorKind::NotAddressable(name.clone()).at(pos.clone()))
                    }
                },
                None => Err(ErrorKind::UnknownIdentifier(name.clone()).at(pos.clone())),
            },

            ast::Expr::Index(base, index) => {
                let (symbol, offset) = self.addrexpr(base)?;
                let idx = match self.constexpr(index) {
                    Ok(ConstExpr::Number(idx)) => idx,
                    _ => {
                        return Err(
                            ErrorKind::NonConstantArrayIndexInAddress.at(index.pos_or_default())
                        )
                    }
                };
                let ty = self.typeinfer(base)?;
                let ofs = ty
                    .get_array_offset(idx)
                    .ok_or(ErrorKind::NotIndexable.at(base.pos_or_default()))?;
                Ok((symbol, offset + ofs))
            }

            ast::Expr::Member(base, (field, field_pos)) => {
                let (symbol, offset) = self.addrexpr(base)?;
                let ty = self.typeinfer(base)?;
                let ofs = ty
                    .get_field_offset(field)
                    .ok_or(ErrorKind::NoSuchField(field.clone()).at(field_pos.clone()))?;
                Ok((symbol, offset + ofs))
            }

            ast::Expr::Cast(inner, _) => self.addrexpr(inner),

            _ => Err(ErrorKind::NotAddressable(format!("{:?}", expr)).at(expr.pos_or_default())),
        }
    }
}

/// Resolved getter methods (returns computed NormType, evaluated addresses, etc.)
impl<'a> Global<'a> {
    /// `@ expr` の固定アドレス指定を定数評価する。数値以外は InvalidImmediate。
    fn fixed_addr(&self, addr: Option<&'a ast::Expr>) -> Result<Option<usize>, Error> {
        match addr {
            None => Ok(None),
            Some(e) => match self.constexpr(e)? {
                ConstExpr::Number(n) => Ok(Some(n)),
                _ => Err(
                    ErrorKind::InvalidImmediate("address must be numeric".to_string())
                        .at(e.pos_or_default()),
                ),
            },
        }
    }

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
            let addr = self.fixed_addr(addr.as_ref()).ok().flatten();
            Some((ty, value, addr))
        } else {
            None
        }
    }

    pub fn get_static_resolved(&self, name: &str) -> Option<(NormType, Option<usize>)> {
        let def = self.defs.get(name).copied()?;
        if let ast::Def::Static(_, addr, ty) = def {
            let ty = self.normtype(ty).ok()?;
            let addr = self.fixed_addr(addr.as_ref()).ok().flatten();
            Some((ty, addr))
        } else {
            None
        }
    }

    pub fn get_asm_resolved(&self, name: &str) -> Option<Option<usize>> {
        let def = self.defs.get(name).copied()?;
        if let ast::Def::Asm(_, addr, _) = def {
            Some(self.fixed_addr(addr.as_ref()).ok().flatten())
        } else {
            None
        }
    }

    /// Walk a function body and produce a map of (local var name → SP-relative
    /// offset)。RISC-V 風 ABI: arg0/arg1 は a0/a1 spill (off -1,-2)、arg2+ は呼出側
    /// スタック (off +0,+1)。locals は arg-spill の直下 (負方向)。デバッグ map 用。
    pub fn get_func_locals(&'a self, name: &str) -> Option<IndexMap<String, i32>> {
        let def = self.defs.get(name).copied()?;
        let ast::Def::Func(_, params, _, body) = def else {
            return None;
        };
        let mut local = super::local::Local::fork(self);
        local.insert_args(params).ok()?;
        // locals は a0/a1 spill の直下 (current_bottom = argc_spilled) から下方向へ、
        // Var 宣言のソース順に確保する (func.rs の compile_stmt と同じ規則)。
        let mut next = params.len().min(2) as i32;
        let mut ok = true;
        for s in body {
            walk_stmt(
                s,
                &mut |stmt| {
                    if let ast::Stmt::Var(ident, ty, _) = stmt {
                        match self.normtype(ty) {
                            Ok(nty) => {
                                // current_bottom (= next) を size 進め、field0 (最深) を head とする。
                                next += nty.sizeof() as i32;
                                let _ = local.insert(ident, nty, -next);
                            }
                            Err(_) => ok = false,
                        }
                    }
                },
                &mut |_| {},
            );
        }
        ok.then(|| local.entries())
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

/// 固定アドレス配置オブジェクト (name, size, addr)
pub type FixedObj<'a> = (&'a str, usize, usize);
/// 自動配置オブジェクト (name, size, section)
pub type AutoObj<'a> = (&'a str, usize, &'a str);

impl<'a> Global<'a> {
    pub fn instobjs(&self) -> Result<(Vec<FixedObj<'_>>, Vec<AutoObj<'_>>), Error> {
        let mut fixed = vec![];
        let mut auto = vec![];

        for (&name, &def) in &self.defs {
            match def {
                ast::Def::Asm(_, addr, body) => match self.fixed_addr(addr.as_ref())? {
                    Some(addr) => fixed.push((name, body.len(), addr)),
                    None => auto.push((name, body.len(), "asm")),
                },
                ast::Def::Func(_, _, _, body) => {
                    let size = body.len(); // placeholder
                    auto.push((name, size, "func"));
                }
                _ => {}
            }
        }

        Ok((fixed, auto))
    }

    pub fn dataobjs(&self) -> Result<(Vec<FixedObj<'_>>, Vec<AutoObj<'_>>), Error> {
        let mut fixed = vec![];
        let mut auto = vec![];

        for (&name, &def) in &self.defs {
            let (addr, size, kind) = match def {
                // const の割付サイズはシリアライズ長 (文字列は NUL 終端込みで文字数 +1)
                ast::Def::Const(_, addr, value) => {
                    (addr, self.constexpr(value)?.bin().len(), "const")
                }
                ast::Def::Static(_, addr, ty) => (addr, self.normtype(ty)?.sizeof(), "static"),
                _ => continue,
            };
            match self.fixed_addr(addr.as_ref())? {
                Some(addr) => fixed.push((name, size, addr)),
                None => auto.push((name, size, kind)),
            }
        }

        Ok((fixed, auto))
    }
}

impl<'a> Global<'a> {
    /// Generate code for an asm or func definition with caching
    pub fn code(&'a self, name: &str) -> Result<Code, Error> {
        // キャッシュキーには IndexMap 側の &'a str を使う (name は短命な借用のため)
        let Some((&key, &def)) = self.defs.get_key_value(name) else {
            return Err(ErrorKind::UnknownIdentifier(name.to_string()).at(Pos::default()));
        };
        cached(&self._code, key, || match def {
            ast::Def::Asm(..) => self.asm2code(key),
            ast::Def::Func(..) => self.func2code(key),
            ast::Def::Type((_, pos), _)
            | ast::Def::Const((_, pos), _, _)
            | ast::Def::Static((_, pos), _, _) => {
                Err(ErrorKind::NotCodeGeneratable(name.to_string()).at(pos.clone()))
            }
        })
    }
}
