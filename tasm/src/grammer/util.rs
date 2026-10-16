use super::ast::{Def, Expr, Ident, Stmt, Type};
use super::token::Pos;

impl Def {
    /// 定義名の Ident。全 variant で先頭フィールドに置く規約。
    pub fn ident(&self) -> &Ident {
        match self {
            Def::Type(id, ..)
            | Def::Const(id, ..)
            | Def::Static(id, ..)
            | Def::Asm(id, ..)
            | Def::Func(id, ..) => id,
        }
    }

    pub fn ident_mut(&mut self) -> &mut Ident {
        match self {
            Def::Type(id, ..)
            | Def::Const(id, ..)
            | Def::Static(id, ..)
            | Def::Asm(id, ..)
            | Def::Func(id, ..) => id,
        }
    }
}

/// e 自身を含む全部分式に f を適用する (pre-order)。
pub fn walk_expr<'a>(e: &'a Expr, f: &mut impl FnMut(&'a Expr)) {
    f(e);
    match e {
        Expr::Binary(_, a, b) | Expr::Index(a, b) => {
            walk_expr(a, f);
            walk_expr(b, f);
        }
        Expr::Unary(_, a)
        | Expr::Addr(a)
        | Expr::Deref(a)
        | Expr::Cast(a, _)
        | Expr::Member(a, _)
        | Expr::SizeofExpr(a, _) => walk_expr(a, f),
        Expr::Call(func, args) => {
            walk_expr(func, f);
            args.iter().for_each(|a| walk_expr(a, f));
        }
        Expr::ArrayLit(es, _) => es.iter().for_each(|x| walk_expr(x, f)),
        Expr::StructLit(fs, _) => fs.iter().for_each(|(_, x)| walk_expr(x, f)),
        Expr::Ident(..)
        | Expr::NumberLit(..)
        | Expr::CharLit(..)
        | Expr::StringLit(..)
        | Expr::SizeofType(..) => {}
    }
}

/// s 自身を含む全文に on_stmt、直接含まれる式 (トップレベル) に on_expr を
/// ソース順 (pre-order) で適用する。部分式まで見るときは on_expr で walk_expr を呼ぶ。
pub fn walk_stmt<'a>(
    s: &'a Stmt,
    on_stmt: &mut impl FnMut(&'a Stmt),
    on_expr: &mut impl FnMut(&'a Expr),
) {
    on_stmt(s);
    match s {
        Stmt::Block(_, ss) => ss.iter().for_each(|x| walk_stmt(x, on_stmt, on_expr)),
        Stmt::Expr(e) => on_expr(e),
        Stmt::Assign(l, r) => {
            on_expr(l);
            on_expr(r);
        }
        Stmt::Cond(c, t, f) => {
            on_expr(c);
            walk_stmt(t, on_stmt, on_expr);
            if let Some(f) = f {
                walk_stmt(f, on_stmt, on_expr);
            }
        }
        Stmt::Loop(c, b) => {
            on_expr(c);
            walk_stmt(b, on_stmt, on_expr);
        }
        Stmt::Return(e) => {
            if let Some(e) = e {
                on_expr(e);
            }
        }
        Stmt::Var(_, _, init) => {
            if let Some(e) = init {
                on_expr(e);
            }
        }
        Stmt::Break(..) | Stmt::Continue(..) => {}
    }
}

/// s に含まれる全ての式 (部分式含む) に f を適用する。
pub fn stmt_exprs<'a>(s: &'a Stmt, f: &mut impl FnMut(&'a Expr)) {
    walk_stmt(s, &mut |_| {}, &mut |e| walk_expr(e, f));
}

impl Expr {
    pub fn pos(&self) -> Option<&Pos> {
        match self {
            Expr::Ident((_, pos)) => Some(pos),
            Expr::Member(_, (_, pos)) => Some(pos),
            Expr::Binary(_, lhs, _) => lhs.pos(),
            Expr::Unary(_, inner) => inner.pos(),
            Expr::Call(func, _) => func.pos(),
            Expr::Index(base, _) => base.pos(),
            Expr::Addr(inner) => inner.pos(),
            Expr::Deref(inner) => inner.pos(),
            Expr::Cast(inner, _) => inner.pos(),
            Expr::SizeofExpr(_, pos) => Some(pos),
            Expr::SizeofType(_, pos) => Some(pos),
            Expr::NumberLit(_, pos) => Some(pos),
            Expr::CharLit(_, pos) => Some(pos),
            Expr::StringLit(_, pos) => Some(pos),
            Expr::ArrayLit(_, pos) => Some(pos),
            Expr::StructLit(_, pos) => Some(pos),
        }
    }

    pub fn pos_or_default(&self) -> Pos {
        self.pos().cloned().unwrap_or_default()
    }
}

impl Def {
    pub fn pos(&self) -> Option<&Pos> {
        Some(&self.ident().1)
    }

    pub fn pos_or_default(&self) -> Pos {
        self.pos().cloned().unwrap_or_default()
    }
}

impl Type {
    pub fn pos(&self) -> Option<&Pos> {
        match self {
            Type::Custom((_, pos)) => Some(pos),
            Type::Addr(inner) => inner.pos(),
            Type::Array(expr, _) => expr.pos(),
            Type::Struct(fields) => fields.first().map(|((_, pos), _)| pos),
            Type::Func(args, _) => args.first().map(|((_, pos), _)| pos),
            _ => None,
        }
    }

    pub fn pos_or_default(&self) -> Pos {
        self.pos().cloned().unwrap_or_default()
    }
}
