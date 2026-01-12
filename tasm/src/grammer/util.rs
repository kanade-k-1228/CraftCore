use super::ast::{Def, Expr, Type};
use super::token::Pos;

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
            Expr::Cond(cond, _, _) => cond.pos(),
            Expr::SizeofExpr(inner) => inner.pos(),
            Expr::SizeofType(ty) => ty.pos(),
            Expr::ArrayLit(elems) => elems.first().and_then(|e| e.pos()),
            Expr::StructLit(fields) => fields.first().map(|((_, pos), _)| pos),
            _ => None,
        }
    }

    pub fn pos_or_default(&self) -> Pos {
        self.pos().cloned().unwrap_or_default()
    }
}

impl Def {
    pub fn pos(&self) -> Option<&Pos> {
        match self {
            Def::Type((_, pos), _) => Some(pos),
            Def::Const((_, pos), _, _) => Some(pos),
            Def::Static((_, pos), _, _) => Some(pos),
            Def::Asm((_, pos), _, _) => Some(pos),
            Def::Func((_, pos), _, _, _) => Some(pos),
        }
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
