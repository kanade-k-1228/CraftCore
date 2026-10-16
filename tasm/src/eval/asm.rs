use crate::{
    error::{Error, ErrorKind},
    grammer::{ast, token::Pos},
};

use super::{
    code::{Code, Imm},
    global::Global,
};
use arch::{op::Op, reg::Reg};
use std::collections::HashMap;

impl<'a> Global<'a> {
    pub fn asm2code(&'a self, name: &str) -> Result<Code, Error> {
        match self.get(name) {
            Some(ast::Def::Asm((_, pos), _, stmts)) => gen_asm(self, stmts, pos.clone()),
            Some(
                ast::Def::Type((_, pos), _)
                | ast::Def::Const((_, pos), _, _)
                | ast::Def::Static((_, pos), _, _)
                | ast::Def::Func((_, pos), _, _, _),
            ) => Err(ErrorKind::NotAnAsm(name.to_string()).at(pos.clone())),
            None => Err(ErrorKind::UnknownIdentifier(name.to_string()).at(Pos::default())),
        }
    }
}

fn gen_asm<'a>(global: &'a Global<'a>, stmts: &'a [ast::Asm], _loc: Pos) -> Result<Code, Error> {
    // 1. Collect local labels
    let mut local: HashMap<&str, usize> = HashMap::new();
    for (idx, ast::Asm(_, _, labels, _)) in stmts.iter().enumerate() {
        for (label, _) in labels {
            local.insert(label.as_str(), idx);
        }
    }

    // 2. Generate code
    let mut insts = Vec::new();
    for (idx, stmt) in stmts.iter().enumerate() {
        let inst = parse_stmt(global, &local, idx, stmt)?;
        insts.push(inst);
    }

    Ok(Code(insts))
}

fn parse_stmt<'a>(
    global: &'a Global<'a>,
    local: &HashMap<&str, usize>,
    idx: usize,
    stmt: &'a ast::Asm,
) -> Result<Op<Reg, Imm>, Error> {
    let ast::Asm((inst, _), args, _, pos) = stmt;
    let loc = pos.clone();
    // オペランド取り出しの短縮形。引数個数は match 側の args.len() で保証される。
    let r = |i: usize| args[i].reg(&loc);
    let im = |i: usize| args[i].imm(global, &loc);
    let gl = |i: usize| args[i].global(global, &loc);
    let lo = |i: usize| args[i].local(local, idx, &loc);
    match (inst.to_lowercase().as_str(), args.len()) {
        ("nop", 0) => Ok(Op::nop()),
        ("mov", 2) => Ok(Op::mov(r(0)?, r(1)?)),
        ("not", 2) => Ok(Op::not(r(0)?, r(1)?)),

        ("add", 3) => Ok(Op::add(r(0)?, r(1)?, r(2)?)),
        ("sub", 3) => Ok(Op::sub(r(0)?, r(1)?, r(2)?)),
        ("and", 3) => Ok(Op::and(r(0)?, r(1)?, r(2)?)),
        ("or", 3) => Ok(Op::or(r(0)?, r(1)?, r(2)?)),
        ("xor", 3) => Ok(Op::xor(r(0)?, r(1)?, r(2)?)),
        ("eq", 3) => Ok(Op::eq(r(0)?, r(1)?, r(2)?)),
        ("neq", 3) => Ok(Op::neq(r(0)?, r(1)?, r(2)?)),
        ("lt", 3) => Ok(Op::lt(r(0)?, r(1)?, r(2)?)),
        ("lts", 3) => Ok(Op::lts(r(0)?, r(1)?, r(2)?)),

        ("addi", 3) => Ok(Op::addi(r(0)?, r(1)?, im(2)?)),
        ("subi", 3) => Ok(Op::subi(r(0)?, r(1)?, im(2)?)),
        ("andi", 3) => Ok(Op::andi(r(0)?, r(1)?, im(2)?)),
        ("ori", 3) => Ok(Op::ori(r(0)?, r(1)?, im(2)?)),
        ("xori", 3) => Ok(Op::xori(r(0)?, r(1)?, im(2)?)),
        ("eqi", 3) => Ok(Op::eqi(r(0)?, r(1)?, im(2)?)),
        ("neqi", 3) => Ok(Op::neqi(r(0)?, r(1)?, im(2)?)),
        ("lti", 3) => Ok(Op::lti(r(0)?, r(1)?, im(2)?)),
        ("ltsi", 3) => Ok(Op::ltsi(r(0)?, r(1)?, im(2)?)),

        ("sr", 2) => Ok(Op::sr(r(0)?, r(1)?)),
        ("srs", 2) => Ok(Op::srs(r(0)?, r(1)?)),
        ("srr", 2) => Ok(Op::srr(r(0)?, r(1)?)),
        ("sl", 2) => Ok(Op::sl(r(0)?, r(1)?)),
        ("slr", 2) => Ok(Op::slr(r(0)?, r(1)?)),

        // "load rd imm" は loadi の別名
        ("loadi", 2) | ("load", 2) => Ok(Op::loadi(r(0)?, im(1)?)),
        ("load", 3) => Ok(Op::load(r(0)?, r(1)?, im(2)?)),
        ("store", 2) => Ok(Op::store(r(1)?, Reg::Z, im(0)?)),
        ("store", 3) => Ok(Op::store(r(0)?, r(1)?, im(2)?)),

        ("jump", 1) => Ok(Op::jump(gl(0)?)),
        ("jumpr", 1) => Ok(Op::jumpr(lo(0)?)),
        ("jumpif", 2) => Ok(Op::jumpif(r(0)?, gl(1)?)),
        ("jumpifr", 2) => Ok(Op::jumpifr(r(0)?, lo(1)?)),
        ("call", 1) => Ok(Op::call(gl(0)?)),
        ("ret", 0) => Ok(Op::ret()),
        ("iret", 0) => Ok(Op::iret()),
        _ => Err(ErrorKind::InvalidInstruction(inst.clone()).at(loc)),
    }
}

impl ast::Expr {
    fn reg(&self, loc: &Pos) -> Result<Reg, Error> {
        match self {
            ast::Expr::Ident((name, _)) => match Reg::parse(name) {
                Some(r) => Ok(r),
                None => Err(ErrorKind::InvalidRegister(name.clone()).at(loc.clone())),
            },
            _ => Err(ErrorKind::InvalidRegister(format!("{:?}", self)).at(loc.clone())),
        }
    }

    fn global(&self, global: &Global, loc: &Pos) -> Result<Imm, Error> {
        match self {
            ast::Expr::Ident((label, pos)) => match global.resolve(label, pos) {
                Some((ast::Def::Asm(..) | ast::Def::Func(..), fqn)) => Ok(Imm::Label(fqn)),
                Some(_) => Err(ErrorKind::NotGlobalLabel(label.clone()).at(loc.clone())),
                None => Err(ErrorKind::UndefinedGlobalLabel(label.clone()).at(loc.clone())),
            },
            _ => Err(ErrorKind::GlobalLabelExpected.at(loc.clone())),
        }
    }

    fn local(&self, local: &HashMap<&str, usize>, idx: usize, loc: &Pos) -> Result<Imm, Error> {
        match self {
            ast::Expr::Ident((label, _)) => match local.get(label.as_str()) {
                Some(&goto) => Ok(Imm::Lit((goto as i32 - idx as i32) as usize)),
                None => Err(ErrorKind::UndefinedLocalLabel(label.clone()).at(loc.clone())),
            },
            _ => Err(ErrorKind::LocalLabelExpected.at(loc.clone())),
        }
    }

    fn imm<'a>(&'a self, global: &'a Global<'a>, loc: &Pos) -> Result<Imm, Error> {
        match self {
            ast::Expr::NumberLit(n, _) => Ok(Imm::Lit(*n)),
            ast::Expr::CharLit(ch, _) => Ok(Imm::Lit(*ch as usize)),
            ast::Expr::Ident((name, pos)) => match global.resolve(name, pos) {
                Some((ast::Def::Const(_, _, expr), fqn)) => {
                    let value = global.constexpr(expr)?;
                    Ok(Imm::Const(fqn, value.to_usize()))
                }
                Some((ast::Def::Static(..), _)) => {
                    Err(ErrorKind::StaticRequiresAddressOf(name.clone()).at(loc.clone()))
                }
                Some((ast::Def::Asm(..) | ast::Def::Func(..) | ast::Def::Type(..), _)) => {
                    Err(ErrorKind::InvalidImmediateValue(name.clone()).at(loc.clone()))
                }
                None => Err(ErrorKind::UnknownIdentifier(name.clone()).at(loc.clone())),
            },
            ast::Expr::Unary(op, inner) => match op {
                ast::UnaryOp::Pos => inner.imm(global, loc),
                ast::UnaryOp::Neg => match inner.imm(global, loc) {
                    Ok(Imm::Lit(val)) => Ok(Imm::Lit((-(val as isize)) as usize)),
                    _ => Err(ErrorKind::CannotNegateSymbol.at(loc.clone())),
                },
                ast::UnaryOp::Not => match inner.imm(global, loc)? {
                    Imm::Lit(val) => Ok(Imm::Lit((!(val as u16)) as usize)),
                    Imm::Const(_, val) => Ok(Imm::Lit((!(val as u16)) as usize)),
                    _ => Err(ErrorKind::CannotNegateSymbol.at(loc.clone())),
                },
            },
            ast::Expr::Addr(inner) => match inner.as_ref() {
                ast::Expr::Ident((name, pos)) => match global.resolve(name, pos) {
                    Some((ast::Def::Static(..), fqn)) => Ok(Imm::Symbol(fqn, 0)),
                    Some((ast::Def::Const(..), fqn)) => Ok(Imm::Symbol(fqn, 0)),
                    _ => {
                        Err(ErrorKind::InvalidImmediateValue(format!("{}*", name)).at(loc.clone()))
                    }
                },
                _ => inner.imm(global, loc),
            },
            ast::Expr::Deref(inner) => inner.imm(global, loc),
            ast::Expr::Cast(inner, _) => inner.imm(global, loc),
            ast::Expr::Member(base_expr, (field, _)) => match base_expr.imm(global, loc)? {
                Imm::Symbol(ident, base) => {
                    let offset = match global.get(ident.as_str()) {
                        Some(ast::Def::Static(_, _, ty)) => {
                            let ty = global.normtype(ty)?;
                            ty.get_field_offset(field).ok_or_else(|| {
                                ErrorKind::FieldNotFoundInStruct(field.clone()).at(loc.clone())
                            })?
                        }
                        Some(ast::Def::Const(_, _, expr)) => {
                            let value = global.constexpr(expr)?;
                            let ty = value.typeinfer()?;
                            ty.get_field_offset(field).ok_or_else(|| {
                                ErrorKind::FieldNotFoundInStruct(field.clone()).at(loc.clone())
                            })?
                        }
                        _ => return Err(ErrorKind::UnknownSymbol(ident).at(loc.clone())),
                    };
                    Ok(Imm::Symbol(ident, base + offset))
                }
                // base が数値即値 (典型的には `@(N as *T)` 経由) なら、
                // base_expr の型からフィールドオフセットを解決して加算。
                // C の `&((T*)N)->field` を `(@(N as *T)).field@` で表す。
                // auto-deref はせず、ユーザが明示的に `@` deref を書く必要がある。
                Imm::Lit(base_addr) => {
                    let base_ty = global.typeinfer(base_expr)?;
                    let ofs = base_ty.get_field_offset(field).ok_or_else(|| {
                        ErrorKind::FieldNotFoundInStruct(field.clone()).at(loc.clone())
                    })?;
                    Ok(Imm::Lit(base_addr + ofs))
                }
                Imm::Const(_, _) => Err(ErrorKind::CannotAccessFieldOfImmediate.at(loc.clone())),
                Imm::Label(_) => Err(ErrorKind::CannotAccessFieldOfLabel.at(loc.clone())),
                Imm::ScopeExit(_) | Imm::ScopeEntry(_) | Imm::FrameRel(_) => {
                    unreachable!("scope/frame placeholders are only produced inside fn bodies")
                }
            },

            ast::Expr::Index(expr, index) => match expr.imm(global, loc)? {
                Imm::Symbol(ident, base) => {
                    if let ast::Expr::NumberLit(idx, _) = index.as_ref() {
                        let offset = match global.get(ident.as_str()) {
                            Some(ast::Def::Static(_, _, ty)) => {
                                let ty = global.normtype(ty)?;
                                ty.get_array_offset(*idx)
                                    .ok_or(ErrorKind::TypeIsNotArray.at(loc.clone()))?
                            }
                            Some(ast::Def::Const(_, _, expr)) => {
                                let value = global.constexpr(expr)?;
                                if matches!(value, crate::eval::constexpr::ConstExpr::String(_)) {
                                    *idx // 文字列は 1 word/char
                                } else {
                                    let ty = value.typeinfer()?;
                                    ty.get_array_offset(*idx)
                                        .ok_or(ErrorKind::TypeIsNotArray.at(loc.clone()))?
                                }
                            }
                            _ => return Err(ErrorKind::UnknownSymbol(ident).at(loc.clone())),
                        };
                        Ok(Imm::Symbol(ident, base + offset))
                    } else {
                        Err(ErrorKind::NonConstantArrayIndex.at(loc.clone()))
                    }
                }
                Imm::Lit(_) | Imm::Const(_, _) => {
                    Err(ErrorKind::CannotIndexImmediate.at(loc.clone()))
                }
                Imm::Label(_) => Err(ErrorKind::CannotIndexLabel.at(loc.clone())),
                Imm::ScopeExit(_) | Imm::ScopeEntry(_) | Imm::FrameRel(_) => {
                    unreachable!("scope/frame placeholders are only produced inside fn bodies")
                }
            },

            ast::Expr::Binary(op, left, right) => {
                let lhs = left.imm(global, loc)?;
                let rhs = right.imm(global, loc)?;
                // Helper to extract value from Lit or Const
                let get_val = |imm: &Imm| -> Option<usize> {
                    match imm {
                        Imm::Lit(v) | Imm::Const(_, v) => Some(*v),
                        _ => None,
                    }
                };
                match (&lhs, &rhs) {
                    (Imm::Symbol(ident, left_offset), _) if get_val(&rhs).is_some() => {
                        let right_val = get_val(&rhs).unwrap();
                        match op {
                            ast::BinaryOp::Add => {
                                Ok(Imm::Symbol(ident.clone(), left_offset + right_val))
                            }
                            ast::BinaryOp::Sub => Ok(Imm::Symbol(
                                ident.clone(),
                                left_offset.wrapping_sub(right_val),
                            )),
                            _ => Err(ErrorKind::UnsupportedOperationInAddress.at(loc.clone())),
                        }
                    }
                    (_, Imm::Symbol(ident, right_offset)) if get_val(&lhs).is_some() => {
                        let left_val = get_val(&lhs).unwrap();
                        match op {
                            ast::BinaryOp::Add => {
                                Ok(Imm::Symbol(ident.clone(), left_val + *right_offset))
                            }
                            _ => Err(ErrorKind::InvalidSubtractionInAddress.at(loc.clone())),
                        }
                    }
                    _ if get_val(&lhs).is_some() && get_val(&rhs).is_some() => {
                        let left_val = get_val(&lhs).unwrap();
                        let right_val = get_val(&rhs).unwrap();
                        match op {
                            ast::BinaryOp::Add => Ok(Imm::Lit(left_val.wrapping_add(right_val))),
                            ast::BinaryOp::Sub => Ok(Imm::Lit(left_val.wrapping_sub(right_val))),
                            _ => Err(ErrorKind::UnsupportedOperationInAddress.at(loc.clone())),
                        }
                    }
                    (Imm::Symbol(_, _), Imm::Symbol(_, _)) => match op {
                        ast::BinaryOp::Add => Err(ErrorKind::CannotAddSymbols.at(loc.clone())),
                        _ => Err(ErrorKind::InvalidSubtractionInAddress.at(loc.clone())),
                    },
                    (Imm::Label(_), _) | (_, Imm::Label(_)) => {
                        Err(ErrorKind::CannotPerformArithmeticOnLabel.at(loc.clone()))
                    }
                    _ => Err(ErrorKind::UnsupportedOperationInAddress.at(loc.clone())),
                }
            }

            ast::Expr::SizeofType(ty, _) => match global.normtype(ty) {
                Ok(ty) => Ok(Imm::Lit(ty.sizeof())),
                Err(e) => Err(ErrorKind::CannotEvaluateSizeofType(e.to_string()).at(loc.clone())),
            },

            ast::Expr::SizeofExpr(inner, _) => match global.typeinfer(inner) {
                Ok(ty) => Ok(Imm::Lit(ty.sizeof())),
                Err(e) => Err(ErrorKind::CannotEvaluateSizeofExpr(e.to_string()).at(loc.clone())),
            },

            _ => Err(ErrorKind::UnsupportedExprType(format!("{:?}", self)).at(loc.clone())),
        }
    }
}
