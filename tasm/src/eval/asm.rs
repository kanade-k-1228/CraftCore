use crate::{
    error::Error,
    grammer::{ast, token::Pos},
};

use super::{
    code::{Code, Imm},
    global::Global,
};
use arch::{inst::Inst, reg::Reg};
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
            ) => Err(Error::NotAnAsm(pos.clone(), name.to_string())),
            None => Err(Error::UnknownIdentifier(Pos::default(), name.to_string())),
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
) -> Result<Inst<Reg, Imm>, Error> {
    let g = global;
    let ast::Asm((inst, _), args, _, pos) = stmt;
    let loc = pos.clone();
    match (inst.to_lowercase().as_str(), args.len()) {
        ("nop", 0) => Ok(Inst::NOP()),
        ("mov", 2) => Ok(Inst::MOV(args[0].reg(&loc)?, args[1].reg(&loc)?)),
        ("add", 3) => Ok(Inst::ADD(
            args[0].reg(&loc)?,
            args[1].reg(&loc)?,
            args[2].reg(&loc)?,
        )),
        ("addi", 3) => Ok(Inst::ADDI(
            args[0].reg(&loc)?,
            args[1].reg(&loc)?,
            args[2].imm(g, &loc)?,
        )),
        ("subi", 3) => Ok(Inst::SUBI(
            args[0].reg(&loc)?,
            args[1].reg(&loc)?,
            args[2].imm(g, &loc)?,
        )),
        ("andi", 3) => Ok(Inst::ANDI(
            args[0].reg(&loc)?,
            args[1].reg(&loc)?,
            args[2].imm(g, &loc)?,
        )),
        ("ori", 3) => Ok(Inst::ORI(
            args[0].reg(&loc)?,
            args[1].reg(&loc)?,
            args[2].imm(g, &loc)?,
        )),
        ("xori", 3) => Ok(Inst::XORI(
            args[0].reg(&loc)?,
            args[1].reg(&loc)?,
            args[2].imm(g, &loc)?,
        )),
        ("eqi", 3) => Ok(Inst::EQI(
            args[0].reg(&loc)?,
            args[1].reg(&loc)?,
            args[2].imm(g, &loc)?,
        )),
        ("neqi", 3) => Ok(Inst::NEQI(
            args[0].reg(&loc)?,
            args[1].reg(&loc)?,
            args[2].imm(g, &loc)?,
        )),
        ("lti", 3) => Ok(Inst::LTI(
            args[0].reg(&loc)?,
            args[1].reg(&loc)?,
            args[2].imm(g, &loc)?,
        )),
        ("ltsi", 3) => Ok(Inst::LTSI(
            args[0].reg(&loc)?,
            args[1].reg(&loc)?,
            args[2].imm(g, &loc)?,
        )),
        ("not", 2) => Ok(Inst::NOT(args[0].reg(&loc)?, args[1].reg(&loc)?)),
        ("loadi", 2) => Ok(Inst::LOADI(args[0].reg(&loc)?, args[1].imm(g, &loc)?)),
        ("sub", 3) => Ok(Inst::SUB(
            args[0].reg(&loc)?,
            args[1].reg(&loc)?,
            args[2].reg(&loc)?,
        )),
        ("and", 3) => Ok(Inst::AND(
            args[0].reg(&loc)?,
            args[1].reg(&loc)?,
            args[2].reg(&loc)?,
        )),
        ("or", 3) => Ok(Inst::OR(
            args[0].reg(&loc)?,
            args[1].reg(&loc)?,
            args[2].reg(&loc)?,
        )),
        ("xor", 3) => Ok(Inst::XOR(
            args[0].reg(&loc)?,
            args[1].reg(&loc)?,
            args[2].reg(&loc)?,
        )),
        ("eq", 3) => Ok(Inst::EQ(
            args[0].reg(&loc)?,
            args[1].reg(&loc)?,
            args[2].reg(&loc)?,
        )),
        ("neq", 3) => Ok(Inst::NEQ(
            args[0].reg(&loc)?,
            args[1].reg(&loc)?,
            args[2].reg(&loc)?,
        )),
        ("lt", 3) => Ok(Inst::LT(
            args[0].reg(&loc)?,
            args[1].reg(&loc)?,
            args[2].reg(&loc)?,
        )),
        ("lts", 3) => Ok(Inst::LTS(
            args[0].reg(&loc)?,
            args[1].reg(&loc)?,
            args[2].reg(&loc)?,
        )),
        ("sr", 2) => Ok(Inst::SR(args[0].reg(&loc)?, args[1].reg(&loc)?)),
        ("srs", 2) => Ok(Inst::SRS(args[0].reg(&loc)?, args[1].reg(&loc)?)),
        ("srr", 2) => Ok(Inst::SRR(args[0].reg(&loc)?, args[1].reg(&loc)?)),
        ("sl", 2) => Ok(Inst::SL(args[0].reg(&loc)?, args[1].reg(&loc)?)),
        ("slr", 2) => Ok(Inst::SLR(args[0].reg(&loc)?, args[1].reg(&loc)?)),
        ("load", 2) => Ok(Inst::LOADI(args[0].reg(&loc)?, args[1].imm(g, &loc)?)),
        ("load", 3) => Ok(Inst::LOAD(
            args[0].reg(&loc)?,
            args[1].reg(&loc)?,
            args[2].imm(g, &loc)?,
        )),
        ("store", 2) => Ok(Inst::STORE(
            args[1].reg(&loc)?,
            Reg::Z,
            args[0].imm(g, &loc)?,
        )),
        ("store", 3) => Ok(Inst::STORE(
            args[0].reg(&loc)?,
            args[1].reg(&loc)?,
            args[2].imm(g, &loc)?,
        )),

        ("jumpif", 2) => Ok(Inst::JUMPIF(
            args[0].reg(&loc)?,
            args[1].global(global, &loc)?,
        )),
        ("jumpifr", 2) => Ok(Inst::JUMPIFR(
            args[0].reg(&loc)?,
            args[1].local(local, idx, &loc)?,
        )),

        ("jump", 1) => Ok(Inst::JUMP(args[0].global(global, &loc)?)),
        ("jumpr", 1) => Ok(Inst::JUMPR(args[0].local(local, idx, &loc)?)),

        ("call", 1) => Ok(Inst::CALL(args[0].global(global, &loc)?)),

        ("ret", 0) => Ok(Inst::RET()),
        ("iret", 0) => Ok(Inst::IRET()),
        _ => Err(Error::InvalidInstruction(loc, inst.clone())),
    }
}

impl ast::Expr {
    fn reg(&self, loc: &Pos) -> Result<Reg, Error> {
        match self {
            ast::Expr::Ident((name, _)) => match Reg::parse(name) {
                Some(r) => Ok(r),
                None => Err(Error::InvalidRegister(loc.clone(), name.clone())),
            },
            _ => Err(Error::InvalidRegister(loc.clone(), format!("{:?}", self))),
        }
    }

    fn global(&self, global: &Global, loc: &Pos) -> Result<Imm, Error> {
        match self {
            ast::Expr::Ident((label, _)) => match global.get(label.as_str()) {
                Some(ast::Def::Asm(..) | ast::Def::Func(..)) => Ok(Imm::Label(label.clone())),
                Some(_) => Err(Error::NotGlobalLabel(loc.clone(), label.clone())),
                None => Err(Error::UndefinedGlobalLabel(loc.clone(), label.clone())),
            },
            _ => Err(Error::GlobalLabelExpected(loc.clone())),
        }
    }

    fn local(&self, local: &HashMap<&str, usize>, idx: usize, loc: &Pos) -> Result<Imm, Error> {
        match self {
            ast::Expr::Ident((label, _)) => match local.get(label.as_str()) {
                Some(&goto) => Ok(Imm::Lit((goto as i32 - idx as i32) as usize)),
                None => Err(Error::UndefinedLocalLabel(loc.clone(), label.clone())),
            },
            _ => Err(Error::LocalLabelExpected(loc.clone())),
        }
    }

    fn imm<'a>(&'a self, global: &'a Global<'a>, loc: &Pos) -> Result<Imm, Error> {
        match self {
            ast::Expr::NumberLit(n) => Ok(Imm::Lit(*n as usize)),
            ast::Expr::CharLit(ch) => Ok(Imm::Lit(*ch as usize)),
            ast::Expr::Ident((name, _)) => match global.get(name.as_str()) {
                Some(ast::Def::Const(_, _, expr)) => {
                    let value = global.constexpr(expr)?;
                    Ok(Imm::Const(name.clone(), value.to_usize()))
                }
                Some(ast::Def::Static(..)) => {
                    Err(Error::StaticRequiresAddressOf(loc.clone(), name.clone()))
                }
                Some(ast::Def::Asm(..) | ast::Def::Func(..) | ast::Def::Type(..)) => {
                    Err(Error::InvalidImmediateValue(loc.clone(), name.clone()))
                }
                None => Err(Error::UnknownIdentifier(loc.clone(), name.clone())),
            },
            ast::Expr::Unary(op, inner) => match op {
                ast::UnaryOp::Pos => inner.imm(global, loc),
                ast::UnaryOp::Neg => match inner.imm(global, loc) {
                    Ok(Imm::Lit(val)) => Ok(Imm::Lit((-(val as isize)) as usize)),
                    _ => Err(Error::CannotNegateSymbol(loc.clone())),
                },
                ast::UnaryOp::Not => match inner.imm(global, loc)? {
                    Imm::Lit(val) => Ok(Imm::Lit((!(val as u16)) as usize)),
                    Imm::Const(_, val) => Ok(Imm::Lit((!(val as u16)) as usize)),
                    _ => Err(Error::CannotNegateSymbol(loc.clone())),
                },
            },
            ast::Expr::Addr(inner) => match inner.as_ref() {
                ast::Expr::Ident((name, _)) => match global.get(name.as_str()) {
                    Some(ast::Def::Static(..)) => Ok(Imm::Symbol(name.clone(), 0)),
                    Some(ast::Def::Const(..)) => Ok(Imm::Symbol(name.clone(), 0)),
                    _ => Err(Error::InvalidImmediateValue(
                        loc.clone(),
                        format!("{}*", name),
                    )),
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
                                Error::FieldNotFoundInStruct(loc.clone(), field.clone())
                            })?
                        }
                        Some(ast::Def::Const(_, _, expr)) => {
                            let value = global.constexpr(expr)?;
                            let ty = value.typeinfer()?;
                            ty.get_field_offset(field).ok_or_else(|| {
                                Error::FieldNotFoundInStruct(loc.clone(), field.clone())
                            })?
                        }
                        _ => return Err(Error::UnknownSymbol(loc.clone(), ident)),
                    };
                    Ok(Imm::Symbol(ident, base + offset))
                }
                // base が数値即値 (典型的には `@(N as *T)` 経由) なら、
                // base_expr の型からフィールドオフセットを解決して加算。
                // C の `&((T*)N)->field` を `(@(N as *T)).field@` で表す。
                // auto-deref はせず、ユーザが明示的に `@` deref を書く必要がある。
                Imm::Lit(base_addr) => {
                    let base_ty = global.typeinfer(base_expr)?;
                    let ofs = base_ty
                        .get_field_offset(field)
                        .ok_or_else(|| Error::FieldNotFoundInStruct(loc.clone(), field.clone()))?;
                    Ok(Imm::Lit(base_addr + ofs))
                }
                Imm::Const(_, _) => Err(Error::CannotAccessFieldOfImmediate(loc.clone())),
                Imm::Label(_) => Err(Error::CannotAccessFieldOfLabel(loc.clone())),
                Imm::ScopeExit(_) | Imm::ScopeEntry(_) => {
                    unreachable!("scope placeholders are only produced inside fn bodies")
                }
            },

            ast::Expr::Index(expr, index) => match expr.imm(global, loc)? {
                Imm::Symbol(ident, base) => {
                    if let ast::Expr::NumberLit(idx) = index.as_ref() {
                        let offset = match global.get(ident.as_str()) {
                            Some(ast::Def::Static(_, _, ty)) => {
                                let ty = global.normtype(ty)?;
                                ty.get_array_offset(*idx)
                                    .ok_or(Error::TypeIsNotArray(loc.clone()))?
                            }
                            Some(ast::Def::Const(_, _, expr)) => {
                                let value = global.constexpr(expr)?;
                                if matches!(value, crate::eval::constexpr::ConstExpr::String(_)) {
                                    idx * 1
                                } else {
                                    let ty = value.typeinfer()?;
                                    ty.get_array_offset(*idx)
                                        .ok_or(Error::TypeIsNotArray(loc.clone()))?
                                }
                            }
                            _ => return Err(Error::UnknownSymbol(loc.clone(), ident)),
                        };
                        Ok(Imm::Symbol(ident, base + offset))
                    } else {
                        Err(Error::NonConstantArrayIndex(loc.clone()))
                    }
                }
                Imm::Lit(_) | Imm::Const(_, _) => Err(Error::CannotIndexImmediate(loc.clone())),
                Imm::Label(_) => Err(Error::CannotIndexLabel(loc.clone())),
                Imm::ScopeExit(_) | Imm::ScopeEntry(_) => {
                    unreachable!("scope placeholders are only produced inside fn bodies")
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
                            _ => Err(Error::UnsupportedOperationInAddress(loc.clone())),
                        }
                    }
                    (_, Imm::Symbol(ident, right_offset)) if get_val(&lhs).is_some() => {
                        let left_val = get_val(&lhs).unwrap();
                        match op {
                            ast::BinaryOp::Add => {
                                Ok(Imm::Symbol(ident.clone(), left_val + *right_offset))
                            }
                            _ => Err(Error::InvalidSubtractionInAddress(loc.clone())),
                        }
                    }
                    _ if get_val(&lhs).is_some() && get_val(&rhs).is_some() => {
                        let left_val = get_val(&lhs).unwrap();
                        let right_val = get_val(&rhs).unwrap();
                        match op {
                            ast::BinaryOp::Add => Ok(Imm::Lit(left_val.wrapping_add(right_val))),
                            ast::BinaryOp::Sub => Ok(Imm::Lit(left_val.wrapping_sub(right_val))),
                            _ => Err(Error::UnsupportedOperationInAddress(loc.clone())),
                        }
                    }
                    (Imm::Symbol(_, _), Imm::Symbol(_, _)) => match op {
                        ast::BinaryOp::Add => Err(Error::CannotAddSymbols(loc.clone())),
                        _ => Err(Error::InvalidSubtractionInAddress(loc.clone())),
                    },
                    (Imm::Label(_), _) | (_, Imm::Label(_)) => {
                        Err(Error::CannotPerformArithmeticOnLabel(loc.clone()))
                    }
                    _ => Err(Error::UnsupportedOperationInAddress(loc.clone())),
                }
            }

            ast::Expr::SizeofType(ty) => match global.normtype(ty) {
                Ok(ty) => Ok(Imm::Lit(ty.sizeof())),
                Err(e) => Err(Error::CannotEvaluateSizeofType(loc.clone(), e.to_string())),
            },

            ast::Expr::SizeofExpr(inner) => match global.typeinfer(inner) {
                Ok(ty) => Ok(Imm::Lit(ty.sizeof())),
                Err(e) => Err(Error::CannotEvaluateSizeofExpr(loc.clone(), e.to_string())),
            },

            _ => Err(Error::UnsupportedExprType(
                loc.clone(),
                format!("{:?}", self),
            )),
        }
    }
}
