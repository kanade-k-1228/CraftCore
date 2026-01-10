use crate::{error::Error, grammer::ast};

use super::{
    code::{Code, Imm},
    global::Global,
};
use arch::{inst::Inst, reg::Reg};
use std::collections::HashMap;

impl Global {
    pub fn asm2code(&self, name: &str) -> Result<Code, Error> {
        match self.get(name) {
            Some(ast::Def::Asm(_, _, stmts)) => gen_asm(self, stmts),
            Some(_) => Err(Error::NotAnAsm(name.to_string())),
            None => Err(Error::UnknownIdentifier(name.to_string())),
        }
    }
}

fn gen_asm(global: &Global, stmts: &[ast::Asm]) -> Result<Code, Error> {
    // 1. Collect local labels
    let mut local: HashMap<&str, usize> = HashMap::new();
    for (idx, ast::Asm(_, _, labels)) in stmts.iter().enumerate() {
        for label in labels {
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

fn parse_stmt(
    global: &Global,
    local: &HashMap<&str, usize>,
    idx: usize,
    stmt: &ast::Asm,
) -> Result<Inst<Reg, Imm>, Error> {
    let g = global;
    let ast::Asm(inst, args, _) = stmt;
    match (inst.to_lowercase().as_str(), args.len()) {
        ("nop", 0) => Ok(Inst::NOP()),
        ("mov", 2) => Ok(Inst::MOV(args[0].reg()?, args[1].reg()?)),
        ("add", 3) => Ok(Inst::ADD(args[0].reg()?, args[1].reg()?, args[2].reg()?)),
        ("addi", 3) => Ok(Inst::ADDI(args[0].reg()?, args[1].reg()?, args[2].imm(g)?)),
        ("subi", 3) => Ok(Inst::SUBI(args[0].reg()?, args[1].reg()?, args[2].imm(g)?)),
        ("andi", 3) => Ok(Inst::ANDI(args[0].reg()?, args[1].reg()?, args[2].imm(g)?)),
        ("ori", 3) => Ok(Inst::ORI(args[0].reg()?, args[1].reg()?, args[2].imm(g)?)),
        ("xori", 3) => Ok(Inst::XORI(args[0].reg()?, args[1].reg()?, args[2].imm(g)?)),
        ("eqi", 3) => Ok(Inst::EQI(args[0].reg()?, args[1].reg()?, args[2].imm(g)?)),
        ("neqi", 3) => Ok(Inst::NEQI(args[0].reg()?, args[1].reg()?, args[2].imm(g)?)),
        ("lti", 3) => Ok(Inst::LTI(args[0].reg()?, args[1].reg()?, args[2].imm(g)?)),
        ("ltsi", 3) => Ok(Inst::LTSI(args[0].reg()?, args[1].reg()?, args[2].imm(g)?)),
        ("not", 2) => Ok(Inst::NOT(args[0].reg()?, args[1].reg()?)),
        ("loadi", 2) => Ok(Inst::LOADI(args[0].reg()?, args[1].imm(g)?)),
        ("sub", 3) => Ok(Inst::SUB(args[0].reg()?, args[1].reg()?, args[2].reg()?)),
        ("and", 3) => Ok(Inst::AND(args[0].reg()?, args[1].reg()?, args[2].reg()?)),
        ("or", 3) => Ok(Inst::OR(args[0].reg()?, args[1].reg()?, args[2].reg()?)),
        ("xor", 3) => Ok(Inst::XOR(args[0].reg()?, args[1].reg()?, args[2].reg()?)),
        ("eq", 3) => Ok(Inst::EQ(args[0].reg()?, args[1].reg()?, args[2].reg()?)),
        ("neq", 3) => Ok(Inst::NEQ(args[0].reg()?, args[1].reg()?, args[2].reg()?)),
        ("lt", 3) => Ok(Inst::LT(args[0].reg()?, args[1].reg()?, args[2].reg()?)),
        ("lts", 3) => Ok(Inst::LTS(args[0].reg()?, args[1].reg()?, args[2].reg()?)),
        ("sr", 2) => Ok(Inst::SR(args[0].reg()?, args[1].reg()?)),
        ("srs", 2) => Ok(Inst::SRS(args[0].reg()?, args[1].reg()?)),
        ("srr", 2) => Ok(Inst::SRR(args[0].reg()?, args[1].reg()?)),
        ("sl", 2) => Ok(Inst::SL(args[0].reg()?, args[1].reg()?)),
        ("slr", 2) => Ok(Inst::SLR(args[0].reg()?, args[1].reg()?)),
        ("load", 2) => Ok(Inst::LOADI(args[0].reg()?, args[1].imm(g)?)),
        ("load", 3) => Ok(Inst::LOAD(args[0].reg()?, args[1].reg()?, args[2].imm(g)?)),
        ("store", 2) => Ok(Inst::STORE(args[1].reg()?, Reg::Z, args[0].imm(g)?)),
        ("store", 3) => Ok(Inst::STORE(args[0].reg()?, args[1].reg()?, args[2].imm(g)?)),

        // jumpif(cond, label)
        ("jumpif", 2) => {
            let cond = args[0].reg()?;
            match &args[1] {
                ast::Expr::Ident(label) => match local.get(label.as_str()) {
                    // Local label: Compiled to relative jump
                    Some(&goto) => {
                        let offset = (goto as i32 - idx as i32) as u16;
                        Ok(Inst::JUMPIFR(cond, Imm::Lit(offset as usize)))
                    }
                    // Global label: Compiled to absolute jump
                    None => Ok(Inst::JUMPIF(cond, Imm::Label(label.clone()))),
                },
                _ => Err(Error::TODO),
            }
        }

        // jump(label)
        ("jump", 1) => {
            match &args[0] {
                ast::Expr::Ident(label) => match local.get(label.as_str()) {
                    // Local label: Compiled to relative jump
                    Some(&goto) => {
                        let offset = (goto as i32 - idx as i32) as u16;
                        Ok(Inst::JUMPR(Imm::Lit(offset as usize)))
                    }
                    // Global label: Compiled to absolute jump
                    None => Ok(Inst::JUMP(Imm::Label(label.clone()))),
                },
                _ => Err(Error::TODO),
            }
        }

        // call(label)
        ("call", 1) => match &args[0] {
            // Global label only
            ast::Expr::Ident(label) => Ok(Inst::CALL(Imm::Label(label.clone()))),
            _ => Err(Error::TODO),
        },
        ("ret", 0) => Ok(Inst::RET()),
        ("iret", 0) => Ok(Inst::IRET()),
        _ => Err(Error::InvalidInstruction(inst.to_string())),
    }
}

impl ast::Expr {
    fn reg(&self) -> Result<Reg, Error> {
        match self {
            ast::Expr::Ident(name) => match Reg::parse(name) {
                Some(r) => Ok(r),
                None => Err(Error::InvalidRegister(name.to_string())),
            },
            _ => Err(Error::InvalidRegister(format!("{:?}", self))),
        }
    }

    fn imm(&self, global: &Global) -> Result<Imm, Error> {
        match self {
            ast::Expr::NumberLit(n) => Ok(Imm::Lit(*n as usize)),
            ast::Expr::CharLit(ch) => Ok(Imm::Lit(*ch as usize)),
            ast::Expr::Ident(name) => Ok(Imm::Symbol(name.clone(), 0)),
            ast::Expr::Unary(op, inner) => match op {
                ast::UnaryOp::Pos => inner.imm(global),
                ast::UnaryOp::Neg => match inner.imm(global) {
                    Ok(Imm::Lit(val)) => Ok(Imm::Lit((-(val as isize)) as usize)),
                    _ => Err(Error::CannotNegateSymbol),
                },
                ast::UnaryOp::Not => todo!(),
            },
            ast::Expr::Addr(inner) => inner.imm(global),
            ast::Expr::Deref(_) => Err(Error::CannotDereferenceInAssembly),
            ast::Expr::Member(expr, field) => match expr.imm(global)? {
                Imm::Symbol(ident, base) => {
                    let offset = match global.get(ident.as_str()) {
                        Some(ast::Def::Static(_, _, ty)) => {
                            let ty = global.normtype(ty)?;
                            ty.get_field_offset(field)
                                .ok_or_else(|| Error::FieldNotFoundInStruct(field.clone()))?
                        }
                        Some(ast::Def::Const(_, _, expr)) => {
                            let value = global.constexpr(expr)?;
                            let ty = value.typeinfer()?;
                            ty.get_field_offset(field)
                                .ok_or_else(|| Error::FieldNotFoundInStruct(field.clone()))?
                        }
                        _ => return Err(Error::UnknownSymbol(ident)),
                    };
                    Ok(Imm::Symbol(ident, base + offset))
                }
                Imm::Lit(_) => Err(Error::CannotAccessFieldOfImmediate),
                Imm::Label(_) => Err(Error::CannotAccessFieldOfLabel),
            },

            ast::Expr::Index(expr, index) => match expr.imm(global)? {
                Imm::Symbol(ident, base) => {
                    if let ast::Expr::NumberLit(idx) = index.as_ref() {
                        let offset = match global.get(ident.as_str()) {
                            Some(ast::Def::Static(_, _, ty)) => {
                                let ty = global.normtype(ty)?;
                                ty.get_array_offset(*idx).ok_or(Error::TypeIsNotArray)?
                            }
                            Some(ast::Def::Const(_, _, expr)) => {
                                let value = global.constexpr(expr)?;
                                if matches!(value, crate::eval::constexpr::ConstExpr::String(_)) {
                                    idx * 1
                                } else {
                                    let ty = value.typeinfer()?;
                                    ty.get_array_offset(*idx).ok_or(Error::TypeIsNotArray)?
                                }
                            }
                            _ => return Err(Error::UnknownSymbol(ident)),
                        };
                        Ok(Imm::Symbol(ident, base + offset))
                    } else {
                        Err(Error::NonConstantArrayIndex)
                    }
                }
                Imm::Lit(_) => Err(Error::CannotIndexImmediate),
                Imm::Label(_) => Err(Error::CannotIndexLabel),
            },

            ast::Expr::Binary(op, left, right) => {
                let lhs = left.imm(global)?;
                let rhs = right.imm(global)?;
                match (lhs, rhs) {
                    (Imm::Symbol(ident, left_offset), Imm::Lit(right_val)) => match op {
                        ast::BinaryOp::Add => {
                            Ok(Imm::Symbol(ident, left_offset + right_val as usize))
                        }
                        ast::BinaryOp::Sub => Ok(Imm::Symbol(
                            ident,
                            left_offset.wrapping_sub(right_val as usize),
                        )),
                        _ => Err(Error::UnsupportedOperationInAddress),
                    },
                    (Imm::Lit(left_val), Imm::Symbol(ident, right_offset)) => match op {
                        ast::BinaryOp::Add => {
                            Ok(Imm::Symbol(ident, left_val as usize + right_offset))
                        }
                        _ => Err(Error::InvalidSubtractionInAddress),
                    },
                    (Imm::Lit(left_val), Imm::Lit(right_val)) => match op {
                        ast::BinaryOp::Add => Ok(Imm::Lit(left_val.wrapping_add(right_val))),
                        ast::BinaryOp::Sub => Ok(Imm::Lit(left_val.wrapping_sub(right_val))),
                        _ => Err(Error::UnsupportedOperationInAddress),
                    },
                    (Imm::Symbol(_, _), Imm::Symbol(_, _)) => match op {
                        ast::BinaryOp::Add => Err(Error::CannotAddSymbols),
                        _ => Err(Error::InvalidSubtractionInAddress),
                    },
                    (Imm::Label(_), _) | (_, Imm::Label(_)) => {
                        Err(Error::CannotPerformArithmeticOnLabel)
                    }
                }
            }

            ast::Expr::SizeofType(ty) => match global.normtype(ty) {
                Ok(ty) => Ok(Imm::Lit(ty.sizeof())),
                Err(e) => Err(Error::CannotEvaluateSizeofType(e.to_string())),
            },

            ast::Expr::SizeofExpr(inner) => match global.typeinfer(inner) {
                Ok(ty) => Ok(Imm::Lit(ty.sizeof())),
                Err(e) => Err(Error::CannotEvaluateSizeofExpr(e.to_string())),
            },

            _ => Err(Error::UnsupportedExprType(format!("{:?}", self))),
        }
    }
}
