use crate::{
    compile::{Code, Imm},
    error::Error,
    eval::global::Global,
    grammer::ast,
};
use arch::{inst::Inst, reg::Reg};
use std::collections::HashMap;

pub fn asm2code<'a>(globals: &'a Global<'a>) -> Result<HashMap<&'a str, Code>, Error> {
    let mut result = HashMap::new();
    for (name, (_, def)) in globals.asms() {
        if let ast::Def::Asm(_, _, stmts) = def {
            result.insert(name, gen_asm(stmts, globals)?);
        }
    }
    Ok(result)
}

fn gen_asm<'a>(stmts: &'a [ast::Asm], globals: &'a Global<'a>) -> Result<Code, Error> {
    // 1. Collect label
    let mut locals: HashMap<&str, usize> = HashMap::new();
    for (pc, ast::Asm(_, _, labs)) in stmts.iter().enumerate() {
        for label in labs {
            locals.insert(label.as_str(), pc);
        }
    }

    // 2. Generate instruction
    let mut insts = Vec::new();
    for (pc, stmt) in stmts.iter().enumerate() {
        let inst = parse_stmt(pc as u16, stmt, &locals, globals)?;
        insts.push(inst);
    }

    Ok(Code(insts))
}

fn parse_stmt<'a>(
    pc: u16,
    stmt: &'a ast::Asm,
    locals: &HashMap<&str, usize>,
    globals: &'a Global<'a>,
) -> Result<Inst<Reg, Imm>, Error> {
    let ast::Asm(inst, args, _) = stmt;
    match inst.to_lowercase().as_str() {
        "nop" => Ok(Inst::NOP()),
        "mov" if args.len() == 2 => {
            let rd = parse_reg(&args[0])?;
            let rs = parse_reg(&args[1])?;
            Ok(Inst::MOV(rd, rs))
        }
        "add" if args.len() == 3 => {
            let rd = parse_reg(&args[0])?;
            let rs1 = parse_reg(&args[1])?;
            let rs2 = parse_reg(&args[2])?;
            Ok(Inst::ADD(rd, rs1, rs2))
        }
        "addi" if args.len() == 3 => {
            let rd = parse_reg(&args[0])?;
            let rs = parse_reg(&args[1])?;
            let imm = parse_imm(&args[2], globals)?;
            Ok(Inst::ADDI(rd, rs, imm))
        }
        "subi" if args.len() == 3 => {
            let rd = parse_reg(&args[0])?;
            let rs = parse_reg(&args[1])?;
            let imm = parse_imm(&args[2], globals)?;
            Ok(Inst::SUBI(rd, rs, imm))
        }
        "andi" if args.len() == 3 => {
            let rd = parse_reg(&args[0])?;
            let rs = parse_reg(&args[1])?;
            let imm = parse_imm(&args[2], globals)?;
            Ok(Inst::ANDI(rd, rs, imm))
        }
        "ori" if args.len() == 3 => {
            let rd = parse_reg(&args[0])?;
            let rs = parse_reg(&args[1])?;
            let imm = parse_imm(&args[2], globals)?;
            Ok(Inst::ORI(rd, rs, imm))
        }
        "xori" if args.len() == 3 => {
            let rd = parse_reg(&args[0])?;
            let rs = parse_reg(&args[1])?;
            let imm = parse_imm(&args[2], globals)?;
            Ok(Inst::XORI(rd, rs, imm))
        }
        "eqi" if args.len() == 3 => {
            let rd = parse_reg(&args[0])?;
            let rs = parse_reg(&args[1])?;
            let imm = parse_imm(&args[2], globals)?;
            Ok(Inst::EQI(rd, rs, imm))
        }
        "neqi" if args.len() == 3 => {
            let rd = parse_reg(&args[0])?;
            let rs = parse_reg(&args[1])?;
            let imm = parse_imm(&args[2], globals)?;
            Ok(Inst::NEQI(rd, rs, imm))
        }
        "lti" if args.len() == 3 => {
            let rd = parse_reg(&args[0])?;
            let rs = parse_reg(&args[1])?;
            let imm = parse_imm(&args[2], globals)?;
            Ok(Inst::LTI(rd, rs, imm))
        }
        "ltsi" if args.len() == 3 => {
            let rd = parse_reg(&args[0])?;
            let rs = parse_reg(&args[1])?;
            let imm = parse_imm(&args[2], globals)?;
            Ok(Inst::LTSI(rd, rs, imm))
        }
        "not" if args.len() == 2 => {
            let rd = parse_reg(&args[0])?;
            let rs = parse_reg(&args[1])?;
            Ok(Inst::NOT(rd, rs))
        }
        "loadi" if args.len() == 2 => {
            let rd = parse_reg(&args[0])?;
            // Use the new expression evaluator
            let imm = parse_imm(&args[1], globals)?;
            Ok(Inst::LOADI(rd, imm))
        }
        "sub" if args.len() == 3 => {
            let rd = parse_reg(&args[0])?;
            let rs1 = parse_reg(&args[1])?;
            let rs2 = parse_reg(&args[2])?;
            Ok(Inst::SUB(rd, rs1, rs2))
        }
        "and" if args.len() == 3 => {
            let rd = parse_reg(&args[0])?;
            let rs1 = parse_reg(&args[1])?;
            let rs2 = parse_reg(&args[2])?;
            Ok(Inst::AND(rd, rs1, rs2))
        }
        "or" if args.len() == 3 => {
            let rd = parse_reg(&args[0])?;
            let rs1 = parse_reg(&args[1])?;
            let rs2 = parse_reg(&args[2])?;
            Ok(Inst::OR(rd, rs1, rs2))
        }
        "xor" if args.len() == 3 => {
            let rd = parse_reg(&args[0])?;
            let rs1 = parse_reg(&args[1])?;
            let rs2 = parse_reg(&args[2])?;
            Ok(Inst::XOR(rd, rs1, rs2))
        }
        "eq" if args.len() == 3 => {
            let rd = parse_reg(&args[0])?;
            let rs1 = parse_reg(&args[1])?;
            let rs2 = parse_reg(&args[2])?;
            Ok(Inst::EQ(rd, rs1, rs2))
        }
        "neq" if args.len() == 3 => {
            let rd = parse_reg(&args[0])?;
            let rs1 = parse_reg(&args[1])?;
            let rs2 = parse_reg(&args[2])?;
            Ok(Inst::NEQ(rd, rs1, rs2))
        }
        "lt" if args.len() == 3 => {
            let rd = parse_reg(&args[0])?;
            let rs1 = parse_reg(&args[1])?;
            let rs2 = parse_reg(&args[2])?;
            Ok(Inst::LT(rd, rs1, rs2))
        }
        "lts" if args.len() == 3 => {
            let rd = parse_reg(&args[0])?;
            let rs1 = parse_reg(&args[1])?;
            let rs2 = parse_reg(&args[2])?;
            Ok(Inst::LTS(rd, rs1, rs2))
        }
        "sr" if args.len() == 2 => {
            let rd = parse_reg(&args[0])?;
            let rs = parse_reg(&args[1])?;
            Ok(Inst::SR(rd, rs))
        }
        "srs" if args.len() == 2 => {
            let rd = parse_reg(&args[0])?;
            let rs = parse_reg(&args[1])?;
            Ok(Inst::SRS(rd, rs))
        }
        "srr" if args.len() == 2 => {
            let rd = parse_reg(&args[0])?;
            let rs = parse_reg(&args[1])?;
            Ok(Inst::SRR(rd, rs))
        }
        "sl" if args.len() == 2 => {
            let rd = parse_reg(&args[0])?;
            let rs = parse_reg(&args[1])?;
            Ok(Inst::SL(rd, rs))
        }
        "slr" if args.len() == 2 => {
            let rd = parse_reg(&args[0])?;
            let rs = parse_reg(&args[1])?;
            Ok(Inst::SLR(rd, rs))
        }
        "load" if args.len() == 2 => {
            // load(rd, addr/imm)
            let rd = parse_reg(&args[0])?;
            // Use expression evaluator for complex expressions
            let imm = parse_imm(&args[1], globals)?;
            Ok(Inst::LOADI(rd, imm))
        }
        "load" if args.len() == 3 => {
            let rd = parse_reg(&args[0])?;
            let rs = parse_reg(&args[1])?;
            let imm = parse_imm(&args[2], globals)?;
            Ok(Inst::LOAD(rd, rs, imm))
        }
        "store" if args.len() == 2 => {
            // store(symbol, rs)
            let rs = parse_reg(&args[1])?;
            let imm = parse_imm(&args[0], globals)?;
            Ok(Inst::STORE(rs, Reg::Z, imm))
        }
        "store" if args.len() == 3 => {
            let rs2 = parse_reg(&args[0])?;
            let rs1 = parse_reg(&args[1])?;
            let imm = parse_imm(&args[2], globals)?;
            Ok(Inst::STORE(rs2, rs1, imm))
        }

        // jumpif(cond, label)
        "jumpif" if args.len() == 2 => {
            let cond = parse_reg(&args[0])?;
            match &args[1] {
                ast::Expr::Ident(label) => match locals.get(label.as_str()) {
                    // Local label: Compiled to relative jump
                    Some(&goto) => {
                        let offset = (goto as i32 - pc as i32) as u16;
                        Ok(Inst::JUMPIFR(cond, Imm::Literal(offset)))
                    }
                    // Global label: Compiled to absolute jump
                    None => Ok(Inst::JUMPIF(cond, Imm::Symbol(label.clone(), 0))),
                },
                _ => Err(Error::TODO),
            }
        }

        // jump(label)
        "jump" if args.len() == 1 => {
            match &args[0] {
                ast::Expr::Ident(label) => match locals.get(label.as_str()) {
                    // Local label: Compiled to relative jump
                    Some(&goto) => {
                        let offset = (goto as i32 - pc as i32) as u16;
                        Ok(Inst::JUMPR(Imm::Literal(offset)))
                    }
                    // Global label: Compiled to absolute jump
                    None => Ok(Inst::JUMP(Imm::Symbol(label.clone(), 0))),
                },
                _ => Err(Error::TODO),
            }
        }

        // call(label)
        "call" if args.len() == 1 => match &args[0] {
            // Global label only
            ast::Expr::Ident(label) => Ok(Inst::CALL(Imm::Symbol(label.clone(), 0))),
            _ => Err(Error::TODO),
        },
        "ret" if args.is_empty() => Ok(Inst::RET()),
        "iret" if args.is_empty() => Ok(Inst::IRET()),
        _ => Err(Error::InvalidInstruction(inst.to_string())),
    }
}

fn parse_reg(expr: &ast::Expr) -> Result<Reg, Error> {
    if let ast::Expr::Ident(name) = expr {
        match name.to_lowercase().as_str() {
            "z" | "zero" => Ok(Reg::Z),
            "sp" => Ok(Reg::SP),
            "ra" => Ok(Reg::RA),
            "fp" => Ok(Reg::FP),
            "a0" => Ok(Reg::A0),
            "a1" => Ok(Reg::A1),
            "t0" => Ok(Reg::T0),
            "t1" => Ok(Reg::T1),
            "t2" => Ok(Reg::T2),
            "t3" => Ok(Reg::T3),
            "s0" => Ok(Reg::S0),
            "s1" => Ok(Reg::S1),
            "s2" => Ok(Reg::S2),
            "s3" => Ok(Reg::S3),
            _ => Err(Error::InvalidRegister(name.clone())),
        }
    } else {
        Err(Error::InvalidRegister(format!("{:?}", expr)))
    }
}

fn parse_imm<'a>(expr: &'a ast::Expr, globals: &'a Global<'a>) -> Result<Imm, Error> {
    // Check type size only for complex expressions that need evaluation
    // Simple literals and identifiers don't need type checking
    match expr {
        ast::Expr::NumberLit(n) => Ok(Imm::Literal(*n as u16)),
        ast::Expr::CharLit(ch) => Ok(Imm::Literal(*ch as u16)),
        ast::Expr::Ident(name) => Ok(Imm::Symbol(name.clone(), 0)),
        ast::Expr::Unary(op, inner) => match op {
            ast::UnaryOp::Pos => parse_imm(inner, globals),
            ast::UnaryOp::Neg => match parse_imm(inner, globals) {
                Ok(Imm::Literal(val)) => Ok(Imm::Literal((-(val as i16)) as u16)),
                _ => Err(Error::CannotNegateSymbol),
            },
            ast::UnaryOp::Not => todo!(),
        },
        ast::Expr::Addr(inner) => parse_imm(inner, globals),
        ast::Expr::Deref(_) => Err(Error::CannotDereferenceInAssembly),
        ast::Expr::Member(expr, field) => match parse_imm(expr, globals)? {
            Imm::Symbol(ident, base) => {
                let offset = if let Some((norm_type, _, _)) = globals.statics().get(ident.as_str())
                {
                    norm_type
                        .get_field_offset(field)
                        .ok_or_else(|| Error::FieldNotFoundInStruct(field.clone()))?
                } else if let Some((norm_type, _, _, _)) = globals.consts().get(ident.as_str()) {
                    norm_type
                        .get_field_offset(field)
                        .ok_or_else(|| Error::FieldNotFoundInStruct(field.clone()))?
                } else {
                    return Err(Error::UnknownSymbol(ident));
                };
                Ok(Imm::Symbol(ident, base + offset))
            }
            Imm::Literal(_) => Err(Error::CannotAccessFieldOfImmediate),
        },

        ast::Expr::Index(expr, index) => {
            match parse_imm(expr, globals)? {
                Imm::Symbol(ident, base) => {
                    // Try to evaluate index to a constant
                    if let ast::Expr::NumberLit(idx) = index.as_ref() {
                        // Look up the type of the base symbol to calculate offset
                        let offset = if let Some((norm_type, _, _)) =
                            globals.statics().get(ident.as_str())
                        {
                            norm_type
                                .get_array_offset(*idx)
                                .ok_or(Error::TypeIsNotArray)?
                        } else if let Some((norm_type, value, _, _)) =
                            globals.consts().get(ident.as_str())
                        {
                            // For string constants, elements are chars (size 1)
                            if matches!(value, crate::eval::constexpr::ConstExpr::String(_)) {
                                idx * 1
                            } else {
                                norm_type
                                    .get_array_offset(*idx)
                                    .ok_or(Error::TypeIsNotArray)?
                            }
                        } else {
                            return Err(Error::UnknownSymbol(ident));
                        };

                        Ok(Imm::Symbol(ident, base + offset))
                    } else {
                        Err(Error::NonConstantArrayIndex)
                    }
                }
                Imm::Literal(_) => Err(Error::CannotIndexImmediate),
            }
        }

        ast::Expr::Binary(op, left, right) => {
            let lhs = parse_imm(left, globals)?;
            let rhs = parse_imm(right, globals)?;
            match (lhs, rhs) {
                (Imm::Symbol(ident, left_offset), Imm::Literal(right_val)) => match op {
                    ast::BinaryOp::Add => Ok(Imm::Symbol(ident, left_offset + right_val as usize)),
                    ast::BinaryOp::Sub => Ok(Imm::Symbol(
                        ident,
                        left_offset.wrapping_sub(right_val as usize),
                    )),
                    _ => Err(Error::UnsupportedOperationInAddress),
                },
                (Imm::Literal(left_val), Imm::Symbol(ident, right_offset)) => match op {
                    ast::BinaryOp::Add => Ok(Imm::Symbol(ident, left_val as usize + right_offset)),
                    _ => Err(Error::InvalidSubtractionInAddress),
                },
                (Imm::Literal(left_val), Imm::Literal(right_val)) => match op {
                    ast::BinaryOp::Add => Ok(Imm::Literal(left_val.wrapping_add(right_val))),
                    ast::BinaryOp::Sub => Ok(Imm::Literal(left_val.wrapping_sub(right_val))),
                    _ => Err(Error::UnsupportedOperationInAddress),
                },
                (Imm::Symbol(_, _), Imm::Symbol(_, _)) => match op {
                    ast::BinaryOp::Add => Err(Error::CannotAddSymbols),
                    _ => Err(Error::InvalidSubtractionInAddress),
                },
            }
        }

        ast::Expr::SizeofType(ty) => match globals.normtype(ty) {
            Ok(ty) => Ok(Imm::Literal(ty.sizeof() as u16)),
            Err(e) => Err(Error::CannotEvaluateSizeofType(e.to_string())),
        },

        ast::Expr::SizeofExpr(inner) => match globals.typeinfer(inner) {
            Ok(ty) => Ok(Imm::Literal(ty.sizeof() as u16)),
            Err(e) => Err(Error::CannotEvaluateSizeofExpr(e.to_string())),
        },

        _ => Err(Error::UnsupportedExprType(format!("{:?}", expr))),
    }
}
