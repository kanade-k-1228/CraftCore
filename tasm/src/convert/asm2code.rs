use crate::{
    convert::types::{Code, Immidiate},
    error::AsmError,
    eval::{eval::Evaluator, normtype::NormType},
    grammer::ast,
};
use arch::{inst::Inst, reg::Reg};
use std::collections::HashMap;

pub fn asm2code<'a>(evaluator: &'a Evaluator<'a>) -> Result<HashMap<&'a str, Code>, AsmError> {
    let mut result = HashMap::new();
    for (&name, entry) in evaluator.asms() {
        if let ast::Def::Asm(_, _, stmts) = entry.def {
            result.insert(name, gen_asm_block(stmts, evaluator)?);
        }
    }
    Ok(result)
}

fn gen_asm_block(stmts: &[ast::AsmStmt], evaluator: &Evaluator) -> Result<Code, AsmError> {
    let mut labels: HashMap<&str, usize> = HashMap::new();
    for (pc, ast::AsmStmt(_, _, labs)) in stmts.iter().enumerate() {
        for label in labs {
            labels.insert(label.as_str(), pc);
        }
    }

    let mut insts = Vec::new();
    for (pc, ast::AsmStmt(inst, args, _)) in stmts.iter().enumerate() {
        let inst = parse_instruction(pc as u16, inst, args, &labels, evaluator)?;
        insts.push(inst);
    }

    Ok(Code(insts))
}

fn parse_instruction(
    pc: u16,
    inst: &str,
    args: &[ast::Expr],
    labels: &HashMap<&str, usize>,
    evaluator: &Evaluator,
) -> Result<(Inst, Option<Immidiate>), AsmError> {
    match inst.to_lowercase().as_str() {
        "nop" => Ok((Inst::NOP(), None)),
        "mov" if args.len() == 2 => {
            let rd = parse_register(&args[0])?;
            let rs = parse_register(&args[1])?;
            Ok((Inst::MOV(rd, rs), None))
        }
        "add" if args.len() == 3 => {
            let rd = parse_register(&args[0])?;
            let rs1 = parse_register(&args[1])?;
            let rs2 = parse_register(&args[2])?;
            Ok((Inst::ADD(rd, rs1, rs2), None))
        }
        "addi" if args.len() == 3 => {
            let rd = parse_register(&args[0])?;
            let rs = parse_register(&args[1])?;
            match parse_immidiate_expr(&args[2], evaluator)? {
                Immidiate::Literal(val) => Ok((Inst::ADDI(rd, rs, val), None)),
                Immidiate::Symbol(name, offset) => Ok((
                    Inst::ADDI(rd, rs, offset as u16),
                    Some(Immidiate::Symbol(name, offset)),
                )),
            }
        }
        "subi" if args.len() == 3 => {
            let rd = parse_register(&args[0])?;
            let rs = parse_register(&args[1])?;
            match parse_immidiate_expr(&args[2], evaluator)? {
                Immidiate::Literal(val) => Ok((Inst::SUBI(rd, rs, val), None)),
                Immidiate::Symbol(name, offset) => Ok((
                    Inst::SUBI(rd, rs, offset as u16),
                    Some(Immidiate::Symbol(name, offset)),
                )),
            }
        }
        "andi" if args.len() == 3 => {
            let rd = parse_register(&args[0])?;
            let rs = parse_register(&args[1])?;
            match parse_immidiate_expr(&args[2], evaluator)? {
                Immidiate::Literal(val) => Ok((Inst::ANDI(rd, rs, val), None)),
                Immidiate::Symbol(name, offset) => Ok((
                    Inst::ANDI(rd, rs, offset as u16),
                    Some(Immidiate::Symbol(name, offset)),
                )),
            }
        }
        "ori" if args.len() == 3 => {
            let rd = parse_register(&args[0])?;
            let rs = parse_register(&args[1])?;
            match parse_immidiate_expr(&args[2], evaluator)? {
                Immidiate::Literal(val) => Ok((Inst::ORI(rd, rs, val), None)),
                Immidiate::Symbol(name, offset) => Ok((
                    Inst::ORI(rd, rs, offset as u16),
                    Some(Immidiate::Symbol(name, offset)),
                )),
            }
        }
        "xori" if args.len() == 3 => {
            let rd = parse_register(&args[0])?;
            let rs = parse_register(&args[1])?;
            match parse_immidiate_expr(&args[2], evaluator)? {
                Immidiate::Literal(val) => Ok((Inst::XORI(rd, rs, val), None)),
                Immidiate::Symbol(name, offset) => Ok((
                    Inst::XORI(rd, rs, offset as u16),
                    Some(Immidiate::Symbol(name, offset)),
                )),
            }
        }
        "eqi" if args.len() == 3 => {
            let rd = parse_register(&args[0])?;
            let rs = parse_register(&args[1])?;
            match parse_immidiate_expr(&args[2], evaluator)? {
                Immidiate::Literal(val) => Ok((Inst::EQI(rd, rs, val), None)),
                Immidiate::Symbol(name, offset) => Ok((
                    Inst::EQI(rd, rs, offset as u16),
                    Some(Immidiate::Symbol(name, offset)),
                )),
            }
        }
        "neqi" if args.len() == 3 => {
            let rd = parse_register(&args[0])?;
            let rs = parse_register(&args[1])?;
            match parse_immidiate_expr(&args[2], evaluator)? {
                Immidiate::Literal(val) => Ok((Inst::NEQI(rd, rs, val), None)),
                Immidiate::Symbol(name, offset) => Ok((
                    Inst::NEQI(rd, rs, offset as u16),
                    Some(Immidiate::Symbol(name, offset)),
                )),
            }
        }
        "lti" if args.len() == 3 => {
            let rd = parse_register(&args[0])?;
            let rs = parse_register(&args[1])?;
            match parse_immidiate_expr(&args[2], evaluator)? {
                Immidiate::Literal(val) => Ok((Inst::LTI(rd, rs, val), None)),
                Immidiate::Symbol(name, offset) => Ok((
                    Inst::LTI(rd, rs, offset as u16),
                    Some(Immidiate::Symbol(name, offset)),
                )),
            }
        }
        "ltsi" if args.len() == 3 => {
            let rd = parse_register(&args[0])?;
            let rs = parse_register(&args[1])?;
            match parse_immidiate_expr(&args[2], evaluator)? {
                Immidiate::Literal(val) => Ok((Inst::LTSI(rd, rs, val), None)),
                Immidiate::Symbol(name, offset) => Ok((
                    Inst::LTSI(rd, rs, offset as u16),
                    Some(Immidiate::Symbol(name, offset)),
                )),
            }
        }
        "not" if args.len() == 2 => {
            let rd = parse_register(&args[0])?;
            let rs = parse_register(&args[1])?;
            Ok((Inst::NOT(rd, rs), None))
        }
        "loadi" if args.len() == 2 => {
            let rd = parse_register(&args[0])?;
            // Use the new expression evaluator
            match parse_immidiate_expr(&args[1], evaluator)? {
                Immidiate::Literal(val) => Ok((Inst::LOADI(rd, val), None)),
                Immidiate::Symbol(name, offset) => Ok((
                    Inst::LOADI(rd, offset as u16),
                    Some(Immidiate::Symbol(name, offset)),
                )),
            }
        }
        "sub" if args.len() == 3 => {
            let rd = parse_register(&args[0])?;
            let rs1 = parse_register(&args[1])?;
            let rs2 = parse_register(&args[2])?;
            Ok((Inst::SUB(rd, rs1, rs2), None))
        }
        "and" if args.len() == 3 => {
            let rd = parse_register(&args[0])?;
            let rs1 = parse_register(&args[1])?;
            let rs2 = parse_register(&args[2])?;
            Ok((Inst::AND(rd, rs1, rs2), None))
        }
        "or" if args.len() == 3 => {
            let rd = parse_register(&args[0])?;
            let rs1 = parse_register(&args[1])?;
            let rs2 = parse_register(&args[2])?;
            Ok((Inst::OR(rd, rs1, rs2), None))
        }
        "xor" if args.len() == 3 => {
            let rd = parse_register(&args[0])?;
            let rs1 = parse_register(&args[1])?;
            let rs2 = parse_register(&args[2])?;
            Ok((Inst::XOR(rd, rs1, rs2), None))
        }
        "eq" if args.len() == 3 => {
            let rd = parse_register(&args[0])?;
            let rs1 = parse_register(&args[1])?;
            let rs2 = parse_register(&args[2])?;
            Ok((Inst::EQ(rd, rs1, rs2), None))
        }
        "neq" if args.len() == 3 => {
            let rd = parse_register(&args[0])?;
            let rs1 = parse_register(&args[1])?;
            let rs2 = parse_register(&args[2])?;
            Ok((Inst::NEQ(rd, rs1, rs2), None))
        }
        "lt" if args.len() == 3 => {
            let rd = parse_register(&args[0])?;
            let rs1 = parse_register(&args[1])?;
            let rs2 = parse_register(&args[2])?;
            Ok((Inst::LT(rd, rs1, rs2), None))
        }
        "lts" if args.len() == 3 => {
            let rd = parse_register(&args[0])?;
            let rs1 = parse_register(&args[1])?;
            let rs2 = parse_register(&args[2])?;
            Ok((Inst::LTS(rd, rs1, rs2), None))
        }
        "sr" if args.len() == 2 => {
            let rd = parse_register(&args[0])?;
            let rs = parse_register(&args[1])?;
            Ok((Inst::SR(rd, rs), None))
        }
        "srs" if args.len() == 2 => {
            let rd = parse_register(&args[0])?;
            let rs = parse_register(&args[1])?;
            Ok((Inst::SRS(rd, rs), None))
        }
        "srr" if args.len() == 2 => {
            let rd = parse_register(&args[0])?;
            let rs = parse_register(&args[1])?;
            Ok((Inst::SRR(rd, rs), None))
        }
        "sl" if args.len() == 2 => {
            let rd = parse_register(&args[0])?;
            let rs = parse_register(&args[1])?;
            Ok((Inst::SL(rd, rs), None))
        }
        "slr" if args.len() == 2 => {
            let rd = parse_register(&args[0])?;
            let rs = parse_register(&args[1])?;
            Ok((Inst::SLR(rd, rs), None))
        }
        "load" if args.len() == 2 => {
            // load(rd, addr/imm)
            let rd = parse_register(&args[0])?;
            // Use expression evaluator for complex expressions
            match parse_immidiate_expr(&args[1], evaluator)? {
                Immidiate::Literal(val) => Ok((Inst::LOADI(rd, val), None)),
                Immidiate::Symbol(name, offset) => Ok((
                    Inst::LOADI(rd, offset as u16),
                    Some(Immidiate::Symbol(name, offset)),
                )),
            }
        }
        "load" if args.len() == 3 => {
            let rd = parse_register(&args[0])?;
            let rs = parse_register(&args[1])?;
            match parse_immidiate_expr(&args[2], evaluator)? {
                Immidiate::Literal(val) => Ok((Inst::LOAD(rd, rs, val), None)),
                Immidiate::Symbol(name, offset) => Ok((
                    Inst::LOAD(rd, rs, offset as u16),
                    Some(Immidiate::Symbol(name, offset)),
                )),
            }
        }
        "store" if args.len() == 2 => {
            // store(symbol, rs)
            let rs = parse_register(&args[1])?;
            match parse_immidiate_expr(&args[0], evaluator)? {
                Immidiate::Literal(val) => Ok((Inst::STORE(rs, Reg::Z, val), None)),
                Immidiate::Symbol(name, offset) => Ok((
                    Inst::STORE(rs, Reg::Z, offset as u16),
                    Some(Immidiate::Symbol(name, offset)),
                )),
            }
        }
        "store" if args.len() == 3 => {
            let rs2 = parse_register(&args[0])?;
            let rs1 = parse_register(&args[1])?;
            match parse_immidiate_expr(&args[2], evaluator)? {
                Immidiate::Literal(val) => Ok((Inst::STORE(rs2, rs1, val), None)),
                Immidiate::Symbol(name, offset) => Ok((
                    Inst::STORE(rs2, rs1, offset as u16),
                    Some(Immidiate::Symbol(name, offset)),
                )),
            }
        }
        "if" if args.len() == 2 => {
            // if(cond_reg, label/offset)
            let cond = parse_register(&args[0])?;

            // Check if it's a local label first
            if let ast::Expr::Ident(label_name) = &args[1] {
                if let Some(&label_pc) = labels.get(label_name.as_str()) {
                    // Convert to relative jump
                    let offset = (label_pc as i32 - pc as i32) as u16;
                    return Ok((Inst::IFR(cond, offset), None));
                }
            }

            // Check for negative immediate (relative jump)
            if let ast::Expr::Unary(ast::UnaryOp::Neg, inner) = &args[1] {
                if let Some(imm) = parse_immediate(inner) {
                    let offset = (-(imm as i16)) as u16;
                    return Ok((Inst::IFR(cond, offset), None));
                }
            }

            // Use expression evaluator for other cases
            match parse_immidiate_expr(&args[1], evaluator)? {
                Immidiate::Literal(val) => Ok((Inst::IF(cond, val), None)),
                Immidiate::Symbol(name, offset) => Ok((
                    Inst::IF(cond, offset as u16),
                    Some(Immidiate::Symbol(name, offset)),
                )),
            }
        }
        "jump" if args.len() == 1 => {
            // Check if it's a local label first
            if let ast::Expr::Ident(label_name) = &args[0] {
                if let Some(&label_pc) = labels.get(label_name.as_str()) {
                    // Convert to relative jump
                    let offset = (label_pc as i32 - pc as i32) as u16;
                    return Ok((Inst::JUMPR(offset), None));
                }
            }

            // Use expression evaluator for other cases
            match parse_immidiate_expr(&args[0], evaluator)? {
                Immidiate::Literal(val) => Ok((Inst::JUMP(val), None)),
                Immidiate::Symbol(name, offset) => Ok((
                    Inst::JUMP(offset as u16),
                    Some(Immidiate::Symbol(name, offset)),
                )),
            }
        }
        "call" if args.len() == 1 => {
            // Use expression evaluator
            match parse_immidiate_expr(&args[0], evaluator)? {
                Immidiate::Literal(val) => Ok((Inst::CALL(val), None)),
                Immidiate::Symbol(name, offset) => Ok((
                    Inst::CALL(offset as u16),
                    Some(Immidiate::Symbol(name, offset)),
                )),
            }
        }
        "ret" if args.is_empty() => Ok((Inst::RET(), None)),
        "ifr" if args.len() == 2 => {
            let cond = parse_register(&args[0])?;

            // Check if it's a local label first
            if let ast::Expr::Ident(label_name) = &args[1] {
                if let Some(&label_pc) = labels.get(label_name.as_str()) {
                    // Calculate relative offset
                    let offset = (label_pc as i32 - pc as i32) as u16;
                    return Ok((Inst::IFR(cond, offset), None));
                }
            }

            // Use expression evaluator for other cases
            match parse_immidiate_expr(&args[1], evaluator)? {
                Immidiate::Literal(val) => Ok((Inst::IFR(cond, val), None)),
                Immidiate::Symbol(name, offset) => Ok((
                    Inst::IFR(cond, offset as u16),
                    Some(Immidiate::Symbol(name, offset)),
                )),
            }
        }
        "jumpr" if args.len() == 1 => {
            // Check if it's a local label first
            if let ast::Expr::Ident(label_name) = &args[0] {
                if let Some(&label_pc) = labels.get(label_name.as_str()) {
                    // Calculate relative offset
                    let offset = (label_pc as i32 - pc as i32) as u16;
                    return Ok((Inst::JUMPR(offset), None));
                }
            }

            // Use expression evaluator for other cases
            match parse_immidiate_expr(&args[0], evaluator)? {
                Immidiate::Literal(val) => Ok((Inst::JUMPR(val), None)),
                Immidiate::Symbol(name, offset) => Ok((
                    Inst::JUMPR(offset as u16),
                    Some(Immidiate::Symbol(name, offset)),
                )),
            }
        }
        "iret" if args.is_empty() => Ok((Inst::IRET(), None)),
        _ => Err(AsmError::InvalidInstruction(inst.to_string())),
    }
}

/// Parse a register from an expression
fn parse_register(expr: &ast::Expr) -> Result<Reg, AsmError> {
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
            _ => Err(AsmError::InvalidRegister(name.clone())),
        }
    } else {
        Err(AsmError::InvalidRegister(format!("{:?}", expr)))
    }
}

/// Parse an immediate value from an expression
fn parse_immediate(expr: &ast::Expr) -> Option<u16> {
    match expr {
        ast::Expr::NumberLit(n) => Some(*n as u16),
        ast::Expr::CharLit(ch) => Some(*ch as u16),
        _ => None,
    }
}

/// Parse an expression into an immediate value
fn parse_immidiate_expr(expr: &ast::Expr, evaluator: &Evaluator) -> Result<Immidiate, AsmError> {
    match expr {
        ast::Expr::NumberLit(n) => Ok(Immidiate::Literal(*n as u16)),
        ast::Expr::CharLit(ch) => Ok(Immidiate::Literal(*ch as u16)),
        ast::Expr::Ident(name) => Ok(Immidiate::Symbol(name.clone(), 0)),
        ast::Expr::Unary(op, inner) => match op {
            ast::UnaryOp::Pos => parse_immidiate_expr(inner, evaluator),
            ast::UnaryOp::Neg => {
                // Handle negative numbers
                if let Ok(Immidiate::Literal(val)) = parse_immidiate_expr(inner, evaluator) {
                    Ok(Immidiate::Literal((-(val as i16)) as u16))
                } else {
                    Err(AsmError::InvalidOperandType(
                        "Cannot negate symbol".to_string(),
                    ))
                }
            }
            ast::UnaryOp::Not => todo!(),
        },
        ast::Expr::Addr(inner) => parse_immidiate_expr(inner, evaluator),
        ast::Expr::Deref(_) => Err(AsmError::InvalidOperandType(
            "Dereference operations cannot be evaluated at assembly time".to_string(),
        )),

        ast::Expr::Member(base, field) => {
            match parse_immidiate_expr(base, evaluator)? {
                Immidiate::Symbol(ident, offset) => {
                    // Look up the type of the base symbol to calculate field offset
                    let field_offset = if let Some(entry) = evaluator.statics().get(ident.as_str())
                    {
                        get_field_offset_from_type(&entry.norm_type, field)?
                    } else if let Some(entry) = evaluator.consts().get(ident.as_str()) {
                        get_field_offset_from_type(&entry.norm_type, field)?
                    } else {
                        return Err(AsmError::InvalidOperandType(format!(
                            "Unknown symbol: {}",
                            ident
                        )));
                    };

                    Ok(Immidiate::Symbol(ident, offset + field_offset))
                }
                Immidiate::Literal(_) => Err(AsmError::InvalidOperandType(
                    "Cannot access field of immediate value".to_string(),
                )),
            }
        }

        ast::Expr::Index(base, index) => {
            match parse_immidiate_expr(base, evaluator)? {
                Immidiate::Symbol(ident, ofset) => {
                    // Try to evaluate index to a constant
                    if let ast::Expr::NumberLit(idx) = index.as_ref() {
                        // Look up the type of the base symbol to calculate element size
                        let elem_size = if let Some(entry) = evaluator.statics().get(ident.as_str())
                        {
                            get_array_element_size_from_type(&entry.norm_type)?
                        } else if let Some(entry) = evaluator.consts().get(ident.as_str()) {
                            // For string constants, elements are chars (size 1)
                            if matches!(entry.value, crate::eval::constexpr::ConstExpr::String(_)) {
                                1
                            } else {
                                get_array_element_size_from_type(&entry.norm_type)?
                            }
                        } else {
                            return Err(AsmError::InvalidOperandType(format!(
                                "Unknown symbol: {}",
                                ident
                            )));
                        };

                        Ok(Immidiate::Symbol(ident, ofset + idx * elem_size))
                    } else {
                        Err(AsmError::InvalidOperandType(
                            "Array index must be a constant in assembly".to_string(),
                        ))
                    }
                }
                Immidiate::Literal(_) => Err(AsmError::InvalidOperandType(
                    "Cannot index immediate value".to_string(),
                )),
            }
        }

        ast::Expr::Binary(op, left, right) => {
            let lhs = parse_immidiate_expr(left, evaluator)?;
            let rhs = parse_immidiate_expr(right, evaluator)?;
            match op {
                ast::BinaryOp::Add => match (lhs, rhs) {
                    (Immidiate::Symbol(ident, left), Immidiate::Literal(right)) => {
                        Ok(Immidiate::Symbol(ident, left + right as usize))
                    }
                    (Immidiate::Literal(left_val), Immidiate::Symbol(ident, right_offset)) => {
                        Ok(Immidiate::Symbol(ident, left_val as usize + right_offset))
                    }
                    (Immidiate::Literal(left_val), Immidiate::Literal(right_val)) => {
                        Ok(Immidiate::Literal(left_val.wrapping_add(right_val)))
                    }
                    _ => Err(AsmError::InvalidOperandType(
                        "Cannot add two symbols".to_string(),
                    )),
                },
                ast::BinaryOp::Sub => match (lhs, rhs) {
                    (Immidiate::Symbol(ident, left_offset), Immidiate::Literal(right_val)) => Ok(
                        Immidiate::Symbol(ident, left_offset.wrapping_sub(right_val as usize)),
                    ),
                    (Immidiate::Literal(left_val), Immidiate::Literal(right_val)) => {
                        Ok(Immidiate::Literal(left_val.wrapping_sub(right_val)))
                    }
                    _ => Err(AsmError::InvalidOperandType(
                        "Invalid subtraction in address expression".to_string(),
                    )),
                },
                _ => Err(AsmError::InvalidOperandType(
                    "Unsupported operation in address expression".to_string(),
                )),
            }
        }

        // Handle sizeof<type>
        ast::Expr::SizeofType(ty) => {
            // Evaluate the type's size at compile time
            match evaluator.normtype(ty) {
                Ok(norm_ty) => Ok(Immidiate::Literal(norm_ty.sizeof() as u16)),
                Err(e) => Err(AsmError::InvalidOperandType(format!(
                    "Cannot evaluate sizeof type: {}",
                    e
                ))),
            }
        }

        // Handle sizeof(expr)
        ast::Expr::SizeofExpr(inner) => {
            // Evaluate the expression's type size at compile time
            match evaluator.typeinfer(inner) {
                Ok(norm_ty) => Ok(Immidiate::Literal(norm_ty.sizeof() as u16)),
                Err(e) => Err(AsmError::InvalidOperandType(format!(
                    "Cannot evaluate sizeof expression: {}",
                    e
                ))),
            }
        }

        _ => Err(AsmError::InvalidOperandType(format!(
            "Unsupported expression type in assembly: {:?}",
            expr
        ))),
    }
}

/// Helper function to get field offset from a NormType
fn get_field_offset_from_type(ty: &NormType, field_name: &str) -> Result<usize, AsmError> {
    if let NormType::Struct(fields) = ty {
        let mut offset = 0;
        for (name, field_ty) in fields {
            if name == field_name {
                return Ok(offset);
            }
            offset += field_ty.sizeof();
        }
        Err(AsmError::InvalidOperandType(format!(
            "Field '{}' not found in struct",
            field_name
        )))
    } else {
        Err(AsmError::InvalidOperandType(
            "Type is not a struct".to_string(),
        ))
    }
}

/// Helper function to get array element size from a NormType
fn get_array_element_size_from_type(ty: &NormType) -> Result<usize, AsmError> {
    if let NormType::Array(_, elem_ty) = ty {
        Ok(elem_ty.sizeof())
    } else {
        Err(AsmError::InvalidOperandType(
            "Type is not an array".to_string(),
        ))
    }
}
