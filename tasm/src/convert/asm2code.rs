use crate::{
    collect::{AsmMap, ConstMap},
    convert::types::Code,
    grammer::ast,
    link::structs::{AsmInst, AsmLine},
};
use arch::{inst::Inst, reg::Reg};
use std::collections::HashMap;

/// Generate code from all assembly blocks in the AST
pub fn asm2code(ast: &ast::AST, _asms: &AsmMap, _consts: &ConstMap) -> HashMap<String, Code> {
    let mut result = HashMap::new();

    // Process each assembly block definition in the AST
    let ast::AST(defs) = ast;
    for def in defs {
        if let ast::Def::Asm(asm_name, _addr, body) = def {
            // Generate instructions for this assembly block
            let instructions = generate_asm_block(asm_name.clone(), body);
            let code = Code::new_asm_block(asm_name.clone(), instructions);
            result.insert(asm_name.clone(), code);
        }
    }

    result
}

/// Generate instructions from an assembly block
fn generate_asm_block(asm_name: String, body: &[ast::AsmStmt]) -> Vec<AsmLine> {
    let mut instructions = Vec::new();

    // Add entry label for the assembly block if no labels in body
    // (for backward compatibility and to ensure block has an entry point)
    let has_labels = body
        .iter()
        .any(|stmt| matches!(stmt, ast::AsmStmt::Label(_)));
    if !has_labels {
        instructions.push(AsmLine {
            inst: AsmInst::Label(asm_name.clone()),
            symbols: Vec::new(),
        });
    }

    // Parse the assembly statements
    for stmt in body {
        compile_asm_stmt(&mut instructions, stmt);
    }

    instructions
}

/// Compile a single assembly statement
fn compile_asm_stmt(instructions: &mut Vec<AsmLine>, stmt: &ast::AsmStmt) {
    match stmt {
        ast::AsmStmt::Label(name) => {
            // Add label
            instructions.push(AsmLine {
                inst: AsmInst::Label(name.clone()),
                symbols: Vec::new(),
            });
        }
        ast::AsmStmt::Inst(name, args) => {
            // Parse instruction
            if let Some(inst) = parse_instruction(name, args) {
                instructions.push(inst);
            }
        }
    }
}

/// Parse a specific instruction with its arguments
fn parse_instruction(inst_name: &str, args: &[ast::Expr]) -> Option<AsmLine> {
    // This is a simplified parser - in reality, you'd need more sophisticated parsing
    let inst = match inst_name.to_lowercase().as_str() {
        "nop" => Some(Inst::NOP()),
        "mov" if args.len() == 2 => {
            let rd = parse_register(&args[0])?;
            let rs = parse_register(&args[1])?;
            Some(Inst::MOV(rd, rs))
        }
        "add" if args.len() == 3 => {
            let rd = parse_register(&args[0])?;
            let rs1 = parse_register(&args[1])?;
            let rs2 = parse_register(&args[2])?;
            Some(Inst::ADD(rd, rs1, rs2))
        }
        "addi" if args.len() == 3 => {
            let rd = parse_register(&args[0])?;
            let rs = parse_register(&args[1])?;
            if let Some(imm) = parse_immediate(&args[2]) {
                Some(Inst::ADDI(rd, rs, imm))
            } else if let ast::Expr::Ident(symbol) = &args[2] {
                return Some(AsmLine {
                    inst: AsmInst::Inst(Inst::ADDI(rd, rs, 0)),
                    symbols: vec![symbol.clone()],
                });
            } else {
                None
            }
        }
        "subi" if args.len() == 3 => {
            let rd = parse_register(&args[0])?;
            let rs = parse_register(&args[1])?;
            if let Some(imm) = parse_immediate(&args[2]) {
                Some(Inst::SUBI(rd, rs, imm))
            } else if let ast::Expr::Ident(symbol) = &args[2] {
                return Some(AsmLine {
                    inst: AsmInst::Inst(Inst::SUBI(rd, rs, 0)),
                    symbols: vec![symbol.clone()],
                });
            } else {
                None
            }
        }
        "andi" if args.len() == 3 => {
            let rd = parse_register(&args[0])?;
            let rs = parse_register(&args[1])?;
            if let Some(imm) = parse_immediate(&args[2]) {
                Some(Inst::ANDI(rd, rs, imm))
            } else if let ast::Expr::Ident(symbol) = &args[2] {
                return Some(AsmLine {
                    inst: AsmInst::Inst(Inst::ANDI(rd, rs, 0)),
                    symbols: vec![symbol.clone()],
                });
            } else {
                None
            }
        }
        "ori" if args.len() == 3 => {
            let rd = parse_register(&args[0])?;
            let rs = parse_register(&args[1])?;
            if let Some(imm) = parse_immediate(&args[2]) {
                Some(Inst::ORI(rd, rs, imm))
            } else if let ast::Expr::Ident(symbol) = &args[2] {
                return Some(AsmLine {
                    inst: AsmInst::Inst(Inst::ORI(rd, rs, 0)),
                    symbols: vec![symbol.clone()],
                });
            } else {
                None
            }
        }
        "xori" if args.len() == 3 => {
            let rd = parse_register(&args[0])?;
            let rs = parse_register(&args[1])?;
            if let Some(imm) = parse_immediate(&args[2]) {
                Some(Inst::XORI(rd, rs, imm))
            } else if let ast::Expr::Ident(symbol) = &args[2] {
                return Some(AsmLine {
                    inst: AsmInst::Inst(Inst::XORI(rd, rs, 0)),
                    symbols: vec![symbol.clone()],
                });
            } else {
                None
            }
        }
        "eqi" if args.len() == 3 => {
            let rd = parse_register(&args[0])?;
            let rs = parse_register(&args[1])?;
            if let Some(imm) = parse_immediate(&args[2]) {
                Some(Inst::EQI(rd, rs, imm))
            } else if let ast::Expr::Ident(symbol) = &args[2] {
                return Some(AsmLine {
                    inst: AsmInst::Inst(Inst::EQI(rd, rs, 0)),
                    symbols: vec![symbol.clone()],
                });
            } else {
                None
            }
        }
        "neqi" if args.len() == 3 => {
            let rd = parse_register(&args[0])?;
            let rs = parse_register(&args[1])?;
            if let Some(imm) = parse_immediate(&args[2]) {
                Some(Inst::NEQI(rd, rs, imm))
            } else if let ast::Expr::Ident(symbol) = &args[2] {
                return Some(AsmLine {
                    inst: AsmInst::Inst(Inst::NEQI(rd, rs, 0)),
                    symbols: vec![symbol.clone()],
                });
            } else {
                None
            }
        }
        "lti" if args.len() == 3 => {
            let rd = parse_register(&args[0])?;
            let rs = parse_register(&args[1])?;
            if let Some(imm) = parse_immediate(&args[2]) {
                Some(Inst::LTI(rd, rs, imm))
            } else if let ast::Expr::Ident(symbol) = &args[2] {
                return Some(AsmLine {
                    inst: AsmInst::Inst(Inst::LTI(rd, rs, 0)),
                    symbols: vec![symbol.clone()],
                });
            } else {
                None
            }
        }
        "ltsi" if args.len() == 3 => {
            let rd = parse_register(&args[0])?;
            let rs = parse_register(&args[1])?;
            if let Some(imm) = parse_immediate(&args[2]) {
                Some(Inst::LTSI(rd, rs, imm))
            } else if let ast::Expr::Ident(symbol) = &args[2] {
                return Some(AsmLine {
                    inst: AsmInst::Inst(Inst::LTSI(rd, rs, 0)),
                    symbols: vec![symbol.clone()],
                });
            } else {
                None
            }
        }
        "not" if args.len() == 2 => {
            let rd = parse_register(&args[0])?;
            let rs = parse_register(&args[1])?;
            Some(Inst::NOT(rd, rs))
        }
        "loadi" if args.len() == 2 => {
            let rd = parse_register(&args[0])?;
            if let Some(imm) = parse_immediate(&args[1]) {
                Some(Inst::LOADI(rd, imm))
            } else if let ast::Expr::Ident(symbol) = &args[1] {
                // Symbol reference - will be resolved later
                return Some(AsmLine {
                    inst: AsmInst::Inst(Inst::LOADI(rd, 0)),
                    symbols: vec![symbol.clone()],
                });
            } else {
                None
            }
        }
        "sub" if args.len() == 3 => {
            let rd = parse_register(&args[0])?;
            let rs1 = parse_register(&args[1])?;
            let rs2 = parse_register(&args[2])?;
            Some(Inst::SUB(rd, rs1, rs2))
        }
        "and" if args.len() == 3 => {
            let rd = parse_register(&args[0])?;
            let rs1 = parse_register(&args[1])?;
            let rs2 = parse_register(&args[2])?;
            Some(Inst::AND(rd, rs1, rs2))
        }
        "or" if args.len() == 3 => {
            let rd = parse_register(&args[0])?;
            let rs1 = parse_register(&args[1])?;
            let rs2 = parse_register(&args[2])?;
            Some(Inst::OR(rd, rs1, rs2))
        }
        "xor" if args.len() == 3 => {
            let rd = parse_register(&args[0])?;
            let rs1 = parse_register(&args[1])?;
            let rs2 = parse_register(&args[2])?;
            Some(Inst::XOR(rd, rs1, rs2))
        }
        "eq" if args.len() == 3 => {
            let rd = parse_register(&args[0])?;
            let rs1 = parse_register(&args[1])?;
            let rs2 = parse_register(&args[2])?;
            Some(Inst::EQ(rd, rs1, rs2))
        }
        "neq" if args.len() == 3 => {
            let rd = parse_register(&args[0])?;
            let rs1 = parse_register(&args[1])?;
            let rs2 = parse_register(&args[2])?;
            Some(Inst::NEQ(rd, rs1, rs2))
        }
        "lt" if args.len() == 3 => {
            let rd = parse_register(&args[0])?;
            let rs1 = parse_register(&args[1])?;
            let rs2 = parse_register(&args[2])?;
            Some(Inst::LT(rd, rs1, rs2))
        }
        "lts" if args.len() == 3 => {
            let rd = parse_register(&args[0])?;
            let rs1 = parse_register(&args[1])?;
            let rs2 = parse_register(&args[2])?;
            Some(Inst::LTS(rd, rs1, rs2))
        }
        "sr" if args.len() == 2 => {
            let rd = parse_register(&args[0])?;
            let rs = parse_register(&args[1])?;
            Some(Inst::SR(rd, rs))
        }
        "srs" if args.len() == 2 => {
            let rd = parse_register(&args[0])?;
            let rs = parse_register(&args[1])?;
            Some(Inst::SRS(rd, rs))
        }
        "srr" if args.len() == 2 => {
            let rd = parse_register(&args[0])?;
            let rs = parse_register(&args[1])?;
            Some(Inst::SRR(rd, rs))
        }
        "sl" if args.len() == 2 => {
            let rd = parse_register(&args[0])?;
            let rs = parse_register(&args[1])?;
            Some(Inst::SL(rd, rs))
        }
        "slr" if args.len() == 2 => {
            let rd = parse_register(&args[0])?;
            let rs = parse_register(&args[1])?;
            Some(Inst::SLR(rd, rs))
        }
        "load" if args.len() == 2 => {
            // load(rd, addr/imm)
            let rd = parse_register(&args[0])?;
            // Check if second arg is an immediate or a symbol
            if let Some(imm) = parse_immediate(&args[1]) {
                Some(Inst::LOADI(rd, imm))
            } else if let ast::Expr::Ident(symbol) = &args[1] {
                // Symbol reference - will be resolved later
                return Some(AsmLine {
                    inst: AsmInst::Inst(Inst::LOADI(rd, 0)),
                    symbols: vec![symbol.clone()],
                });
            } else {
                None
            }
        }
        "load" if args.len() == 3 => {
            let rd = parse_register(&args[0])?;
            let rs = parse_register(&args[1])?;
            if let Some(imm) = parse_immediate(&args[2]) {
                Some(Inst::LOAD(rd, rs, imm))
            } else if let ast::Expr::Ident(symbol) = &args[2] {
                return Some(AsmLine {
                    inst: AsmInst::Inst(Inst::LOAD(rd, rs, 0)),
                    symbols: vec![symbol.clone()],
                });
            } else {
                None
            }
        }
        "store" if args.len() == 2 => {
            // store(symbol, rs)
            if let ast::Expr::Ident(symbol) = &args[0] {
                let rs = parse_register(&args[1])?;
                // Symbol reference - will be resolved later
                return Some(AsmLine {
                    inst: AsmInst::Inst(Inst::STORE(rs, Reg::Z, 0)),
                    symbols: vec![symbol.clone()],
                });
            } else {
                None
            }
        }
        "store" if args.len() == 3 => {
            let rs2 = parse_register(&args[0])?;
            let rs1 = parse_register(&args[1])?;
            // Check if third arg is an immediate or a symbol
            if let Some(imm) = parse_immediate(&args[2]) {
                Some(Inst::STORE(rs2, rs1, imm))
            } else if let ast::Expr::Ident(symbol) = &args[2] {
                // Symbol reference - will be resolved later
                return Some(AsmLine {
                    inst: AsmInst::Inst(Inst::STORE(rs2, rs1, 0)),
                    symbols: vec![symbol.clone()],
                });
            } else {
                None
            }
        }
        "if" if args.len() == 2 => {
            // if(cond_reg, label/offset)
            let cond = parse_register(&args[0])?;
            // Check if it's a symbol or immediate
            if let Some(imm) = parse_immediate(&args[1]) {
                // Absolute address jump
                Some(Inst::IF(cond, imm))
            } else if let ast::Expr::Ident(label) = &args[1] {
                // Label reference - will be resolved later
                return Some(AsmLine {
                    inst: AsmInst::Inst(Inst::IF(cond, 0)),
                    symbols: vec![label.clone()],
                });
            } else if let ast::Expr::Unary(ast::UnaryOp::Neg, inner) = &args[1] {
                // Negative immediate for relative backward jump
                if let Some(imm) = parse_immediate(inner) {
                    // Use IFR for relative jumps with negative offsets
                    let offset = (-(imm as i16)) as u16;
                    Some(Inst::IFR(cond, offset))
                } else {
                    None
                }
            } else {
                None
            }
        }
        "jump" if args.len() == 1 => {
            // Check if it's a symbol or immediate
            if let Some(imm) = parse_immediate(&args[0]) {
                Some(Inst::JUMP(imm))
            } else if let ast::Expr::Ident(label) = &args[0] {
                // Symbol reference - will be resolved later
                return Some(AsmLine {
                    inst: AsmInst::Inst(Inst::JUMP(0)),
                    symbols: vec![label.clone()],
                });
            } else {
                None
            }
        }
        "call" if args.len() == 1 => {
            // Check if it's a symbol or immediate
            if let Some(imm) = parse_immediate(&args[0]) {
                Some(Inst::CALL(imm))
            } else if let ast::Expr::Ident(func) = &args[0] {
                // Function reference - will be resolved later
                return Some(AsmLine {
                    inst: AsmInst::Inst(Inst::CALL(0)),
                    symbols: vec![func.clone()],
                });
            } else {
                None
            }
        }
        "ret" if args.is_empty() => Some(Inst::RET()),
        "ifr" if args.len() == 2 => {
            let cond = parse_register(&args[0])?;
            if let Some(imm) = parse_immediate(&args[1]) {
                Some(Inst::IFR(cond, imm))
            } else if let ast::Expr::Ident(label) = &args[1] {
                return Some(AsmLine {
                    inst: AsmInst::Inst(Inst::IFR(cond, 0)),
                    symbols: vec![label.clone()],
                });
            } else {
                None
            }
        }
        "jumpr" if args.len() == 1 => {
            if let Some(imm) = parse_immediate(&args[0]) {
                Some(Inst::JUMPR(imm))
            } else if let ast::Expr::Ident(label) = &args[0] {
                return Some(AsmLine {
                    inst: AsmInst::Inst(Inst::JUMPR(0)),
                    symbols: vec![label.clone()],
                });
            } else {
                None
            }
        }
        "iret" if args.is_empty() => Some(Inst::IRET()),
        _ => None,
    }?;

    Some(AsmLine {
        inst: AsmInst::Inst(inst),
        symbols: Vec::new(),
    })
}

/// Parse a register from an expression
fn parse_register(expr: &ast::Expr) -> Option<Reg> {
    if let ast::Expr::Ident(name) = expr {
        match name.to_lowercase().as_str() {
            "z" | "zero" => Some(Reg::Z),
            "sp" => Some(Reg::SP),
            "ra" => Some(Reg::RA),
            "fp" => Some(Reg::FP),
            "a0" => Some(Reg::A0),
            "a1" => Some(Reg::A1),
            "t0" => Some(Reg::T0),
            "t1" => Some(Reg::T1),
            "t2" => Some(Reg::T2),
            "t3" => Some(Reg::T3),
            "s0" => Some(Reg::S0),
            "s1" => Some(Reg::S1),
            "s2" => Some(Reg::S2),
            "s3" => Some(Reg::S3),
            _ => None,
        }
    } else {
        None
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
