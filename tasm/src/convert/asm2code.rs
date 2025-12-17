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
fn generate_asm_block(asm_name: String, body: &ast::Stmt) -> Vec<AsmLine> {
    let mut instructions = Vec::new();

    // Add entry label for the assembly block
    instructions.push(AsmLine {
        inst: AsmInst::Label(asm_name.clone()),
        symbols: Vec::new(),
    });

    // Parse the assembly statements
    compile_asm_stmt(&mut instructions, body);

    instructions
}

/// Compile assembly statements
fn compile_asm_stmt(instructions: &mut Vec<AsmLine>, stmt: &ast::Stmt) {
    match stmt {
        ast::Stmt::Block(stmts) => {
            for s in stmts {
                compile_asm_stmt(instructions, s);
            }
        }
        ast::Stmt::Expr(expr) => {
            // In assembly blocks, expressions are typically assembly instructions
            if let Some(inst) = parse_asm_expr(expr) {
                instructions.push(inst);
            }
        }
        _ => {
            // Other statement types might not be relevant in assembly blocks
            // Or could be handled specially
        }
    }
}

/// Parse an expression as an assembly instruction
fn parse_asm_expr(expr: &ast::Expr) -> Option<AsmLine> {
    match expr {
        ast::Expr::Call(func_expr, args) => {
            if let ast::Expr::Ident(inst_name) = &**func_expr {
                // Parse instruction based on name
                parse_instruction(inst_name, args)
            } else {
                None
            }
        }
        ast::Expr::Ident(label) => {
            // Standalone identifier might be a label
            Some(AsmLine {
                inst: AsmInst::Label(label.clone()),
                symbols: Vec::new(),
            })
        }
        _ => None,
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
            let imm = parse_immediate(&args[2])?;
            Some(Inst::ADDI(rd, rs, imm))
        }
        "sub" if args.len() == 3 => {
            let rd = parse_register(&args[0])?;
            let rs1 = parse_register(&args[1])?;
            let rs2 = parse_register(&args[2])?;
            Some(Inst::SUB(rd, rs1, rs2))
        }
        "load" if args.len() == 3 => {
            let rd = parse_register(&args[0])?;
            let rs = parse_register(&args[1])?;
            let imm = parse_immediate(&args[2])?;
            Some(Inst::LOAD(rd, rs, imm))
        }
        "store" if args.len() == 3 => {
            let rs2 = parse_register(&args[0])?;
            let rs1 = parse_register(&args[1])?;
            let imm = parse_immediate(&args[2])?;
            Some(Inst::STORE(rs2, rs1, imm))
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
    if let ast::Expr::NumberLit(n) = expr {
        Some(*n as u16)
    } else {
        None
    }
}
