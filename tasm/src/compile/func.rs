use crate::{
    compile::{Code, Imm},
    error::Error,
    eval::{global::Global, local::Local, normtype::NormType},
    grammer::ast,
};
use arch::{inst::Inst, reg::Reg};
use itertools::chain;
use std::collections::HashMap;

pub fn func2code<'a>(global: &'a Global<'a>) -> Result<HashMap<&'a str, Code>, Error> {
    let mut result = HashMap::new();
    for (name, (_, _, def)) in global.funcs() {
        if let ast::Def::Func(_, args, ret, stmts) = def {
            let compiler = FuncCompiler::new(global, args)?;
            let code = compiler.compile(args, ret, stmts)?;
            result.insert(name, code);
        }
    }
    Ok(result)
}

struct FuncCompiler<'a> {
    local: Local<'a>,
}

impl<'a> FuncCompiler<'a> {
    fn new(global: &'a Global<'a>, args: &'a [(String, ast::Type)]) -> Result<Self, Error> {
        let mut local = Local::fork(global);
        local.args(args)?;
        Ok(Self { local })
    }

    fn compile(
        mut self,
        args: &'a Vec<(String, ast::Type)>,
        ret: &'a ast::Type,
        stmts: &'a Vec<ast::Stmt>,
    ) -> Result<Code, Error> {
        let mut insts = Vec::new();

        // Convert AST types to normalized types for prologue/epilogue
        let mut norm_args = Vec::new();
        for (name, arg_type) in args {
            let norm_type = self
                .local
                .normtype(arg_type)
                .map_err(|_| Error::TypeCollectionFailed(name.clone()))?;
            norm_args.push((name.clone(), norm_type));
        }

        let norm_ret_type = self
            .local
            .normtype(ret)
            .map_err(|_| Error::TypeCollectionFailed("return type".to_string()))?;

        // Add prologue
        insts.extend(Self::prologue(&norm_args));

        // Compile function body - process all statements
        for stmt in stmts {
            let stmt_insts = self.compile_stmt(stmt)?;
            insts.extend(stmt_insts);
        }

        // Add epilogue
        insts.extend(Self::epilogue(&norm_args, &norm_ret_type));

        Ok(Code(insts))
    }

    fn prologue(args: &[(String, NormType)]) -> Vec<Inst<Reg, Imm>> {
        let mut insts = Vec::new();

        // Calculate total size needed for saved registers (RA + FP)
        let saved_regs_size = 2u16;

        // Calculate size needed for arguments on stack
        let mut args_stack_size = 0u16;
        for (_name, arg_type) in args {
            args_stack_size += arg_type.sizeof() as u16;
        }

        // Total stack allocation for prologue
        let stack_alloc = saved_regs_size + args_stack_size;

        // 1. Allocate stack space
        if stack_alloc > 0 {
            insts.push(Inst::SUBI(Reg::SP, Reg::SP, Imm::Literal(stack_alloc)));
        }

        // 2. Save return address and frame pointer
        insts.push(Inst::STORE(Reg::RA, Reg::SP, Imm::Literal(0)));
        insts.push(Inst::STORE(Reg::FP, Reg::SP, Imm::Literal(1)));

        // 3. Set new frame pointer
        insts.push(Inst::MOV(Reg::FP, Reg::SP));

        // 4. Save arguments to stack
        // First 2 arguments come in A0, A1 registers
        // Additional arguments are already on the stack (passed by caller)
        let mut offset = saved_regs_size;
        for (i, (_name, arg_type)) in args.iter().enumerate() {
            let arg_size = arg_type.sizeof() as u16;

            if i < 2 {
                // Arguments in registers - save them to stack
                let arg_reg = match i {
                    0 => Reg::A0,
                    1 => Reg::A1,
                    _ => unreachable!(),
                };

                // Store each word of the argument
                for j in 0..arg_size {
                    if j == 0 {
                        insts.push(Inst::STORE(arg_reg, Reg::FP, Imm::Literal(offset + j)));
                    } else {
                        // For multi-word arguments, would need to handle appropriately
                        // For now, assume single-word arguments
                    }
                }
            }
            // Arguments beyond the first 2 are already on the stack (caller pushed them)

            offset += arg_size;
        }

        insts
    }

    fn epilogue(args: &[(String, NormType)], _ret: &NormType) -> Vec<Inst<Reg, Imm>> {
        let mut insts = Vec::new();

        // Calculate stack sizes
        let saved_regs_size = 2u16;
        let mut args_stack_size = 0u16;
        for (_name, arg_type) in args {
            args_stack_size += arg_type.sizeof() as u16;
        }
        let stack_alloc = saved_regs_size + args_stack_size;

        // 1. Restore stack pointer (discard local variables)
        insts.push(Inst::MOV(Reg::SP, Reg::FP));

        // 2. Restore frame pointer and return address
        insts.push(Inst::LOAD(Reg::FP, Reg::SP, Imm::Literal(1)));
        insts.push(Inst::LOAD(Reg::RA, Reg::SP, Imm::Literal(0)));

        // 3. Deallocate stack frame
        if stack_alloc > 0 {
            insts.push(Inst::ADDI(Reg::SP, Reg::SP, Imm::Literal(stack_alloc)));
        }

        // 4. Return to caller
        insts.push(Inst::RET());

        insts
    }

    fn compile_stmt(&mut self, stmt: &'a ast::Stmt) -> Result<Vec<Inst<Reg, Imm>>, Error> {
        match stmt {
            ast::Stmt::Block(stmts) => {
                let mut insts = Vec::new();
                for s in stmts {
                    insts.extend(self.compile_stmt(s)?);
                }
                Ok(insts)
            }

            ast::Stmt::Expr(expr) => {
                let (expr, _) = self.compile_expr(expr, Reg::T0)?;
                Ok(expr)
            }

            ast::Stmt::Assign(lhs, rhs) => {
                let (rhs_insts, rhs_reg) = self.compile_expr(rhs, Reg::T0)?;
                let lhs_insts = self.compile_lvalue(lhs, rhs_reg)?;
                Ok(chain!(rhs_insts, lhs_insts).collect())
            }

            ast::Stmt::Cond(cond, then_stmt, else_stmt) => {
                // Compile condition
                let (cond_insts, cond_reg) = self.compile_expr(cond, Reg::T0)?;

                // Compile then branch
                let then_insts = self.compile_stmt(then_stmt)?;

                if let Some(else_stmt) = else_stmt {
                    // Compile else branch
                    let else_insts = self.compile_stmt(else_stmt)?;

                    // Calculate jump offsets
                    let else_jump_offset = (then_insts.len() + 1) as u16; // +1 for the end jump
                    let end_jump_offset = else_insts.len() as u16;

                    Ok(chain!(
                        cond_insts,
                        vec![Inst::NOT(Reg::T1, cond_reg)],
                        vec![Inst::JUMPIFR(Reg::T1, Imm::Literal(else_jump_offset))],
                        then_insts,
                        vec![Inst::JUMPR(Imm::Literal(end_jump_offset))],
                        else_insts
                    )
                    .collect())
                } else {
                    // No else branch - just jump over then if false
                    let jump_offset = then_insts.len() as u16;

                    Ok(chain!(
                        cond_insts,
                        vec![Inst::NOT(Reg::T1, cond_reg)],
                        vec![Inst::JUMPIFR(Reg::T1, Imm::Literal(jump_offset))],
                        then_insts
                    )
                    .collect())
                }
            }

            ast::Stmt::Loop(cond, body) => {
                // Compile condition and body
                let (cond_insts, cond_reg) = self.compile_expr(cond, Reg::T0)?;
                let body_insts = self.compile_stmt(body)?;

                // Calculate offsets
                let exit_offset = (body_insts.len() + 1) as u16; // +1 for the loop jump
                                                                 // Jump back offset: -(cond_insts.len() + 2 + body_insts.len() + 1)
                                                                 // = -(cond_insts.len() + NOT + JUMPIFR + body_insts.len() + JUMPR)
                let loop_offset = -((cond_insts.len() + 2 + body_insts.len() + 1) as i32) as u16;

                Ok(chain!(
                    cond_insts.clone(),
                    vec![Inst::NOT(Reg::T1, cond_reg)],
                    vec![Inst::JUMPIFR(Reg::T1, Imm::Literal(exit_offset))],
                    body_insts,
                    vec![Inst::JUMPR(Imm::Literal(loop_offset))]
                )
                .collect())
            }

            ast::Stmt::Var(name, ty, init) => {
                // Allocate stack space for the variable
                let offset = self.local.push(name, ty).map_err(|e| {
                    Error::TypeCollectionFailed(format!("local variable {}: {}", name, e))
                })?;

                // Initialize if provided
                if let Some(init_expr) = init {
                    let (init_insts, init_reg) = self.compile_expr(init_expr, Reg::T0)?;
                    let store_offset = (-offset) as u16;

                    Ok(chain!(
                        init_insts,
                        vec![Inst::STORE(init_reg, Reg::FP, Imm::Literal(store_offset))]
                    )
                    .collect())
                } else {
                    Ok(Vec::new())
                }
            }

            ast::Stmt::Return(expr) => {
                if let Some(expr) = expr {
                    // Compile return value into A0 (return value register)
                    let (expr_insts, expr_reg) = self.compile_expr(expr, Reg::A0)?;
                    if expr_reg != Reg::A0 {
                        Ok(chain!(
                            expr_insts,
                            vec![Inst::MOV(Reg::A0, expr_reg)],
                            vec![Inst::RET()]
                        )
                        .collect())
                    } else {
                        Ok(chain!(expr_insts, vec![Inst::RET()]).collect())
                    }
                } else {
                    Ok(vec![Inst::RET()])
                }
            }
        }
    }

    fn compile_expr(
        &mut self,
        expr: &'a ast::Expr,
        target: Reg,
    ) -> Result<(Vec<Inst<Reg, Imm>>, Reg), Error> {
        let (insts, result_reg) = match expr {
            ast::Expr::NumberLit(n) => {
                let mut insts = Vec::new();
                insts.push(Inst::LOADI(target, Imm::Literal(*n as u16)));
                (insts, target)
            }

            ast::Expr::CharLit(c) => {
                let mut insts = Vec::new();
                insts.push(Inst::LOADI(target, Imm::Literal(*c as u16)));
                (insts, target)
            }

            ast::Expr::StringLit(_s) => {
                let mut insts = Vec::new();
                // For string literals, we use a symbol that will be resolved later
                insts.push(Inst::LOADI(target, Imm::Symbol("string_placeholder".to_string(), 0)));
                (insts, target)
            }

            ast::Expr::Ident(name) => {
                let mut insts = Vec::new();
                // Check if it's a local variable
                if let Some(offset) = self.local.offset(name) {
                    // Note: offset is negative (below FP), need to negate for LOAD instruction
                    let load_offset = (-offset) as u16;
                    insts.push(Inst::LOAD(target, Reg::FP, Imm::Literal(load_offset)));
                } else {
                    // Could be a global/static - emit with symbol reference
                    insts.push(Inst::LOADI(target, Imm::Symbol(name.clone(), 0)));
                }
                (insts, target)
            }

            ast::Expr::Binary(op, lhs, rhs) => {
                // Compile operands
                let (lhs_insts, lhs_reg) = self.compile_expr(lhs, Reg::T0)?;
                let (rhs_insts, rhs_reg) = self.compile_expr(rhs, Reg::T1)?;

                // Generate operation
                let op_insts = match op {
                    ast::BinaryOp::Add => vec![Inst::ADD(target, lhs_reg, rhs_reg)],
                    ast::BinaryOp::Sub => vec![Inst::SUB(target, lhs_reg, rhs_reg)],
                    ast::BinaryOp::And => vec![Inst::AND(target, lhs_reg, rhs_reg)],
                    ast::BinaryOp::Or => vec![Inst::OR(target, lhs_reg, rhs_reg)],
                    ast::BinaryOp::Xor => vec![Inst::XOR(target, lhs_reg, rhs_reg)],
                    ast::BinaryOp::Eq => vec![Inst::EQ(target, lhs_reg, rhs_reg)],
                    ast::BinaryOp::Ne => vec![Inst::NEQ(target, lhs_reg, rhs_reg)],
                    ast::BinaryOp::Lt => vec![Inst::LT(target, lhs_reg, rhs_reg)],
                    ast::BinaryOp::Le => vec![
                        Inst::LT(Reg::T2, rhs_reg, lhs_reg),
                        Inst::NOT(target, Reg::T2),
                    ],
                    ast::BinaryOp::Gt => vec![Inst::LT(target, rhs_reg, lhs_reg)],
                    ast::BinaryOp::Ge => vec![
                        Inst::LT(Reg::T2, lhs_reg, rhs_reg),
                        Inst::NOT(target, Reg::T2),
                    ],
                    ast::BinaryOp::Shl => vec![Inst::SL(target, lhs_reg)],
                    ast::BinaryOp::Shr => vec![Inst::SR(target, lhs_reg)],
                    _ => {
                        // Mul, Div, Mod not directly supported - would need software implementation
                        vec![Inst::LOADI(target, Imm::Literal(0))]
                    }
                };

                (chain!(lhs_insts, rhs_insts, op_insts).collect(), target)
            }

            ast::Expr::Unary(op, operand) => {
                let (operand_insts, operand_reg) = self.compile_expr(operand, Reg::T0)?;

                let op_insts = match op {
                    ast::UnaryOp::Pos => {
                        // Positive is a no-op, just move the value
                        if operand_reg != target {
                            vec![Inst::MOV(target, operand_reg)]
                        } else {
                            vec![]
                        }
                    }
                    ast::UnaryOp::Neg => {
                        vec![
                            Inst::LOADI(Reg::T1, Imm::Literal(0)),
                            Inst::SUB(target, Reg::T1, operand_reg),
                        ]
                    }
                    ast::UnaryOp::Not => {
                        vec![Inst::NOT(target, operand_reg)]
                    }
                };

                (chain!(operand_insts, op_insts).collect(), target)
            }

            ast::Expr::Deref(operand) => {
                let (operand_insts, operand_reg) = self.compile_expr(operand, Reg::T0)?;

                let insts = chain!(
                    operand_insts,
                    vec![Inst::LOAD(target, operand_reg, Imm::Literal(0))]
                )
                .collect();
                (insts, target)
            }

            ast::Expr::Addr(operand) => {
                // Address-of would need to handle lvalues specially
                // For now, just compile the operand
                let (operand_insts, operand_reg) = self.compile_expr(operand, Reg::T0)?;

                let insts = if operand_reg != target {
                    chain!(operand_insts, vec![Inst::MOV(target, operand_reg)]).collect()
                } else {
                    operand_insts
                };
                (insts, target)
            }

            ast::Expr::Call(func_expr, args) => {
                let mut insts = Vec::new();

                // Evaluate arguments and push onto stack or registers
                for (i, arg) in args.iter().enumerate() {
                    let (arg_insts, arg_reg) = self.compile_expr(arg, Reg::T0)?;
                    insts.extend(arg_insts);

                    // For simplicity, pass first 2 args in A0-A1
                    if i < 2 {
                        let dest_reg = match i {
                            0 => Reg::A0,
                            1 => Reg::A1,
                            _ => unreachable!(),
                        };
                        if arg_reg != dest_reg {
                            insts.push(Inst::MOV(dest_reg, arg_reg));
                        }
                    } else {
                        // Additional args go on stack
                        insts.push(Inst::SUBI(Reg::SP, Reg::SP, Imm::Literal(1)));
                        insts.push(Inst::STORE(arg_reg, Reg::SP, Imm::Literal(0)));
                    }
                }

                // Call the function
                if let ast::Expr::Ident(func_name) = &**func_expr {
                    insts.push(Inst::CALL(Imm::Symbol(func_name.clone(), 0)));
                } else {
                    // Indirect call through register
                    let (func_insts, _func_reg) = self.compile_expr(func_expr, Reg::T0)?;
                    insts.extend(func_insts);
                    // Would need a CALLR instruction for indirect calls
                    // For now, just use placeholder
                    insts.push(Inst::NOP());
                }

                // Clean up stack if we pushed arguments
                let stack_args = if args.len() > 2 { args.len() - 2 } else { 0 };
                if stack_args > 0 {
                    insts.push(Inst::ADDI(Reg::SP, Reg::SP, Imm::Literal(stack_args as u16)));
                }

                // Result is in A0, move to target if needed
                if target != Reg::A0 {
                    insts.push(Inst::MOV(target, Reg::A0));
                }
                (insts, target)
            }

            ast::Expr::Cond(cond, then_expr, else_expr) => {
                let mut insts = Vec::new();

                // Ternary conditional expression
                let (cond_insts, cond_reg) = self.compile_expr(cond, Reg::T0)?;
                insts.extend(cond_insts);

                // Jump to else if false
                insts.push(Inst::NOT(Reg::T1, cond_reg));

                // Compile both branches
                let (then_insts, then_reg) = self.compile_expr(then_expr, target)?;
                let (else_insts, else_reg) = self.compile_expr(else_expr, target)?;

                // Jump to else if condition is false
                let else_jump_offset = (then_insts.len() + 1) as u16; // +1 for the end jump
                insts.push(Inst::JUMPIFR(Reg::T1, Imm::Literal(else_jump_offset)));

                // Then expression
                insts.extend(then_insts);
                if then_reg != target {
                    insts.push(Inst::MOV(target, then_reg));
                }

                // Jump over else
                let end_jump_offset = else_insts.len() as u16;
                if else_reg != target {
                    // +1 for the MOV instruction
                    insts.push(Inst::JUMPR(Imm::Literal(end_jump_offset + 1)));
                } else {
                    insts.push(Inst::JUMPR(Imm::Literal(end_jump_offset)));
                }

                // Else expression
                insts.extend(else_insts);
                if else_reg != target {
                    insts.push(Inst::MOV(target, else_reg));
                }

                (insts, target)
            }

            ast::Expr::SizeofType(typ) => {
                let mut insts = Vec::new();
                // Calculate size at compile time
                let norm_type = self
                    .local
                    .normtype(typ)
                    .map_err(|_| Error::TypeCollectionFailed("sizeof".to_string()))?;
                let size = norm_type.sizeof() as u16;
                insts.push(Inst::LOADI(target, Imm::Literal(size)));
                (insts, target)
            }

            _ => {
                let mut insts = Vec::new();
                // Other expression types not yet implemented
                insts.push(Inst::LOADI(target, Imm::Literal(0)));
                (insts, target)
            }
        };

        Ok((insts, result_reg))
    }

    fn compile_lvalue(
        &mut self,
        lvalue: &'a ast::Expr,
        value_reg: Reg,
    ) -> Result<Vec<Inst<Reg, Imm>>, Error> {
        match lvalue {
            ast::Expr::Ident(name) => {
                let mut insts = Vec::new();
                if let Some(offset) = self.local.offset(name) {
                    // Note: offset is negative (below FP), need to negate for STORE instruction
                    let store_offset = (-offset) as u16;
                    insts.push(Inst::STORE(value_reg, Reg::FP, Imm::Literal(store_offset)));
                } else {
                    // Global/static variable - emit with symbol reference
                    insts.push(Inst::STORE(value_reg, Reg::Z, Imm::Symbol(name.clone(), 0)));
                }
                Ok(insts)
            }

            ast::Expr::Deref(addr_expr) => {
                // Store through pointer
                let (addr_insts, addr_reg) = self.compile_expr(addr_expr, Reg::T1)?;
                Ok(chain!(
                    addr_insts,
                    vec![Inst::STORE(value_reg, addr_reg, Imm::Literal(0))]
                )
                .collect())
            }

            ast::Expr::Index(array_expr, index_expr) => {
                // Compile array base address and index
                let (array_insts, array_reg) = self.compile_expr(array_expr, Reg::T1)?;
                let (index_insts, index_reg) = self.compile_expr(index_expr, Reg::T2)?;

                Ok(chain!(
                    array_insts,
                    index_insts,
                    vec![Inst::ADD(Reg::T1, array_reg, index_reg)],
                    vec![Inst::STORE(value_reg, Reg::T1, Imm::Literal(0))]
                )
                .collect())
            }

            ast::Expr::Member(struct_expr, _field_name) => {
                // Would need type information to calculate field offset
                // For now, just store at base address
                let (struct_insts, struct_reg) = self.compile_expr(struct_expr, Reg::T1)?;
                Ok(chain!(
                    struct_insts,
                    vec![Inst::STORE(value_reg, struct_reg, Imm::Literal(0))]
                )
                .collect())
            }

            _ => Err(Error::InvalidLValue(format!("{:?}", lvalue))),
        }
    }
}
