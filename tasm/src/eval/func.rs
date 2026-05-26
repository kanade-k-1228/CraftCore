use crate::{
    error::Error,
    grammer::{ast, token::Pos},
};

use super::{
    code::{Code, Imm},
    global::Global,
    local::Local,
    normtype::NormType,
};
use arch::{inst::Inst, reg::Reg};
use itertools::chain;

impl<'a> Global<'a> {
    pub fn func2code(&'a self, name: &str) -> Result<Code, Error> {
        match self.get(name) {
            Some(ast::Def::Func((_, pos), args, ret, stmts)) => {
                let loc = pos.clone();
                let context = Context::new(self, args, &loc)?;
                context.compile(args, ret, stmts, &loc)
            }
            Some(
                ast::Def::Type((_, pos), _)
                | ast::Def::Const((_, pos), _, _)
                | ast::Def::Static((_, pos), _, _)
                | ast::Def::Asm((_, pos), _, _),
            ) => Err(Error::NotAFunction(pos.clone(), name.to_string())),
            None => Err(Error::UnknownIdentifier(Pos::default(), name.to_string())),
        }
    }
}

struct ScopeFrame {
    id: usize,
    name: String,
}

struct Context<'a> {
    local: Local<'a>,
    scope_stack: Vec<ScopeFrame>,
    scope_counter: usize,
    /// Total stack size reserved by the prologue (saved RA + saved FP + args).
    /// Used to emit a matching epilogue at every `return`.
    prologue_stack_alloc: usize,
}

impl<'a> Context<'a> {
    fn new(
        global: &'a Global<'a>,
        args: &'a [(ast::Ident, ast::Type)],
        _loc: &Pos,
    ) -> Result<Self, Error> {
        let mut local = Local::fork(global);
        local.args(args)?;
        let saved_regs_size: usize = 2;
        let args_stack_size: usize = args
            .iter()
            .map(|(_, ty)| global.normtype(ty).map(|t| t.sizeof()).unwrap_or(0))
            .sum();
        Ok(Self {
            local,
            scope_stack: Vec::new(),
            scope_counter: 0,
            prologue_stack_alloc: saved_regs_size + args_stack_size,
        })
    }

    /// The epilogue: tear the frame down and RET.
    fn epilogue_insts(&self) -> Vec<Inst<Reg, Imm>> {
        let mut v = vec![
            Inst::MOV(Reg::SP, Reg::FP),
            Inst::LOAD(Reg::FP, Reg::SP, Imm::Lit(1)),
            Inst::LOAD(Reg::RA, Reg::SP, Imm::Lit(0)),
        ];
        if self.prologue_stack_alloc > 0 {
            v.push(Inst::ADDI(
                Reg::SP,
                Reg::SP,
                Imm::Lit(self.prologue_stack_alloc),
            ));
        }
        v.push(Inst::RET());
        v
    }

    fn fresh_scope_id(&mut self) -> usize {
        let id = self.scope_counter;
        self.scope_counter += 1;
        id
    }

    /// Find the innermost enclosing scope with the given name.
    fn find_scope(&self, name: &str) -> Option<usize> {
        self.scope_stack
            .iter()
            .rev()
            .find(|f| f.name == name)
            .map(|f| f.id)
    }

    fn compile(
        mut self,
        args: &'a [(ast::Ident, ast::Type)],
        ret: &'a ast::Type,
        stmts: &'a [ast::Stmt],
        loc: &Pos,
    ) -> Result<Code, Error> {
        let mut insts = Vec::new();

        // Convert AST types to normalized types for prologue/epilogue.
        // We currently only handle 1-word arguments — multi-word (arrays /
        // structs by value) need either struct-copy-on-call or pointer-passing
        // conventions that aren't implemented yet.
        let mut norm_args = Vec::new();
        for ((name, _), arg_type) in args {
            let norm_type = self
                .local
                .normtype(arg_type)
                .map_err(|_| Error::TypeCollectionFailed(loc.clone(), name.clone()))?;
            if norm_type.sizeof() > 1 {
                return Err(Error::UnsupportedExpression(
                    loc.clone(),
                    format!(
                        "argument `{}` has type of size {} — pass by pointer (*{}) instead",
                        name,
                        norm_type.sizeof(),
                        norm_type.fmt()
                    ),
                ));
            }
            norm_args.push((name.clone(), norm_type));
        }

        let norm_ret_type = self
            .local
            .normtype(ret)
            .map_err(|_| Error::TypeCollectionFailed(loc.clone(), "return type".to_string()))?;

        // Add prologue
        insts.extend(Self::prologue(&norm_args));

        // Compile function body - process all statements
        for stmt in stmts {
            let stmt_insts = self.compile_stmt(stmt)?;
            insts.extend(stmt_insts);
        }

        // Fall-through epilogue (for void functions and functions whose body
        // doesn't end with an explicit `return`). Explicit `return` statements
        // inline their own copy.
        insts.extend(self.epilogue_insts());

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
            insts.push(Inst::SUBI(Reg::SP, Reg::SP, Imm::Lit(stack_alloc as usize)));
        }

        // 2. Save return address and frame pointer
        insts.push(Inst::STORE(Reg::RA, Reg::SP, Imm::Lit(0)));
        insts.push(Inst::STORE(Reg::FP, Reg::SP, Imm::Lit(1)));

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
                        insts.push(Inst::STORE(
                            arg_reg,
                            Reg::FP,
                            Imm::Lit((offset + j) as usize),
                        ));
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


    fn compile_stmt(&mut self, stmt: &'a ast::Stmt) -> Result<Vec<Inst<Reg, Imm>>, Error> {
        match stmt {
            ast::Stmt::Block(scope_name, stmts) => {
                // If the block names a scope, push a frame so that break/continue
                // inside can resolve to this scope's id.
                let scope_id = scope_name.as_ref().map(|(name, _)| {
                    let id = self.fresh_scope_id();
                    self.scope_stack.push(ScopeFrame {
                        id,
                        name: name.clone(),
                    });
                    id
                });

                let mut block_insts = Vec::new();
                for s in stmts {
                    block_insts.extend(self.compile_stmt(s)?);
                }

                if let Some(id) = scope_id {
                    self.scope_stack.pop();

                    // Layout: block_insts[0..B]. For a placeholder at index i:
                    //   PC = block_start + i,  next_PC = block_start + i + 1
                    //   break  target = block_start + B          → offset = B - i - 1
                    //   continue target = block_start            → offset = -(i + 1)
                    let block_len = block_insts.len();
                    block_insts = block_insts
                        .into_iter()
                        .enumerate()
                        .map(|(i, inst)| {
                            inst.resolve(|imm| match imm {
                                Imm::ScopeExit(target) if target == id => {
                                    let off = (block_len as isize) - (i as isize) - 1;
                                    Imm::Lit(off as usize)
                                }
                                Imm::ScopeEntry(target) if target == id => {
                                    let off = -((i as isize) + 1);
                                    Imm::Lit(off as usize)
                                }
                                other => other,
                            })
                        })
                        .collect();
                }

                Ok(block_insts)
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
                        vec![Inst::JUMPIFR(Reg::T1, Imm::Lit(else_jump_offset as usize))],
                        then_insts,
                        vec![Inst::JUMPR(Imm::Lit(end_jump_offset as usize))],
                        else_insts
                    )
                    .collect())
                } else {
                    // No else branch - just jump over then if false
                    let jump_offset = then_insts.len() as u16;

                    Ok(chain!(
                        cond_insts,
                        vec![Inst::NOT(Reg::T1, cond_reg)],
                        vec![Inst::JUMPIFR(Reg::T1, Imm::Lit(jump_offset as usize))],
                        then_insts
                    )
                    .collect())
                }
            }

            ast::Stmt::Loop(cond, body) => {
                let (cond_insts, cond_reg) = self.compile_expr(cond, Reg::T0)?;
                let body_insts = self.compile_stmt(body)?;

                let exit_offset = (body_insts.len() + 1) as u16; // skip body + final JUMPR
                let loop_offset = -((cond_insts.len() + 2 + body_insts.len() + 1) as i32) as u16;

                Ok(chain!(
                    cond_insts.clone(),
                    vec![Inst::NOT(Reg::T1, cond_reg)],
                    vec![Inst::JUMPIFR(Reg::T1, Imm::Lit(exit_offset as usize))],
                    body_insts,
                    vec![Inst::JUMPR(Imm::Lit(loop_offset as usize))]
                )
                .collect())
            }

            ast::Stmt::Break((name, _name_pos), pos) => {
                let id = self
                    .find_scope(name)
                    .ok_or_else(|| Error::UnknownScope(pos.clone(), name.clone()))?;
                Ok(vec![Inst::JUMPR(Imm::ScopeExit(id))])
            }

            ast::Stmt::Continue((name, _name_pos), pos) => {
                let id = self
                    .find_scope(name)
                    .ok_or_else(|| Error::UnknownScope(pos.clone(), name.clone()))?;
                Ok(vec![Inst::JUMPR(Imm::ScopeEntry(id))])
            }

            ast::Stmt::Var(ident, ty, init) => {
                let (name, pos) = ident;
                // Register the variable and get its base FP-offset (negative).
                let offset = self.local.push(ident, ty).map_err(|e| {
                    Error::TypeCollectionFailed(
                        pos.clone(),
                        format!("local variable {}: {}", name, e),
                    )
                })?;
                let size = self.local.normtype(ty)?.sizeof();

                // Reserve stack space below FP for this variable.
                let mut insts: Vec<Inst<Reg, Imm>> = Vec::new();
                if size > 0 {
                    insts.push(Inst::SUBI(Reg::SP, Reg::SP, Imm::Lit(size)));
                }

                // Optionally initialize the first slot from the init expression.
                if let Some(init_expr) = init {
                    let (init_insts, init_reg) = self.compile_expr(init_expr, Reg::T0)?;
                    let store_offset = (offset as i16) as u16 as usize;
                    insts.extend(init_insts);
                    insts.push(Inst::STORE(init_reg, Reg::FP, Imm::Lit(store_offset)));
                }
                Ok(insts)
            }

            ast::Stmt::Return(expr) => {
                // Emit the return value (if any) into A0, then inline the full
                // epilogue. This ensures every `return` restores SP / FP / RA
                // before the actual RET — otherwise a return inside a function
                // body would leak its stack frame.
                let mut insts = Vec::new();
                if let Some(expr) = expr {
                    let (expr_insts, expr_reg) = self.compile_expr(expr, Reg::A0)?;
                    insts.extend(expr_insts);
                    if expr_reg != Reg::A0 {
                        insts.push(Inst::MOV(Reg::A0, expr_reg));
                    }
                }
                insts.extend(self.epilogue_insts());
                Ok(insts)
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
                insts.push(Inst::LOADI(target, Imm::Lit(*n as usize)));
                (insts, target)
            }

            ast::Expr::CharLit(c) => {
                let mut insts = Vec::new();
                insts.push(Inst::LOADI(target, Imm::Lit(*c as usize)));
                (insts, target)
            }

            ast::Expr::StringLit(_s) => {
                // Inline string literals as runtime values would need automatic
                // const synthesis (each "..." materialized in the const section
                // with a generated name). For now, point the user at the
                // `const name = "..."; ... name ...;` pattern instead.
                return Err(Error::UnsupportedExpression(
                    expr.pos_or_default(),
                    "string literal as runtime value; use a named const instead"
                        .to_string(),
                ));
            }

            ast::Expr::Ident((name, _)) => {
                let mut insts = Vec::new();
                // Multi-word types (arrays, structs) cannot fit in a register;
                // an identifier of such a type yields its address (C-style array
                // decay). Scalar types load the value.
                let ty = self.local.typeinfer(expr).ok();
                let multi_word = ty.as_ref().map(|t| t.sizeof() > 1).unwrap_or(false);

                if let Some(offset) = self.local.offset(name) {
                    if multi_word {
                        // Address of local/arg.
                        if offset >= 0 {
                            insts.push(Inst::ADDI(target, Reg::FP, Imm::Lit(offset as usize)));
                        } else {
                            insts.push(Inst::SUBI(
                                target,
                                Reg::FP,
                                Imm::Lit((-offset) as usize),
                            ));
                        }
                    } else {
                        let load_offset = (offset as i16) as u16 as usize;
                        insts.push(Inst::LOAD(target, Reg::FP, Imm::Lit(load_offset)));
                    }
                } else {
                    match self.local.global_def(name) {
                        Some(ast::Def::Static(..)) | Some(ast::Def::Const(..)) => {
                            if multi_word {
                                // Array/struct identifier → its address.
                                insts.push(Inst::LOADI(
                                    target,
                                    Imm::Symbol(name.clone(), 0),
                                ));
                            } else {
                                insts.push(Inst::LOAD(
                                    target,
                                    Reg::Z,
                                    Imm::Symbol(name.clone(), 0),
                                ));
                            }
                        }
                        // For code globals, the identifier denotes the code address.
                        Some(ast::Def::Func(..)) | Some(ast::Def::Asm(..)) => {
                            insts.push(Inst::LOADI(target, Imm::Label(name.clone())));
                        }
                        _ => {
                            insts.push(Inst::LOAD(
                                target,
                                Reg::Z,
                                Imm::Symbol(name.clone(), 0),
                            ));
                        }
                    }
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
                    ast::BinaryOp::Shl | ast::BinaryOp::Shr => {
                        // Repeat 1-bit shift `count` times. Convention:
                        //   T0 = value (= lhs_reg), T1 = count (= rhs_reg, trashed)
                        //   T2 = scratch for the count==0 check
                        //
                        //   loop:                        relative position
                        //     EQI T2, T1, 0                0
                        //     JUMPIFR T2, +3               1   (skip body when count==0)
                        //     SL/SR T0, T0                 2
                        //     SUBI T1, T1, 1               3
                        //     JUMPR -5                     4   (back to loop)
                        //   exit:
                        let shift = match op {
                            ast::BinaryOp::Shl => Inst::SL(Reg::T0, Reg::T0),
                            ast::BinaryOp::Shr => Inst::SR(Reg::T0, Reg::T0),
                            _ => unreachable!(),
                        };
                        let mut v = vec![
                            Inst::EQI(Reg::T2, Reg::T1, Imm::Lit(0)),
                            Inst::JUMPIFR(Reg::T2, Imm::Lit(3)),
                            shift,
                            Inst::SUBI(Reg::T1, Reg::T1, Imm::Lit(1)),
                            Inst::JUMPR(Imm::Lit((-5i32) as u16 as usize)),
                        ];
                        if target != Reg::T0 {
                            v.push(Inst::MOV(target, Reg::T0));
                        }
                        v
                    }

                    ast::BinaryOp::Mul => {
                        // Shift-and-add multiplication.
                        //   T0 = multiplicand (= lhs_reg, moved to T2)
                        //   T1 = multiplier (= rhs_reg, shifted right each iter)
                        //   T2 = multiplicand copy (shifted left each iter)
                        //   T3 = scratch (low-bit / zero check)
                        // Result accumulates in T0.
                        //
                        //   MOV   T2, T0                ; T2 = original lhs
                        //   LOADI T0, 0                 ; result = 0
                        //   loop:                              relative pos
                        //     EQI     T3, T1, 0                 0
                        //     JUMPIFR T3, +7  (exit)            1
                        //     ANDI    T3, T1, 1                 2
                        //     EQI     T3, T3, 0                 3
                        //     JUMPIFR T3, +1  (skip add)        4
                        //     ADD     T0, T0, T2                5
                        //     SL      T2, T2                    6
                        //     SR      T1, T1                    7
                        //     JUMPR   -9                        8
                        //   exit:                               9
                        let mut v = vec![
                            Inst::MOV(Reg::T2, Reg::T0),
                            Inst::LOADI(Reg::T0, Imm::Lit(0)),
                            // loop body
                            Inst::EQI(Reg::T3, Reg::T1, Imm::Lit(0)),
                            Inst::JUMPIFR(Reg::T3, Imm::Lit(7)),
                            Inst::ANDI(Reg::T3, Reg::T1, Imm::Lit(1)),
                            Inst::EQI(Reg::T3, Reg::T3, Imm::Lit(0)),
                            Inst::JUMPIFR(Reg::T3, Imm::Lit(1)),
                            Inst::ADD(Reg::T0, Reg::T0, Reg::T2),
                            Inst::SL(Reg::T2, Reg::T2),
                            Inst::SR(Reg::T1, Reg::T1),
                            Inst::JUMPR(Imm::Lit((-9i32) as u16 as usize)),
                        ];
                        if target != Reg::T0 {
                            v.push(Inst::MOV(target, Reg::T0));
                        }
                        v
                    }

                    ast::BinaryOp::Div | ast::BinaryOp::Mod => {
                        // Restoring shift-subtract division (unsigned, 16-bit).
                        //   Q = 0; R = 0; N = dividend
                        //   for i = 0..16:
                        //     R = (R << 1) | MSB(N)
                        //     N <<= 1
                        //     Q <<= 1
                        //     if R >= D: R -= D; Q |= 1
                        //
                        // Register usage:
                        //   T0 = R   (final remainder)
                        //   T1 = D   (divisor, = rhs_reg)
                        //   T2 = N→Q (combined: high bits of N shift out, Q bits OR in at LSB)
                        //   T3 = scratch
                        //   S0 = iteration counter (saved to stack)
                        let want_q = matches!(op, ast::BinaryOp::Div);
                        // Setup: 5 insts
                        let mut v = vec![
                            Inst::MOV(Reg::T2, Reg::T0),                       // T2 = N
                            Inst::LOADI(Reg::T0, Imm::Lit(0)),                 // R = 0
                            Inst::SUBI(Reg::SP, Reg::SP, Imm::Lit(1)),         // spill S0
                            Inst::STORE(Reg::S0, Reg::SP, Imm::Lit(0)),
                            Inst::LOADI(Reg::S0, Imm::Lit(16)),                // counter = 16
                        ];
                        // Loop body. Positions are relative to the EQI at the top.
                        // Layout (with want_q):
                        //   pos  0: EQI T3, S0, 0
                        //   pos  1: JUMPIFR T3, +exit
                        //   pos  2: ANDI T3, T2, 0x8000   ; T3 = MSB bit
                        //   pos  3: NEQI T3, T3, 0        ; T3 = boolean MSB
                        //   pos  4: SL T2, T2             ; N <<= 1   (Q's new LSB = 0)
                        //   pos  5: SL T0, T0             ; R <<= 1
                        //   pos  6: ADD T0, T0, T3        ; R |= MSB(N)
                        //   pos  7: LT T3, T0, T1         ; T3 = (R < D)
                        //   pos  8: JUMPIFR T3, +skip     ; skip subtract if R < D
                        //   pos  9: SUB T0, T0, T1
                        //   pos 10: ORI T2, T2, 1         ; (only when want_q)
                        //   pos 11/10: SUBI S0, S0, 1
                        //   pos 12/11: JUMPR -back
                        //   pos 13/12: exit
                        let body_len = if want_q { 13 } else { 12 };
                        let exit_off = body_len - 2; // JUMPIFR at pos 1: target = body_len, next_PC = 2
                        let skip_off = if want_q { 2 } else { 1 };
                        let back_off = -(body_len as i32);
                        v.push(Inst::EQI(Reg::T3, Reg::S0, Imm::Lit(0)));
                        v.push(Inst::JUMPIFR(Reg::T3, Imm::Lit(exit_off as usize)));
                        v.push(Inst::ANDI(Reg::T3, Reg::T2, Imm::Lit(0x8000)));
                        v.push(Inst::NEQI(Reg::T3, Reg::T3, Imm::Lit(0)));
                        v.push(Inst::SL(Reg::T2, Reg::T2));
                        v.push(Inst::SL(Reg::T0, Reg::T0));
                        v.push(Inst::ADD(Reg::T0, Reg::T0, Reg::T3));
                        v.push(Inst::LT(Reg::T3, Reg::T0, Reg::T1));
                        v.push(Inst::JUMPIFR(Reg::T3, Imm::Lit(skip_off as usize)));
                        v.push(Inst::SUB(Reg::T0, Reg::T0, Reg::T1));
                        if want_q {
                            v.push(Inst::ORI(Reg::T2, Reg::T2, Imm::Lit(1)));
                        }
                        v.push(Inst::SUBI(Reg::S0, Reg::S0, Imm::Lit(1)));
                        v.push(Inst::JUMPR(Imm::Lit(back_off as u16 as usize)));
                        // Restore S0
                        v.push(Inst::LOAD(Reg::S0, Reg::SP, Imm::Lit(0)));
                        v.push(Inst::ADDI(Reg::SP, Reg::SP, Imm::Lit(1)));
                        // Result: remainder in T0, quotient in T2
                        let result_reg = if want_q { Reg::T2 } else { Reg::T0 };
                        if target != result_reg {
                            v.push(Inst::MOV(target, result_reg));
                        }
                        v
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
                        // -x == 0 - x, using Z (the always-zero register).
                        vec![Inst::SUB(target, Reg::Z, operand_reg)]
                    }
                    ast::UnaryOp::Not => {
                        // ~x — hardware NOT (equivalent to x XOR 0xFFFF).
                        vec![Inst::NOT(target, operand_reg)]
                    }
                };

                (chain!(operand_insts, op_insts).collect(), target)
            }

            ast::Expr::Deref(operand) => {
                let (operand_insts, operand_reg) = self.compile_expr(operand, Reg::T0)?;

                let insts = chain!(
                    operand_insts,
                    vec![Inst::LOAD(target, operand_reg, Imm::Lit(0))]
                )
                .collect();
                (insts, target)
            }

            ast::Expr::Addr(operand) => {
                // Address-of: compute the operand's address into target.
                let addr_insts = self.compile_addr(operand, target)?;
                (addr_insts, target)
            }

            ast::Expr::Index(..) | ast::Expr::Member(..) => {
                // Compute the element address into target, then LOAD from it.
                let addr_insts = self.compile_addr(expr, target)?;
                let insts = chain!(
                    addr_insts,
                    vec![Inst::LOAD(target, target, Imm::Lit(0))]
                )
                .collect();
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
                        insts.push(Inst::SUBI(Reg::SP, Reg::SP, Imm::Lit(1)));
                        insts.push(Inst::STORE(arg_reg, Reg::SP, Imm::Lit(0)));
                    }
                }

                // Call the function. A bare identifier referring to a func/asm
                // is a direct call; anything else is treated as a function
                // pointer expression and dispatched through CALLR.
                let direct = if let ast::Expr::Ident((name, _)) = &**func_expr {
                    matches!(
                        self.local.global_def(name),
                        Some(ast::Def::Func(..)) | Some(ast::Def::Asm(..)),
                    )
                } else {
                    false
                };
                if direct {
                    let name = if let ast::Expr::Ident((n, _)) = &**func_expr {
                        n.clone()
                    } else {
                        unreachable!()
                    };
                    insts.push(Inst::CALL(Imm::Label(name)));
                } else {
                    // Indirect call: evaluate the function-pointer expression
                    // into a temp register and jump through it.
                    let (func_insts, func_reg) = self.compile_expr(func_expr, Reg::T2)?;
                    insts.extend(func_insts);
                    insts.push(Inst::CALLR(func_reg));
                }

                // Clean up stack if we pushed arguments
                let stack_args = if args.len() > 2 { args.len() - 2 } else { 0 };
                if stack_args > 0 {
                    insts.push(Inst::ADDI(Reg::SP, Reg::SP, Imm::Lit(stack_args)));
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
                insts.push(Inst::JUMPIFR(Reg::T1, Imm::Lit(else_jump_offset as usize)));

                // Then expression
                insts.extend(then_insts);
                if then_reg != target {
                    insts.push(Inst::MOV(target, then_reg));
                }

                // Jump over else
                let end_jump_offset = else_insts.len() as u16;
                if else_reg != target {
                    // +1 for the MOV instruction
                    insts.push(Inst::JUMPR(Imm::Lit((end_jump_offset + 1) as usize)));
                } else {
                    insts.push(Inst::JUMPR(Imm::Lit(end_jump_offset as usize)));
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
                let norm_type = self.local.normtype(typ).map_err(|_| {
                    Error::TypeCollectionFailed(Pos::default(), "sizeof".to_string())
                })?;
                let size = norm_type.sizeof() as u16;
                insts.push(Inst::LOADI(target, Imm::Lit(size as usize)));
                (insts, target)
            }

            _ => {
                let mut insts = Vec::new();
                // Other expression types not yet implemented
                insts.push(Inst::LOADI(target, Imm::Lit(0)));
                (insts, target)
            }
        };

        Ok((insts, result_reg))
    }

    /// Compile the *address* of an lvalue expression into the given target register.
    /// Honors the existing local-variable convention: locals at FP + abs(offset).
    /// The Index case spills `target` to the stack while computing the index.
    fn compile_addr(
        &mut self,
        expr: &'a ast::Expr,
        target: Reg,
    ) -> Result<Vec<Inst<Reg, Imm>>, Error> {
        match expr {
            ast::Expr::Ident((name, _)) => {
                let mut insts = Vec::new();
                if let Some(offset) = self.local.offset(name) {
                    if offset >= 0 {
                        // Arg above FP: addr = FP + offset.
                        insts.push(Inst::ADDI(target, Reg::FP, Imm::Lit(offset as usize)));
                    } else {
                        // Local below FP: addr = FP - |offset|.
                        insts.push(Inst::SUBI(
                            target,
                            Reg::FP,
                            Imm::Lit((-offset) as usize),
                        ));
                    }
                } else {
                    // Global: data globals get a data-symbol address, code
                    // globals (asm/func) get a code-label address.
                    match self.local.global_def(name) {
                        Some(ast::Def::Func(..)) | Some(ast::Def::Asm(..)) => {
                            insts.push(Inst::LOADI(target, Imm::Label(name.clone())));
                        }
                        _ => {
                            insts.push(Inst::LOADI(target, Imm::Symbol(name.clone(), 0)));
                        }
                    }
                }
                Ok(insts)
            }

            ast::Expr::Member(base, (field, field_pos)) => {
                let mut insts = self.compile_addr(base, target)?;
                let base_ty = self.local.typeinfer(base)?;
                let field_offset = base_ty
                    .get_field_offset(field)
                    .ok_or_else(|| Error::NoSuchField(field_pos.clone(), field.clone()))?;
                if field_offset != 0 {
                    insts.push(Inst::ADDI(target, target, Imm::Lit(field_offset)));
                }
                Ok(insts)
            }

            ast::Expr::Index(base, idx) => {
                let mut insts = self.compile_addr(base, target)?;
                let base_ty = self.local.typeinfer(base)?;
                let elem_size = match &base_ty {
                    NormType::Array(_, elem) => elem.sizeof(),
                    _ => return Err(Error::NotIndexable(base.pos_or_default())),
                };

                // Pick a scratch reg for idx that differs from `target`.
                let idx_target = if target == Reg::T0 { Reg::T1 } else { Reg::T0 };

                // Spill `target` to stack while we evaluate idx (which may clobber it).
                insts.push(Inst::SUBI(Reg::SP, Reg::SP, Imm::Lit(1)));
                insts.push(Inst::STORE(target, Reg::SP, Imm::Lit(0)));
                let (idx_insts, idx_reg) = self.compile_expr(idx, idx_target)?;
                insts.extend(idx_insts);
                insts.push(Inst::LOAD(target, Reg::SP, Imm::Lit(0)));
                insts.push(Inst::ADDI(Reg::SP, Reg::SP, Imm::Lit(1)));

                // addr += idx * elem_size, via elem_size repeated ADDs.
                for _ in 0..elem_size {
                    insts.push(Inst::ADD(target, target, idx_reg));
                }
                Ok(insts)
            }

            ast::Expr::Deref(inner) => {
                // &(@p) == p — the value of the pointer IS the address.
                let (mut insts, reg) = self.compile_expr(inner, target)?;
                if reg != target {
                    insts.push(Inst::MOV(target, reg));
                }
                Ok(insts)
            }

            _ => Err(Error::NotAddressable(
                expr.pos_or_default(),
                format!("{:?}", expr),
            )),
        }
    }

    fn compile_lvalue(
        &mut self,
        lvalue: &'a ast::Expr,
        value_reg: Reg,
    ) -> Result<Vec<Inst<Reg, Imm>>, Error> {
        match lvalue {
            // Direct path for plain identifiers (avoids the ADDI/SUBI + STORE 0
            // pattern in favor of a single STORE with FP-relative immediate).
            ast::Expr::Ident((name, _)) => {
                let mut insts = Vec::new();
                if let Some(offset) = self.local.offset(name) {
                    // FP-relative store (signed offset; 2's-complement wraps).
                    let store_offset = (offset as i16) as u16 as usize;
                    insts.push(Inst::STORE(value_reg, Reg::FP, Imm::Lit(store_offset)));
                } else {
                    insts.push(Inst::STORE(value_reg, Reg::Z, Imm::Symbol(name.clone(), 0)));
                }
                Ok(insts)
            }

            ast::Expr::Deref(addr_expr) => {
                // Store through pointer: addr_expr's value is the destination address.
                // Use T1 so we don't clobber value_reg if it happens to be T0.
                let (addr_insts, addr_reg) = self.compile_expr(addr_expr, Reg::T1)?;
                Ok(chain!(
                    addr_insts,
                    vec![Inst::STORE(value_reg, addr_reg, Imm::Lit(0))]
                )
                .collect())
            }

            ast::Expr::Member(..) | ast::Expr::Index(..) => {
                // compile_addr may need to clobber T0/T1/T2 to evaluate indices, so
                // spill the rhs value to stack and reload it after the address is ready.
                let addr_reg = Reg::T1;
                let mut insts = vec![
                    Inst::SUBI(Reg::SP, Reg::SP, Imm::Lit(1)),
                    Inst::STORE(value_reg, Reg::SP, Imm::Lit(0)),
                ];
                insts.extend(self.compile_addr(lvalue, addr_reg)?);
                // Reload value into a register distinct from addr_reg.
                let val_reg = if addr_reg == Reg::T0 { Reg::T2 } else { Reg::T0 };
                insts.push(Inst::LOAD(val_reg, Reg::SP, Imm::Lit(0)));
                insts.push(Inst::ADDI(Reg::SP, Reg::SP, Imm::Lit(1)));
                insts.push(Inst::STORE(val_reg, addr_reg, Imm::Lit(0)));
                Ok(insts)
            }

            _ => Err(Error::InvalidLValue(
                lvalue.pos_or_default(),
                format!("{:?}", lvalue),
            )),
        }
    }
}
