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
use arch::{op::Op, reg::Reg};
use itertools::chain;

impl<'a> Global<'a> {
    pub fn func2code(&'a self, name: &str) -> Result<Code, Error> {
        match self.get(name) {
            Some(ast::Def::Func((_, pos), args, ret, stmts)) => {
                let loc = pos.clone();
                let context = Context::new(self, args, ret, &loc)?;
                context.compile(args, stmts, &loc)
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

/// 関数 1 つ分のコンパイル状態。
/// 新 ABI (SP 下向き) では事前 AST 走査が不要なので、フレームは動的に伸びる。
///
/// フレームレイアウト (callee 視点):
///   SP + 2 + ret_size .. SP + 3   : 戻り値スロット (head = SP+3, field i = SP+3+i)
///   SP + 2                        : saved RA          (callee の prologue で書く)
///   SP + 1                        : saved caller's SP (caller が書く)
///   SP + 0 .. SP - (args_total-1) : 引数 (arg_0 が最上位 SP+0)
///   SP - args_total ..            : locals / spills (下方向に伸びる)
struct Context<'a> {
    local: Local<'a>,
    scope_stack: Vec<ScopeFrame>,
    scope_counter: usize,
    /// 自関数の戻り値サイズ (signature から)
    ret_size: usize,
    /// 現在使用中のフレーム深度 (= args_total + locals_total + active_spills + 一時 call-outgoing)
    /// 次に空いているスロットは SP - current_bottom。
    current_bottom: usize,
    /// 次に置くローカル変数の SP+offset (最上位スロット、負方向に伸びる)
    /// 初期値は -args_total (引数の直下)。
    next_local_offset: i32,
}

/// SP+offset の符号付き値を Imm にエンコード (負は 16bit 二の補数)。
fn sp_off(off: i32) -> Imm {
    Imm::Lit((off as u16) as usize)
}

impl<'a> Context<'a> {
    fn new(
        global: &'a Global<'a>,
        args: &'a [(ast::Ident, ast::Type)],
        ret: &'a ast::Type,
        _loc: &Pos,
    ) -> Result<Self, Error> {
        let mut local = Local::fork(global);
        let ret_size = global.normtype(ret)?.sizeof();
        local.insert_args(args)?;
        // args_total: 引数の総サイズ
        let mut args_total: usize = 0;
        for (_, ty) in args {
            args_total += global.normtype(ty)?.sizeof();
        }
        Ok(Self {
            local,
            scope_stack: Vec::new(),
            scope_counter: 0,
            ret_size,
            current_bottom: args_total,
            next_local_offset: -(args_total as i32),
        })
    }

    /// 新 ABI prologue: saved RA を SP+2 に書くだけ。
    /// (saved caller_SP は caller が new_SP+1 にあらかじめ書いている。)
    fn prologue(&self) -> Vec<Op<Reg, Imm>> {
        vec![Op::store(Reg::RA, Reg::SP, Imm::Lit(2))]
    }

    /// 新 ABI epilogue:
    ///   LOAD T0, SP, 1      ; saved caller_SP
    ///   LOAD RA, SP, 2
    ///   MOV  SP, T0
    ///   RET
    fn epilogue_insts(&self) -> Vec<Op<Reg, Imm>> {
        vec![
            Op::load(Reg::T0, Reg::SP, Imm::Lit(1)),
            Op::load(Reg::RA, Reg::SP, Imm::Lit(2)),
            Op::mov(Reg::SP, Reg::T0),
            Op::ret(),
        ]
    }

    /// 一時 spill スロットを取り、SP+offset (負) を返す。
    /// current_bottom は「次に空いているスロットの深さ」を表す。
    /// 確保前: 次の空きは SP - current_bottom。確保後: そこから 1 つ下が次の空き。
    fn alloc_spill_slot(&mut self) -> i32 {
        let slot = -(self.current_bottom as i32);
        self.current_bottom += 1;
        slot
    }

    fn free_spill_slot(&mut self) {
        debug_assert!(self.current_bottom > 0);
        self.current_bottom -= 1;
    }

    fn fresh_scope_id(&mut self) -> usize {
        let id = self.scope_counter;
        self.scope_counter += 1;
        id
    }

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
        stmts: &'a [ast::Stmt],
        loc: &Pos,
    ) -> Result<Code, Error> {
        // 多 word 引数は未サポート
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
        }

        let mut insts = Vec::new();
        insts.extend(self.prologue());

        for stmt in stmts {
            insts.extend(self.compile_stmt(stmt)?);
        }

        // void 関数や明示的 return がない関数のための fall-through epilogue。
        insts.extend(self.epilogue_insts());

        Ok(Code(insts))
    }

    fn compile_stmt(&mut self, stmt: &'a ast::Stmt) -> Result<Vec<Op<Reg, Imm>>, Error> {
        match stmt {
            ast::Stmt::Block(scope_name, stmts) => {
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
                let (cond_insts, cond_reg) = self.compile_expr(cond, Reg::T0)?;
                let then_insts = self.compile_stmt(then_stmt)?;

                if let Some(else_stmt) = else_stmt {
                    let else_insts = self.compile_stmt(else_stmt)?;

                    let else_jump_offset = (then_insts.len() + 2) as u16;
                    let end_jump_offset = (else_insts.len() + 1) as u16;

                    Ok(chain!(
                        cond_insts,
                        vec![Op::eqi(Reg::T1, cond_reg, Imm::Lit(0))],
                        vec![Op::jumpifr(Reg::T1, Imm::Lit(else_jump_offset as usize))],
                        then_insts,
                        vec![Op::jumpr(Imm::Lit(end_jump_offset as usize))],
                        else_insts
                    )
                    .collect())
                } else {
                    let jump_offset = (then_insts.len() + 1) as u16;

                    Ok(chain!(
                        cond_insts,
                        vec![Op::eqi(Reg::T1, cond_reg, Imm::Lit(0))],
                        vec![Op::jumpifr(Reg::T1, Imm::Lit(jump_offset as usize))],
                        then_insts
                    )
                    .collect())
                }
            }

            ast::Stmt::Loop(cond, body) => {
                let (cond_insts, cond_reg) = self.compile_expr(cond, Reg::T0)?;
                let body_insts = self.compile_stmt(body)?;

                let exit_offset = (body_insts.len() + 2) as u16;
                let loop_offset = -((cond_insts.len() + 2 + body_insts.len()) as i32) as u16;

                Ok(chain!(
                    cond_insts.clone(),
                    vec![Op::eqi(Reg::T1, cond_reg, Imm::Lit(0))],
                    vec![Op::jumpifr(Reg::T1, Imm::Lit(exit_offset as usize))],
                    body_insts,
                    vec![Op::jumpr(Imm::Lit(loop_offset as usize))]
                )
                .collect())
            }

            ast::Stmt::Break((name, _name_pos), pos) => {
                let id = self
                    .find_scope(name)
                    .ok_or_else(|| Error::UnknownScope(pos.clone(), name.clone()))?;
                Ok(vec![Op::jumpr(Imm::ScopeExit(id))])
            }

            ast::Stmt::Continue((name, _name_pos), pos) => {
                let id = self
                    .find_scope(name)
                    .ok_or_else(|| Error::UnknownScope(pos.clone(), name.clone()))?;
                Ok(vec![Op::jumpr(Imm::ScopeEntry(id))])
            }

            ast::Stmt::Var(ident, ty, init) => {
                let (name, pos) = ident;
                let norm_ty = self.local.normtype(ty).map_err(|e| {
                    Error::TypeCollectionFailed(
                        pos.clone(),
                        format!("local variable {}: {}", name, e),
                    )
                })?;
                let size = norm_ty.sizeof();
                // 先頭 word の offset (低アドレス側 = field 0 の位置)
                // 例: args_total=1, size=1 なら SP-1。 size=2 なら SP-2..SP-1 (head=SP-2)。
                let head_offset = self.next_local_offset - (size as i32) + 1;
                self.next_local_offset -= size as i32;
                self.current_bottom += size;
                self.local
                    .insert(ident, norm_ty, head_offset)
                    .map_err(|e| {
                        Error::TypeCollectionFailed(
                            pos.clone(),
                            format!("local variable {}: {}", name, e),
                        )
                    })?;

                let mut insts: Vec<Op<Reg, Imm>> = Vec::new();
                if let Some(init_expr) = init {
                    let (init_insts, init_reg) = self.compile_expr(init_expr, Reg::T0)?;
                    insts.extend(init_insts);
                    insts.push(Op::store(init_reg, Reg::SP, sp_off(head_offset)));
                }
                Ok(insts)
            }

            ast::Stmt::Return(expr) => {
                let mut insts = Vec::new();
                if let Some(expr) = expr {
                    let (expr_insts, expr_reg) = self.compile_expr(expr, Reg::T0)?;
                    insts.extend(expr_insts);
                    if self.ret_size == 1 {
                        // 単一 word 戻り値: SP+3 に書く (戻り値スロットは SP+3 から開始)
                        insts.push(Op::store(expr_reg, Reg::SP, Imm::Lit(3)));
                    } else if self.ret_size > 1 {
                        // 多 word 戻り値: expr は対象アドレスを返す (multi-word ident は addr を返す慣習)
                        // expr_reg にあるアドレスから ret_size word をコピーして SP+3..SP+(2+ret_size) に置く。
                        // 1 word ずつ T1 経由でコピー。
                        let addr_reg = expr_reg;
                        for i in 0..self.ret_size {
                            insts.push(Op::load(Reg::T1, addr_reg, Imm::Lit(i)));
                            insts.push(Op::store(Reg::T1, Reg::SP, Imm::Lit(3 + i)));
                        }
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
    ) -> Result<(Vec<Op<Reg, Imm>>, Reg), Error> {
        let (insts, result_reg) = match expr {
            ast::Expr::NumberLit(n) => {
                let mut insts = Vec::new();
                insts.push(Op::loadi(target, Imm::Lit(*n as usize)));
                (insts, target)
            }

            ast::Expr::CharLit(c) => {
                let mut insts = Vec::new();
                insts.push(Op::loadi(target, Imm::Lit(*c as usize)));
                (insts, target)
            }

            ast::Expr::StringLit(_s) => {
                return Err(Error::UnsupportedExpression(
                    expr.pos_or_default(),
                    "string literal as runtime value; use a named const instead".to_string(),
                ));
            }

            ast::Expr::Ident((name, _)) => {
                let mut insts = Vec::new();
                let ty = self.local.typeinfer(expr).ok();
                let multi_word = ty.as_ref().map(|t| t.sizeof() > 1).unwrap_or(false);

                if let Some(offset) = self.local.offset(name) {
                    if multi_word {
                        insts.push(Op::addi(target, Reg::SP, sp_off(offset)));
                    } else {
                        insts.push(Op::load(target, Reg::SP, sp_off(offset)));
                    }
                } else if name == "csr" {
                    insts.push(Op::mov(target, Reg::CSR));
                } else {
                    match self.local.global_def(name) {
                        Some(ast::Def::Static(..)) | Some(ast::Def::Const(..)) => {
                            if multi_word {
                                insts.push(Op::loadi(target, Imm::Symbol(name.clone(), 0)));
                            } else {
                                insts.push(Op::load(target, Reg::Z, Imm::Symbol(name.clone(), 0)));
                            }
                        }
                        Some(ast::Def::Func(..)) | Some(ast::Def::Asm(..)) => {
                            insts.push(Op::loadi(target, Imm::Label(name.clone())));
                        }
                        _ => {
                            insts.push(Op::load(target, Reg::Z, Imm::Symbol(name.clone(), 0)));
                        }
                    }
                }
                (insts, target)
            }

            ast::Expr::Binary(op, lhs, rhs) => {
                let mut prelude: Vec<Op<Reg, Imm>> = Vec::new();
                let (lhs_insts, lhs_inner_reg) = self.compile_expr(lhs, Reg::T0)?;
                prelude.extend(lhs_insts);
                if lhs_inner_reg != Reg::T0 {
                    prelude.push(Op::mov(Reg::T0, lhs_inner_reg));
                }
                let spill = self.alloc_spill_slot();
                prelude.push(Op::store(Reg::T0, Reg::SP, sp_off(spill)));
                let (rhs_insts, rhs_inner_reg) = self.compile_expr(rhs, Reg::T1)?;
                prelude.extend(rhs_insts);
                if rhs_inner_reg != Reg::T1 {
                    prelude.push(Op::mov(Reg::T1, rhs_inner_reg));
                }
                prelude.push(Op::load(Reg::T0, Reg::SP, sp_off(spill)));
                self.free_spill_slot();
                let lhs_insts = prelude;
                let rhs_insts: Vec<Op<Reg, Imm>> = Vec::new();
                let lhs_reg = Reg::T0;
                let rhs_reg = Reg::T1;

                let op_insts = match op {
                    ast::BinaryOp::Add => vec![Op::add(target, lhs_reg, rhs_reg)],
                    ast::BinaryOp::Sub => vec![Op::sub(target, lhs_reg, rhs_reg)],
                    ast::BinaryOp::And => vec![Op::and(target, lhs_reg, rhs_reg)],
                    ast::BinaryOp::Or => vec![Op::or(target, lhs_reg, rhs_reg)],
                    ast::BinaryOp::Xor => vec![Op::xor(target, lhs_reg, rhs_reg)],
                    ast::BinaryOp::Eq => vec![Op::eq(target, lhs_reg, rhs_reg)],
                    ast::BinaryOp::Ne => vec![Op::neq(target, lhs_reg, rhs_reg)],
                    ast::BinaryOp::Lt => vec![Op::lt(target, lhs_reg, rhs_reg)],
                    ast::BinaryOp::Le => vec![
                        Op::lt(Reg::T2, rhs_reg, lhs_reg),
                        Op::eqi(target, Reg::T2, Imm::Lit(0)),
                    ],
                    ast::BinaryOp::Gt => vec![Op::lt(target, rhs_reg, lhs_reg)],
                    ast::BinaryOp::Ge => vec![
                        Op::lt(Reg::T2, lhs_reg, rhs_reg),
                        Op::eqi(target, Reg::T2, Imm::Lit(0)),
                    ],
                    ast::BinaryOp::Shl | ast::BinaryOp::Shr => {
                        let shift = match op {
                            ast::BinaryOp::Shl => Op::sl(Reg::T0, Reg::T0),
                            ast::BinaryOp::Shr => Op::sr(Reg::T0, Reg::T0),
                            _ => unreachable!(),
                        };
                        let mut v = vec![
                            Op::eqi(Reg::T2, Reg::T1, Imm::Lit(0)),
                            Op::jumpifr(Reg::T2, Imm::Lit(4)),
                            shift,
                            Op::subi(Reg::T1, Reg::T1, Imm::Lit(1)),
                            Op::jumpr(Imm::Lit((-4i32) as u16 as usize)),
                        ];
                        if target != Reg::T0 {
                            v.push(Op::mov(target, Reg::T0));
                        }
                        v
                    }

                    ast::BinaryOp::Mul => {
                        let mut v = vec![
                            Op::mov(Reg::T2, Reg::T0),
                            Op::loadi(Reg::T0, Imm::Lit(0)),
                            Op::eqi(Reg::T3, Reg::T1, Imm::Lit(0)),
                            Op::jumpifr(Reg::T3, Imm::Lit(8)),
                            Op::andi(Reg::T3, Reg::T1, Imm::Lit(1)),
                            Op::eqi(Reg::T3, Reg::T3, Imm::Lit(0)),
                            Op::jumpifr(Reg::T3, Imm::Lit(2)),
                            Op::add(Reg::T0, Reg::T0, Reg::T2),
                            Op::sl(Reg::T2, Reg::T2),
                            Op::sr(Reg::T1, Reg::T1),
                            Op::jumpr(Imm::Lit((-8i32) as u16 as usize)),
                        ];
                        if target != Reg::T0 {
                            v.push(Op::mov(target, Reg::T0));
                        }
                        v
                    }

                    ast::BinaryOp::Div => {
                        let mut v = vec![
                            Op::mov(Reg::T2, Reg::T0),
                            Op::loadi(Reg::T0, Imm::Lit(0)),
                            Op::loadi(Reg::T4, Imm::Lit(16)),
                            Op::eqi(Reg::T3, Reg::T4, Imm::Lit(0)),
                            Op::jumpifr(Reg::T3, Imm::Lit(11)),
                            Op::andi(Reg::T3, Reg::T2, Imm::Lit(0x8000)),
                            Op::neqi(Reg::T3, Reg::T3, Imm::Lit(0)),
                            Op::sl(Reg::T2, Reg::T2),
                            Op::sl(Reg::T0, Reg::T0),
                            Op::add(Reg::T0, Reg::T0, Reg::T3),
                            Op::lt(Reg::T3, Reg::T0, Reg::T1),
                            Op::jumpifr(Reg::T3, Imm::Lit(3)),
                            Op::sub(Reg::T0, Reg::T0, Reg::T1),
                            Op::ori(Reg::T2, Reg::T2, Imm::Lit(1)),
                            Op::subi(Reg::T4, Reg::T4, Imm::Lit(1)),
                            Op::jumpr(Imm::Lit((-12i32) as u16 as usize)),
                        ];
                        if target != Reg::T2 {
                            v.push(Op::mov(target, Reg::T2));
                        }
                        v
                    }

                    ast::BinaryOp::Mod => {
                        let mut v = vec![
                            Op::mov(Reg::T2, Reg::T0),
                            Op::loadi(Reg::T0, Imm::Lit(0)),
                            Op::loadi(Reg::T4, Imm::Lit(16)),
                            Op::eqi(Reg::T3, Reg::T4, Imm::Lit(0)),
                            Op::jumpifr(Reg::T3, Imm::Lit(10)),
                            Op::andi(Reg::T3, Reg::T2, Imm::Lit(0x8000)),
                            Op::neqi(Reg::T3, Reg::T3, Imm::Lit(0)),
                            Op::sl(Reg::T2, Reg::T2),
                            Op::sl(Reg::T0, Reg::T0),
                            Op::add(Reg::T0, Reg::T0, Reg::T3),
                            Op::lt(Reg::T3, Reg::T0, Reg::T1),
                            Op::jumpifr(Reg::T3, Imm::Lit(2)),
                            Op::sub(Reg::T0, Reg::T0, Reg::T1),
                            Op::subi(Reg::T4, Reg::T4, Imm::Lit(1)),
                            Op::jumpr(Imm::Lit((-11i32) as u16 as usize)),
                        ];
                        if target != Reg::T0 {
                            v.push(Op::mov(target, Reg::T0));
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
                        if operand_reg != target {
                            vec![Op::mov(target, operand_reg)]
                        } else {
                            vec![]
                        }
                    }
                    ast::UnaryOp::Neg => {
                        vec![Op::sub(target, Reg::Z, operand_reg)]
                    }
                    ast::UnaryOp::Not => {
                        vec![Op::not(target, operand_reg)]
                    }
                };

                (chain!(operand_insts, op_insts).collect(), target)
            }

            ast::Expr::Deref(operand) => {
                let (operand_insts, operand_reg) = self.compile_expr(operand, Reg::T0)?;
                let insts = chain!(
                    operand_insts,
                    vec![Op::load(target, operand_reg, Imm::Lit(0))]
                )
                .collect();
                (insts, target)
            }

            ast::Expr::Addr(operand) => {
                let addr_insts = self.compile_addr(operand, target)?;
                (addr_insts, target)
            }

            ast::Expr::Index(..) | ast::Expr::Member(..) => {
                let addr_insts = self.compile_addr(expr, target)?;
                let insts =
                    chain!(addr_insts, vec![Op::load(target, target, Imm::Lit(0))]).collect();
                (insts, target)
            }

            ast::Expr::Call(func_expr, args) => self.compile_call(expr, func_expr, args, target)?,

            ast::Expr::Cond(cond, then_expr, else_expr) => {
                let mut insts = Vec::new();

                let (cond_insts, cond_reg) = self.compile_expr(cond, Reg::T0)?;
                insts.extend(cond_insts);
                insts.push(Op::eqi(Reg::T1, cond_reg, Imm::Lit(0)));

                let (then_insts, then_reg) = self.compile_expr(then_expr, target)?;
                let (else_insts, else_reg) = self.compile_expr(else_expr, target)?;

                let else_jump_offset = (then_insts.len() + 1) as u16;
                insts.push(Op::jumpifr(Reg::T1, Imm::Lit(else_jump_offset as usize)));

                insts.extend(then_insts);
                if then_reg != target {
                    insts.push(Op::mov(target, then_reg));
                }

                let end_jump_offset = else_insts.len() as u16;
                if else_reg != target {
                    insts.push(Op::jumpr(Imm::Lit((end_jump_offset + 1) as usize)));
                } else {
                    insts.push(Op::jumpr(Imm::Lit(end_jump_offset as usize)));
                }

                insts.extend(else_insts);
                if else_reg != target {
                    insts.push(Op::mov(target, else_reg));
                }

                (insts, target)
            }

            ast::Expr::SizeofType(typ) => {
                let mut insts = Vec::new();
                let norm_type = self.local.normtype(typ).map_err(|_| {
                    Error::TypeCollectionFailed(Pos::default(), "sizeof".to_string())
                })?;
                let size = norm_type.sizeof() as u16;
                insts.push(Op::loadi(target, Imm::Lit(size as usize)));
                (insts, target)
            }

            ast::Expr::Cast(inner, _) => {
                let (insts, reg) = self.compile_expr(inner, target)?;
                (insts, reg)
            }

            _ => {
                let mut insts = Vec::new();
                insts.push(Op::loadi(target, Imm::Lit(0)));
                (insts, target)
            }
        };

        Ok((insts, result_reg))
    }

    /// 新 ABI の関数呼び出し (SP 基準):
    ///
    /// 呼出直前の current_bottom を L とすると、各スロットの caller_SP からの offset:
    ///   -L .. -(L + ret_size - 1) : Return slots (head = -(L + ret_size - 1) の field 0)
    ///                               ※ 単一 word: ret slot = -L
    ///   -(L + ret_size)           : saved RA (callee の prologue で書く)
    ///   -(L + ret_size + 1)       : saved caller_SP (caller が書く)
    ///   -(L + ret_size + 2)       : new_SP+0 = arg_0 の最上位 ← new_SP がここ
    ///   -(L + ret_size + 2 + 1)   : arg_0 の続き or arg_1
    ///       :
    ///
    /// 手順:
    ///   1. (CALLR の場合のみ) func ptr を T0 へ評価 → caller の spill に退避
    ///   2. T1 := SP - (L + ret_size + 2)  (= new_SP)
    ///   3. STORE SP, T1, 1   (= 新フレームの saved caller_SP)
    ///   4. 各 arg を T0 へ評価 → STORE T0, T1, -cum   (cum は今まで積んだ word 数)
    ///        ※ 引数評価中に内側 call が起きると T1 が破壊されるため、毎回再構築する。
    ///   5. MOV SP, T1
    ///   6. CALL(Label) または CALLR(T0)
    ///   7. 戻り後 SP は caller_SP に戻っている。
    ///      単一 word 戻り値: LOAD target, SP, -L
    fn compile_call(
        &mut self,
        expr: &'a ast::Expr,
        func_expr: &'a ast::Expr,
        args: &'a [ast::Expr],
        target: Reg,
    ) -> Result<(Vec<Op<Reg, Imm>>, Reg), Error> {
        let (ret_size, arg_sizes) = self.local.callee_signature(func_expr, args)?;
        if args.len() != arg_sizes.len() {
            return Err(Error::UnsupportedExpression(
                expr.pos_or_default(),
                format!(
                    "argument count mismatch: callee expects {}, given {}",
                    arg_sizes.len(),
                    args.len()
                ),
            ));
        }
        if ret_size > 1 {
            return Err(Error::UnsupportedExpression(
                expr.pos_or_default(),
                format!("multi-word return value (size {}) not supported", ret_size),
            ));
        }
        for (i, &sz) in arg_sizes.iter().enumerate() {
            if sz > 1 {
                return Err(Error::UnsupportedExpression(
                    args[i].pos_or_default(),
                    format!("multi-word argument (size {}) not supported", sz),
                ));
            }
        }

        let direct = if let ast::Expr::Ident((name, _)) = func_expr {
            matches!(
                self.local.global_def(name),
                Some(ast::Def::Func(..)) | Some(ast::Def::Asm(..)),
            )
        } else {
            false
        };

        let args_total: usize = arg_sizes.iter().sum();
        // call frame 占有スロット数 (caller_SP の下に確保):
        //   ret_size (Return) + 1 (saved RA) + 1 (saved caller_SP) + args_total
        let call_frame = ret_size + 2 + args_total;
        let mut insts: Vec<Op<Reg, Imm>> = Vec::new();

        // 間接呼びの場合は事前に func ptr を T0 で評価して caller の spill へ退避
        let func_ptr_spill: Option<i32> = if !direct {
            let (func_insts, func_reg) = self.compile_expr(func_expr, Reg::T0)?;
            insts.extend(func_insts);
            if func_reg != Reg::T0 {
                insts.push(Op::mov(Reg::T0, func_reg));
            }
            let s = self.alloc_spill_slot();
            insts.push(Op::store(Reg::T0, Reg::SP, sp_off(s)));
            Some(s)
        } else {
            None
        };

        // 呼び出し直前の current_bottom を記録 (戻り値ロード時に使う)
        let bottom_before = self.current_bottom;
        // 内側 call から見ても call frame 領域が「使用中」になるよう current_bottom に加算
        self.current_bottom += call_frame;

        // new_SP = caller_SP - (bottom_before + ret_size + 2)
        let new_sp_depth = bottom_before + ret_size + 2;
        let build_new_sp = |insts: &mut Vec<Op<Reg, Imm>>, depth: usize| {
            insts.push(Op::loadi(Reg::T1, Imm::Lit(depth)));
            insts.push(Op::sub(Reg::T1, Reg::SP, Reg::T1));
        };

        // (2)(3) new_SP を T1 で構築し、saved caller_SP を new_SP+1 に書く
        build_new_sp(&mut insts, new_sp_depth);
        insts.push(Op::store(Reg::SP, Reg::T1, Imm::Lit(1)));

        // (4) 各引数を評価し new_SP - cum へ STORE。
        //   単一 word arg: head_offset (from T1) = -cum。
        //   引数評価中の内側 call が T1 を破壊する可能性があるため、
        //   STORE 直前に T1 を再構築する。
        let mut cum: usize = 0;
        for (i, arg) in args.iter().enumerate() {
            let (arg_insts, arg_reg) = self.compile_expr(arg, Reg::T0)?;
            insts.extend(arg_insts);
            if arg_reg != Reg::T0 {
                insts.push(Op::mov(Reg::T0, arg_reg));
            }
            build_new_sp(&mut insts, new_sp_depth);
            let off = -(cum as i32);
            insts.push(Op::store(Reg::T0, Reg::T1, sp_off(off)));
            cum += arg_sizes[i];
            let _ = i;
        }

        // (5)(6) SP 切替 + CALL
        if direct {
            let name = if let ast::Expr::Ident((n, _)) = func_expr {
                n.clone()
            } else {
                unreachable!()
            };
            build_new_sp(&mut insts, new_sp_depth);
            insts.push(Op::mov(Reg::SP, Reg::T1));
            insts.push(Op::call(Imm::Label(name)));
        } else {
            let s = func_ptr_spill.unwrap();
            insts.push(Op::load(Reg::T0, Reg::SP, sp_off(s)));
            build_new_sp(&mut insts, new_sp_depth);
            insts.push(Op::mov(Reg::SP, Reg::T1));
            insts.push(Op::callr(Reg::T0));
        }

        // 内側 call 用に増やしていた current_bottom を戻す
        self.current_bottom -= call_frame;

        // (7) 戻り値ロード (戻り後 SP は caller_SP)。
        //   単一 word: Return slot は SP - bottom_before。
        if ret_size == 1 {
            let ret_off = -(bottom_before as i32);
            insts.push(Op::load(target, Reg::SP, sp_off(ret_off)));
        }

        // func_ptr 用 spill を解放
        if func_ptr_spill.is_some() {
            self.free_spill_slot();
        }

        Ok((insts, target))
    }

    fn compile_addr(
        &mut self,
        expr: &'a ast::Expr,
        target: Reg,
    ) -> Result<Vec<Op<Reg, Imm>>, Error> {
        match expr {
            ast::Expr::Ident((name, _)) => {
                let mut insts = Vec::new();
                if let Some(offset) = self.local.offset(name) {
                    insts.push(Op::addi(target, Reg::SP, sp_off(offset)));
                } else {
                    match self.local.global_def(name) {
                        Some(ast::Def::Func(..)) | Some(ast::Def::Asm(..)) => {
                            insts.push(Op::loadi(target, Imm::Label(name.clone())));
                        }
                        _ => {
                            insts.push(Op::loadi(target, Imm::Symbol(name.clone(), 0)));
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
                    insts.push(Op::addi(target, target, Imm::Lit(field_offset)));
                }
                Ok(insts)
            }

            ast::Expr::Index(base, idx) => {
                let base_ty = self.local.typeinfer(base)?;
                let (mut insts, elem_size) = match &base_ty {
                    NormType::Array(_, elem) => {
                        let elem_size = elem.sizeof();
                        (self.compile_addr(base, target)?, elem_size)
                    }
                    NormType::Addr(elem) => {
                        let elem_size = elem.sizeof();
                        let (load_insts, reg) = self.compile_expr(base, target)?;
                        let mut v = load_insts;
                        if reg != target {
                            v.push(Op::mov(target, reg));
                        }
                        (v, elem_size)
                    }
                    _ => return Err(Error::NotIndexable(base.pos_or_default())),
                };

                let idx_target = if target == Reg::T0 { Reg::T1 } else { Reg::T0 };

                let spill = self.alloc_spill_slot();
                insts.push(Op::store(target, Reg::SP, sp_off(spill)));
                let (idx_insts, idx_reg) = self.compile_expr(idx, idx_target)?;
                insts.extend(idx_insts);
                insts.push(Op::load(target, Reg::SP, sp_off(spill)));
                self.free_spill_slot();

                for _ in 0..elem_size {
                    insts.push(Op::add(target, target, idx_reg));
                }
                Ok(insts)
            }

            ast::Expr::Deref(inner) => {
                let (mut insts, reg) = self.compile_expr(inner, target)?;
                if reg != target {
                    insts.push(Op::mov(target, reg));
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
    ) -> Result<Vec<Op<Reg, Imm>>, Error> {
        match lvalue {
            ast::Expr::Ident((name, _)) => {
                let mut insts = Vec::new();
                if let Some(offset) = self.local.offset(name) {
                    insts.push(Op::store(value_reg, Reg::SP, sp_off(offset)));
                } else if name == "csr" {
                    insts.push(Op::mov(Reg::CSR, value_reg));
                } else {
                    insts.push(Op::store(value_reg, Reg::Z, Imm::Symbol(name.clone(), 0)));
                }
                Ok(insts)
            }

            ast::Expr::Deref(addr_expr) => {
                let (addr_insts, addr_reg) = self.compile_expr(addr_expr, Reg::T1)?;
                Ok(chain!(
                    addr_insts,
                    vec![Op::store(value_reg, addr_reg, Imm::Lit(0))]
                )
                .collect())
            }

            ast::Expr::Member(..) | ast::Expr::Index(..) => {
                let addr_reg = Reg::T1;
                let spill = self.alloc_spill_slot();
                let mut insts = vec![Op::store(value_reg, Reg::SP, sp_off(spill))];
                insts.extend(self.compile_addr(lvalue, addr_reg)?);
                let val_reg = if addr_reg == Reg::T0 {
                    Reg::T2
                } else {
                    Reg::T0
                };
                insts.push(Op::load(val_reg, Reg::SP, sp_off(spill)));
                self.free_spill_slot();
                insts.push(Op::store(val_reg, addr_reg, Imm::Lit(0)));
                Ok(insts)
            }

            _ => Err(Error::InvalidLValue(
                lvalue.pos_or_default(),
                format!("{:?}", lvalue),
            )),
        }
    }
}
