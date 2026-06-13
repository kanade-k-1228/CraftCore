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
use std::collections::{HashMap, HashSet};

/// レジスタ割付に使う callee-save レジスタ。
const S_REGS: [Reg; 4] = [Reg::S0, Reg::S1, Reg::S2, Reg::S3];

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

/// 関数 1 つ分のコンパイル状態 (RISC-V 風固定フレーム ABI)。
/// 本体を 1 パスでコンパイルしてフレーム深 (peak_bottom) と outgoing 引数数 (max_outgoing)
/// を測り、N = max_outgoing + peak_bottom を確定する。その後 prologue/epilogue を付け、
/// FrameRel(off) → Lit(N + off) を解決する (フレーム参照は全て FrameRel で表す)。
///
/// フレームレイアウト (body SP 基準, 低位→高位):
///   sp + 0 .. max_outgoing-1   : outgoing 引数 (call 時に第3引数以降を置く)
///   sp + max_outgoing ..       : saved RA (ra_off) + saved-s + a0/a1 spill + locals + 一時 spill
///   sp + N (= 呼出側 sp) ..     : caller が積んだ第3引数以降
/// エントリ SP 基準オフセット (FrameRel) は a0/a1 spill が -1,-2、saved-s が -3.. 、
/// saved RA がその直下、locals/spill がさらに下。caller stack 引数のみ off >= 0。
/// エントリ SP 基準では local/spill/arg-spill が off<0、caller stack 引数が off>=0。
struct Context<'a> {
    local: Local<'a>,
    scope_stack: Vec<ScopeFrame>,
    scope_counter: usize,
    /// 自関数の戻り値サイズ (signature から)
    ret_size: usize,
    /// 引数の本数 (a0/a1 へスピルする数 = min(argc,2) の決定に使う)
    argc: usize,
    /// 現在使用中のフレーム深度 (a0/a1 spill + locals + 一時 spill)。
    /// 次に確保するスロットの深さは current_bottom+1 (off = -(current_bottom+1))。
    current_bottom: usize,
    /// current_bottom のピーク (= フレームの local/spill/arg-spill 総深 D)。
    peak_bottom: usize,
    /// この関数が行う call の最大スタック引数数 (= 第3引数以降の最大本数, outgoing 領域サイズ)。
    max_outgoing: usize,
    /// 本体に call/callr を含むか。leaf 関数 (false) は ra を退避しない。
    nonleaf: bool,
    /// レジスタ割付されたローカル変数 (name → s0-s3)。callee-save なので呼出を跨いで保持される。
    reg_vars: HashMap<String, Reg>,
}

/// 式が関数呼び出しを含むか (leaf 判定用、保守的に全部分式を走査)。
fn expr_has_call(e: &ast::Expr) -> bool {
    match e {
        ast::Expr::Call(..) => true,
        ast::Expr::Binary(_, a, b) | ast::Expr::Index(a, b) => expr_has_call(a) || expr_has_call(b),
        ast::Expr::Unary(_, a)
        | ast::Expr::Addr(a)
        | ast::Expr::Deref(a)
        | ast::Expr::Cast(a, _)
        | ast::Expr::Member(a, _)
        | ast::Expr::SizeofExpr(a) => expr_has_call(a),
        ast::Expr::ArrayLit(es) => es.iter().any(expr_has_call),
        ast::Expr::StructLit(fs) => fs.iter().any(|(_, e)| expr_has_call(e)),
        _ => false,
    }
}

/// 文 (とその部分式) が関数呼び出しを含むか。
fn stmt_has_call(s: &ast::Stmt) -> bool {
    match s {
        ast::Stmt::Block(_, ss) => ss.iter().any(stmt_has_call),
        ast::Stmt::Expr(e) => expr_has_call(e),
        ast::Stmt::Assign(l, r) => expr_has_call(l) || expr_has_call(r),
        ast::Stmt::Cond(c, t, f) => {
            expr_has_call(c) || stmt_has_call(t) || f.as_ref().is_some_and(|x| stmt_has_call(x))
        }
        ast::Stmt::Loop(c, b) => expr_has_call(c) || stmt_has_call(b),
        ast::Stmt::Return(e) => e.as_ref().is_some_and(expr_has_call),
        ast::Stmt::Var(_, _, init) => init.as_ref().is_some_and(expr_has_call),
        ast::Stmt::Break(..) | ast::Stmt::Continue(..) => false,
    }
}

/// 式に含まれる全ての ident 名を集める (address-taken 判定用、保守的)。
fn collect_idents(e: &ast::Expr, out: &mut HashSet<String>) {
    match e {
        ast::Expr::Ident((n, _)) => {
            out.insert(n.clone());
        }
        ast::Expr::Binary(_, a, b) | ast::Expr::Index(a, b) => {
            collect_idents(a, out);
            collect_idents(b, out);
        }
        ast::Expr::Unary(_, a)
        | ast::Expr::Addr(a)
        | ast::Expr::Deref(a)
        | ast::Expr::Cast(a, _)
        | ast::Expr::Member(a, _)
        | ast::Expr::SizeofExpr(a) => collect_idents(a, out),
        ast::Expr::Call(f, args) => {
            collect_idents(f, out);
            for a in args {
                collect_idents(a, out);
            }
        }
        ast::Expr::ArrayLit(es) => es.iter().for_each(|x| collect_idents(x, out)),
        ast::Expr::StructLit(fs) => fs.iter().for_each(|(_, x)| collect_idents(x, out)),
        _ => {}
    }
}

/// 式を走査し、Addr(...) の中に現れる ident を address-taken として記録する。
fn scan_expr_addr(e: &ast::Expr, taken: &mut HashSet<String>) {
    if let ast::Expr::Addr(inner) = e {
        collect_idents(inner, taken);
    }
    match e {
        ast::Expr::Binary(_, a, b) | ast::Expr::Index(a, b) => {
            scan_expr_addr(a, taken);
            scan_expr_addr(b, taken);
        }
        ast::Expr::Unary(_, a)
        | ast::Expr::Addr(a)
        | ast::Expr::Deref(a)
        | ast::Expr::Cast(a, _)
        | ast::Expr::Member(a, _)
        | ast::Expr::SizeofExpr(a) => scan_expr_addr(a, taken),
        ast::Expr::Call(f, args) => {
            scan_expr_addr(f, taken);
            for a in args {
                scan_expr_addr(a, taken);
            }
        }
        ast::Expr::ArrayLit(es) => es.iter().for_each(|x| scan_expr_addr(x, taken)),
        ast::Expr::StructLit(fs) => fs.iter().for_each(|(_, x)| scan_expr_addr(x, taken)),
        _ => {}
    }
}

/// 本体を走査して Var 宣言 (name, type) と address-taken 名を集める (レジスタ割付用)。
fn scan_stmt_alloc<'a>(
    s: &'a ast::Stmt,
    decls: &mut Vec<(String, &'a ast::Type)>,
    taken: &mut HashSet<String>,
) {
    match s {
        ast::Stmt::Block(_, ss) => ss.iter().for_each(|x| scan_stmt_alloc(x, decls, taken)),
        ast::Stmt::Expr(e) => scan_expr_addr(e, taken),
        ast::Stmt::Assign(l, r) => {
            scan_expr_addr(l, taken);
            scan_expr_addr(r, taken);
        }
        ast::Stmt::Cond(c, t, f) => {
            scan_expr_addr(c, taken);
            scan_stmt_alloc(t, decls, taken);
            if let Some(e) = f {
                scan_stmt_alloc(e, decls, taken);
            }
        }
        ast::Stmt::Loop(c, b) => {
            scan_expr_addr(c, taken);
            scan_stmt_alloc(b, decls, taken);
        }
        ast::Stmt::Return(e) => {
            if let Some(x) = e {
                scan_expr_addr(x, taken);
            }
        }
        ast::Stmt::Var(ident, ty, init) => {
            decls.push((ident.0.clone(), ty));
            if let Some(x) = init {
                scan_expr_addr(x, taken);
            }
        }
        ast::Stmt::Break(..) | ast::Stmt::Continue(..) => {}
    }
}

/// 関数エントリ SP 基準の符号付きフレームオフセットを Imm にエンコード。
/// compile 完了時に Lit((N + off) as u16) へ解決される。
fn sp_off(off: i32) -> Imm {
    Imm::FrameRel(off)
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
        let argc = args.len();
        // a0/a1 にスピルする引数の数 (各 1 word)。フレーム最上位 (off -1, -2) を占有する。
        let argc_spilled = argc.min(2);
        Ok(Self {
            local,
            scope_stack: Vec::new(),
            scope_counter: 0,
            ret_size,
            argc,
            current_bottom: argc_spilled,
            peak_bottom: argc_spilled,
            max_outgoing: 0,
            nonleaf: false,
            reg_vars: HashMap::new(),
        })
    }

    /// RISC-V 風 prologue (固定フレーム):
    ///   subi(sp, sp, N)                        ; フレーム確保 (FrameRel(0) → Lit(N))
    ///   store(ra, sp, ra_off)                  ; nonleaf のみ
    ///   store(s_k, sp, saved_s_off(k))         ; 使用する callee-save レジスタ
    ///   store(a0, sp, -1) / store(a1, sp, -2)  ; 引数レジスタをフレームへスピル
    fn prologue(&self) -> Vec<Op<Reg, Imm>> {
        let mut v = vec![Op::subi(Reg::SP, Reg::SP, Imm::FrameRel(0))];
        if self.nonleaf {
            v.push(Op::store(Reg::RA, Reg::SP, Imm::FrameRel(self.ra_off())));
        }
        // 使用する s-reg (caller の値) をフレームへ退避。
        for k in 0..self.reg_vars.len() {
            v.push(Op::store(S_REGS[k], Reg::SP, Imm::FrameRel(self.saved_s_off(k))));
        }
        if self.argc >= 1 {
            v.push(Op::store(Reg::A0, Reg::SP, Imm::FrameRel(-1)));
        }
        if self.argc >= 2 {
            v.push(Op::store(Reg::A1, Reg::SP, Imm::FrameRel(-2)));
        }
        v
    }

    /// RISC-V 風 epilogue:
    ///   load(s_k, sp, saved_s_off(k))    ; callee-save レジスタ復元
    ///   load(ra, sp, ra_off)             ; nonleaf のみ
    ///   addi(sp, sp, N)                  ; フレーム解放 (FrameRel(0) → Lit(N))
    ///   ret()
    fn epilogue_insts(&self) -> Vec<Op<Reg, Imm>> {
        let mut v = Vec::new();
        // 使用した s-reg を caller の値へ復元。
        for k in 0..self.reg_vars.len() {
            v.push(Op::load(S_REGS[k], Reg::SP, Imm::FrameRel(self.saved_s_off(k))));
        }
        if self.nonleaf {
            v.push(Op::load(Reg::RA, Reg::SP, Imm::FrameRel(self.ra_off())));
        }
        v.push(Op::addi(Reg::SP, Reg::SP, Imm::FrameRel(0)));
        v.push(Op::ret());
        v
    }

    /// 一時 spill スロットを取り、エントリ SP 基準オフセット (負) を返す。
    /// current_bottom は「占有済みの最大深さ」。次の空きは深さ current_bottom+1 (off = -(current_bottom+1))。
    fn alloc_spill_slot(&mut self) -> i32 {
        self.current_bottom += 1;
        if self.current_bottom > self.peak_bottom {
            self.peak_bottom = self.current_bottom;
        }
        -(self.current_bottom as i32)
    }

    fn free_spill_slot(&mut self) {
        debug_assert!(self.current_bottom > 0);
        self.current_bottom -= 1;
    }

    /// 保存する s-reg (k 番目) のエントリ SP 基準オフセット。arg-spill の直下に並べる。
    fn saved_s_off(&self, k: usize) -> i32 {
        -((self.argc.min(2) + 1 + k) as i32)
    }

    /// saved RA のオフセット。saved-s の直下の固定スロット (nonleaf のときのみ使用)。
    fn ra_off(&self) -> i32 {
        self.saved_s_off(self.reg_vars.len())
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

        // --- レジスタ割付 (Phase 3): address を取られない単一 word のローカルを s0-s3 へ ---
        let mut decls: Vec<(String, &'a ast::Type)> = Vec::new();
        let mut addr_taken: HashSet<String> = HashSet::new();
        for stmt in stmts {
            scan_stmt_alloc(stmt, &mut decls, &mut addr_taken);
        }
        let mut candidates: Vec<String> = Vec::new();
        for (name, ty) in &decls {
            if addr_taken.contains(name) {
                continue;
            }
            let size = self.local.normtype(ty).map(|t| t.sizeof()).unwrap_or(2);
            if size == 1 {
                candidates.push(name.clone());
            }
        }
        for (k, name) in candidates.into_iter().take(S_REGS.len()).enumerate() {
            self.reg_vars.insert(name, S_REGS[k]);
        }
        // leaf 判定 (本体に call があるか)。body コンパイル前に確定させる
        // (epilogue_insts が Return 内でも呼ばれるため)。
        self.nonleaf = stmts.iter().any(stmt_has_call);

        // フレームヘッダを arg-spill の直下に予約: saved-s (reg_vars.len()) + saved-ra (nonleaf 時 1)。
        self.current_bottom += self.reg_vars.len() + if self.nonleaf { 1 } else { 0 };
        if self.current_bottom > self.peak_bottom {
            self.peak_bottom = self.current_bottom;
        }

        // 本体を先にコンパイルしてフレーム深 (peak_bottom) と outgoing 数 (max_outgoing) を測る。
        let mut body = Vec::new();
        for stmt in stmts {
            body.extend(self.compile_stmt(stmt)?);
        }
        // void 関数や明示的 return がない関数のための fall-through epilogue。
        body.extend(self.epilogue_insts());

        // フレームサイズ N = outgoing 領域 + フレーム深 (saved-ra/s, arg-spill, locals, spill 込み)。
        let n = self.max_outgoing + self.peak_bottom;

        // prologue を先頭に置き、FrameRel を実オフセットへ解決する。
        let mut insts = self.prologue();
        insts.extend(body);
        let resolved = insts
            .into_iter()
            .map(|inst| {
                inst.resolve(|imm| match imm {
                    Imm::FrameRel(off) => Imm::Lit(((n as i32 + off) as u16) as usize),
                    other => other,
                })
            })
            .collect();

        Ok(Code(resolved))
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
                let (expr, _) = self.compile_expr(expr, Reg::A0)?;
                Ok(expr)
            }

            ast::Stmt::Assign(lhs, rhs) => {
                let (rhs_insts, rhs_reg) = self.compile_expr(rhs, Reg::A0)?;
                let lhs_insts = self.compile_lvalue(lhs, rhs_reg)?;
                Ok(chain!(rhs_insts, lhs_insts).collect())
            }

            ast::Stmt::Cond(cond, then_stmt, else_stmt) => {
                let (cond_insts, cond_reg) = self.compile_expr(cond, Reg::A0)?;
                let then_insts = self.compile_stmt(then_stmt)?;

                if let Some(else_stmt) = else_stmt {
                    let else_insts = self.compile_stmt(else_stmt)?;

                    let else_jump_offset = (then_insts.len() + 2) as u16;
                    let end_jump_offset = (else_insts.len() + 1) as u16;

                    Ok(chain!(
                        cond_insts,
                        vec![Op::eqi(Reg::A1, cond_reg, Imm::Lit(0))],
                        vec![Op::jumpifr(Reg::A1, Imm::Lit(else_jump_offset as usize))],
                        then_insts,
                        vec![Op::jumpr(Imm::Lit(end_jump_offset as usize))],
                        else_insts
                    )
                    .collect())
                } else {
                    let jump_offset = (then_insts.len() + 1) as u16;

                    Ok(chain!(
                        cond_insts,
                        vec![Op::eqi(Reg::A1, cond_reg, Imm::Lit(0))],
                        vec![Op::jumpifr(Reg::A1, Imm::Lit(jump_offset as usize))],
                        then_insts
                    )
                    .collect())
                }
            }

            ast::Stmt::Loop(cond, body) => {
                let (cond_insts, cond_reg) = self.compile_expr(cond, Reg::A0)?;
                let body_insts = self.compile_stmt(body)?;

                let exit_offset = (body_insts.len() + 2) as u16;
                let loop_offset = -((cond_insts.len() + 2 + body_insts.len()) as i32) as u16;

                Ok(chain!(
                    cond_insts.clone(),
                    vec![Op::eqi(Reg::A1, cond_reg, Imm::Lit(0))],
                    vec![Op::jumpifr(Reg::A1, Imm::Lit(exit_offset as usize))],
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
                // レジスタ割付済みのローカルは s-reg に常駐。スタックスロットは取らず、
                // 型解決のため local テーブルには dummy offset で登録しておく。
                if let Some(r) = self.reg_vars.get(name.as_str()).copied() {
                    self.local.insert(ident, norm_ty, 0).map_err(|e| {
                        Error::TypeCollectionFailed(
                            pos.clone(),
                            format!("local variable {}: {}", name, e),
                        )
                    })?;
                    let mut insts: Vec<Op<Reg, Imm>> = Vec::new();
                    if let Some(init_expr) = init {
                        let (init_insts, init_reg) = self.compile_expr(init_expr, r)?;
                        insts.extend(init_insts);
                        if init_reg != r {
                            insts.push(Op::mov(r, init_reg));
                        }
                    }
                    return Ok(insts);
                }

                let size = norm_ty.sizeof();
                // ローカル変数をフレームに確保: current_bottom を size 進め、
                // field 0 (最深 = エントリ SP 基準で最も負) を head とする。
                self.current_bottom += size;
                if self.current_bottom > self.peak_bottom {
                    self.peak_bottom = self.current_bottom;
                }
                let head_offset = -(self.current_bottom as i32);
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
                    let (init_insts, init_reg) = self.compile_expr(init_expr, Reg::A0)?;
                    insts.extend(init_insts);
                    insts.push(Op::store(init_reg, Reg::SP, sp_off(head_offset)));
                }
                Ok(insts)
            }

            ast::Stmt::Return(expr) => {
                let mut insts = Vec::new();
                if let Some(expr) = expr {
                    let (expr_insts, expr_reg) = self.compile_expr(expr, Reg::A0)?;
                    insts.extend(expr_insts);
                    // 単一 word 戻り値は a0 で返す。
                    // (多 word 戻り値は compile_call 側で弾かれるため a0 にアドレスを置くだけ。)
                    if self.ret_size == 1 && expr_reg != Reg::A0 {
                        insts.push(Op::mov(Reg::A0, expr_reg));
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
                if let Some(r) = self.reg_vars.get(name.as_str()).copied() {
                    // s-reg 常駐変数: 命令不要、その s-reg をそのまま結果として返す。
                    (Vec::new(), r)
                } else {
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
            }

            ast::Expr::Binary(op, lhs, rhs) => {
                let mut prelude: Vec<Op<Reg, Imm>> = Vec::new();
                let (lhs_insts, lhs_inner_reg) = self.compile_expr(lhs, Reg::A0)?;
                prelude.extend(lhs_insts);
                if lhs_inner_reg != Reg::A0 {
                    prelude.push(Op::mov(Reg::A0, lhs_inner_reg));
                }
                let spill = self.alloc_spill_slot();
                prelude.push(Op::store(Reg::A0, Reg::SP, sp_off(spill)));
                let (rhs_insts, rhs_inner_reg) = self.compile_expr(rhs, Reg::A1)?;
                prelude.extend(rhs_insts);
                if rhs_inner_reg != Reg::A1 {
                    prelude.push(Op::mov(Reg::A1, rhs_inner_reg));
                }
                prelude.push(Op::load(Reg::A0, Reg::SP, sp_off(spill)));
                self.free_spill_slot();
                let lhs_insts = prelude;
                let rhs_insts: Vec<Op<Reg, Imm>> = Vec::new();
                let lhs_reg = Reg::A0;
                let rhs_reg = Reg::A1;

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
                        Op::lt(Reg::T0, rhs_reg, lhs_reg),
                        Op::eqi(target, Reg::T0, Imm::Lit(0)),
                    ],
                    ast::BinaryOp::Gt => vec![Op::lt(target, rhs_reg, lhs_reg)],
                    ast::BinaryOp::Ge => vec![
                        Op::lt(Reg::T0, lhs_reg, rhs_reg),
                        Op::eqi(target, Reg::T0, Imm::Lit(0)),
                    ],
                    ast::BinaryOp::Shl | ast::BinaryOp::Shr => {
                        let shift = match op {
                            ast::BinaryOp::Shl => Op::sl(Reg::A0, Reg::A0),
                            ast::BinaryOp::Shr => Op::sr(Reg::A0, Reg::A0),
                            _ => unreachable!(),
                        };
                        let mut v = vec![
                            Op::eqi(Reg::T0, Reg::A1, Imm::Lit(0)),
                            Op::jumpifr(Reg::T0, Imm::Lit(4)),
                            shift,
                            Op::subi(Reg::A1, Reg::A1, Imm::Lit(1)),
                            Op::jumpr(Imm::Lit((-4i32) as u16 as usize)),
                        ];
                        if target != Reg::A0 {
                            v.push(Op::mov(target, Reg::A0));
                        }
                        v
                    }

                    ast::BinaryOp::Mul => {
                        let mut v = vec![
                            Op::mov(Reg::T0, Reg::A0),
                            Op::loadi(Reg::A0, Imm::Lit(0)),
                            Op::eqi(Reg::T1, Reg::A1, Imm::Lit(0)),
                            Op::jumpifr(Reg::T1, Imm::Lit(8)),
                            Op::andi(Reg::T1, Reg::A1, Imm::Lit(1)),
                            Op::eqi(Reg::T1, Reg::T1, Imm::Lit(0)),
                            Op::jumpifr(Reg::T1, Imm::Lit(2)),
                            Op::add(Reg::A0, Reg::A0, Reg::T0),
                            Op::sl(Reg::T0, Reg::T0),
                            Op::sr(Reg::A1, Reg::A1),
                            Op::jumpr(Imm::Lit((-8i32) as u16 as usize)),
                        ];
                        if target != Reg::A0 {
                            v.push(Op::mov(target, Reg::A0));
                        }
                        v
                    }

                    ast::BinaryOp::Div => {
                        let mut v = vec![
                            Op::mov(Reg::T0, Reg::A0),
                            Op::loadi(Reg::A0, Imm::Lit(0)),
                            Op::loadi(Reg::T2, Imm::Lit(16)),
                            Op::eqi(Reg::T1, Reg::T2, Imm::Lit(0)),
                            Op::jumpifr(Reg::T1, Imm::Lit(12)),
                            Op::andi(Reg::T1, Reg::T0, Imm::Lit(0x8000)),
                            Op::neqi(Reg::T1, Reg::T1, Imm::Lit(0)),
                            Op::sl(Reg::T0, Reg::T0),
                            Op::sl(Reg::A0, Reg::A0),
                            // T1 は 0 または 0xFFFF(=-1)。sub で「+0 または +1」を実現する。
                            Op::sub(Reg::A0, Reg::A0, Reg::T1),
                            Op::lt(Reg::T1, Reg::A0, Reg::A1),
                            Op::jumpifr(Reg::T1, Imm::Lit(3)),
                            Op::sub(Reg::A0, Reg::A0, Reg::A1),
                            Op::ori(Reg::T0, Reg::T0, Imm::Lit(1)),
                            Op::subi(Reg::T2, Reg::T2, Imm::Lit(1)),
                            Op::jumpr(Imm::Lit((-12i32) as u16 as usize)),
                        ];
                        if target != Reg::T0 {
                            v.push(Op::mov(target, Reg::T0));
                        }
                        v
                    }

                    ast::BinaryOp::Mod => {
                        let mut v = vec![
                            Op::mov(Reg::T0, Reg::A0),
                            Op::loadi(Reg::A0, Imm::Lit(0)),
                            Op::loadi(Reg::T2, Imm::Lit(16)),
                            Op::eqi(Reg::T1, Reg::T2, Imm::Lit(0)),
                            Op::jumpifr(Reg::T1, Imm::Lit(11)),
                            Op::andi(Reg::T1, Reg::T0, Imm::Lit(0x8000)),
                            Op::neqi(Reg::T1, Reg::T1, Imm::Lit(0)),
                            Op::sl(Reg::T0, Reg::T0),
                            Op::sl(Reg::A0, Reg::A0),
                            // T1 は 0 または 0xFFFF(=-1)。sub で「+0 または +1」を実現する。
                            Op::sub(Reg::A0, Reg::A0, Reg::T1),
                            Op::lt(Reg::T1, Reg::A0, Reg::A1),
                            Op::jumpifr(Reg::T1, Imm::Lit(2)),
                            Op::sub(Reg::A0, Reg::A0, Reg::A1),
                            Op::subi(Reg::T2, Reg::T2, Imm::Lit(1)),
                            Op::jumpr(Imm::Lit((-11i32) as u16 as usize)),
                        ];
                        if target != Reg::A0 {
                            v.push(Op::mov(target, Reg::A0));
                        }
                        v
                    }
                };

                (chain!(lhs_insts, rhs_insts, op_insts).collect(), target)
            }

            ast::Expr::Unary(op, operand) => {
                let (operand_insts, operand_reg) = self.compile_expr(operand, Reg::A0)?;

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
                let (operand_insts, operand_reg) = self.compile_expr(operand, Reg::A0)?;
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

    /// RISC-V 風 ABI の関数呼び出し。sp は callee-save なので caller は sp を動かさず、
    /// callee が prologue/epilogue で subi/addi する。手順:
    ///   1. (間接呼びのみ) func ptr を T0 へ評価 → spill へ退避
    ///   2. 全引数を T0 へ評価 → spill へ退避 (内側 call でのレジスタ破壊を回避)
    ///   3. 第3引数以降を outgoing 領域 (body sp + 0,1,..) へ store (max_outgoing で予約済み)
    ///   4. 第1,2引数を a0, a1 へロード
    ///   5. CALL(Label) または CALLR(T0)
    ///   6. 戻り値は a0。target へ移す。
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

        let argc = args.len();
        // a0/a1 を超える第3引数以降は caller フレームの outgoing 領域 (body sp + 0..) に積む。
        let stack_argc = argc.saturating_sub(2);
        if stack_argc > self.max_outgoing {
            self.max_outgoing = stack_argc;
        }

        let mut insts: Vec<Op<Reg, Imm>> = Vec::new();

        // 間接呼び: 関数ポインタを評価して spill へ退避 (引数評価で壊れないように)。
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

        // 全引数を評価し spill へ退避する (sp は動かさない。レジスタ破壊を回避)。
        let mut arg_spills: Vec<i32> = Vec::new();
        for arg in args {
            let (arg_insts, arg_reg) = self.compile_expr(arg, Reg::T0)?;
            insts.extend(arg_insts);
            if arg_reg != Reg::T0 {
                insts.push(Op::mov(Reg::T0, arg_reg));
            }
            let s = self.alloc_spill_slot();
            insts.push(Op::store(Reg::T0, Reg::SP, sp_off(s)));
            arg_spills.push(s);
        }

        // 第3引数以降を outgoing 領域 (body sp + out_j = Lit(out_j)) へ移す。
        for (j, &spill) in arg_spills.iter().enumerate().skip(2) {
            let out_j = j - 2;
            insts.push(Op::load(Reg::T0, Reg::SP, sp_off(spill)));
            insts.push(Op::store(Reg::T0, Reg::SP, Imm::Lit(out_j)));
        }

        // 第1,2引数を a0, a1 へ。
        if argc >= 1 {
            insts.push(Op::load(Reg::A0, Reg::SP, sp_off(arg_spills[0])));
        }
        if argc >= 2 {
            insts.push(Op::load(Reg::A1, Reg::SP, sp_off(arg_spills[1])));
        }

        // call / callr (sp は触らない。callee が prologue/epilogue で subi/addi する)。
        if direct {
            let name = if let ast::Expr::Ident((n, _)) = func_expr {
                n.clone()
            } else {
                unreachable!()
            };
            insts.push(Op::call(Imm::Label(name)));
        } else {
            let s = func_ptr_spill.unwrap();
            insts.push(Op::load(Reg::T0, Reg::SP, sp_off(s)));
            insts.push(Op::callr(Reg::T0));
        }

        // spill を解放 (alloc した分だけ current_bottom を戻す)。
        for _ in &arg_spills {
            self.free_spill_slot();
        }
        if func_ptr_spill.is_some() {
            self.free_spill_slot();
        }

        // 戻り値は a0。target へ移す。
        if ret_size == 1 && target != Reg::A0 {
            insts.push(Op::mov(target, Reg::A0));
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
                // s-reg 常駐変数はアドレスを取れない (割付時に address-taken を除外済み)。
                if self.reg_vars.contains_key(name.as_str()) {
                    return Err(Error::NotAddressable(
                        expr.pos_or_default(),
                        format!("register-allocated variable `{}`", name),
                    ));
                }
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

                let idx_target = if target == Reg::A0 { Reg::A1 } else { Reg::A0 };

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
                // s-reg 常駐変数への代入は mov で済む。
                if let Some(r) = self.reg_vars.get(name.as_str()).copied() {
                    let mut insts = Vec::new();
                    if r != value_reg {
                        insts.push(Op::mov(r, value_reg));
                    }
                    return Ok(insts);
                }
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
                let (addr_insts, addr_reg) = self.compile_expr(addr_expr, Reg::A1)?;
                Ok(chain!(
                    addr_insts,
                    vec![Op::store(value_reg, addr_reg, Imm::Lit(0))]
                )
                .collect())
            }

            ast::Expr::Member(..) | ast::Expr::Index(..) => {
                let addr_reg = Reg::A1;
                let spill = self.alloc_spill_slot();
                let mut insts = vec![Op::store(value_reg, Reg::SP, sp_off(spill))];
                insts.extend(self.compile_addr(lvalue, addr_reg)?);
                let val_reg = if addr_reg == Reg::A0 {
                    Reg::T0
                } else {
                    Reg::A0
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
