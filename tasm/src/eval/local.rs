use indexmap::IndexMap;

use crate::{
    error::Error,
    eval::{global::Global, normtype::NormType},
    grammer::{ast, token::Pos},
};

// 関数のフレーム (SP 下向き ABI)
//
//   high addr
//      SP + 2 + ret_size            ← ret slot 最後 (= Return[0] 相当の最上位)
//           :
//      SP + 3                       ← ret slot 先頭 (= 戻り値型の field 0)
//      SP + 2                       ← saved RA          (callee の prologue で書く)
//      SP + 1                       ← saved caller's SP (caller が書く)
//      SP + 0                       ← arg_0 (の最上位スロット)
//      SP - 1                       ← arg_0 続き or arg_1
//           :                       args は宣言順にスロット下方向へ詰める
//      SP - (args_total - 1)        ← 最後の arg スロット
//      SP - args_total              ← local 0 (の最上位スロット)
//           :                       locals は宣言順にスロット下方向へ
//      SP - args_total - locals_total ← spill 0 (動的)
//           :
//   low addr
//
// - レジスタは全て caller-save (T0-T9)
// - 引数・戻り値は全て stack で渡す
// - 多 word 値は head_offset (低アドレス) を基準に field i は head_offset + i に配置
//   (= 引数 / ローカル / 戻り値とも自然な struct レイアウトに従う)

/// 関数の引数 + ローカル変数のシンボルテーブル。
pub struct Local<'a> {
    global: &'a Global<'a>,
    stack: IndexMap<&'a str, (NormType, i32)>, // (name → (type, SP+offset))
}

impl<'a> Local<'a> {
    pub fn fork(global: &'a Global<'a>) -> Self {
        Self {
            global,
            stack: IndexMap::new(),
        }
    }

    /// 名前 → (型, SP+offset) を登録する。offset は呼び出し側が決める (符号付き)。
    pub fn insert(
        &mut self,
        ident: &'a ast::Ident,
        ty: NormType,
        offset: i32,
    ) -> Result<(), Error> {
        let (name, pos) = ident;
        if self.stack.contains_key(name.as_str()) {
            return Err(Error::DuplicateLocal(pos.clone(), name.clone()));
        }
        self.stack.insert(name.as_str(), (ty, offset));
        Ok(())
    }

    /// 引数を登録する (RISC-V 風 ABI)。offset は「関数エントリ時 SP 基準」の符号付き値。
    ///   arg_0 → a0, arg_1 → a1: レジスタ渡しだが Phase1 では prologue でフレームにスピルし、
    ///     エントリ SP 基準オフセット -1, -2 (a0_spill, a1_spill) で参照する。
    ///   arg_2 以降 → caller が積むスタック引数。エントリ SP 基準の正オフセット 0, 1, ... 。
    ///   (多 word 引数は pass-by-pointer なので各 arg は 1 word。)
    pub fn insert_args(&mut self, args: &'a [(ast::Ident, ast::Type)]) -> Result<(), Error> {
        let mut reg_idx = 0; // a0/a1 に乗せた数
        let mut spill_off: i32 = -1; // 次の a0/a1 スピル先 (-1, -2)
        let mut caller_off: i32 = 0; // 次のスタック引数 (エントリ SP +0, +1, ...)
        for (ident, ty) in args {
            let nty = self.global.normtype(ty)?;
            let sz = nty.sizeof() as i32;
            if reg_idx < 2 {
                self.insert(ident, nty, spill_off)?;
                spill_off -= sz;
                reg_idx += 1;
            } else {
                self.insert(ident, nty, caller_off)?;
                caller_off += sz;
            }
        }
        Ok(())
    }

    pub fn vartype(&self, name: &str) -> Option<&NormType> {
        self.stack.get(name).map(|(ty, _)| ty)
    }

    pub fn offset(&self, name: &str) -> Option<i32> {
        self.stack.get(name).map(|(_, offset)| *offset)
    }

    pub fn normtype(&self, ty: &'a ast::Type) -> Result<NormType, Error> {
        self.global.normtype(ty)
    }

    /// Infer the type of an expression with local context.
    pub fn typeinfer(&self, expr: &'a ast::Expr) -> Result<NormType, Error> {
        match expr {
            ast::Expr::Ident((name, _)) => {
                if let Some(ty) = self.vartype(name) {
                    return Ok(ty.clone());
                }
                if name == "csr" {
                    return Ok(NormType::Int);
                }
                self.global.typeinfer(expr)
            }
            ast::Expr::Addr(inner) => {
                let ty = self.typeinfer(inner)?;
                Ok(NormType::Addr(Box::new(ty)))
            }
            ast::Expr::Deref(inner) => {
                let ty = self.typeinfer(inner)?;
                match ty {
                    NormType::Addr(t) => Ok(*t),
                    _ => Err(Error::CannotDereferenceNonPointer(inner.pos_or_default())),
                }
            }
            ast::Expr::Member(base, (field, field_pos)) => {
                let base_ty = self.typeinfer(base)?;
                match base_ty {
                    NormType::Struct(fields) => {
                        match fields.into_iter().find(|(n, _)| n == field) {
                            Some((_, ty)) => Ok(ty),
                            None => Err(Error::NoSuchField(field_pos.clone(), field.clone())),
                        }
                    }
                    _ => Err(Error::NotAStruct(base.pos_or_default())),
                }
            }
            ast::Expr::Index(base, _) => {
                let base_ty = self.typeinfer(base)?;
                match base_ty {
                    NormType::Array(_, elem) => Ok(*elem),
                    NormType::Addr(inner) => Ok(*inner),
                    _ => Err(Error::NotIndexable(base.pos_or_default())),
                }
            }
            ast::Expr::Cast(_, ty) => Ok(self.normtype(ty)?),
            ast::Expr::Unary(_, inner) => self.typeinfer(inner),
            ast::Expr::Binary(_, left, _) => self.typeinfer(left),
            _ => self.global.typeinfer(expr),
        }
    }

    /// 参照名をモジュール文脈で解決し (def, canonical FQN) を返す。
    pub fn resolve(&self, name: &str, pos: &Pos) -> Option<(&'a ast::Def, String)> {
        self.global.resolve(name, pos)
    }

    /// (name → FP+offset) を挿入順で返す。
    pub fn entries(&self) -> IndexMap<String, i32> {
        self.stack
            .iter()
            .map(|(n, (_, off))| (n.to_string(), *off))
            .collect()
    }

    /// callee の (ret_size, [arg sizes]) を返す。
    ///
    /// - 直接呼出 (`Ident → Def::Func`): 関数定義の signature を見る。
    /// - 直接呼出 (`Ident → Def::Asm`): asm に signature 定義は無いので
    ///   「ret_size=0, 各 arg は 1 word」と仮定。
    /// - 間接呼出: func_expr の型 `*(...)->R` から抜き出す。
    pub fn callee_signature(
        &self,
        func_expr: &'a ast::Expr,
        args: &'a [ast::Expr],
    ) -> Result<(usize, Vec<usize>), Error> {
        if let ast::Expr::Ident((name, pos)) = func_expr {
            match self.global.resolve(name, pos).map(|(d, _)| d) {
                Some(ast::Def::Func(_, fargs, fret, _)) => {
                    let ret_size = self.global.normtype(fret)?.sizeof();
                    let mut arg_sizes = Vec::new();
                    for (_, ty) in fargs {
                        arg_sizes.push(self.global.normtype(ty)?.sizeof());
                    }
                    return Ok((ret_size, arg_sizes));
                }
                Some(ast::Def::Asm(..)) => {
                    let arg_sizes = args.iter().map(|_| 1).collect();
                    return Ok((0, arg_sizes));
                }
                _ => {}
            }
        }
        let func_ty = self.typeinfer(func_expr)?;
        let inner = match func_ty {
            NormType::Addr(inner) => *inner,
            other => other,
        };
        match inner {
            NormType::Func(fargs, fret) => {
                let ret_size = fret.sizeof();
                let arg_sizes = fargs.into_iter().map(|(_, ty)| ty.sizeof()).collect();
                Ok((ret_size, arg_sizes))
            }
            _ => {
                let arg_sizes = args.iter().map(|_| 1).collect();
                Ok((0, arg_sizes))
            }
        }
    }
}
