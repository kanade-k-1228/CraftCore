//! definition / hover / documentSymbol の実装。

use std::path::{Path, PathBuf};

use lsp_types::{
    DocumentSymbol, Hover, HoverContents, Location, MarkupContent, MarkupKind, Position, SymbolKind,
};
use tasm::{ast, Global, NormType, Pos, TokenKind};

use crate::analysis::Snapshot;
use crate::convert::{path_to_url, pos_to_range, position_to_rowcol};

enum Resolved<'a> {
    Global {
        def: &'a ast::Def,
        fqn: String,
    },
    Local {
        ident: &'a ast::Ident,
        ty: &'a ast::Type,
        is_param: bool,
    },
    AsmLabel {
        ident: &'a ast::Ident,
    },
}

/// カーソル位置のトークンから修飾名を再組立する。
/// 返り値: (修飾名, 先頭セグメントの Pos)
fn qualified_name_at(
    snap: &Snapshot,
    path: &Path,
    row: usize,
    col: usize,
) -> Option<(String, Pos)> {
    let tokens = snap.tokens.get(path)?;
    let i = tokens
        .iter()
        .position(|t| t.pos.row() == row && t.pos.col() <= col && col < t.pos.end_col())?;
    let TokenKind::Ident(_) = &tokens[i].kind else {
        return None;
    };

    // メンバアクセス (expr.field) のフィールド名はグローバル名ではない
    if i > 0 && matches!(tokens[i - 1].kind, TokenKind::Period) {
        return None;
    }

    // 左右の `:: ident` 連鎖を辿って全セグメントを集める
    let mut first = i;
    while first >= 2
        && matches!(tokens[first - 1].kind, TokenKind::ColonColon)
        && matches!(tokens[first - 2].kind, TokenKind::Ident(_))
    {
        first -= 2;
    }
    let mut last = i;
    while last + 2 < tokens.len()
        && matches!(tokens[last + 1].kind, TokenKind::ColonColon)
        && matches!(tokens[last + 2].kind, TokenKind::Ident(_))
    {
        last += 2;
    }

    let name = tokens[first..=last]
        .iter()
        .filter_map(|t| match &t.kind {
            TokenKind::Ident(s) => Some(s.as_str()),
            _ => None,
        })
        .collect::<Vec<_>>()
        .join("::");
    Some((name, tokens[first].pos.clone()))
}

/// row を含む同一ファイル内の直前の def (def はネストしない)
fn enclosing_def<'a>(snap: &'a Snapshot, path_str: &str, row: usize) -> Option<&'a ast::Def> {
    snap.ast.0.iter().rfind(|d| {
        d.pos()
            .map(|p| p.file() == path_str && p.row() <= row)
            .unwrap_or(false)
    })
}

/// Stmt を再帰して名前の一致する Stmt::Var を探す
fn find_var<'a>(stmts: &'a [ast::Stmt], name: &str) -> Option<(&'a ast::Ident, &'a ast::Type)> {
    for stmt in stmts {
        match stmt {
            ast::Stmt::Var(ident, ty, _) if ident.0 == name => return Some((ident, ty)),
            ast::Stmt::Block(_, inner) => {
                if let Some(found) = find_var(inner, name) {
                    return Some(found);
                }
            }
            ast::Stmt::Cond(_, tstmt, fstmt) => {
                if let Some(found) = find_var(std::slice::from_ref(tstmt.as_ref()), name) {
                    return Some(found);
                }
                if let Some(fstmt) = fstmt {
                    if let Some(found) = find_var(std::slice::from_ref(fstmt.as_ref()), name) {
                        return Some(found);
                    }
                }
            }
            ast::Stmt::Loop(_, body) => {
                if let Some(found) = find_var(std::slice::from_ref(body.as_ref()), name) {
                    return Some(found);
                }
            }
            _ => {}
        }
    }
    None
}

fn resolve_at<'a>(
    snap: &'a Snapshot,
    global: &Global<'a>,
    path: &Path,
    position: Position,
) -> Option<Resolved<'a>> {
    let text = snap.text_of(path)?;
    let (row, col) = position_to_rowcol(position, text);
    let (name, anchor) = qualified_name_at(snap, path, row, col)?;
    let path_str = path.to_string_lossy();

    if !name.contains("::") {
        match enclosing_def(snap, &path_str, row) {
            Some(ast::Def::Func(_, args, _, stmts)) => {
                if let Some((ident, ty)) = args
                    .iter()
                    .find(|(id, _)| id.0 == name)
                    .map(|(id, ty)| (id, ty))
                {
                    return Some(Resolved::Local {
                        ident,
                        ty,
                        is_param: true,
                    });
                }
                if let Some((ident, ty)) = find_var(stmts, &name) {
                    return Some(Resolved::Local {
                        ident,
                        ty,
                        is_param: false,
                    });
                }
            }
            Some(ast::Def::Asm(_, _, body)) => {
                for ast::Asm(_, _, labels, _) in body {
                    if let Some(ident) = labels.iter().find(|(label, _)| *label == name) {
                        return Some(Resolved::AsmLabel { ident });
                    }
                }
            }
            _ => {}
        }
    }

    let (def, fqn) = global.resolve(&name, &anchor)?;
    Some(Resolved::Global { def, fqn })
}

pub fn definition(snap: &Snapshot, path: &Path, position: Position) -> Option<Location> {
    let global = snap.global().ok()?;
    let resolved = resolve_at(snap, &global, path, position)?;
    let pos = match &resolved {
        Resolved::Global { def, .. } => def.pos()?,
        Resolved::Local { ident, .. } => &ident.1,
        Resolved::AsmLabel { ident } => &ident.1,
    };
    let target = PathBuf::from(pos.file());
    let text = snap.text_of(&target)?;
    Some(Location {
        uri: path_to_url(&target)?,
        range: pos_to_range(pos, text),
    })
}

/// FQN から末尾セグメントを取り出す
fn short_name(fqn: &str) -> &str {
    fqn.rsplit("::").next().unwrap_or(fqn)
}

fn module_of(fqn: &str) -> Option<&str> {
    fqn.rfind("::").map(|i| &fqn[..i])
}

fn fmt_addr(addr: Option<usize>) -> String {
    addr.map(|a| format!(" @ 0x{:04X}", a)).unwrap_or_default()
}

/// グローバル定義のホバー本文 (```tasm フェンス内)
fn global_hover_text(global: &Global<'_>, def: &ast::Def, fqn: &str) -> String {
    let name = short_name(fqn);
    match def {
        ast::Def::Func(..) => match global.get_func_resolved(fqn) {
            Some(NormType::Func(args, ret)) => {
                let args = args
                    .iter()
                    .map(|(n, t)| format!("{}: {}", n, t.fmt()))
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("fn {}({}) -> {}", name, args, ret.fmt())
            }
            _ => format!("fn {}(...)", name),
        },
        ast::Def::Const(..) => match global.get_const_resolved(fqn) {
            Some((ty, value, addr)) => {
                format!("const {}: {} = {}{}", name, ty.fmt(), value, fmt_addr(addr))
            }
            None => format!("const {}", name),
        },
        ast::Def::Static(..) => match global.get_static_resolved(fqn) {
            Some((ty, addr)) => format!("static {}: {}{}", name, ty.fmt(), fmt_addr(addr)),
            None => format!("static {}", name),
        },
        ast::Def::Type(..) => match global.get_type_resolved(fqn) {
            Some((ty, size)) => format!("type {} = {}\n// size = {} words", name, ty.fmt(), size),
            None => format!("type {}", name),
        },
        ast::Def::Asm(..) => {
            let addr = global.get_asm_resolved(fqn).flatten();
            format!("asm {}{}", name, fmt_addr(addr))
        }
    }
}

pub fn hover(snap: &Snapshot, path: &Path, position: Position) -> Option<Hover> {
    let global = snap.global().ok()?;
    let resolved = resolve_at(snap, &global, path, position)?;
    let mut value = String::from("```tasm\n");
    let mut footer = String::new();
    match &resolved {
        Resolved::Global { def, fqn } => {
            value.push_str(&global_hover_text(&global, def, fqn));
            if let Some(module) = module_of(fqn) {
                footer = format!("\nmodule: `{}`", module);
            }
        }
        Resolved::Local {
            ident,
            ty,
            is_param,
        } => {
            let ty = global
                .normtype(ty)
                .map(|t| t.fmt())
                .unwrap_or_else(|_| "?".to_string());
            let kw = if *is_param { "param" } else { "var" };
            value.push_str(&format!("{} {}: {}", kw, ident.0, ty));
        }
        Resolved::AsmLabel { ident } => {
            value.push_str(&format!("label {}", ident.0));
        }
    }
    value.push_str("\n```");
    value.push_str(&footer);
    Some(Hover {
        contents: HoverContents::Markup(MarkupContent {
            kind: MarkupKind::Markdown,
            value,
        }),
        range: None,
    })
}

pub fn document_symbols(snap: &Snapshot, path: &Path) -> Vec<DocumentSymbol> {
    let path_str = path.to_string_lossy();
    let text = match snap.text_of(path) {
        Some(t) => t,
        None => return vec![],
    };
    let global = snap.global().ok();
    snap.ast
        .0
        .iter()
        .filter_map(|def| {
            let (fqn, pos) = match def {
                ast::Def::Type((n, p), _)
                | ast::Def::Const((n, p), _, _)
                | ast::Def::Static((n, p), _, _)
                | ast::Def::Asm((n, p), _, _)
                | ast::Def::Func((n, p), _, _, _) => (n, p),
            };
            if pos.file() != path_str {
                return None;
            }
            let kind = match def {
                ast::Def::Func(..) | ast::Def::Asm(..) => SymbolKind::FUNCTION,
                ast::Def::Const(..) => SymbolKind::CONSTANT,
                ast::Def::Static(..) => SymbolKind::VARIABLE,
                ast::Def::Type(..) => SymbolKind::STRUCT,
            };
            let detail = global
                .as_ref()
                .map(|g| global_hover_text(g, def, fqn))
                .map(|s| s.lines().next().unwrap_or("").to_string());
            let range = pos_to_range(pos, text);
            #[allow(deprecated)]
            Some(DocumentSymbol {
                name: short_name(fqn).to_string(),
                detail,
                kind,
                tags: None,
                deprecated: None,
                range,
                selection_range: range,
                children: None,
            })
        })
        .collect()
}
