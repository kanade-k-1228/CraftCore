//! コンパイルユニットの構築と診断の生成。
//!
//! ユニットの決め方:
//! 1. 対象ファイルの祖先ディレクトリに project.yaml (proj のプロジェクト定義)
//!    があれば、その src + include を使う。include のキーがモジュール名。
//! 2. なければエディタ設定の includeDirs を使い、「対象ファイル + モジュール群」
//!    で構成する。対象自体がモジュールファイルならモジュール群のみ。

use std::collections::{HashMap, HashSet};
use std::path::{Path, PathBuf};

use lsp_types::{Diagnostic, DiagnosticSeverity};
use tasm::{apply_module_prefixes, ast, collect_module_files, Error, Global, Token};

use crate::convert::{normalize_path, pos_to_range};
use crate::state::ServerState;

pub struct ProjectConfig {
    /// ルートソース (プレフィックスなし)。project.yaml のディレクトリ基準で解決済み。
    pub srcs: Vec<PathBuf>,
    /// (モジュールルート名, ディレクトリ)。名前は includes のキー。
    pub include_dirs: Vec<(String, PathBuf)>,
}

/// target の祖先ディレクトリを遡って最初の project.yaml (proj の共通定義) を読む。
/// 見つからない・壊れている場合は None (エディタ設定にフォールバック)。
pub fn find_project(target: &Path) -> Option<ProjectConfig> {
    let path = proj::find(target)?;
    let cfg = proj::Config::load(&path).ok()?;
    let dir = path.parent()?;
    let srcs = cfg
        .src
        .iter()
        .map(|s| normalize_path(&dir.join(s)))
        .collect();
    let include_dirs = cfg
        .include
        .iter()
        .map(|(name, d)| (name.clone(), normalize_path(&dir.join(d))))
        .collect();
    Some(ProjectConfig { srcs, include_dirs })
}

pub struct Snapshot {
    /// 解析対象の全ソース (パース順)。診断 Range 変換のため全文を保持する。
    pub files: Vec<(PathBuf, String)>,
    /// カーソル位置からのトークン特定用 (ファイル単位)
    pub tokens: HashMap<PathBuf, Vec<Token>>,
    /// モジュール prefix 適用済み AST
    pub ast: ast::AST,
    /// Pos.file() の文字列 → モジュール prefix
    pub modmap: HashMap<String, String>,
    pub parse_errors: Vec<Error>,
}

impl Snapshot {
    pub fn text_of(&self, path: &Path) -> Option<&str> {
        self.files
            .iter()
            .find(|(p, _)| p == path)
            .map(|(_, t)| t.as_str())
    }

    /// リクエスト処理用の Global をローカル構築する (借用の都合でキャッシュしない)
    pub fn global(&self) -> Result<Global<'_>, Error> {
        Global::new(&self.ast, self.modmap.clone())
    }
}

/// 対象ファイルのコンパイルユニットを構築して全ファイルを字句・構文解析する
pub fn build_snapshot(state: &ServerState, target: &Path) -> Snapshot {
    let target = normalize_path(target);

    // project.yaml があればそれを優先し、なければエディタ設定を使う
    let project = find_project(&target);
    let include_dirs: &[(String, PathBuf)] = match &project {
        Some(p) => &p.include_dirs,
        None => &state.config.include_dirs,
    };

    // include_dirs 配下のモジュールファイルを列挙
    let mut module_files: Vec<(String, String)> = vec![];
    for (root, dir) in include_dirs {
        let _ = collect_module_files(dir, root, &mut module_files);
    }

    let module_paths: Vec<(PathBuf, String)> = module_files
        .into_iter()
        .map(|(p, prefix)| (normalize_path(Path::new(&p)), prefix))
        .collect();
    let target_is_module = module_paths.iter().any(|(p, _)| *p == target);

    // ルートソースの決定:
    // - target が srcs の一員 or モジュールファイル → srcs 全体 (プログラム全体の文脈)
    // - それ以外 (srcs 外のファイル) → target 単体 (srcs と混ぜると重複定義になる)
    let roots: Vec<PathBuf> = match &project {
        Some(p) if p.srcs.contains(&target) || target_is_module => p.srcs.clone(),
        _ if target_is_module => vec![],
        _ => vec![target.clone()],
    };

    let mut modmap: HashMap<String, String> = HashMap::new();
    let mut order: Vec<PathBuf> = vec![];
    let mut seen: HashSet<PathBuf> = HashSet::new();

    for root in roots {
        if seen.insert(root.clone()) {
            order.push(root);
        }
    }
    for (path, prefix) in module_paths {
        if seen.insert(path.clone()) {
            modmap.insert(path.to_string_lossy().into_owned(), prefix);
            order.push(path);
        }
    }

    // 読み込み (docs 優先) + 字句解析
    let mut files: Vec<(PathBuf, String)> = vec![];
    let mut tokens_by_file: HashMap<PathBuf, Vec<Token>> = HashMap::new();
    let mut all_tokens: Vec<Token> = vec![];
    for path in order {
        let Ok(text) = state.read_file(&path) else {
            continue;
        };
        let toks = tasm::Lexer::new(&path.to_string_lossy(), &text).parse();
        all_tokens.extend(toks.iter().cloned());
        tokens_by_file.insert(path.clone(), toks);
        files.push((path, text));
    }

    let (mut ast, parse_errors) = tasm::Parser::new(all_tokens.into_iter()).parse();
    apply_module_prefixes(&mut ast, &modmap);

    Snapshot {
        files,
        tokens: tokens_by_file,
        ast,
        modmap,
        parse_errors,
    }
}

const MAX_SEMANTIC_ERRORS: usize = 20;

/// スナップショットの診断をファイル別に生成する。
/// fallback: Pos を持たないエラーの割り当て先 (解析を起動したドキュメント)。
pub fn diagnostics(snap: &Snapshot, fallback: &Path) -> HashMap<PathBuf, Vec<Diagnostic>> {
    let mut out: HashMap<PathBuf, Vec<Diagnostic>> = HashMap::new();
    let mut seen: HashSet<String> = HashSet::new();

    let mut push = |err: &Error, out: &mut HashMap<PathBuf, Vec<Diagnostic>>| {
        let (path, range) = match err.pos() {
            Some(pos) if pos.row() > 0 && !pos.file().is_empty() => {
                let path = PathBuf::from(pos.file());
                let range = snap
                    .text_of(&path)
                    .map(|text| pos_to_range(pos, text))
                    .unwrap_or_default();
                (path, range)
            }
            _ => (fallback.to_path_buf(), Default::default()),
        };
        let message = err.to_string();
        if !seen.insert(format!("{}:{:?}:{}", path.display(), range, message)) {
            return;
        }
        out.entry(path).or_default().push(Diagnostic {
            range,
            severity: Some(DiagnosticSeverity::ERROR),
            source: Some("tasm".to_string()),
            message,
            ..Default::default()
        });
    };

    for err in &snap.parse_errors {
        push(err, &mut out);
    }

    // パースエラーがあると AST が欠けるため、セマンティック診断は構文が
    // 通っている場合のみ実施する (欠けた定義由来の誤検出を避ける)。
    if !snap.parse_errors.is_empty() {
        return out;
    }

    let global = match snap.global() {
        Ok(g) => g,
        Err(e) => {
            push(&e, &mut out);
            return out;
        }
    };

    let mut count = 0;
    for def in &snap.ast.0 {
        if count >= MAX_SEMANTIC_ERRORS {
            break;
        }
        // def 単位で最初のエラーのみ報告する
        let result: Result<(), Error> = (|| {
            match def {
                ast::Def::Type(_, ty) => {
                    global.normtype(ty)?;
                }
                ast::Def::Const(_, addr, expr) => {
                    if let Some(addr) = addr {
                        global.constexpr(addr)?;
                    }
                    global.constexpr(expr)?;
                }
                ast::Def::Static(_, addr, ty) => {
                    if let Some(addr) = addr {
                        global.constexpr(addr)?;
                    }
                    global.normtype(ty)?;
                }
                ast::Def::Asm((name, _), addr, _) => {
                    if let Some(addr) = addr {
                        global.constexpr(addr)?;
                    }
                    global.code(name)?;
                }
                ast::Def::Func((name, _), _, _, _) => {
                    global.code(name)?;
                }
            }
            Ok(())
        })();
        if let Err(e) = result {
            push(&e, &mut out);
            count += 1;
        }
    }

    out
}
