use std::collections::{HashMap, HashSet};
use std::path::{Path, PathBuf};

use crate::convert::normalize_path;

#[derive(Debug, Default, Clone)]
pub struct Config {
    /// (モジュールルート名, 検索ディレクトリ) (tasm -I 相当)。パスは絶対に解決済み。
    pub include_dirs: Vec<(String, PathBuf)>,
    /// 相対 include_dirs の解決基準 (ワークスペースルート)
    pub root: Option<PathBuf>,
}

impl Config {
    /// `{"includeDirs": ["rtos", "name=dir", ...]}` 形式の設定値を取り込む
    pub fn update(&mut self, settings: &serde_json::Value) {
        let Some(dirs) = settings.get("includeDirs").and_then(|v| v.as_array()) else {
            return;
        };
        self.include_dirs = dirs
            .iter()
            .filter_map(|v| v.as_str())
            .map(|s| {
                let (name, dir) = tasm::parse_include_spec(s);
                let p = PathBuf::from(dir);
                let abs = if p.is_absolute() {
                    p
                } else if let Some(root) = &self.root {
                    root.join(p)
                } else {
                    p
                };
                (name, normalize_path(&abs))
            })
            .collect();
    }
}

pub struct ServerState {
    /// 開いているドキュメント (正規化済み絶対パス → 最新テキスト)
    pub docs: HashMap<PathBuf, String>,
    pub config: Config,
    /// ファイルごとに前回 publish した診断の有無 (クリア用)
    pub published: HashSet<PathBuf>,
}

impl ServerState {
    pub fn new(root: Option<PathBuf>) -> Self {
        ServerState {
            docs: HashMap::new(),
            config: Config {
                include_dirs: vec![],
                root,
            },
            published: HashSet::new(),
        }
    }

    /// docs にあればそれを、なければディスクから読む
    pub fn read_file(&self, path: &Path) -> std::io::Result<String> {
        if let Some(text) = self.docs.get(path) {
            return Ok(text.clone());
        }
        std::fs::read_to_string(path)
    }
}
