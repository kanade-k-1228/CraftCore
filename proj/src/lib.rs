//! project.yaml (プロジェクト定義) の共通スキーマ。
//! 各コマンド (tasm / cemu / tasm-lsp) はこのクレートを通して同じ定義を読む。

use std::collections::BTreeMap;
use std::fs;
use std::path::{Path, PathBuf};

use anyhow::{Context, Result};
use serde::Deserialize;

pub const FILE_NAME: &str = "project.yaml";

#[derive(Deserialize)]
pub struct Config {
    #[serde(default = "default_srcs")]
    pub src: Vec<String>,
    #[serde(default)]
    pub include: BTreeMap<String, String>,
    #[serde(default)]
    pub test: BTreeMap<String, CemuConfig>,
}

fn default_srcs() -> Vec<String> {
    vec!["main.tasm".to_string()]
}

impl Default for Config {
    fn default() -> Self {
        Self {
            src: default_srcs(),
            include: BTreeMap::new(),
            test: BTreeMap::new(),
        }
    }
}

#[derive(Deserialize, Default)]
pub struct CemuConfig {
    pub tmax: Option<u64>,
    #[serde(default)]
    pub dump_all: bool,
    pub sin: Option<String>,
    pub sout: Option<String>,
    pub vram_out: Option<String>,
}

impl Config {
    /// path の project.yaml を読む (ファイル必須)
    pub fn load(path: &Path) -> Result<Config> {
        let content = fs::read_to_string(path)
            .with_context(|| format!("failed to read {}", path.display()))?;
        serde_yaml::from_str(&content)
            .with_context(|| format!("failed to parse {}", path.display()))
    }

    /// カレントディレクトリの project.yaml を読む。存在しなければデフォルト値。
    pub fn load_or_default() -> Result<Config> {
        let path = Path::new(FILE_NAME);
        if path.exists() {
            Self::load(path)
        } else {
            Ok(Config::default())
        }
    }
}

/// start (ファイルまたはディレクトリ) の祖先を遡り、
/// 最初に見つかった project.yaml のパスを返す。
pub fn find(start: &Path) -> Option<PathBuf> {
    let skip = usize::from(start.is_file());
    start
        .ancestors()
        .skip(skip)
        .map(|dir| dir.join(FILE_NAME))
        .find(|p| p.is_file())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_full() {
        let cfg: Config = serde_yaml::from_str(
            "src:\n  - a.tasm\ninclude:\n  rtos: ../rtos\ntest:\n  default:\n    tmax: 100\n",
        )
        .unwrap();
        assert_eq!(cfg.src, vec!["a.tasm"]);
        assert_eq!(cfg.include["rtos"], "../rtos");
        assert_eq!(cfg.test["default"].tmax, Some(100));
    }

    #[test]
    fn defaults() {
        let cfg: Config = serde_yaml::from_str("{}").unwrap();
        assert_eq!(cfg.src, vec!["main.tasm"]);
        assert!(cfg.include.is_empty());
        assert!(cfg.test.is_empty());
    }
}
