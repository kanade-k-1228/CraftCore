use std::fs;
use std::path::Path;

/// `-I` の指定をパースして (モジュールルート名, ディレクトリ) を返す。
/// `NAME=DIR` は NAME を、`DIR` のみなら basename をルート名にする。
pub fn parse_include_spec(spec: &str) -> (String, String) {
    if let Some((name, dir)) = spec.split_once('=') {
        return (name.to_string(), dir.to_string());
    }
    let name = Path::new(spec)
        .file_name()
        .map(|n| n.to_string_lossy().into_owned())
        .unwrap_or_default();
    (name, spec.to_string())
}

/// `-I` で指定されたディレクトリ配下の .tasm を再帰収集し、
/// (path, module_prefix) を out へ積む。
/// prefix は親までのモジュールパス (`rtos`, `rtos::sub` など)。
pub fn collect_module_files(
    dir: &Path,
    prefix: &str,
    out: &mut Vec<(String, String)>,
) -> std::io::Result<()> {
    let mut entries: Vec<_> = fs::read_dir(dir)?.collect::<Result<_, _>>()?;
    entries.sort_by_key(|e| e.path());
    for e in entries {
        let p = e.path();
        if p.is_dir() {
            let name = p
                .file_name()
                .map(|n| n.to_string_lossy().into_owned())
                .unwrap_or_default();
            let sub = format!("{}::{}", prefix, name);
            collect_module_files(&p, &sub, out)?;
        } else if p.extension().and_then(|s| s.to_str()) == Some("tasm") {
            let stem = p
                .file_stem()
                .map(|n| n.to_string_lossy().into_owned())
                .unwrap_or_default();
            let mod_prefix = format!("{}::{}", prefix, stem);
            out.push((p.to_string_lossy().into_owned(), mod_prefix));
        }
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn include_spec_named() {
        assert_eq!(
            parse_include_spec("mylib=../../rtos"),
            ("mylib".to_string(), "../../rtos".to_string())
        );
    }

    #[test]
    fn include_spec_bare_dir_uses_basename() {
        assert_eq!(
            parse_include_spec("../../rtos"),
            ("rtos".to_string(), "../../rtos".to_string())
        );
    }
}
