//! tasm の Pos (1-indexed, byte 単位) と LSP の Position/Range (0-indexed, UTF-16) の変換。

use lsp_types::{Position, Range, Url};
use std::path::{Path, PathBuf};
use tasm::Pos;

/// 行テキスト内の byte offset (0-indexed) を UTF-16 code unit offset に変換する
fn byte_to_utf16(line: &str, byte: usize) -> u32 {
    let byte = byte.min(line.len());
    line[..byte].encode_utf16().count() as u32
}

/// 行テキスト内の UTF-16 code unit offset を byte offset (0-indexed) に変換する
fn utf16_to_byte(line: &str, utf16: u32) -> usize {
    let mut count = 0u32;
    for (idx, ch) in line.char_indices() {
        if count >= utf16 {
            return idx;
        }
        count += ch.len_utf16() as u32;
    }
    line.len()
}

/// Pos → LSP Range。text は Pos が指すファイルの全文。
/// Pos が無効 (row == 0、Pos::default()) の場合はファイル先頭を返す。
pub fn pos_to_range(pos: &Pos, text: &str) -> Range {
    if pos.row() == 0 {
        return Range::default();
    }
    let line = text.lines().nth(pos.row() - 1).unwrap_or("");
    let row = (pos.row() - 1) as u32;
    let start = byte_to_utf16(line, pos.col().saturating_sub(1));
    let end = byte_to_utf16(line, pos.end_col().saturating_sub(1)).max(start + 1);
    Range {
        start: Position::new(row, start),
        end: Position::new(row, end),
    }
}

/// LSP Position → (row, byte col)。どちらも 1-indexed で Pos と同じ単位。
pub fn position_to_rowcol(position: Position, text: &str) -> (usize, usize) {
    let line = text.lines().nth(position.line as usize).unwrap_or("");
    let byte = utf16_to_byte(line, position.character);
    (position.line as usize + 1, byte + 1)
}

/// 絶対パスへ正規化する。シンボリックリンク解決に失敗したらそのまま返す。
pub fn normalize_path(path: &Path) -> PathBuf {
    path.canonicalize().unwrap_or_else(|_| path.to_path_buf())
}

pub fn url_to_path(url: &Url) -> Option<PathBuf> {
    url.to_file_path().ok().map(|p| normalize_path(&p))
}

pub fn path_to_url(path: &Path) -> Option<Url> {
    Url::from_file_path(path).ok()
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::rc::Rc;

    fn pos(row: usize, col: usize, end: usize) -> Pos {
        Pos::new(Rc::from("test.tasm"), row, col).with_end(end)
    }

    #[test]
    fn ascii_range() {
        let text = "fn main() {\n}\n";
        let r = pos_to_range(&pos(1, 4, 8), text);
        assert_eq!(r.start, Position::new(0, 3));
        assert_eq!(r.end, Position::new(0, 7));
    }

    #[test]
    fn multibyte_comment_line() {
        // 行頭に日本語コメント (UTF-8 3byte/char) がある行の後方トークン
        let text = "x; // あいう ok\n";
        // "あいう " は byte 6..16、"ok" は byte 16..18 (1-indexed col 17..19)
        let r = pos_to_range(&pos(1, 17, 19), text);
        // UTF-16 では "x; // " = 6, "あいう " = 4 → ok は 10 開始
        assert_eq!(r.start, Position::new(0, 10));
        assert_eq!(r.end, Position::new(0, 12));
    }

    #[test]
    fn multibyte_string_literal() {
        let text = r#"x = "あ";"#;
        // ';' は byte col 10 (x=1, sp=2, ==3, sp=4, "=5, あ=6..8, "=9, ;=10)
        let r = pos_to_range(&pos(1, 10, 11), text);
        assert_eq!(r.start, Position::new(0, 7));
        assert_eq!(r.end, Position::new(0, 8));
    }

    #[test]
    fn default_pos_falls_back_to_origin() {
        let r = pos_to_range(&Pos::default(), "abc");
        assert_eq!(r, Range::default());
    }

    #[test]
    fn roundtrip_position() {
        let text = "print(\"あいう\"); // x\n";
        // LSP (0, 10) = "あいう" の後ろの '"'
        let (row, col) = position_to_rowcol(Position::new(0, 10), text);
        assert_eq!(row, 1);
        // byte: print(" = 7 byte, あいう = 9 byte → col = 17 (1-indexed)
        assert_eq!(col, 17);
    }

    #[test]
    fn position_past_line_end_clamps() {
        let (row, col) = position_to_rowcol(Position::new(0, 100), "ab");
        assert_eq!((row, col), (1, 3));
    }
}
