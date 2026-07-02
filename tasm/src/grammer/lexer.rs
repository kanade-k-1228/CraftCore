use super::token::{Pos, Token, TokenKind};
use std::iter::Peekable;
use std::rc::Rc;
use std::str::CharIndices;

pub struct Lexer {
    file: Rc<str>,
    code: Rc<str>,
}

impl Lexer {
    pub fn new(file: &str, code: &str) -> Self {
        Self {
            file: Rc::from(file),
            code: Rc::from(code),
        }
    }

    pub fn parse(&self) -> Vec<Token> {
        let mut tokens = Vec::new();
        for (row, line) in self.code.lines().enumerate() {
            let lexer = LineLexer::new(line, Rc::clone(&self.file), row);
            tokens.extend(lexer.parse());
        }
        tokens
    }
}

struct LineLexer<'a> {
    iter: Peekable<CharIndices<'a>>,
    line: &'a str,
    file: Rc<str>,
    row: usize,
}

impl<'a> LineLexer<'a> {
    fn new(line: &'a str, file: Rc<str>, row: usize) -> Self {
        let iter = line.char_indices().peekable();
        Self {
            iter,
            line,
            file,
            row,
        }
    }
}

// ----------------------------------------------------------------------------
// Helpers
// ----------------------------------------------------------------------------

impl<'a> LineLexer<'a> {
    fn peek_nth(&self, n: usize) -> Option<(usize, char)> {
        self.iter.clone().nth(n)
    }
    fn consume(&mut self) -> Option<(usize, char)> {
        self.iter.next()
    }
    /// 現在のイテレータ位置 (byte index)。行末なら line.len()。
    fn cur_byte(&mut self) -> usize {
        self.iter.peek().map(|(i, _)| *i).unwrap_or(self.line.len())
    }
    /// 消費済み範囲を終端としてトークンを積む
    fn push(&mut self, tokens: &mut Vec<Token>, kind: TokenKind, pos: Pos) {
        let end = self.cur_byte() + 1;
        tokens.push(Token::new(kind, pos.with_end(end)));
    }
}

// ----------------------------------------------------------------------------
// Parser
// ----------------------------------------------------------------------------

impl<'a> LineLexer<'a> {
    pub fn parse(mut self) -> Vec<Token> {
        let mut tokens = Vec::new();
        while let Some((idx, ch0)) = self.peek_nth(0) {
            // 0. Skip whitespaces
            if ch0.is_whitespace() {
                self.consume();
                continue;
            }

            // Pos is 1-indexed (row, col) as in conventional compiler output
            let pos = Pos::new(Rc::clone(&self.file), self.row + 1, idx + 1);

            // 1. Double character token
            if let Some((_, ch1)) = self.peek_nth(1) {
                // Comment
                if ch0 == '/' && ch1 == '/' {
                    self.consume(); // consume '/'
                    self.consume(); // consume '/'
                    while let Some(_) = self.iter.next_if(|(_, c)| c.is_whitespace()) {}
                    let comment = self.iter.by_ref().map(|(_, ch)| ch).collect::<String>();
                    self.push(&mut tokens, TokenKind::Comment(comment), pos);
                    break;
                }

                if let Some(kind) = double_char_token(ch0, ch1) {
                    self.consume(); // consume
                    self.consume(); // consume second char
                    self.push(&mut tokens, kind, pos);
                    continue;
                }
            }

            // 2. Single character token
            if let Some(kind) = single_char_token(ch0) {
                self.consume();
                self.push(&mut tokens, kind, pos);
                continue;
            }

            // 3. Number literal
            if ch0.is_ascii_digit() {
                let kind = self.parse_number();
                self.push(&mut tokens, kind, pos);
                continue;
            }

            // 4. Char literal or scope name
            if ch0 == '\'' {
                // Disambiguate between char literal ('X' / '\n') and scope name ('ident).
                // - Char literal with escape: peek_nth(1) == '\\'
                // - Plain char literal: peek_nth(2) == '\''
                // - Otherwise: scope name (e.g. 'outer)
                let is_escape = matches!(self.peek_nth(1), Some((_, '\\')));
                let is_plain_char = matches!(self.peek_nth(2), Some((_, '\'')));

                if is_escape || is_plain_char {
                    self.consume(); // consume opening '
                    let (_, ch1) = self.consume().unwrap();

                    let ch_value = if ch1 == '\\' {
                        match self.parse_escape() {
                            Ok(ch) => ch,
                            Err(e) => {
                                // Resync on the closing quote if present
                                self.iter.next_if(|(_, ch)| *ch == '\'');
                                self.push(&mut tokens, TokenKind::Error(e), pos);
                                continue;
                            }
                        }
                    } else {
                        ch1
                    };

                    match self.consume() {
                        Some((_, '\'')) => {
                            self.push(&mut tokens, TokenKind::Char(ch_value), pos);
                        }
                        _ => {
                            self.push(&mut tokens, TokenKind::Error(format!("'{}", ch_value)), pos);
                        }
                    }
                    continue;
                }

                // Scope name: 'ident
                self.consume(); // consume opening '
                let mut lexeme = Vec::new();
                while let Some((_, ch)) = self
                    .iter
                    .next_if(|(_, ch)| matches!(ch, '_' | '0'..='9' | 'a'..='z' | 'A'..='Z'))
                {
                    lexeme.push(ch);
                }
                let lexeme: String = lexeme.into_iter().collect();
                if lexeme.is_empty() {
                    self.push(&mut tokens, TokenKind::Error("'".to_string()), pos);
                } else {
                    self.push(&mut tokens, TokenKind::Scope(lexeme), pos);
                }
                continue;
            }

            // 5. String literal
            if ch0 == '"' {
                let kind = self.parse_text();
                self.push(&mut tokens, kind, pos);
                continue;
            }

            // 6. Identifier or keyword
            if ch0.is_ascii_alphabetic() || ch0 == '_' {
                let kind = self.parse_string(ch0);
                self.push(&mut tokens, kind, pos);
                continue;
            }

            // Error
            self.iter.next();
            self.push(&mut tokens, TokenKind::Error(format!("{ch0}")), pos);
        }
        tokens
    }

    fn parse_string(&mut self, ch: char) -> TokenKind {
        self.iter.next();
        let mut lexeme = vec![ch];
        while let Some((_, ch)) = self
            .iter
            .next_if(|(_, ch)| matches!(ch, '_' | '0'..='9' | 'a'..='z' | 'A'..='Z' ))
        {
            lexeme.push(ch);
        }
        let lexeme = lexeme.into_iter().collect::<String>();
        match keyword(&lexeme) {
            Some(kind) => kind,
            None => TokenKind::Ident(lexeme.to_string()),
        }
    }

    // Text: "hoge\nfuga"
    fn parse_text(&mut self) -> TokenKind {
        self.consume();

        let mut lexeme = vec![];
        let mut error = None;
        while let Some((_, ch)) = self.consume() {
            match ch {
                '"' => break,
                '\\' => match self.parse_escape() {
                    Ok(ch) => lexeme.push(ch),
                    Err(e) => error = error.or(Some(e)),
                },
                ch => lexeme.push(ch),
            }
        }
        if let Some(e) = error {
            return TokenKind::Error(e);
        }
        let lexeme = lexeme.into_iter().collect::<String>();
        TokenKind::Text(lexeme.to_string())
    }

    /// Decode the character following a `\` in a char/string literal.
    /// Returns the offending lexeme on EOF or unknown escape.
    fn parse_escape(&mut self) -> Result<char, String> {
        let Some((_, ch)) = self.consume() else {
            return Err("\\".to_string());
        };
        match ch {
            'n' => Ok('\n'),
            't' => Ok('\t'),
            'r' => Ok('\r'),
            '\\' => Ok('\\'),
            '\'' => Ok('\''),
            '"' => Ok('"'),
            '0' => Ok('\0'),
            _ => Err(format!("\\{}", ch)),
        }
    }

    fn parse_number(&mut self) -> TokenKind {
        let (_, ch0) = self.consume().unwrap();
        if ch0 == '0' {
            if let Some(&(_, ch1)) = self.iter.peek() {
                if ch1 == 'x' || ch1 == 'X' {
                    self.consume();
                    return self.parse_number_hex(ch0, ch1);
                }
                if ch1 == 'o' || ch1 == 'O' {
                    self.consume();
                    return self.parse_number_oct(ch0, ch1);
                }
                if ch1 == 'b' || ch1 == 'B' {
                    self.consume();
                    return self.parse_number_bin(ch0, ch1);
                }
            }
        }
        return self.parse_number_dec(ch0);
    }

    fn parse_number_hex(&mut self, ch0: char, ch1: char) -> TokenKind {
        let mut lexeme = vec![ch0, ch1];
        while let Some((_, ch)) = self
            .iter
            .next_if(|(_, ch)| matches!(ch, '_' | '0'..='9' | 'a'..='f' | 'A'..='F' ))
        {
            lexeme.push(ch);
        }
        let lexeme = lexeme.into_iter().collect::<String>();
        match usize::from_str_radix(&lexeme[2..].replace("_", ""), 16) {
            Ok(num) => TokenKind::Number(lexeme.to_string(), num),
            Err(_) => TokenKind::Error(lexeme.to_string()),
        }
    }

    fn parse_number_oct(&mut self, ch0: char, ch1: char) -> TokenKind {
        let mut lexeme = vec![ch0, ch1];
        while let Some((_, ch)) = self.iter.next_if(|(_, ch)| matches!(ch, '_' | '0'..='7')) {
            lexeme.push(ch);
        }
        let lexeme = lexeme.into_iter().collect::<String>();
        match usize::from_str_radix(&lexeme[2..].replace("_", ""), 8) {
            Ok(num) => TokenKind::Number(lexeme.to_string(), num),
            Err(_) => TokenKind::Error(lexeme.to_string()),
        }
    }

    fn parse_number_bin(&mut self, ch0: char, ch1: char) -> TokenKind {
        let mut lexeme = vec![ch0, ch1];
        while let Some((_, ch)) = self.iter.next_if(|(_, ch)| matches!(ch, '_' | '0' | '1')) {
            lexeme.push(ch);
        }
        let lexeme = lexeme.into_iter().collect::<String>();
        match usize::from_str_radix(&lexeme[2..].replace("_", ""), 2) {
            Ok(num) => TokenKind::Number(lexeme.to_string(), num),
            Err(_) => TokenKind::Error(lexeme.to_string()),
        }
    }

    fn parse_number_dec(&mut self, ch: char) -> TokenKind {
        let mut lexeme = vec![ch];
        while let Some((_, ch)) = self.iter.next_if(|(_, ch)| matches!(ch, '_' | '0'..='9')) {
            lexeme.push(ch);
        }
        let lexeme = lexeme.into_iter().collect::<String>();
        match usize::from_str_radix(&lexeme.replace("_", ""), 10) {
            Ok(num) => TokenKind::Number(lexeme.to_string(), num),
            Err(_) => TokenKind::Error(lexeme.to_string()),
        }
    }
}

fn double_char_token(ch0: char, ch1: char) -> Option<TokenKind> {
    match (ch0, ch1) {
        ('=', '=') => Some(TokenKind::EqualEqual),
        ('!', '=') => Some(TokenKind::ExclEqual),
        ('<', '=') => Some(TokenKind::LAngleEqual),
        ('>', '=') => Some(TokenKind::RAngleEqual),
        ('<', '<') => Some(TokenKind::LAngleLAngle),
        ('>', '>') => Some(TokenKind::RAngleRAngle),
        ('-', '>') => Some(TokenKind::Arrow),
        (':', ':') => Some(TokenKind::ColonColon),
        _ => None,
    }
}

fn single_char_token(ch: char) -> Option<TokenKind> {
    match ch {
        '=' => Some(TokenKind::Equal),
        '+' => Some(TokenKind::Plus),
        '-' => Some(TokenKind::Minus),
        '*' => Some(TokenKind::Star),
        '@' => Some(TokenKind::Atmark),
        '/' => Some(TokenKind::Slash),
        '%' => Some(TokenKind::Percent),
        '&' => Some(TokenKind::Ampasand),
        '|' => Some(TokenKind::Pipe),
        '^' => Some(TokenKind::Caret),
        '!' => Some(TokenKind::Excl),
        ':' => Some(TokenKind::Colon),
        ';' => Some(TokenKind::Semicolon),
        ',' => Some(TokenKind::Comma),
        '.' => Some(TokenKind::Period),
        '(' => Some(TokenKind::LParen),
        ')' => Some(TokenKind::RParen),
        '[' => Some(TokenKind::LBracket),
        ']' => Some(TokenKind::RBracket),
        '{' => Some(TokenKind::LCurly),
        '}' => Some(TokenKind::RCurly),
        '<' => Some(TokenKind::LAngle),
        '>' => Some(TokenKind::RAngle),
        _ => None,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn spans(code: &str) -> Vec<(TokenKind, usize, usize, usize)> {
        Lexer::new("test.tasm", code)
            .parse()
            .into_iter()
            .map(|t| (t.kind, t.pos.row(), t.pos.col(), t.pos.end_col()))
            .collect()
    }

    #[test]
    fn ascii_token_spans() {
        let toks = spans("fn main() {");
        assert_eq!(toks[0], (TokenKind::KwFunc, 1, 1, 3));
        assert_eq!(toks[1], (TokenKind::Ident("main".into()), 1, 4, 8));
        assert_eq!(toks[2], (TokenKind::LParen, 1, 8, 9));
        assert_eq!(toks[3], (TokenKind::RParen, 1, 9, 10));
        assert_eq!(toks[4], (TokenKind::LCurly, 1, 11, 12));
    }

    #[test]
    fn multiline_rows() {
        let toks = spans("var x: int;\n  x = 1;");
        let x2 = &toks[5];
        assert_eq!(*x2, (TokenKind::Ident("x".into()), 2, 3, 4));
    }

    #[test]
    fn double_char_and_number_spans() {
        let toks = spans("a == 0x1_F");
        assert_eq!(toks[1], (TokenKind::EqualEqual, 1, 3, 5));
        assert_eq!(toks[2], (TokenKind::Number("0x1_F".into(), 0x1F), 1, 6, 11));
    }

    #[test]
    fn multibyte_string_byte_cols() {
        // "あ" は UTF-8 で 3 byte。後続トークンの col は byte 単位でずれる。
        let toks = spans(r#"x = "あ";"#);
        assert_eq!(toks[0], (TokenKind::Ident("x".into()), 1, 1, 2));
        // 文字列リテラル: 開始 col 5 ('"')、中身 3 byte + 引用符 2 = 終端 col 10
        assert_eq!(toks[2], (TokenKind::Text("あ".into()), 1, 5, 10));
        assert_eq!(toks[3], (TokenKind::Semicolon, 1, 10, 11));
    }

    #[test]
    fn comment_span_to_eol() {
        let toks = spans("x; // コメント");
        let (kind, row, col, end) = &toks[2];
        assert!(matches!(kind, TokenKind::Comment(_)));
        assert_eq!((*row, *col), (1, 4));
        assert_eq!(*end, "x; // コメント".len() + 1);
    }

    #[test]
    fn escaped_string_span() {
        // エスケープを含む文字列: lexeme 長 ≠ ソース長でも終端はソース位置基準
        let toks = spans(r#""a\n""#);
        assert_eq!(toks[0], (TokenKind::Text("a\n".into()), 1, 1, 6));
    }
}

fn keyword(s: &str) -> Option<TokenKind> {
    match s {
        "fn" => Some(TokenKind::KwFunc),
        "var" => Some(TokenKind::KwVar),
        "type" => Some(TokenKind::KwType),
        "const" => Some(TokenKind::KwConst),
        "static" => Some(TokenKind::KwStatic),
        "if" => Some(TokenKind::KwIf),
        "else" => Some(TokenKind::KwElse),
        "while" => Some(TokenKind::KwWhile),
        "asm" => Some(TokenKind::KwAsm),
        "break" => Some(TokenKind::KwBreak),
        "continue" => Some(TokenKind::KwContinue),
        "int" => Some(TokenKind::KwInt),
        "void" => Some(TokenKind::KwVoid),
        "return" => Some(TokenKind::KwReturn),
        "as" => Some(TokenKind::KwAs),
        "sizeof" => Some(TokenKind::KwSizeof),
        _ => None,
    }
}
