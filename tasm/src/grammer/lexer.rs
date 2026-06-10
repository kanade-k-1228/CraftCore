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
    file: Rc<str>,
    row: usize,
}

impl<'a> LineLexer<'a> {
    fn new(line: &'a str, file: Rc<str>, row: usize) -> Self {
        let iter = line.char_indices().peekable();
        Self { iter, file, row }
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
                    let comment = self.iter.map(|(_, ch)| ch).collect::<String>();
                    tokens.push(Token::new(TokenKind::Comment(comment), pos));
                    break;
                }

                if let Some(kind) = double_char_token(ch0, ch1) {
                    self.consume(); // consume
                    self.consume(); // consume second char
                    tokens.push(Token::new(kind, pos));
                    continue;
                }
            }

            // 2. Single character token
            if let Some(kind) = single_char_token(ch0) {
                self.consume();
                tokens.push(Token::new(kind, pos));
                continue;
            }

            // 3. Number literal
            if ch0.is_ascii_digit() {
                tokens.push(Token::new(self.parse_number(), pos));
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
                                tokens.push(Token::new(TokenKind::Error(e), pos));
                                continue;
                            }
                        }
                    } else {
                        ch1
                    };

                    match self.consume() {
                        Some((_, '\'')) => tokens.push(Token::new(TokenKind::Char(ch_value), pos)),
                        _ => tokens
                            .push(Token::new(TokenKind::Error(format!("'{}", ch_value)), pos)),
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
                    tokens.push(Token::new(TokenKind::Error("'".to_string()), pos));
                } else {
                    tokens.push(Token::new(TokenKind::Scope(lexeme), pos));
                }
                continue;
            }

            // 5. String literal
            if ch0 == '"' {
                tokens.push(Token::new(self.parse_text(), pos));
                continue;
            }

            // 6. Identifier or keyword
            if ch0.is_ascii_alphabetic() || ch0 == '_' {
                tokens.push(Token::new(self.parse_string(ch0), pos));
                continue;
            }

            // Error
            self.iter.next();
            tokens.push(Token::new(TokenKind::Error(format!("{ch0}")), pos));
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
