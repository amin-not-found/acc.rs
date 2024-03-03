use regex;
use std::ops::Range;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Token<'t, K> {
    pub kind: K,
    pub span: Range<usize>,
    pub text: &'t str,
}

struct Tokenizer<'a, K> {
    kinds: Vec<K>,
    regexes: Vec<&'a str>,
    line_comment: Option<String>,
}

impl<'a, K> Tokenizer<'a, K> {
    fn new() -> Self {
        Tokenizer {
            kinds: Vec::new(),
            regexes: Vec::new(),
            line_comment: None,
        }
    }
    fn add_token(&mut self, kind: K, re: &'a str) {
        self.regexes.push(re);
        self.kinds.push(kind);
    }
    fn add_tokens(&mut self, tokens: Vec<(K, &'a str)>) {
        for token in tokens {
            self.add_token(token.0, token.1);
        }
    }
    fn set_line_comment(&mut self, lc: String) {
        self.line_comment = Some(lc);
    }
    fn tokens(self, source: &str) -> Tokens<'_, K> {
        let regex_set = regex::RegexSet::new(&self.regexes).unwrap();
        let mut regexes = Vec::new();
        for pattern in regex_set.patterns() {
            regexes.push(regex::Regex::new(pattern).unwrap());
        }
        Tokens {
            kinds: self.kinds,
            regexes,
            source,
            position: 0,
            current: None,
            peeked: false,
            line_comment: self.line_comment,
        }
    }
}

#[derive(Debug)]
pub struct Tokens<'s, K> {
    kinds: Vec<K>,
    regexes: Vec<regex::Regex>,
    source: &'s str,
    position: usize,
    current: Option<Token<'s, K>>,
    peeked: bool,
    line_comment: Option<String>,
}

impl<'s, K: Copy> Tokens<'s, K> {
    pub fn current_pos(&self) -> usize {
        self.position
    }
    pub fn current(&self) -> Option<Token<'s, K>> {
        if self.peeked {
            panic!("Can't see current item while peeking at next item.")
        }
        self.current.clone()
    }
    pub fn peek(&mut self) -> Option<Token<'s, K>> {
        if self.peeked {
            return self.current.clone();
        }
        let token = self.next();
        self.peeked = true;
        token
    }
}

impl<'s, K: Copy> Iterator for Tokens<'s, K> {
    type Item = Token<'s, K>;
    fn next(&mut self) -> Option<Self::Item> {
        if self.peeked {
            self.peeked = false;
            return self.current();
        }
        let whitespace_rx = regex::Regex::new(r"^\s").unwrap();
        loop {
            if self.position == self.source.len() {
                return None;
            }
            let string = &self.source[self.position..];
            if whitespace_rx.is_match(string) {
                self.position += 1;
                continue;
            }
            if let Some(lc) = &self.line_comment {
                if string.starts_with(lc) {
                    let comment_len = string.find('\n').unwrap_or(string.len());
                    self.position += comment_len;
                    continue;
                }
            }

            let mut rx: &regex::Regex = &self.regexes[0];
            let mut kind: Option<K> = None;
            for i in 0..self.regexes.len() {
                rx = &self.regexes[i];
                if rx.is_match(string) {
                    kind = Some(self.kinds[i]);
                    break;
                }
            }
            if kind.is_none() {
                // TODO : print a more useful message and handle the error better
                panic!(
                    "Invalid syntax at {}: \"{}\"",
                    self.position,
                    string[0..string.find('\n').unwrap_or(string.len())].to_owned()
                );
            }

            let m = rx.find(string).unwrap();
            let span = m.start() + self.position..m.end() + self.position;
            let text = &self.source[span.clone()];
            self.position += m.end();
            self.current = Some(Token {
                kind: kind.unwrap(),
                span,
                text,
            });
            return self.current.clone();
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum CToken {
    OpenBrace,
    CloseBrace,
    OpenParenthesis,
    CloseParenthesis,
    Semicolon,
    BitWiseComplement,
    LogicalNegation,
    MultiplicationSign,
    DivisionSign,
    PlusSign,
    MinusSign,
    ReturnKeyword,
    IntKeyword,
    Identifier,
    IntLiteral,
}

pub struct CTokenizer<'a> {
    tokenizer: Tokenizer<'a, CToken>,
}
impl<'a> CTokenizer<'a> {
    pub fn new(tokens: Vec<(CToken, &'a str)>) -> Self {
        let mut t = Tokenizer::new();
        t.add_tokens(tokens);
        t.set_line_comment("//".into());
        Self { tokenizer: t }
    }
    pub fn tokens<'t, 's: 't>(self, source: &'s str) -> Tokens<CToken> {
        self.tokenizer.tokens(source)
    }
}

impl<'a> Default for CTokenizer<'a> {
    fn default() -> Self {
        Self::new(vec![
            (CToken::OpenBrace, r"^\{"),
            (CToken::CloseBrace, r"^\}"),
            (CToken::OpenParenthesis, r"^\("),
            (CToken::CloseParenthesis, r"^\)"),
            (CToken::Semicolon, r"^;"),
            (CToken::BitWiseComplement, r"^\~"),
            (CToken::LogicalNegation, r"^!"),
            (CToken::MultiplicationSign, r"^\*"),
            (CToken::DivisionSign, r"^/"),
            (CToken::PlusSign, r"^\+"),
            (CToken::MinusSign, r"^\-"),
            ///////// Start of keywords
            (CToken::ReturnKeyword, r"^return\s"),
            (CToken::IntKeyword, r"^int\s"),
            ///////// End of keywords
            (CToken::Identifier, r"^[a-zA-Z]\w*"),
            (CToken::IntLiteral, r"^[0-9]+"),
        ])
    }
}
