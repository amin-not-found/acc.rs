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
}

impl<'a, K> Tokenizer<'a, K> {
    fn new() -> Self {
        Tokenizer {
            kinds: Vec::new(),
            regexes: Vec::new(),
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
    fn tokens<'k, 't: 'k, 's>(&'t self, source: &'s str) -> Tokens<'k, 's, K> {
        let regex_set = regex::RegexSet::new(&self.regexes).unwrap();
        let mut regexes = Vec::new();
        for pattern in regex_set.patterns() {
            regexes.push(regex::Regex::new(pattern).unwrap());
        }
        Tokens {
            kinds: &self.kinds,
            regexes: regexes,
            source: source,
            position: 0,
            current: None,
        }
    }
}

#[derive(Debug)]
pub struct Tokens<'k, 's, K> {
    kinds: &'k Vec<K>,
    regexes: Vec<regex::Regex>,
    source: &'s str,
    position: usize,
    current: Option<Token<'s, K>>,
}

impl<'k, 's, K: Clone> Tokens<'k, 's, K> {
    pub fn current_pos(&self) -> usize {
        self.position
    }
    pub fn current(&self) -> Option<Token<'s, K>> {
        self.current.clone()
    }
}

impl<'k, 's, K: Copy> Iterator for Tokens<'k, 's, K> {
    type Item = Token<'s, K>;
    fn next(&mut self) -> Option<Self::Item> {
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
            let mut rx: &regex::Regex = &self.regexes[0];
            let mut kind: Option<K> = None;
            for i in 0..self.regexes.len() {
                rx = &self.regexes[i];
                if rx.is_match(string) {
                    kind = Some(self.kinds[i]);
                    break;
                }
            }
            if let None = kind {
                // TODO : print a more useful message and handle the error
                println!("Invalid syntax at {}", self.position);
                return None;
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
    Negation,
    BitWiseComplement,
    LogicalNegation,
    ReturnKeyword,
    IntKeyword,
    Identifier,
    IntLiteral,
}

pub struct CTokenizer<'a> {
    tokenizer: Tokenizer<'a, CToken>,
}
impl<'a> CTokenizer<'a> {
    pub fn new() -> Self {
        let mut t = Tokenizer::new();
        t.add_tokens(vec![
            (CToken::OpenBrace, r"^\{"),
            (CToken::CloseBrace, r"^\}"),
            (CToken::OpenParenthesis, r"^\("),
            (CToken::CloseParenthesis, r"^\)"),
            (CToken::Semicolon, r"^;"),
            (CToken::Negation, r"^\-"),
            (CToken::BitWiseComplement, r"^\~"),
            (CToken::LogicalNegation, r"^!"),
            // TODO : do something about keywords needing a whitespace after them
            ///////// Start of keywords
            (CToken::ReturnKeyword, r"^return\s"),
            (CToken::IntKeyword, r"^int\s"),
            ///////// End of keywords
            (CToken::Identifier, r"^[a-zA-Z]\w*"),
            (CToken::IntLiteral, r"^[0-9]+"),
        ]);
        Self { tokenizer: t }
    }
    pub fn tokens<'t, 's: 't>(&'t self, source: &'s str) -> Tokens<CToken> {
        self.tokenizer.tokens(source)
    }
}
