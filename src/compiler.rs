
use crate::ast;
use crate::ast::Parse;
use crate::tokenizer;

pub fn parse(code: &str) -> ast::Program {
    let t = tokenizer::CTokenizer::new();
    let mut tokens = t.tokens(code);
    ast::Program::parse(&mut tokens).unwrap()
}
