use std::{fmt::Display, mem::discriminant};

use crate::tokenizer::{CToken, Token, Tokens};

fn next_token<'a>(
    tokens: &mut Tokens<'a, 'a, CToken>,
) -> Result<Token<'a, CToken>, ParseError<'a>> {
    let t = tokens.next();
    match t {
        Some(token) => Ok(token),
        None => Err(ParseError::new("Unexpected end of tokens".into(), tokens)),
    }
}

fn expect_token<'a>(
    tokens: &mut Tokens<'a, 'a, CToken>,
    typ: CToken,
    error_msg: Option<String>,
) -> Result<Token<'a, CToken>, ParseError<'a>> {
    let token = next_token(tokens)?;
    if discriminant(&token.kind) != discriminant(&typ) {
        return Err(ParseError::new(
            error_msg.unwrap_or(format!("Expected {:?}", typ)),
            tokens,
        ));
    }
    Ok(token)
}

#[derive(Debug)]
pub struct ParseError<'a> {
    pub message: String,
    pub position: usize,
    pub token: Option<Token<'a, CToken>>,
}

impl<'a> ParseError<'a> {
    fn new(message: String, tokens: &Tokens<'a, 'a, CToken>) -> Self {
        ParseError {
            message,
            position: tokens.current_pos(),
            token: tokens.current(),
        }
    }
}

pub trait Parse {
    fn parse<'a>(tokens: &mut Tokens<'a, 'a, CToken>) -> Result<Self, ParseError<'a>>
    where
        Self: Sized;
}

pub trait AsmGen {
    fn to_asm(&self) -> String;
}


#[derive(Debug)]
pub enum Expression {
    Constant(i32),
}

impl Parse for Expression {
    fn parse<'a>(tokens: &mut Tokens<'a, 'a, CToken>) -> Result<Self, ParseError<'a>> {
        let token = next_token(tokens)?;
        match token.kind {
            CToken::IntLiteral => {
                let Ok(int) = token.text.parse::<i32>() else {
                    return Err(ParseError::new("Invalid int literal".into(), tokens));
                };
                Ok(Expression::Constant(int))
            }
            _ => Err(ParseError::new("Expected int literal".into(), tokens)),
        }
    }
}

impl AsmGen for Expression {
    fn to_asm(&self) -> String {
        match self {
            Self::Constant(i) => format!("movl ${}, %eax", i),
        }
    }
}

impl Display for Expression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Constant(i) => write!(f, "Int<{}>", i),
        }
    }
}

#[derive(Debug)]
pub enum Statement {
    Return(Expression),
}

impl Parse for Statement {
    fn parse<'a>(tokens: &mut Tokens<'a, 'a, CToken>) -> Result<Self, ParseError<'a>> {
        let token = next_token(tokens)?;
        match token.kind {
            CToken::ReturnKeyword => {
                let expr = Expression::parse(tokens)?;
                expect_token(tokens, CToken::Semicolon, None)?;
                Ok(Statement::Return(expr))
            }
            _ => Err(ParseError::new("Return keyword expected".into(), tokens)),
        }
    }
}

impl AsmGen for Statement {
    fn to_asm(&self) -> String {
        match self {
            Self::Return(expr) => format!("{}\nret\n", expr.to_asm()),
        }
    }
}

impl Display for Statement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Return(expr) => write!(f, "RETURN {}", expr),
        }
    }
}

#[derive(Debug)]
pub struct Function {
    pub name: String,
    pub body: Statement,
}

impl Parse for Function {
    fn parse<'a>(tokens: &mut Tokens<'a, 'a, CToken>) -> Result<Self, ParseError<'a>> {
        let mut token = next_token(tokens)?;
        match token.kind {
            CToken::IntKeyword => {
                token = expect_token(tokens, CToken::Identifier, None)?;
                let name = token.text.to_string();
                expect_token(tokens, CToken::OpenParenthesis, None)?;
                expect_token(tokens, CToken::CloseParenthesis, None)?;
                expect_token(tokens, CToken::OpenBrace, None)?;
                let func = Function {
                    name,
                    body: Statement::parse(tokens)?,
                };
                expect_token(tokens, CToken::CloseBrace, None)?;

                Ok(func)
            }
            _ => Err(ParseError::new("Expected keyword".into(), tokens)),
        }
    }
}

impl AsmGen for Function {
    fn to_asm(&self) -> String {
        format!(".global {0}\n{0}:\n{1}", self.name, self.body.to_asm())
    }
}

impl Display for Function{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let body = self.body.to_string().replace('\n', "\n\t\t");
        write!(f, "FUN INT {}:\n\tparams: ()\n\tbody:\n\t\t{}", self.name, body)
    }
}

#[derive(Debug)]
pub enum Program {
    Main(Function),
}

impl Parse for Program {
    fn parse<'a>(tokens: &mut Tokens<'a, 'a, CToken>) -> Result<Self, ParseError<'a>> {
        Ok(Program::Main(Function::parse(tokens)?))
    }
}

impl AsmGen for Program {
    fn to_asm(&self) -> String {
        match self {
            Self::Main(f) => f.to_asm(),
        }
    }
}

impl Display for Program{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self{
            Self::Main(func) => write!(f, "{}", func)
        }
    }
}