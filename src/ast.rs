use std::{fmt::Display, mem::discriminant};

use crate::tokenizer::{CToken, Token, Tokens};

type CTokens<'a> = Tokens<'a, CToken>;

fn next_token<'a>(
    tokens: &mut CTokens<'a>,
) -> Result<Token<'a, CToken>, ParseError<'a>> {
    let t = tokens.next();
    match t {
        Some(token) => Ok(token),
        None => Err(ParseError::new("Unexpected end of tokens".into(), tokens)),
    }
}

fn peek_token<'a>(
    tokens: &mut CTokens<'a>,
) -> Result<Token<'a, CToken>, ParseError<'a>> {
    let t = tokens.peek();
    match t {
        Some(token) => Ok(token.clone()),
        None => Err(ParseError::new("Unexpected end of tokens".into(), tokens)),
    }
}

fn expect_token<'a>(
    tokens: &mut CTokens<'a>,
    typ: CToken,
    custom_err: Option<String>,
) -> Result<Token<'a, CToken>, ParseError<'a>> {
    let token = next_token(tokens)?;
    if discriminant(&token.kind) != discriminant(&typ) {
        return Err(ParseError::new(
            custom_err.unwrap_or(format!("Expected {:?}", typ)),
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
    fn new(message: String, tokens: &CTokens<'a>) -> Self {
        ParseError {
            message,
            position: tokens.current_pos(),
            token: tokens.current(),
        }
    }
}

pub trait Parse {
    fn parse<'a>(tokens: &mut CTokens<'a>) -> Result<Self, ParseError<'a>>
    where
        Self: Sized;
}

pub trait AsmGen {
    fn to_asm(&self) -> String;
}

#[derive(Debug)]
pub enum PrimaryExpr {
    Constant(i32),
    UnaryOp(CToken, Box<PrimaryExpr>),
    Expression(Box<Expression>), // e.g. in case of parentheses
}

impl Parse for PrimaryExpr {
    // primary-expr ::= "(" <expr> ")" | <unary_op> <primary-expr> | <int>
    fn parse<'a>(tokens: &mut CTokens<'a>) -> Result<Self, ParseError<'a>> {
        use CToken::*;
        let token = next_token(tokens)?;
        match token.kind {
            IntLiteral => {
                let Ok(int) = token.text.parse::<i32>() else {
                    return Err(ParseError::new("Invalid int literal".into(), tokens));
                };
                Ok(PrimaryExpr::Constant(int))
            }
            MinusSign | LogicalNegation | BitWiseComplement => {
                let expr = PrimaryExpr::parse(tokens)?;
                Ok(PrimaryExpr::UnaryOp(token.kind, Box::new(expr)))
            }
            OpenParenthesis => {
                let expr = Expression::parse(tokens)?;
                expect_token(tokens, CToken::CloseParenthesis, None)?;
                Ok(PrimaryExpr::Expression(expr.into()))
            }
            _ => Err(ParseError::new("Expected int literal".into(), tokens)),
        }
    }
}

impl AsmGen for PrimaryExpr {
    fn to_asm(&self) -> String {
        use CToken::*;
        match &self {
            Self::Constant(i) => format!("mov rax, {}\n", i),
            Self::UnaryOp(op, expr) => match op {
                MinusSign => format!("{}neg rax\n", expr.to_asm()),
                BitWiseComplement => format!("{}not rax\n", expr.to_asm()),
                LogicalNegation => format!(
                    "{}\
                     test eax, eax\n\
                     sete al\n\
                     movzx eax, al\n",
                    expr.to_asm()
                ),
                _ => panic!("Unsupported unary operator {:?}", op),
            },
            Self::Expression(expr) => expr.to_asm(),
        }
    }
}

impl Display for PrimaryExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self {
            Self::Constant(i) => write!(f, "IntLiteral({})", i),
            Self::UnaryOp(op, expr) => write!(f, "UnaryOp(op={:?}, arg={})", op, expr),
            Self::Expression(expr) => write!(f, "{}", expr),
        }
    }
}
#[derive(Debug)]
pub enum MultiplicativeExpr {
    Multiplication(PrimaryExpr, PrimaryExpr),
    Division(PrimaryExpr, PrimaryExpr),
    PrimaryExpr(PrimaryExpr),
}

impl Parse for MultiplicativeExpr {
    // multiplicative-expr ::= <primary-expr> ("*" | "/") <primary-expr> | <primary-expr>
    fn parse<'a>(tokens: &mut CTokens<'a>) -> Result<Self, ParseError<'a>> {
        use CToken::*;
        let lhs = PrimaryExpr::parse(tokens)?;
        let token = peek_token(tokens)?;
        match token.kind {
            MultiplicationSign => {
                _ = tokens.next();
                let rhs = PrimaryExpr::parse(tokens)?;
                Ok(Self::Multiplication(lhs, rhs))
            }
            DivisionSign => {
                _ = tokens.next();
                let rhs = PrimaryExpr::parse(tokens)?;
                Ok(Self::Division(lhs, rhs))
            }
            _ => Ok(Self::PrimaryExpr(lhs)),
        }
    }
}
impl AsmGen for MultiplicativeExpr {
    fn to_asm(&self) -> String {
        if let Self::PrimaryExpr(expr) = &self {
            expr.to_asm()
        } else {
            let (op, lhs, rhs) = match self {
                Self::Multiplication(lhs, rhs) => ("imul", lhs, rhs),
                Self::Division(lhs, rhs) => ("cdq\nidiv", lhs, rhs),
                Self::PrimaryExpr(_) => panic!("Unreachable"),
            };
            format!(
                "{}\
                push rax\n\
                {}\
                pop rbx\n\
                {} rax, rbx\n",
                rhs.to_asm(), lhs.to_asm(), op
            )
        }
    }
}
impl Display for MultiplicativeExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self {
            Self::Multiplication(lhs, rhs) => write!(f, "Multiplication(lhs={}, rhs={})", lhs, rhs),
            Self::Division(lhs, rhs) => write!(f, "Division(lhs={}, rhs={})", lhs, rhs),
            Self::PrimaryExpr(expr) => write!(f, "{}", expr),
        }
    }
}

#[derive(Debug)]
pub enum AdditiveExpr {
    Addition(MultiplicativeExpr, MultiplicativeExpr),
    Subtraction(MultiplicativeExpr, MultiplicativeExpr),
    MultiplicativeExpr(MultiplicativeExpr),
}

impl Parse for AdditiveExpr {
    fn parse<'a>(tokens: &mut CTokens<'a>) -> Result<Self, ParseError<'a>> {
        use CToken::*;
        let lhs = MultiplicativeExpr::parse(tokens)?;
        let token = peek_token(tokens)?;
        match token.kind {
            PlusSign => {
                _ = tokens.next();
                let rhs = MultiplicativeExpr::parse(tokens)?;
                Ok(Self::Addition(lhs, rhs))
            }
            MinusSign => {
                _ = tokens.next();
                let rhs = MultiplicativeExpr::parse(tokens)?;
                Ok(Self::Subtraction(lhs, rhs))
            }
            _ => Ok(Self::MultiplicativeExpr(lhs)),
        }
    }
}

impl AsmGen for AdditiveExpr {
    fn to_asm(&self) -> String {
        if let Self::MultiplicativeExpr(expr) = &self {
            expr.to_asm()
        } else {
            let (op, lhs, rhs) = match self {
                Self::Addition(lhs, rhs) => ("add", lhs, rhs),
                Self::Subtraction(lhs, rhs) => ("sub", lhs, rhs),
                Self::MultiplicativeExpr(_) => panic!("Unreachable"),
            };
            format!(
                "{}\
                push rax\n\
                {}\
                pop rbx\n\
                {} rax, rbx\n",
                rhs.to_asm(), lhs.to_asm(), op
            )
        }
    }
}
impl Display for AdditiveExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self {
            Self::Addition(lhs, rhs) => write!(f, "Addition(lhs={}, rhs={})", lhs, rhs),
            Self::Subtraction(lhs, rhs) => write!(f, "Subtraction(lhs={}, rhs={})", lhs, rhs),
            Self::MultiplicativeExpr(expr) => write!(f, "{}", expr),
        }
    }
}

type Expression = AdditiveExpr;

#[derive(Debug)]
pub enum Statement {
    Return(Expression),
}

impl Parse for Statement {
    // <statement> ::= "return" <expr> ";"
    fn parse<'a>(tokens: &mut CTokens<'a>) -> Result<Self, ParseError<'a>> {
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
            Self::Return(expr) => format!("{}ret\n", expr.to_asm()),
        }
    }
}

impl Display for Statement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Return(expr) => writeln!(f, "Return(\n    {}\n  )", expr),
        }
    }
}

#[derive(Debug)]
pub struct Function {
    pub name: String,
    pub body: Statement,
}

impl Parse for Function {
    // <function> ::= "int" <id> "(" ")" "{" <statement> "}"
    fn parse<'a>(tokens: &mut CTokens<'a>) -> Result<Self, ParseError<'a>> {
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
        format!(".global {0}\n{0}:\n{1}\n", self.name, self.body.to_asm())
    }
}

impl Display for Function {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let body = self.body.to_string();
        write!(
            f,
            "Function(return=Int, \n  name={}:\n  params=()\n  body={})",
            self.name,
            body,
        )
    }
}

#[derive(Debug)]
pub enum Program {
    Main(Function),
}

impl Parse for Program {
    // <program> ::= <function>
    fn parse<'a>(tokens: &mut CTokens<'a>) -> Result<Self, ParseError<'a>> {
        Ok(Program::Main(Function::parse(tokens)?))
    }
}

impl AsmGen for Program {
    fn to_asm(&self) -> String {
        match self {
            Self::Main(f) => format!(".intel_syntax noprefix\n{}", f.to_asm()),
        }
    }
}

impl Display for Program {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Main(func) => write!(f, "{}", func),
        }
    }
}
