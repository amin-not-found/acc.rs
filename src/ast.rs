use std::{fmt::Display, mem::discriminant, sync::Mutex};

use crate::tokenizer::{CToken, Token, Tokens};

type CTokens<'a> = Tokens<'a, CToken>;

fn next_token<'a>(tokens: &mut CTokens<'a>) -> Result<Token<'a, CToken>, ParseError<'a>> {
    let t = tokens.next();
    match t {
        Some(token) => Ok(token),
        None => Err(ParseError::new("Unexpected end of tokens".into(), tokens)),
    }
}

fn peek_token<'a>(tokens: &mut CTokens<'a>) -> Result<Token<'a, CToken>, ParseError<'a>> {
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

// simple auto incremental label generation in form of "L%d"
fn gen_label() -> String {
    const LABEL_PREFIX: char = 'L';
    static COUNTER: Mutex<u32> = Mutex::new(0);
    let mut counter = COUNTER.lock().unwrap();
    *counter += 1;
    format!("{}{}", LABEL_PREFIX, *counter)
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

pub enum PrimaryExpr {
    Constant(i32),
    UnaryOp(CToken, Box<PrimaryExpr>),
    Expression(Box<BinaryOperand>), // e.g. in case of parentheses
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

pub struct BinaryOp {
    lhs: BinaryOperand,
    rhs: BinaryOperand,
    op: CToken,
}

impl BinaryOp {
    fn new(lhs: BinaryOperand, rhs: BinaryOperand, op: CToken) -> Self {
        Self { lhs, rhs, op }
    }
}

impl AsmGen for BinaryOp {
    fn to_asm(&self) -> String {
        // first operand will be in rax
        // second operand will be in rbx
        // then op will be executed
        let op: String = match self.op {
            CToken::MultiplicationSign => "imul rbx".to_owned(),
            CToken::DivisionSign => String::from(
                "cqo\n\
                 idiv rbx\n",
            ),
            CToken::PlusSign => String::from("add rax, rbx"),
            CToken::MinusSign => "sub rax, rbx".to_owned(),
            CToken::LessThan => String::from(
                "cmp rax, rbx\n\
                 setl al\n\
                 movzx eax, al\n",
            ),
            CToken::GreaterThan => String::from(
                "cmp rax, rbx\n\
                 setg al\n\
                 movzx eax, al\n",
            ),
            CToken::LessEqual => String::from(
                "cmp rax, rbx\n\
                 setle al\n\
                 movzx eax, al\n",
            ),
            CToken::GreaterEqual => String::from(
                "cmp rax, rbx\n\
                 setge al\n\
                 movzx eax, al\n",
            ),

            CToken::Equal => String::from(
                "cmp rax, rbx\n\
                 sete al\n\
                 movzx eax, al\n",
            ),
            CToken::NotEqual => String::from(
                "cmp rax, rbx\n\
                 setne al\n\
                 movzx eax, al\n",
            ),
            // We're gonna handle OR and AND differently
            // as they need to support short circuiting
            _ => String::new(),
        };
        if !op.is_empty() {
            format!(
                "{}\
                push rax\n\
                {}\
                pop rbx\n\
                {}\n",
                self.rhs.to_asm(),
                self.lhs.to_asm(),
                op
            )
        } else {
            match self.op {
                // Here we only handle OR and AND
                CToken::LogicalOr => {
                    // TEST sets ZF to the result of AND operation
                    // bitwise AND is zero when both operands are zero
                    // in this we have not equal if eax is not zero
                    format!(
                        "{1}\
                         test rax, rax\n\
                         mov rax, 1\n\
                         jne .{0}\n\
                         {2}\
                         test rax, rax\n\
                         mov rax, 0\n\
                         setne al\n\
                         .{0}:\n",
                        gen_label(),
                        self.lhs.to_asm(),
                        self.rhs.to_asm()
                    )
                }
                CToken::LogicalAnd => {
                    // opposite of what we did for OR
                    format!(
                        "{1}\
                         test rax, rax\n\
                         mov rax, 0\n\
                         je .{0}\n\
                         {2}\
                         test rax, rax\n\
                         mov rax, 0\n\
                         setne al\n\
                         .{0}:\n",
                        gen_label(),
                        self.lhs.to_asm(),
                        self.rhs.to_asm()
                    )
                }
                _ => panic!("Can't produce assembly from operation with {:?}", self.op),
            }
        }
    }
}

pub enum BinaryOperand {
    BinaryOp(Box<BinaryOp>),
    Operand(PrimaryExpr),
}

impl BinaryOperand {
    fn from_operands(lhs: BinaryOperand, rhs: BinaryOperand, op: CToken) -> Self {
        let op = BinaryOp::new(rhs, lhs, op);
        BinaryOperand::BinaryOp(op.into())
    }
    fn parse_selective_binop<'a>(
        tokens: &mut CTokens<'a>,
        precedences: &Vec<Vec<CToken>>,
        precedence_idx: usize,
    ) -> Result<BinaryOperand, ParseError<'a>> {
        let mut lhs;

        if precedence_idx >= precedences.len() {
            return Ok(BinaryOperand::Operand(PrimaryExpr::parse(tokens)?));
        } else {
            lhs = Self::parse_selective_binop(tokens, precedences, precedence_idx + 1)?;
        }

        loop {
            let token = peek_token(tokens)?;
            if precedences[precedence_idx].contains(&token.kind) {
                _ = tokens.next();
                let rhs = Self::parse_selective_binop(tokens, precedences, precedence_idx + 1)?;
                lhs = BinaryOperand::from_operands(rhs, lhs, token.kind);
            } else {
                return Ok(lhs);
            }
        }
    }
}

impl Parse for BinaryOperand {
    fn parse<'a>(tokens: &mut CTokens<'a>) -> Result<Self, ParseError<'a>> {
        use CToken::*;

        let precedence_ops = vec![
            vec![LogicalOr],
            vec![LogicalAnd],
            vec![Equal, NotEqual],
            vec![LessThan, GreaterThan, LessEqual, GreaterEqual],
            vec![PlusSign, MinusSign],
            vec![MultiplicationSign, DivisionSign],
        ];

        Self::parse_selective_binop(tokens, &precedence_ops, 0)
    }
}

impl AsmGen for BinaryOperand {
    fn to_asm(&self) -> String {
        match self {
            BinaryOperand::BinaryOp(op) => (**op).to_asm(),
            BinaryOperand::Operand(op) => op.to_asm(),
        }
    }
}

impl Display for BinaryOperand {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            BinaryOperand::BinaryOp(boxed_op) => {
                let op = boxed_op;
                writeln!(
                    f,
                    "BinaryOp(op={:?}, lhs={}, rhs={})",
                    op.op, op.lhs, op.rhs
                )
            }
            BinaryOperand::Operand(op) => write!(f, "{}", op),
        }
    }
}

type Expression = BinaryOperand;

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
            self.name, body,
        )
    }
}

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
