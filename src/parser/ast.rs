use super::print::TreePrint;
use crate::lexer::*;
use std::fmt;
use std::mem::discriminant;
use std::{collections::VecDeque, hash::Hash};
use strum_macros::{Display, EnumIs};
use thiserror::Error;

#[derive(Debug, Error)]
pub enum ParserError {
    #[error("Syntax Error: Expected {0}, got {1}")]
    FailedExpect(TokenKind, TokenKind),
    #[error("Syntax Error: Unexpected token {0}")]
    UnexpectedToken(TokenKind),
    #[error("No tokens left in the token stream")]
    NoTokens,
    #[error("Malformed expression")]
    MalformedExpression,
    #[error("Could not parse identifier")]
    IdentifierParsingError,
    #[error("Trailing comma")]
    TrailingComma,
}

pub type ParserResult<T> = Result<T, ParserError>;

#[derive(Debug, PartialEq)]
#[allow(dead_code)]
pub struct Program {
    pub body: Vec<FunctionDeclaration>,
}

impl Program {
    pub fn parse(tokens: Vec<TokenKind>) -> ParserResult<Self> {
        let mut tokens = VecDeque::from(tokens);
        let mut body = vec![];

        while !tokens.is_empty() {
            let func = FunctionDeclaration::parse(&mut tokens);
            body.push(func?);
        }

        Ok(Program { body })
    }
}

impl fmt::Display for Program {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for function in &self.body {
            function.tree_print(f, "")?;
        }
        Ok(())
    }
}

#[derive(Debug, PartialEq, Clone)]
#[allow(dead_code)]
pub struct FunctionDeclaration {
    pub name: Identifier,
    pub params: Vec<Identifier>,
    pub return_type: String,
    pub body: Option<Block>,
}

impl FunctionDeclaration {
    pub fn parse(tokens: &mut VecDeque<TokenKind>) -> ParserResult<Self> {
        log_trace("parsing function from", tokens);
        let return_type = expect_token(TokenKind::Int, tokens)?;

        let name = Identifier::parse(expect_token(TokenKind::Identifier("".to_owned()), tokens)?)?;

        expect_token(TokenKind::ParenOpen, tokens)?;

        let mut params = vec![];

        // Parse parameters if any are present
        while !tokens.front().unwrap().is_paren_close() && !tokens.front().unwrap().is_void() {
            if tokens.front().unwrap().is_comma() {
                expect_token(TokenKind::Comma, tokens)?;
            }
            let mut param_result = || -> Result<Identifier, ParserError> {
                expect_token(TokenKind::Int, tokens)?;
                Identifier::parse(expect_token(TokenKind::Identifier("".to_owned()), tokens)?)
            };

            if let Ok(ident) = param_result() {
                params.push(ident);
            } else {
                return Err(ParserError::TrailingComma);
            }
        }

        if params.len() == 0 {
            expect_token(TokenKind::Void, tokens)?;
        }
        expect_token(TokenKind::ParenClose, tokens)?;

        let body = if tokens.front().unwrap().is_brace_open() {
            Some(Block::parse(tokens)?)
        } else {
            expect_token(TokenKind::Semicolon, tokens)?;
            None
        };

        let func = FunctionDeclaration {
            name,
            params,
            return_type: return_type.to_string(),
            body,
        };

        log::trace!("--- Parsed function declaration:\n{}", func);
        Ok(func)
    }
}

impl fmt::Display for FunctionDeclaration {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.tree_print(f, "")
    }
}

#[derive(Debug, PartialEq, Clone)]
#[allow(dead_code)]
pub struct Block {
    pub body: Vec<BlockItem>,
}

impl Block {
    fn parse(tokens: &mut VecDeque<TokenKind>) -> ParserResult<Self> {
        expect_token(TokenKind::BraceOpen, tokens)?;

        let mut body = vec![];
        while !tokens.front().unwrap().to_owned().is_brace_close() {
            body.push(BlockItem::parse(tokens)?);
        }

        expect_token(TokenKind::BraceClose, tokens)?;

        Ok(Block { body })
    }
}

impl fmt::Display for Block {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.tree_print(f, "")
    }
}

#[derive(Debug, PartialEq, Clone)]
#[allow(dead_code)]
pub enum BlockItem {
    Stmt(Statement),
    Decl(Declaration),
}

impl BlockItem {
    pub fn parse(tokens: &mut VecDeque<TokenKind>) -> ParserResult<Self> {
        log_trace("parsing block item from", tokens);

        let token = tokens.front().unwrap().to_owned();
        Ok(if token.is_int() {
            let decl = Declaration::parse(tokens)?;
            BlockItem::Decl(decl)
        } else {
            BlockItem::Stmt(Statement::parse(tokens)?)
        })
    }
}

impl fmt::Display for BlockItem {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.tree_print(f, "")
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Declaration {
    FunDecl(FunctionDeclaration),
    VarDecl(VariableDeclaration),
}

impl Declaration {
    fn parse(tokens: &mut VecDeque<TokenKind>) -> ParserResult<Self> {
        // If the 3rd token is a '(', we're looking at a function declaration
        if tokens.get(2).unwrap().is_paren_open() {
            Ok(Self::FunDecl(FunctionDeclaration::parse(tokens)?))
        } else {
            Ok(Self::VarDecl(VariableDeclaration::parse(tokens)?))
        }
    }
}

impl fmt::Display for Declaration {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.tree_print(f, "")
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct VariableDeclaration {
    pub name: Identifier,
    pub init: Option<Expression>,
}

impl VariableDeclaration {
    fn parse(tokens: &mut VecDeque<TokenKind>) -> ParserResult<Self> {
        log_trace("Parsing declaration from", tokens);
        // TODO: actually use the type
        let _ty = expect_token(TokenKind::Int, tokens)?;
        let ident = Identifier::parse(expect_token(TokenKind::Identifier("".to_owned()), tokens)?)?;

        let init = if tokens.front().unwrap().to_owned().is_semicolon() {
            None
        } else {
            expect_token(TokenKind::Assignment, tokens)?;
            Some(Expression::parse(tokens, 0)?)
        };
        expect_token(TokenKind::Semicolon, tokens)?;

        let result = Self { name: ident, init };
        log::trace!("-- Parsed declaration:\n{}", result);
        Ok(result)
    }
}

impl fmt::Display for VariableDeclaration {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.tree_print(f, "")
    }
}

#[derive(Debug, PartialEq, Clone)]
#[allow(dead_code)]
pub enum Statement {
    Return(Expression),
    Exp(Expression),
    If(Expression, Box<Statement>, Option<Box<Statement>>),
    Compound(Block),
    Break(Option<Identifier>),
    Continue(Option<Identifier>),
    While(Expression, Box<Statement>, Option<Identifier>),
    DoWhile(Box<Statement>, Expression, Option<Identifier>),
    For(
        ForInit,
        Option<Expression>,
        Option<Expression>,
        Box<Statement>,
        Option<Identifier>,
    ),
    Null,
}

impl Statement {
    pub fn parse(tokens: &mut VecDeque<TokenKind>) -> ParserResult<Self> {
        log_trace("Trying statement from", tokens);

        let token = tokens.front().unwrap().to_owned();

        if token.is_semicolon() {
            expect_token(TokenKind::Semicolon, tokens)?;
            return Ok(Statement::Null);
        }

        let result = match token {
            TokenKind::Return => {
                expect_token(TokenKind::Return, tokens)?;
                let expr = Expression::parse(tokens, 0)?;
                expect_token(TokenKind::Semicolon, tokens)?;
                Self::Return(expr)
            }
            TokenKind::If => {
                expect_token(TokenKind::If, tokens)?;
                expect_token(TokenKind::ParenOpen, tokens)?;
                let cond = Expression::parse(tokens, 0)?;
                expect_token(TokenKind::ParenClose, tokens)?;
                let then_stmt = Box::new(Statement::parse(tokens)?);
                let else_stmt = if tokens.front().unwrap().is_else() {
                    expect_token(TokenKind::Else, tokens)?;
                    Some(Box::new(Statement::parse(tokens)?))
                } else {
                    None
                };
                Self::If(cond, then_stmt, else_stmt)
            }
            TokenKind::BraceOpen => {
                let block = Block::parse(tokens)?;
                Self::Compound(block)
            }
            TokenKind::While => {
                expect_token(TokenKind::While, tokens)?;
                expect_token(TokenKind::ParenOpen, tokens)?;
                let cond = Expression::parse(tokens, 0)?;
                expect_token(TokenKind::ParenClose, tokens)?;
                let body = Statement::parse(tokens)?;
                Self::While(cond, Box::new(body), None)
            }
            TokenKind::Do => {
                expect_token(TokenKind::Do, tokens)?;
                let body = Statement::parse(tokens)?;
                expect_token(TokenKind::While, tokens)?;
                expect_token(TokenKind::ParenOpen, tokens)?;
                let cond = Expression::parse(tokens, 0)?;
                expect_token(TokenKind::ParenClose, tokens)?;
                expect_token(TokenKind::Semicolon, tokens)?;
                Self::DoWhile(Box::new(body), cond, None)
            }
            TokenKind::For => {
                expect_token(TokenKind::For, tokens)?;
                expect_token(TokenKind::ParenOpen, tokens)?;
                let init = ForInit::parse(tokens)?;
                let cond = Expression::parse_optional(tokens)?;
                expect_token(TokenKind::Semicolon, tokens)?;
                let post = Expression::parse_optional(tokens)?;
                expect_token(TokenKind::ParenClose, tokens)?;
                let body = Statement::parse(tokens)?;
                Self::For(init, cond, post, Box::new(body), None)
            }
            TokenKind::Break => {
                expect_token(TokenKind::Break, tokens)?;
                expect_token(TokenKind::Semicolon, tokens)?;
                Self::Break(None)
            }
            TokenKind::Continue => {
                expect_token(TokenKind::Continue, tokens)?;
                expect_token(TokenKind::Semicolon, tokens)?;
                Self::Continue(None)
            }
            _ => {
                let exp = Self::Exp(Expression::parse(tokens, 0)?);
                expect_token(TokenKind::Semicolon, tokens)?;
                exp
            }
        };

        log::trace!("-- Parsed statement:\n{}", result);
        Ok(result)
    }
}

impl fmt::Display for Statement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.tree_print(f, "")
    }
}

#[derive(Debug, PartialEq, Clone)]
#[allow(dead_code)]
pub enum ForInit {
    InitDecl(VariableDeclaration),
    InitExp(Expression),
    InitNull,
}

impl ForInit {
    fn parse(tokens: &mut VecDeque<TokenKind>) -> ParserResult<Self> {
        let result = if let Ok(decl) = VariableDeclaration::parse(tokens) {
            Self::InitDecl(decl)
        } else if let Ok(exp) = Expression::parse(tokens, 0) {
            expect_token(TokenKind::Semicolon, tokens)?;
            Self::InitExp(exp)
        } else {
            expect_token(TokenKind::Semicolon, tokens)?;
            Self::InitNull
        };
        Ok(result)
    }
}

impl fmt::Display for ForInit {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.tree_print(f, "")
    }
}

#[derive(Debug, PartialEq, Clone)]
#[allow(dead_code)]
pub enum Expression {
    Constant(i64),
    Var(Identifier),
    Unary(UnaryOperator, Box<Expression>),
    Binary(BinaryOperator, Box<Expression>, Box<Expression>),
    Assignment(Box<Expression>, Box<Expression>),
    Conditional(Box<Expression>, Box<Expression>, Box<Expression>),
    FunctionCall(Identifier, Vec<Expression>),
}

impl Expression {
    pub fn parse(tokens: &mut VecDeque<TokenKind>, min_precedence: u32) -> ParserResult<Self> {
        log_trace("Trying expr from", tokens);

        if tokens.len() == 0 {
            return Err(ParserError::NoTokens);
        }
        let mut left = Expression::parse_factor(tokens)?;
        let mut token = tokens.front().unwrap().to_owned();

        while token.is_binary_operator() && token.precedence() >= min_precedence {
            if token.is_assignment() {
                expect_token(TokenKind::Assignment, tokens)?;
                let right = Expression::parse(tokens, token.precedence())?;
                left = Expression::Assignment(Box::new(left), Box::new(right));
            } else if token.is_question() {
                let middle = {
                    expect_token(TokenKind::Question, tokens)?;
                    let cond_middle = Expression::parse(tokens, 0)?;
                    expect_token(TokenKind::Colon, tokens)?;
                    cond_middle
                };
                let right = Expression::parse(tokens, token.precedence())?;
                left = Expression::Conditional(Box::new(left), Box::new(middle), Box::new(right));
            } else {
                let operator = BinaryOperator::parse(tokens)?;
                let right = Expression::parse(tokens, token.precedence() + 1)?;
                left = Expression::Binary(operator, Box::new(left), Box::new(right));
            }
            token = tokens.front().unwrap().to_owned();
        }
        log::trace!("-- Parsed expression:\n{}", left);
        Ok(left)
    }

    pub fn parse_factor(tokens: &mut VecDeque<TokenKind>) -> ParserResult<Self> {
        log_trace("Trying factor from", tokens);

        if tokens.len() == 0 {
            return Err(ParserError::NoTokens);
        }
        let token = tokens.front().unwrap().to_owned();

        if token.is_identifier() {
            // If we have a '(' after an identifier, it's a function call
            return Ok(if tokens.get(1).unwrap().is_paren_open() {
                let name = Identifier::parse(tokens.pop_front().unwrap())?;
                expect_token(TokenKind::ParenOpen, tokens)?;
                let mut args = vec![];

                // Parse arguments if any are present
                while !tokens.front().unwrap().is_paren_close() {
                    if tokens.front().unwrap().is_comma() {
                        expect_token(TokenKind::Comma, tokens)?;
                    }
                    if let Ok(exp) = Expression::parse(tokens, 0) {
                        args.push(exp);
                    } else {
                        return Err(ParserError::TrailingComma);
                    }
                }
                expect_token(TokenKind::ParenClose, tokens)?;

                Self::FunctionCall(name, args)
            } else {
                Self::Var(Identifier::parse(tokens.pop_front().unwrap())?)
            });
        }

        if token.is_constant() {
            if let TokenKind::Constant(val) = expect_token(TokenKind::Constant(0), tokens)? {
                return Ok(Self::Constant(val));
            }
        }

        if token.is_unary_operator() {
            let operator = UnaryOperator::parse(tokens)?;
            let inner = Expression::parse_factor(tokens)?;
            return Ok(Self::Unary(operator, Box::new(inner)));
        }

        if token.is_paren_open() {
            expect_token(TokenKind::ParenOpen, tokens)?;
            let inner = Expression::parse(tokens, 0)?;
            expect_token(TokenKind::ParenClose, tokens)?;
            return Ok(inner);
        }

        log_trace("Could not parse any factor from", tokens);
        Err(ParserError::MalformedExpression)
    }

    pub fn parse_optional(tokens: &mut VecDeque<TokenKind>) -> ParserResult<Option<Expression>> {
        let expr = Expression::parse(tokens, 0);
        if let Ok(expr) = expr {
            Ok(Some(expr))
        } else {
            Ok(None)
        }
    }
}

impl fmt::Display for Expression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.tree_print(f, "")
    }
}

#[derive(Debug, PartialEq, Clone, EnumIs, Display)]
pub enum BinaryOperator {
    Add,
    Subtract,
    Multiply,
    Divide,
    Remainder,
    And,
    Or,
    Equal,
    NotEqual,
    LessEqualThan,
    GreaterEqualThan,
    LessThan,
    GreaterThan,
}

impl BinaryOperator {
    fn parse(tokens: &mut VecDeque<TokenKind>) -> ParserResult<Self> {
        let token = tokens.pop_front().unwrap();

        match token {
            TokenKind::Plus => Ok(Self::Add),
            TokenKind::Minus => Ok(Self::Subtract),
            TokenKind::Asterisk => Ok(Self::Multiply),
            TokenKind::Slash => Ok(Self::Divide),
            TokenKind::Percent => Ok(Self::Remainder),
            TokenKind::And => Ok(Self::And),
            TokenKind::Or => Ok(Self::Or),
            TokenKind::Equal => Ok(Self::Equal),
            TokenKind::NotEqual => Ok(Self::NotEqual),
            TokenKind::LessEqualThan => Ok(Self::LessEqualThan),
            TokenKind::GreaterEqualThan => Ok(Self::GreaterEqualThan),
            TokenKind::LessThan => Ok(Self::LessThan),
            TokenKind::GreaterThan => Ok(Self::GreaterThan),
            _ => Err(ParserError::UnexpectedToken(token)),
        }
    }

    pub fn is_short_circuit(&self) -> bool {
        match &self {
            &BinaryOperator::And | &BinaryOperator::Or => true,
            _ => false,
        }
    }
}

#[derive(Debug, PartialEq, Clone, Display)]
#[allow(dead_code)]
pub enum UnaryOperator {
    Complement,
    Negation,
    Not,
}

impl UnaryOperator {
    pub fn parse(tokens: &mut VecDeque<TokenKind>) -> ParserResult<Self> {
        let token = tokens.pop_front().unwrap();

        match token {
            TokenKind::Complement => Ok(Self::Complement),
            TokenKind::Minus => Ok(Self::Negation),
            TokenKind::Not => Ok(Self::Not),
            _ => Err(ParserError::UnexpectedToken(token)),
        }
    }
}

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
#[allow(dead_code)]
pub struct Identifier {
    pub name: String,
}

impl Identifier {
    pub fn parse(token: TokenKind) -> ParserResult<Self> {
        if let TokenKind::Identifier(name) = token {
            Ok(Self { name })
        } else {
            Err(ParserError::IdentifierParsingError)
        }
    }

    pub fn new(name: &str) -> Self {
        Self {
            name: name.to_owned(),
        }
    }
}

impl fmt::Display for Identifier {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.name)
    }
}

#[inline(always)]
fn log_trace(msg: &str, tokens: &mut VecDeque<TokenKind>) {
    log::trace!(
        "{} {:?}",
        msg,
        tokens.iter().take(4).collect::<Vec<&TokenKind>>()
    );
}

#[inline(always)]
fn expect_token(expected: TokenKind, tokens: &mut VecDeque<TokenKind>) -> ParserResult<TokenKind> {
    let exp = discriminant(&expected);
    let actual = discriminant(&tokens[0]);

    if actual != exp {
        Err(ParserError::FailedExpect(expected, tokens[0].clone()))
    } else {
        Ok(tokens.pop_front().unwrap())
    }
}
