use crate::lexer::*;
use display_tree::{format_tree, DisplayTree, StyleBuilder};
use std::collections::VecDeque;
use std::mem::discriminant;
use std::{error::Error, fmt};
use strum_macros::{Display, EnumIs};

#[inline(always)]
fn log_trace(msg: &str, tokens: &mut VecDeque<TokenKind>) {
    log::trace!(
        "{} {:?}",
        msg,
        tokens.iter().take(4).collect::<Vec<&TokenKind>>()
    );
}

#[derive(Debug)]
pub enum ParserError {
    UnexpectedToken,
    NoTokens,
    MalformedExpression,
    IdentifierParsingError,
}

impl fmt::Display for ParserError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?}", self)
    }
}

impl Error for ParserError {}

#[inline(always)]
fn expect_token_silent(
    expected: TokenKind,
    tokens: &mut VecDeque<TokenKind>,
) -> Result<TokenKind, ParserError> {
    let exp = discriminant(&expected);
    let actual = discriminant(&tokens[0]);

    if actual != exp {
        Err(ParserError::UnexpectedToken)
    } else {
        Ok(tokens.pop_front().unwrap())
    }
}

#[inline(always)]
fn expect_token(
    expected: TokenKind,
    tokens: &mut VecDeque<TokenKind>,
) -> Result<TokenKind, ParserError> {
    let result = expect_token_silent(expected.clone(), tokens);
    if let Err(_) = result {
        log::error!(
            "Syntax Error: Expected {:?}, got {:?}",
            &expected,
            &tokens[0]
        );
    }
    result
}

#[derive(Debug, PartialEq, DisplayTree)]
#[allow(dead_code)]
pub struct Program {
    #[tree]
    pub body: Function,
}

impl Program {
    pub fn parse(tokens: Vec<TokenKind>) -> Program {
        let mut tokens = VecDeque::from(tokens);

        // TODO: better error handling here
        let program = Program {
            body: Function::parse(&mut tokens).unwrap(),
        };

        if !tokens.is_empty() {
            panic!("Syntax Error: Unexpected token {:?}", tokens[0]);
        }

        program
    }
}

#[derive(Debug, PartialEq, Clone, DisplayTree)]
#[allow(dead_code)]
pub struct Function {
    pub name: Identifier,
    pub return_type: String,
    #[tree]
    pub body: Block,
}

impl Function {
    fn parse(tokens: &mut VecDeque<TokenKind>) -> Result<Function, ParserError> {
        let return_type = expect_token(TokenKind::Int, tokens)?;

        let name = Identifier::parse(expect_token(TokenKind::Identifier("".to_owned()), tokens)?)?;

        expect_token(TokenKind::ParenOpen, tokens)?;
        expect_token(TokenKind::Void, tokens)?;
        expect_token(TokenKind::ParenClose, tokens)?;

        let body = Block::parse(tokens)?;

        Ok(Function {
            name,
            return_type: return_type.to_string(),
            body,
        })
    }
}

#[derive(Debug, PartialEq, Clone)]
#[allow(dead_code)]
pub struct Block {
    pub body: Vec<BlockItem>,
}

impl Block {
    fn parse(tokens: &mut VecDeque<TokenKind>) -> Result<Self, ParserError> {
        expect_token(TokenKind::BraceOpen, tokens)?;

        let mut body = vec![];
        while !tokens.front().unwrap().to_owned().is_brace_close() {
            body.push(BlockItem::parse(tokens)?);
        }

        expect_token(TokenKind::BraceClose, tokens)?;

        Ok(Block { body })
    }
}

impl DisplayTree for Block {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>, style: display_tree::Style) -> std::fmt::Result {
        for basic_block in &self.body {
            writeln!(
                f,
                "{}{} {}",
                style.char_set.connector,
                std::iter::repeat(style.char_set.horizontal)
                    .take(style.indentation as usize)
                    .collect::<String>(),
                display_tree::format_tree!(*basic_block)
            )?;
        }
        Ok(())
    }
}

impl fmt::Display for Block {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for basic_block in &self.body {
            writeln!(f, "{}", basic_block)?;
        }
        Ok(())
    }
}

#[derive(Debug, PartialEq, Clone, DisplayTree)]
#[allow(dead_code)]
pub enum BlockItem {
    Stmt(#[tree] Statement),
    Decl(#[tree] Declaration),
}

impl BlockItem {
    fn parse(tokens: &mut VecDeque<TokenKind>) -> Result<Self, ParserError> {
        log_trace("parsing block item from", tokens);

        let token = tokens.front().unwrap().to_owned();
        Ok(if token.is_int() {
            let decl = Declaration::parse(tokens)?;
            expect_token(TokenKind::Semicolon, tokens)?;
            BlockItem::Decl(decl)
        } else {
            BlockItem::Stmt(Statement::parse(tokens)?)
        })
    }
}

impl fmt::Display for BlockItem {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            BlockItem::Stmt(stmt) => write!(f, "{}", stmt),
            BlockItem::Decl(decl) => write!(f, "{}", decl),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
#[allow(dead_code)]
pub struct Declaration {
    pub name: Identifier,
    pub init: Option<Expression>,
}

impl Declaration {
    fn parse(tokens: &mut VecDeque<TokenKind>) -> Result<Self, ParserError> {
        log_trace("Parsing declaration from", tokens);
        // Silent expect here because we can use this failing to check
        // whether we're parsing a declaration or something else
        // so we don't want to log an error.
        // TODO: actually use the type
        let _ty = expect_token_silent(TokenKind::Int, tokens)?;
        let ident = Identifier::parse(expect_token(TokenKind::Identifier("".to_owned()), tokens)?)?;

        let init = if tokens.front().unwrap().to_owned().is_semicolon() {
            None
        } else {
            expect_token(TokenKind::Assignment, tokens)?;
            Some(Expression::parse(tokens, 0)?)
        };

        let result = Self { name: ident, init };
        log::trace!("-- Parsed declaration: {}", result);
        Ok(result)
    }
}

impl fmt::Display for Declaration {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "int {}", self.name)?;
        if let Some(init) = &self.init {
            write!(f, " = {}", init)?;
        } else {
            write!(f, ";")?;
        }
        Ok(())
    }
}

impl DisplayTree for Declaration {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>, style: display_tree::Style) -> std::fmt::Result {
        writeln!(f, "{}", self.name)?;
        if let Some(init) = &self.init {
            writeln!(
                f,
                "{}{} {}",
                style.char_set.connector,
                std::iter::repeat(style.char_set.horizontal)
                    .take(style.indentation as usize)
                    .collect::<String>(),
                display_tree::format_tree!(*init)
            )?;
        }
        Ok(())
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

impl DisplayTree for Statement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>, style: display_tree::Style) -> std::fmt::Result {
        match self {
            Statement::Return(val) => writeln!(
                f,
                "{}{} Return {}",
                style.char_set.connector,
                std::iter::repeat(style.char_set.horizontal)
                    .take(style.indentation as usize)
                    .collect::<String>(),
                display_tree::format_tree!(*val)
            ),
            Statement::While(cond, body, label) => {
                writeln!(
                    f,
                    "{}{} while [{}]",
                    style.char_set.connector,
                    std::iter::repeat(style.char_set.horizontal)
                        .take(style.indentation as usize)
                        .collect::<String>(),
                    option_ident_to_string(label)
                )?;
                writeln!(
                    f,
                    "{}{} {}",
                    style.char_set.connector,
                    std::iter::repeat(style.char_set.horizontal)
                        .take(style.indentation as usize + 4)
                        .collect::<String>(),
                    format_tree!(*cond, style.indentation(style.indentation + 4)),
                )?;
                display_tree::writeln_tree!(f, **body)
            }
            Statement::DoWhile(body, cond, label) => {
                writeln!(
                    f,
                    "{}{} do-while [{}]",
                    style.char_set.connector,
                    std::iter::repeat(style.char_set.horizontal)
                        .take(style.indentation as usize)
                        .collect::<String>(),
                    option_ident_to_string(label)
                )?;
                writeln!(
                    f,
                    "{}{} {}",
                    style.char_set.connector,
                    std::iter::repeat(style.char_set.horizontal)
                        .take(style.indentation as usize + 4)
                        .collect::<String>(),
                    format_tree!(*cond, style.indentation(style.indentation + 4)),
                )?;
                display_tree::writeln_tree!(f, **body)
            }
            Statement::If(cond, then_stmt, else_stmt) => {
                writeln!(
                    f,
                    "{}{} if",
                    style.char_set.connector,
                    std::iter::repeat(style.char_set.horizontal)
                        .take(style.indentation as usize)
                        .collect::<String>(),
                )?;
                writeln!(
                    f,
                    "{}{} {}",
                    style.char_set.connector,
                    std::iter::repeat(style.char_set.horizontal)
                        .take(style.indentation as usize + 4)
                        .collect::<String>(),
                    format_tree!(*cond, style.indentation(style.indentation + 4)),
                )?;
                display_tree::writeln_tree!(f, **then_stmt)?;
                if let Some(else_stmt) = else_stmt {
                    display_tree::writeln_tree!(f, **else_stmt)?;
                }
                Ok(())
            }
            Statement::Break(label) => writeln!(
                f,
                "{}{} break [{}]",
                style.char_set.connector,
                std::iter::repeat(style.char_set.horizontal)
                    .take(style.indentation as usize)
                    .collect::<String>(),
                option_ident_to_string(label)
            ),
            Statement::Continue(label) => writeln!(
                f,
                "{}{} continue [{}]",
                style.char_set.connector,
                std::iter::repeat(style.char_set.horizontal)
                    .take(style.indentation as usize)
                    .collect::<String>(),
                option_ident_to_string(label)
            ),
            Statement::For(init, cond, post, body, label) => {
                writeln!(
                    f,
                    "{}{} for [{}]",
                    style.char_set.connector,
                    std::iter::repeat(style.char_set.horizontal)
                        .take(style.indentation as usize)
                        .collect::<String>(),
                    option_ident_to_string(label)
                )?;
                display_tree::writeln_tree!(f, *init)?;
                if let Some(cond) = cond {
                    display_tree::writeln_tree!(f, *cond)?;
                }
                if let Some(post) = post {
                    display_tree::writeln_tree!(f, *post)?;
                }
                display_tree::writeln_tree!(f, **body)?;
                Ok(())
            }
            Statement::Compound(block) => display_tree::writeln_tree!(f, *block),
            Statement::Exp(expr) => display_tree::writeln_tree!(f, *expr),
            Statement::Null => writeln!(f, "Null"),
        }?;
        Ok(())
    }
}

impl fmt::Display for Statement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Statement::Return(exp) => write!(f, "return {}", exp),
            Statement::Exp(exp) => write!(f, "{};", exp),
            Statement::If(cond, then_stmt, else_stmt) => {
                writeln!(f, "\nif ({})", cond)?;
                writeln!(f, "\t{}", then_stmt)?;
                if let Some(else_stmt) = else_stmt {
                    writeln!(f, "else")?;
                    writeln!(f, "\t{}", else_stmt)?;
                }
                Ok(())
            }
            Statement::Compound(block) => {
                write!(f, "\n{}", block)
            }
            Statement::While(cond, body, label) => {
                writeln!(f, "\nwhile ({}) [{}]", cond, option_ident_to_string(label))?;
                write!(f, "\t{}", body)
            }
            Statement::DoWhile(body, cond, label) => {
                writeln!(f, "\ndo")?;
                writeln!(f, "\t{}", body)?;
                writeln!(f, "while ({}) [{}]", cond, option_ident_to_string(label))?;
                Ok(())
            }
            Statement::For(init, cond, post, body, label) => {
                let cond = if let Some(cond) = cond {
                    cond.to_string()
                } else {
                    " ".to_string()
                };

                let post = if let Some(post) = post {
                    post.to_string()
                } else {
                    " ".to_string()
                };

                writeln!(
                    f,
                    "\nfor ({} ; {} ; {}) [{}]",
                    init,
                    cond,
                    post,
                    option_ident_to_string(label)
                )?;
                write!(f, "\t{}", body)
            } // _ => write!(f, "{:?}", self),
            Statement::Break(label) => writeln!(f, "break [{}]", option_ident_to_string(label)),
            Statement::Continue(label) => {
                writeln!(f, "continue [{}]", option_ident_to_string(label))
            }
            Statement::Null => write!(f, "NULL"),
        }
    }
}

impl Statement {
    fn parse(tokens: &mut VecDeque<TokenKind>) -> Result<Statement, ParserError> {
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
                expect_token(TokenKind::Semicolon, tokens)?;
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

        log::trace!("-- Parsed statement: {}", result);
        Ok(result)
    }
}

#[derive(Debug, PartialEq, Clone, DisplayTree)]
#[allow(dead_code)]
pub enum ForInit {
    InitDecl(#[tree] Declaration),
    InitExp(#[tree] Expression),
    InitNull,
}

impl ForInit {
    fn parse(tokens: &mut VecDeque<TokenKind>) -> Result<Self, ParserError> {
        Ok(if let Ok(decl) = Declaration::parse(tokens) {
            Self::InitDecl(decl)
        } else if let Ok(exp) = Expression::parse(tokens, 0) {
            Self::InitExp(exp)
        } else {
            Self::InitNull
        })
    }
}

impl fmt::Display for ForInit {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::InitDecl(decl) => write!(f, "{}", decl),
            Self::InitExp(expr) => write!(f, "{}", expr),
            Self::InitNull => write!(f, " "),
        }
    }
}

#[derive(Debug, PartialEq, Clone, DisplayTree)]
#[allow(dead_code)]
pub enum Expression {
    Constant(i64),
    Var(Identifier),
    Unary(#[node_label] UnaryOperator, #[tree] Box<Expression>),
    Binary(
        #[node_label] BinaryOperator,
        #[tree] Box<Expression>,
        #[tree] Box<Expression>,
    ),
    Assignment(#[tree] Box<Expression>, #[tree] Box<Expression>),
    Conditional(
        #[tree] Box<Expression>,
        #[tree] Box<Expression>,
        #[tree] Box<Expression>,
    ),
}

impl Expression {
    fn parse(
        tokens: &mut VecDeque<TokenKind>,
        min_precedence: u32,
    ) -> Result<Expression, ParserError> {
        log_trace("Trying expr from", tokens);

        if tokens.len() == 0 {
            log::error!("No tokens passed to the Expression parser");
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
        log::trace!("-- Parsed expression: {}", left);
        Ok(left)
    }

    fn parse_factor(tokens: &mut VecDeque<TokenKind>) -> Result<Expression, ParserError> {
        log_trace("Trying factor from", tokens);

        if tokens.len() == 0 {
            log::error!("No tokens passed to the Expression parser");
            return Err(ParserError::NoTokens);
        }
        let token = tokens.front().unwrap().to_owned();

        if token.is_identifier() {
            return Ok(Self::Var(Identifier::parse(tokens.pop_front().unwrap())?));
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

    fn parse_optional(tokens: &mut VecDeque<TokenKind>) -> Result<Option<Expression>, ParserError> {
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
        match self {
            Expression::Binary(op, left, right) => write!(f, "{} {} {}", left, op, right),
            Expression::Unary(op, expr) => write!(f, "{}{}", op, expr),
            Expression::Assignment(left, right) => write!(f, "{} = {}", left, right),
            Expression::Conditional(cond, a, b) => write!(f, "{} ? {} : {}", cond, a, b),
            Expression::Var(ident) => write!(f, "Var({})", ident.name),
            Expression::Constant(val) => write!(f, "Constant({})", val),
        }
    }
}

#[derive(Debug, PartialEq, Clone, DisplayTree, EnumIs)]
#[allow(dead_code)]
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
    fn parse(tokens: &mut VecDeque<TokenKind>) -> Result<Self, ParserError> {
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
            _ => Err(ParserError::UnexpectedToken),
        }
    }

    pub fn is_short_circuit(&self) -> bool {
        match &self {
            &BinaryOperator::And | &BinaryOperator::Or => true,
            _ => false,
        }
    }
}

impl fmt::Display for BinaryOperator {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                BinaryOperator::Add => "+",
                BinaryOperator::Subtract => "-",
                BinaryOperator::Multiply => "*",
                BinaryOperator::Divide => "/",
                BinaryOperator::Remainder => "%",
                BinaryOperator::And => "&&",
                BinaryOperator::Or => "||",
                BinaryOperator::Equal => "==",
                BinaryOperator::NotEqual => "!=",
                BinaryOperator::LessEqualThan => "<=",
                BinaryOperator::GreaterEqualThan => ">=",
                BinaryOperator::LessThan => "<",
                BinaryOperator::GreaterThan => ">",
            }
        )
    }
}

#[derive(Debug, PartialEq, Clone, DisplayTree, Display)]
#[allow(dead_code)]
pub enum UnaryOperator {
    Complement,
    Negation,
    Not,
}

impl UnaryOperator {
    fn parse(tokens: &mut VecDeque<TokenKind>) -> Result<Self, ParserError> {
        let token = tokens.pop_front().unwrap();

        match token {
            TokenKind::Complement => Ok(Self::Complement),
            TokenKind::Minus => Ok(Self::Negation),
            TokenKind::Not => Ok(Self::Not),
            _ => Err(ParserError::UnexpectedToken),
        }
    }
}

#[derive(Debug, PartialEq, Clone, DisplayTree)]
#[allow(dead_code)]
pub struct Identifier {
    #[node_label]
    pub name: String,
}

impl Identifier {
    pub fn parse(token: TokenKind) -> Result<Self, ParserError> {
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

fn option_ident_to_string(ident: &Option<Identifier>) -> String {
    if let Some(ident) = ident {
        ident.to_string()
    } else {
        "None".to_string()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_program() {
        let tokens = vec![
            TokenKind::Int,
            TokenKind::Identifier("main".to_owned()),
            TokenKind::ParenOpen,
            TokenKind::Void,
            TokenKind::ParenClose,
            TokenKind::BraceOpen,
            TokenKind::Return,
            TokenKind::Constant(7),
            TokenKind::Semicolon,
            TokenKind::BraceClose,
        ];

        let function_expected = Function {
            name: Identifier::new("main"),
            return_type: "Int".to_owned(),
            body: Block {
                body: vec![BlockItem::Stmt(Statement::Return(Expression::Constant(7)))],
            },
        };

        let program_expected = Program {
            body: function_expected,
        };

        assert_eq!(Program::parse(tokens), program_expected);
    }

    #[test]
    fn parse_function() {
        let mut tokens = VecDeque::from([
            TokenKind::Int,
            TokenKind::Identifier("main".to_owned()),
            TokenKind::ParenOpen,
            TokenKind::Void,
            TokenKind::ParenClose,
            TokenKind::BraceOpen,
            TokenKind::Return,
            TokenKind::Constant(6),
            TokenKind::Semicolon,
            TokenKind::BraceClose,
        ]);

        let function_expected = Function {
            name: Identifier::new("main"),
            return_type: "Int".to_owned(),
            body: Block {
                body: vec![BlockItem::Stmt(Statement::Return(Expression::Constant(6)))],
            },
        };

        assert_eq!(Function::parse(&mut tokens).unwrap(), function_expected);
        assert!(tokens.is_empty());
    }

    #[test]
    fn parse_statement_return() {
        let mut tokens = VecDeque::from([
            TokenKind::Return,
            TokenKind::Constant(6),
            TokenKind::Semicolon,
        ]);
        assert_eq!(
            Statement::parse(&mut tokens).unwrap(),
            Statement::Return(Expression::Constant(6))
        );
        assert!(tokens.is_empty());
    }

    #[test]
    fn parse_expression_factor_constant() {
        let mut tokens = VecDeque::from([TokenKind::Constant(3)]);
        assert_eq!(
            Expression::parse_factor(&mut tokens).unwrap(),
            Expression::Constant(3)
        );
        assert!(tokens.is_empty());
    }

    #[test]
    fn parse_expression_factor_unary() {
        let mut tokens = VecDeque::from([TokenKind::Minus, TokenKind::Constant(2)]);

        let expr = Expression::parse_factor(&mut tokens).unwrap();
        let expected =
            Expression::Unary(UnaryOperator::Negation, Box::new(Expression::Constant(2)));
        assert_eq!(expr, expected);
        assert!(tokens.is_empty());
    }

    #[test]
    fn parse_expression_unary_nested() {
        let mut tokens = VecDeque::from([
            TokenKind::Complement,
            TokenKind::ParenOpen,
            TokenKind::Minus,
            TokenKind::Constant(4),
            TokenKind::ParenClose,
        ]);

        let expr = Expression::parse_factor(&mut tokens).unwrap();
        let expected = Expression::Unary(
            UnaryOperator::Complement,
            Box::new(Expression::Unary(
                UnaryOperator::Negation,
                Box::new(Expression::Constant(4)),
            )),
        );
        assert_eq!(expr, expected);
        assert!(tokens.is_empty());
    }

    #[test]
    fn parse_unary() {
        let mut tokens = VecDeque::from([TokenKind::Complement, TokenKind::Minus]);
        assert_eq!(
            UnaryOperator::parse(&mut tokens).unwrap(),
            UnaryOperator::Complement
        );
        assert_eq!(
            UnaryOperator::parse(&mut tokens).unwrap(),
            UnaryOperator::Negation
        );
        assert!(tokens.is_empty());
    }
}
