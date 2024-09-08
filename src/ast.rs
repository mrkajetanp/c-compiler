use crate::lexer::*;
use display_tree::{format_tree, DisplayTree, StyleBuilder};
use std::collections::VecDeque;
use std::fmt;
use std::mem::discriminant;
use strum_macros::{Display, EnumIs};
use thiserror::Error;

static INDENT: &str = "  ";
static CON_VERT: &str = "│";
static CON_LINE: &str = "─";
static CON_MID: &str = "├─";
static CON_END: &str = "└─";

#[derive(Debug, Error)]
pub enum ParserError {
    #[error("Unexpected token in the token stream")]
    UnexpectedToken,
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
            match FunctionDeclaration::parse(&mut tokens) {
                Ok(func) => body.push(func),
                Err(err) => {
                    log::error!("Parser error: {}", err);
                    return Err(err);
                }
            }
        }

        Ok(Program { body })
    }
}

impl fmt::Display for Program {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for function in &self.body {
            function.tree_print(f, 0)?;
        }
        Ok(())
    }
}

impl DisplayTree for Program {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>, style: display_tree::Style) -> std::fmt::Result {
        for function in &self.body {
            writeln!(
                f,
                "{}{} {}",
                style.char_set.connector,
                std::iter::repeat(style.char_set.horizontal)
                    .take(style.indentation as usize)
                    .collect::<String>(),
                display_tree::format_tree!(*function)
            )?;
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
    fn parse(tokens: &mut VecDeque<TokenKind>) -> ParserResult<Self> {
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

        log::trace!("--- Parsed function declaration: {}", func);
        Ok(func)
    }
}

impl FunctionDeclaration {
    pub fn tree_print(&self, f: &mut fmt::Formatter, indent_level: usize) -> fmt::Result {
        let indent = std::iter::repeat(INDENT)
            .take(indent_level)
            .collect::<String>();

        writeln!(f, "{}FunctionDeclaration", indent)?;
        writeln!(f, "{}{} ({:?})", indent, self.name, self.params)?;
        writeln!(f, "{}{}", indent, self.return_type)?;
        if let Some(body) = &self.body {
            body.tree_print(f, indent_level + 1)?;
        }
        Ok(())
    }
}

impl fmt::Display for FunctionDeclaration {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let params = self
            .params
            .iter()
            .map(|p| p.to_string())
            .collect::<Vec<String>>()
            .join(",");
        writeln!(f, "{} {}({})", self.return_type, self.name, params)?;
        if let Some(body) = &self.body {
            writeln!(f, "\t{}", body)?;
        }
        Ok(())
    }
}

impl DisplayTree for FunctionDeclaration {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>, style: display_tree::Style) -> std::fmt::Result {
        writeln!(f, "{} {} ({:?})", self.return_type, self.name, self.params)?;
        if let Some(body) = &self.body {
            writeln!(
                f,
                "{}{} {}",
                style.char_set.connector,
                std::iter::repeat(style.char_set.horizontal)
                    .take(style.indentation as usize)
                    .collect::<String>(),
                display_tree::format_tree!(*body)
            )?;
        }
        Ok(())
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

impl Block {
    pub fn tree_print(&self, f: &mut fmt::Formatter, indent_level: usize) -> fmt::Result {
        let indent = std::iter::repeat(INDENT)
            .take(indent_level)
            .collect::<String>();
        writeln!(f, "{}Block", indent)?;
        for block_item in &self.body {
            block_item.tree_print(f, indent_level + 1)?;
        }
        Ok(())
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
    fn parse(tokens: &mut VecDeque<TokenKind>) -> ParserResult<Self> {
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

impl BlockItem {
    pub fn tree_print(&self, f: &mut fmt::Formatter, indent_level: usize) -> fmt::Result {
        let indent = std::iter::repeat(INDENT)
            .take(indent_level)
            .collect::<String>();
        writeln!(f, "{}BlockItem", indent)?;
        match self {
            BlockItem::Stmt(stmt) => stmt.tree_print(f, indent_level + 1)?,
            BlockItem::Decl(decl) => decl.tree_print(f, indent_level + 1)?,
        };
        Ok(())
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

#[derive(Debug, PartialEq, Clone, DisplayTree)]
pub enum Declaration {
    FunDecl(#[tree] FunctionDeclaration),
    VarDecl(#[tree] VariableDeclaration),
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

impl Declaration {
    pub fn tree_print(&self, f: &mut fmt::Formatter, indent_level: usize) -> fmt::Result {
        let indent = std::iter::repeat(INDENT)
            .take(indent_level)
            .collect::<String>();

        writeln!(f, "{}Declaration", indent)?;
        match self {
            Declaration::FunDecl(decl) => decl.tree_print(f, indent_level + 1)?,
            Declaration::VarDecl(decl) => decl.tree_print(f, indent_level + 1)?,
        }
        Ok(())
    }
}

impl fmt::Display for Declaration {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Declaration::FunDecl(decl) => write!(f, "{:?}", decl),
            Declaration::VarDecl(decl) => write!(f, "{}", decl),
        }
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
        expect_token(TokenKind::Semicolon, tokens)?;

        let result = Self { name: ident, init };
        log::trace!("-- Parsed declaration: {}", result);
        Ok(result)
    }
}

impl VariableDeclaration {
    pub fn tree_print(&self, f: &mut fmt::Formatter, indent_level: usize) -> fmt::Result {
        let indent = std::iter::repeat(INDENT)
            .take(indent_level)
            .collect::<String>();

        writeln!(f, "{}VariableDeclaration(int, {})", indent, self.name)?;
        if let Some(init) = &self.init {
            init.tree_print(f, indent_level + 1)?;
        }
        Ok(())
    }
}

impl fmt::Display for VariableDeclaration {
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

impl DisplayTree for VariableDeclaration {
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

impl Statement {
    pub fn tree_print(&self, f: &mut fmt::Formatter, indent_level: usize) -> fmt::Result {
        let indent = std::iter::repeat(INDENT)
            .take(indent_level)
            .collect::<String>();

        match self {
            Statement::Return(val) => {
                writeln!(f, "{}Return", indent)?;
                val.tree_print(f, indent_level + 1)?;
            }
            Statement::While(cond, body, label) => {
                writeln!(
                    f,
                    "{}While ({}) [{}]",
                    indent,
                    cond,
                    option_ident_to_string(label)
                )?;
                body.tree_print(f, indent_level + 1)?;
            }
            Statement::DoWhile(body, cond, label) => {
                writeln!(
                    f,
                    "{}Do-While ({}) [{}]",
                    indent,
                    cond,
                    option_ident_to_string(label)
                )?;
                body.tree_print(f, indent_level + 1)?;
            }
            Statement::If(cond, then_stmt, else_stmt) => {
                writeln!(f, "{}If ({})", indent, cond)?;
                then_stmt.tree_print(f, indent_level + 2)?;
                if let Some(else_stmt) = else_stmt {
                    writeln!(f, "{}Else", indent)?;
                    else_stmt.tree_print(f, indent_level + 2)?;
                }
            }
            Statement::For(init, cond, post, body, label) => {
                writeln!(f, "{}For [{}]", indent, option_ident_to_string(label))?;
                writeln!(f, "{}{CON_MID}init: {}", indent, init)?;
                if let Some(cond) = cond {
                    writeln!(f, "{}{CON_MID}cond: {}", indent, cond)?;
                }
                if let Some(post) = post {
                    writeln!(f, "{}{CON_MID}post: {}", indent, post)?;
                }
                writeln!(f, "{}{CON_END}body:", indent)?;
                body.tree_print(f, indent_level + 1)?;
            }
            Statement::Compound(block) => {
                writeln!(f, "{}Compound", indent)?;
                block.tree_print(f, indent_level + 1)?
            }
            Statement::Exp(exp) => exp.tree_print(f, indent_level + 1)?,
            Statement::Break(label) => {
                writeln!(f, "{indent}Break [{}]", option_ident_to_string(label))?
            }
            Statement::Continue(label) => {
                writeln!(f, "{indent}Continue [{}]", option_ident_to_string(label))?
            }
            Statement::Null => writeln!(f, "{indent}Null")?,
        }

        Ok(())
    }
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
            Statement::Break(label) => write!(f, "break [{}]", option_ident_to_string(label)),
            Statement::Continue(label) => {
                write!(f, "continue [{}]", option_ident_to_string(label))
            }
            Statement::Null => write!(f, "NULL"),
        }
    }
}

impl Statement {
    fn parse(tokens: &mut VecDeque<TokenKind>) -> ParserResult<Self> {
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

        log::trace!("-- Parsed statement: {}", result);
        Ok(result)
    }
}

#[derive(Debug, PartialEq, Clone, DisplayTree)]
#[allow(dead_code)]
pub enum ForInit {
    InitDecl(#[tree] VariableDeclaration),
    InitExp(#[tree] Expression),
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
    FunctionCall(Identifier, #[ignore_field] Vec<Expression>),
}

impl Expression {
    fn parse(tokens: &mut VecDeque<TokenKind>, min_precedence: u32) -> ParserResult<Self> {
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

    fn parse_factor(tokens: &mut VecDeque<TokenKind>) -> ParserResult<Self> {
        log_trace("Trying factor from", tokens);

        if tokens.len() == 0 {
            log::error!("No tokens passed to the Expression parser");
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

    fn parse_optional(tokens: &mut VecDeque<TokenKind>) -> ParserResult<Option<Expression>> {
        let expr = Expression::parse(tokens, 0);
        if let Ok(expr) = expr {
            Ok(Some(expr))
        } else {
            Ok(None)
        }
    }
}

impl Expression {
    pub fn tree_print(&self, f: &mut fmt::Formatter, indent_level: usize) -> fmt::Result {
        let indent = std::iter::repeat(INDENT)
            .take(indent_level)
            .collect::<String>();

        match self {
            Expression::Assignment(left, right) => {
                writeln!(f, "{indent}Assignment")?;
                writeln!(f, "{indent}{CON_MID}Left")?;
                left.tree_print(f, indent_level + 2)?;
                writeln!(f, "{indent}{CON_END}Right")?;
                right.tree_print(f, indent_level + 2)?;
            }
            Expression::Binary(op, left, right) => {
                writeln!(f, "{indent}Binary")?;
                writeln!(f, "{indent}{CON_MID}operator: {}", op)?;
                writeln!(f, "{indent}{CON_MID}Left")?;
                left.tree_print(f, indent_level + 2)?;
                writeln!(f, "{indent}{CON_END}Right")?;
                right.tree_print(f, indent_level + 2)?;
            }
            _ => writeln!(f, "{}{}", indent, self)?,
        }
        Ok(())
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
            Expression::FunctionCall(name, args) => write!(f, "{}({:?})", name, args),
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
    fn parse(tokens: &mut VecDeque<TokenKind>) -> ParserResult<Self> {
        let token = tokens.pop_front().unwrap();

        match token {
            TokenKind::Complement => Ok(Self::Complement),
            TokenKind::Minus => Ok(Self::Negation),
            TokenKind::Not => Ok(Self::Not),
            _ => Err(ParserError::UnexpectedToken),
        }
    }
}

#[derive(Debug, PartialEq, Eq, Hash, Clone, DisplayTree)]
#[allow(dead_code)]
pub struct Identifier {
    #[node_label]
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

fn option_ident_to_string(ident: &Option<Identifier>) -> String {
    if let Some(ident) = ident {
        ident.to_string()
    } else {
        "None".to_string()
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
fn expect_token_silent(
    expected: TokenKind,
    tokens: &mut VecDeque<TokenKind>,
) -> ParserResult<TokenKind> {
    let exp = discriminant(&expected);
    let actual = discriminant(&tokens[0]);

    if actual != exp {
        Err(ParserError::UnexpectedToken)
    } else {
        Ok(tokens.pop_front().unwrap())
    }
}

#[inline(always)]
fn expect_token(expected: TokenKind, tokens: &mut VecDeque<TokenKind>) -> ParserResult<TokenKind> {
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

        let function_expected = FunctionDeclaration {
            name: Identifier::new("main"),
            params: vec![],
            return_type: "Int".to_owned(),
            body: Some(Block {
                body: vec![BlockItem::Stmt(Statement::Return(Expression::Constant(7)))],
            }),
        };

        let program_expected = Program {
            body: vec![function_expected],
        };

        assert_eq!(Program::parse(tokens).unwrap(), program_expected);
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

        let function_expected = FunctionDeclaration {
            name: Identifier::new("main"),
            params: vec![],
            return_type: "Int".to_owned(),
            body: Some(Block {
                body: vec![BlockItem::Stmt(Statement::Return(Expression::Constant(6)))],
            }),
        };

        assert_eq!(
            FunctionDeclaration::parse(&mut tokens).unwrap(),
            function_expected
        );
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
