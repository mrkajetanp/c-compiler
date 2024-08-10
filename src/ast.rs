use crate::lexer::*;
use display_tree::DisplayTree;
use std::collections::VecDeque;
use std::fmt;
use std::mem::discriminant;
use strum_macros::{Display, EnumIs};

fn expect_token(expected: TokenKind, tokens: &mut VecDeque<TokenKind>) -> TokenKind {
    let exp = discriminant(&expected);
    let actual = discriminant(&tokens[0]);

    if actual != exp {
        panic!(
            "Syntax Error: Expected {:?}, got {:?}",
            &expected, &tokens[0]
        );
    }

    tokens.pop_front().unwrap()
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

        let program = Program {
            body: Function::parse(&mut tokens),
        };

        if !tokens.is_empty() {
            panic!("Syntax Error: Unexpected token {:?}", tokens[0]);
        }

        program
    }
}

#[derive(Debug, PartialEq, Clone)]
#[allow(dead_code)]
pub struct Function {
    pub name: Identifier,
    pub return_type: String,
    pub body: Vec<BlockItem>,
}

impl DisplayTree for Function {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>, style: display_tree::Style) -> std::fmt::Result {
        writeln!(f, "{} {}", self.return_type, self.name)?;
        for block in &self.body {
            writeln!(
                f,
                "{}{} {}",
                style.char_set.connector,
                std::iter::repeat(style.char_set.horizontal)
                    .take(style.indentation as usize)
                    .collect::<String>(),
                display_tree::format_tree!(*block)
            )?;
        }
        Ok(())
    }
}

impl Function {
    fn parse(tokens: &mut VecDeque<TokenKind>) -> Function {
        let return_type = expect_token(TokenKind::Int, tokens);

        let name = Identifier::parse(expect_token(TokenKind::Identifier("".to_owned()), tokens));

        expect_token(TokenKind::ParenOpen, tokens);
        expect_token(TokenKind::Void, tokens);
        expect_token(TokenKind::ParenClose, tokens);
        expect_token(TokenKind::BraceOpen, tokens);

        let mut body = vec![];
        while !tokens.front().unwrap().to_owned().is_brace_close() {
            body.push(BlockItem::parse(tokens));
        }
        expect_token(TokenKind::BraceClose, tokens);

        Function {
            name,
            return_type: return_type.to_string(),
            body,
        }
    }
}

#[derive(Debug, PartialEq, Clone, DisplayTree)]
#[allow(dead_code)]
pub enum BlockItem {
    Stmt(#[tree] Statement),
    Decl(#[tree] Declaration),
}

impl BlockItem {
    fn parse(tokens: &mut VecDeque<TokenKind>) -> Self {
        log::trace!("parsing block item from {:?}", tokens);

        let token = tokens.front().unwrap().to_owned();
        if token.is_int() {
            BlockItem::Decl(Declaration::parse(tokens))
        } else {
            BlockItem::Stmt(Statement::parse(tokens))
        }
    }
}

#[derive(Debug, PartialEq, Clone, DisplayTree)]
#[allow(dead_code)]
pub struct Declaration {
    #[node_label]
    pub name: Identifier,
    #[ignore_field]
    pub init: Option<Expression>,
}

impl Declaration {
    fn parse(tokens: &mut VecDeque<TokenKind>) -> Self {
        log::trace!("parsing declaration from {:?}", tokens);
        // TODO: actually use the type
        let _ty = expect_token(TokenKind::Int, tokens);
        let ident = Identifier::parse(expect_token(TokenKind::Identifier("".to_owned()), tokens));

        let init = if tokens.front().unwrap().to_owned().is_semicolon() {
            None
        } else {
            expect_token(TokenKind::Assignment, tokens);
            Some(Expression::parse(tokens, 0))
        };
        expect_token(TokenKind::Semicolon, tokens);

        let result = Self { name: ident, init };
        log::trace!("-- Parsed declaration {:?}", result);
        result
    }
}

#[derive(Debug, PartialEq, Clone, DisplayTree)]
#[allow(dead_code)]
pub enum Statement {
    Return(#[tree] Expression),
    Exp(#[tree] Expression),
    Null,
}

impl Statement {
    fn parse(tokens: &mut VecDeque<TokenKind>) -> Statement {
        log::trace!("Trying statement from {:?}", tokens);

        let token = tokens.front().unwrap().to_owned();

        if token.is_semicolon() {
            expect_token(TokenKind::Semicolon, tokens);
            return Statement::Null;
        }

        let result = if token.is_return() {
            expect_token(TokenKind::Return, tokens);
            let expr = Expression::parse(tokens, 0);
            Self::Return(expr)
        } else {
            Statement::Exp(Expression::parse(tokens, 0))
        };

        expect_token(TokenKind::Semicolon, tokens);
        log::trace!("-- Parsed stmt {:?}", result);
        result
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
}

impl Expression {
    fn parse(tokens: &mut VecDeque<TokenKind>, min_precedence: u32) -> Expression {
        log::trace!(
            "Trying expr from {:?} precedence {}",
            tokens,
            min_precedence
        );

        if tokens.len() == 0 {
            panic!("No tokens to parse");
        }
        let mut left = Expression::parse_factor(tokens);
        let mut token = tokens.front().unwrap().to_owned();

        while token.is_binary_operator() && token.precedence() > min_precedence {
            if token.is_assignment() {
                expect_token(TokenKind::Assignment, tokens);
                let right = Expression::parse(tokens, token.precedence());
                left = Expression::Assignment(Box::new(left), Box::new(right));
            } else {
                let operator = BinaryOperator::parse(tokens);
                let right = Expression::parse(tokens, token.precedence() + 1);
                left = Expression::Binary(operator, Box::new(left), Box::new(right));
            }
            token = tokens.front().unwrap().to_owned();
        }
        log::trace!("-- Parsed expr {:?}", left);
        left
    }

    fn parse_factor(tokens: &mut VecDeque<TokenKind>) -> Expression {
        log::trace!("Trying factor from {:?}", tokens);

        if tokens.len() == 0 {
            panic!("No tokens to parse");
        }
        let token = tokens.front().unwrap().to_owned();

        if token.is_identifier() {
            return Self::Var(Identifier::parse(tokens.pop_front().unwrap()));
        }

        if token.is_constant() {
            if let TokenKind::Constant(val) = expect_token(TokenKind::Constant(0), tokens) {
                return Self::Constant(val);
            }
        }

        if token.is_unary_operator() {
            let operator = UnaryOperator::parse(tokens);
            let inner = Expression::parse_factor(tokens);
            return Self::Unary(operator, Box::new(inner));
        }

        if token.is_paren_open() {
            expect_token(TokenKind::ParenOpen, tokens);
            let inner = Expression::parse(tokens, 0);
            expect_token(TokenKind::ParenClose, tokens);
            return inner;
        }

        panic!("Syntax Error: Malformed expression");
    }
}

#[derive(Debug, PartialEq, Clone, DisplayTree, Display, EnumIs)]
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
    fn parse(tokens: &mut VecDeque<TokenKind>) -> Self {
        let token = tokens.pop_front().unwrap();

        match token {
            TokenKind::Plus => Self::Add,
            TokenKind::Minus => Self::Subtract,
            TokenKind::Asterisk => Self::Multiply,
            TokenKind::Slash => Self::Divide,
            TokenKind::Percent => Self::Remainder,
            TokenKind::And => Self::And,
            TokenKind::Or => Self::Or,
            TokenKind::Equal => Self::Equal,
            TokenKind::NotEqual => Self::NotEqual,
            TokenKind::LessEqualThan => Self::LessEqualThan,
            TokenKind::GreaterEqualThan => Self::GreaterEqualThan,
            TokenKind::LessThan => Self::LessThan,
            TokenKind::GreaterThan => Self::GreaterThan,
            _ => panic!("Fatal error: Unexpected binary operator token"),
        }
    }

    pub fn is_short_circuit(&self) -> bool {
        match &self {
            &BinaryOperator::And | &BinaryOperator::Or => true,
            _ => false,
        }
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
    fn parse(tokens: &mut VecDeque<TokenKind>) -> Self {
        let token = tokens.pop_front().unwrap();

        match token {
            TokenKind::Complement => Self::Complement,
            TokenKind::Minus => Self::Negation,
            TokenKind::Not => Self::Not,
            _ => panic!("Fatal error: Unexpected unary operator token"),
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
    pub fn parse(token: TokenKind) -> Self {
        if let TokenKind::Identifier(name) = token {
            Self { name }
        } else {
            panic!("Failed to parse Identifier from {:?}", token)
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
            body: vec![BlockItem::Stmt(Statement::Return(Expression::Constant(7)))],
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
            body: vec![BlockItem::Stmt(Statement::Return(Expression::Constant(6)))],
        };

        assert_eq!(Function::parse(&mut tokens), function_expected);
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
            Statement::parse(&mut tokens),
            Statement::Return(Expression::Constant(6))
        );
        assert!(tokens.is_empty());
    }

    #[test]
    fn parse_expression_factor_constant() {
        let mut tokens = VecDeque::from([TokenKind::Constant(3)]);
        assert_eq!(
            Expression::parse_factor(&mut tokens),
            Expression::Constant(3)
        );
        assert!(tokens.is_empty());
    }

    #[test]
    fn parse_expression_factor_unary() {
        let mut tokens = VecDeque::from([TokenKind::Minus, TokenKind::Constant(2)]);

        let expr = Expression::parse_factor(&mut tokens);
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

        let expr = Expression::parse_factor(&mut tokens);
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
        assert_eq!(UnaryOperator::parse(&mut tokens), UnaryOperator::Complement);
        assert_eq!(UnaryOperator::parse(&mut tokens), UnaryOperator::Negation);
        assert!(tokens.is_empty());
    }
}
