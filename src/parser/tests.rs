use super::ast::*;
use crate::lexer::*;
use std::collections::VecDeque;

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
    let expected = Expression::Unary(UnaryOperator::Negation, Box::new(Expression::Constant(2)));
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
