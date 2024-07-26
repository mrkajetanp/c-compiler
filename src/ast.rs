use crate::lexer::*;
use std::mem::discriminant;
use std::collections::VecDeque;

fn expect_token(expected: TokenKind, tokens: &VecDeque<TokenKind>) {
    let exp = discriminant(&expected);
    let actual = discriminant(&tokens[0]);

    if actual != exp {
        panic!("Syntax Error: Expected {:?}, got {:?}", &expected, &tokens[0]);
    }
}

fn expect_token_pop(expected: TokenKind, tokens: &mut VecDeque<TokenKind>) -> TokenKind {
    expect_token(expected, tokens);
    tokens.pop_front().unwrap()
}

#[derive(Debug)]
#[allow(dead_code)]
pub struct Program {
    source: Function
}

impl Program {
    pub fn parse(tokens: Vec<TokenKind>) -> Program {
        let mut tokens = VecDeque::from(tokens);

        let program = Program {
            source: Function::parse(&mut tokens)
        };

        if !tokens.is_empty() {
            panic!("Syntax Error: Unexpected token {:?}", tokens[0]);
        }

        program
    }
}

#[derive(Debug)]
#[allow(dead_code)]
pub struct Function {
    name: String,
    return_type: String,
    body: Statement,
}

impl Function {
    fn parse(tokens: &mut VecDeque<TokenKind>) -> Function {
        let return_type = expect_token_pop(TokenKind::Int, tokens);

        expect_token(TokenKind::Identifier("".to_owned()), tokens);
        let name =
            if let TokenKind::Identifier(n) = tokens.pop_front().unwrap() {
                n
            } else {
                panic!("fatal Function name parsing error");
            };

        expect_token_pop(TokenKind::ParenOpen, tokens);
        expect_token_pop(TokenKind::Void, tokens);
        expect_token_pop(TokenKind::ParenClose, tokens);
        expect_token_pop(TokenKind::BraceOpen, tokens);

        let body = Statement::parse(tokens);

        expect_token_pop(TokenKind::BraceClose, tokens);

        Function {
            name,
            return_type: return_type.to_string(),
            body,
        }
    }
}

#[derive(Debug)]
#[allow(dead_code)]
pub enum Statement {
    Return(Expression)
}

impl Statement {
    fn parse(tokens: &mut VecDeque<TokenKind>) -> Statement {
        expect_token_pop(TokenKind::Return, tokens);

        let expr = Expression::parse(tokens);

        expect_token_pop(TokenKind::Semicolon, tokens);

        Self::Return(expr)
    }
}

#[derive(Debug)]
#[allow(dead_code)]
pub enum Expression {
    Constant(i64)
}

impl Expression {
    fn parse(tokens: &mut VecDeque<TokenKind>) -> Expression {
        expect_token(TokenKind::Constant(0), tokens);
        if let TokenKind::Constant(val) = tokens.pop_front().unwrap() {
            return Self::Constant(val);
        }
        panic!("fatal Expression parsing error");
    }
}

