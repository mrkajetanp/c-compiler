use crate::lexer::*;
use std::mem::discriminant;
use std::collections::VecDeque;

fn expect_token(expected: TokenKind, tokens: &mut VecDeque<TokenKind>) -> TokenKind {
    let exp = discriminant(&expected);
    let actual = discriminant(&tokens[0]);

    if actual != exp {
        panic!("Syntax Error: Expected {:?}, got {:?}", &expected, &tokens[0]);
    }

    tokens.pop_front().unwrap()
}

#[derive(Debug)]
#[allow(dead_code)]
pub struct Program {
    pub body: Function
}

impl Program {
    pub fn parse(tokens: Vec<TokenKind>) -> Program {
        let mut tokens = VecDeque::from(tokens);

        let program = Program {
            body: Function::parse(&mut tokens)
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
    pub name: String,
    pub return_type: String,
    pub body: Statement,
}

impl Function {
    fn parse(tokens: &mut VecDeque<TokenKind>) -> Function {
        let return_type = expect_token(TokenKind::Int, tokens);

        let name = if let TokenKind::Identifier(n) =
            expect_token(TokenKind::Identifier("".to_owned()), tokens) {
                n
            } else {
                panic!("fatal Function name parsing error");
            };

        expect_token(TokenKind::ParenOpen, tokens);
        expect_token(TokenKind::Void, tokens);
        expect_token(TokenKind::ParenClose, tokens);
        expect_token(TokenKind::BraceOpen, tokens);

        let body = Statement::parse(tokens);

        expect_token(TokenKind::BraceClose, tokens);

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
        expect_token(TokenKind::Return, tokens);

        let expr = Expression::parse(tokens);

        expect_token(TokenKind::Semicolon, tokens);

        Self::Return(expr)
    }
}

#[derive(Debug)]
#[allow(dead_code)]
pub enum Expression {
    Constant(i64),
    Unary(UnaryOperator, Box<Expression>)
}

impl Expression {
    fn parse(tokens: &mut VecDeque<TokenKind>) -> Expression {
        let token = tokens.front().unwrap().to_owned();

        if token.is_constant() {
            if let TokenKind::Constant(val) =
                expect_token(TokenKind::Constant(0), tokens) {
                    return Self::Constant(val);
            }
        }

        if token.is_complement() || token.is_negation() {
            let operator = UnaryOperator::parse(tokens);
            let inner = Expression::parse(tokens);
            return Self::Unary(operator, Box::new(inner));
        }

        if token.is_paren_open() {
            expect_token(TokenKind::ParenOpen, tokens);
            let inner = Expression::parse(tokens);
            expect_token(TokenKind::ParenClose, tokens);
            return inner;
        }

        panic!("Syntax Error: Malformed expression");
    }
}


#[derive(Debug)]
#[allow(dead_code)]
pub enum UnaryOperator {
    Complement,
    Negation
}

impl UnaryOperator {
    fn parse(tokens: &mut VecDeque<TokenKind>) -> UnaryOperator {
        let token = tokens.pop_front().unwrap();

        match token {
            TokenKind::Complement => Self::Complement,
            TokenKind::Negation => Self::Negation,
            _ => panic!("Fatal error: Unexpected unary operator token")
        }
    }
}
