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

#[derive(Debug, PartialEq)]
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

#[derive(Debug, PartialEq)]
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

#[derive(Debug, PartialEq)]
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

#[derive(Debug, PartialEq)]
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


#[derive(Debug, PartialEq)]
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_program() {
        let tokens = vec![
            TokenKind::Int, TokenKind::Identifier("main".to_owned()),
            TokenKind::ParenOpen, TokenKind::Void, TokenKind::ParenClose,
            TokenKind::BraceOpen, TokenKind::Return, TokenKind::Constant(7),
            TokenKind::Semicolon, TokenKind::BraceClose
        ];

        let function_expected = Function {
            name: "main".to_owned(),
            return_type: "Int".to_owned(),
            body: Statement::Return(Expression::Constant(7))
        };

        let program_expected = Program {
            body: function_expected
        };

        assert_eq!(Program::parse(tokens), program_expected);
    }

    #[test]
    fn parse_function() {
        let mut tokens = VecDeque::from([
            TokenKind::Int, TokenKind::Identifier("main".to_owned()),
            TokenKind::ParenOpen, TokenKind::Void, TokenKind::ParenClose,
            TokenKind::BraceOpen, TokenKind::Return, TokenKind::Constant(6),
            TokenKind::Semicolon, TokenKind::BraceClose
        ]);

        let function_expected = Function {
            name: "main".to_owned(),
            return_type: "Int".to_owned(),
            body: Statement::Return(Expression::Constant(6))
        };

        assert_eq!(Function::parse(&mut tokens), function_expected);
        assert!(tokens.is_empty());
    }

    #[test]
    fn parse_statement_return() {
        let mut tokens = VecDeque::from([
            TokenKind::Return, TokenKind::Constant(6), TokenKind::Semicolon
        ]);
        assert_eq!(Statement::parse(&mut tokens), Statement::Return(Expression::Constant(6)));
        assert!(tokens.is_empty());
    }

    #[test]
    fn parse_expression_constant() {
        let mut tokens = VecDeque::from([TokenKind::Constant(3)]);
        assert_eq!(Expression::parse(&mut tokens), Expression::Constant(3));
        assert!(tokens.is_empty());
    }

    #[test]
    fn parse_expression_unary() {
        let mut tokens = VecDeque::from([
            TokenKind::Negation, TokenKind::Constant(2),
        ]);

        let expr = Expression::parse(&mut tokens);
        let expected = Expression::Unary(
            UnaryOperator::Negation, Box::new(Expression::Constant(2))
        );
        assert_eq!(expr, expected);
        assert!(tokens.is_empty());
    }

    #[test]
    fn parse_expression_unary_nested() {
        let mut tokens = VecDeque::from([
            TokenKind::Complement, TokenKind::ParenOpen, TokenKind::Negation,
            TokenKind::Constant(4), TokenKind::ParenClose
        ]);

        let expr = Expression::parse(&mut tokens);
        let expected = Expression::Unary(
            UnaryOperator::Complement, Box::new(
                Expression::Unary(UnaryOperator::Negation, Box::new(Expression::Constant(4)))
            )
        );
        assert_eq!(expr, expected);
        assert!(tokens.is_empty());
    }

    #[test]
    fn parse_unary() {
        let mut tokens = VecDeque::from([
            TokenKind::Complement, TokenKind::Negation
        ]);
        assert_eq!(UnaryOperator::parse(&mut tokens), UnaryOperator::Complement);
        assert_eq!(UnaryOperator::parse(&mut tokens), UnaryOperator::Negation);
        assert!(tokens.is_empty());
    }
}

