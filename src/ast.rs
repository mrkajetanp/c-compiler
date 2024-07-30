use crate::lexer::*;
use std::mem::discriminant;
use std::collections::VecDeque;
use display_tree::DisplayTree;
use strum_macros::Display;

fn expect_token(expected: TokenKind, tokens: &mut VecDeque<TokenKind>) -> TokenKind {
    let exp = discriminant(&expected);
    let actual = discriminant(&tokens[0]);

    if actual != exp {
        panic!("Syntax Error: Expected {:?}, got {:?}", &expected, &tokens[0]);
    }

    tokens.pop_front().unwrap()
}

#[derive(Debug, PartialEq, DisplayTree)]
#[allow(dead_code)]
pub struct Program {
    #[tree]
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

#[derive(Debug, PartialEq, Clone, DisplayTree)]
#[allow(dead_code)]
pub struct Function {
    #[node_label]
    pub name: String,
    pub return_type: String,
    #[tree]
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

#[derive(Debug, PartialEq, Clone, DisplayTree)]
#[allow(dead_code)]
pub enum Statement {
    Return(
        #[tree]
        Expression
    )
}

impl Statement {
    fn parse(tokens: &mut VecDeque<TokenKind>) -> Statement {
        expect_token(TokenKind::Return, tokens);

        let expr = Expression::parse(tokens, 0);

        expect_token(TokenKind::Semicolon, tokens);

        Self::Return(expr)
    }
}

#[derive(Debug, PartialEq, Clone, DisplayTree)]
#[allow(dead_code)]
pub enum Expression {
    Constant(i64),
    Unary(
        #[node_label]
        UnaryOperator,
        #[tree]
        Box<Expression>
    ),
    Binary(
        #[node_label]
        BinaryOperator,
        #[tree]
        Box<Expression>,
        #[tree]
        Box<Expression>
    )
}

impl Expression {
    fn parse(tokens: &mut VecDeque<TokenKind>, min_precedence: u32) -> Expression {
        if tokens.len() == 0 {
            panic!("No tokens to parse");
        }
        let mut left = Expression::parse_factor(tokens);
        let mut token = tokens.front().unwrap().to_owned();

        while token.is_binary_operator() && token.precedence() > min_precedence {
            let operator = BinaryOperator::parse(tokens);
            let right = Expression::parse(
                tokens, token.precedence() + 1
            );
            left = Expression::Binary(operator, Box::new(left), Box::new(right));
            token = tokens.front().unwrap().to_owned();
        }
        left
    }

    fn parse_factor(tokens: &mut VecDeque<TokenKind>) -> Expression {
        if tokens.len() == 0 {
            panic!("No tokens to parse");
        }
        let token = tokens.front().unwrap().to_owned();

        if token.is_constant() {
            if let TokenKind::Constant(val) =
                expect_token(TokenKind::Constant(0), tokens) {
                    return Self::Constant(val);
            }
        }

        if token.is_complement() || token.is_minus() {
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

#[derive(Debug, PartialEq, Clone, DisplayTree, Display)]
#[allow(dead_code)]
pub enum BinaryOperator {
    Add,
    Subtract,
    Multiply,
    Divide,
    Remainder
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
            _ => panic!("Fatal error: Unexpected binary operator token")
        }
    }


}

#[derive(Debug, PartialEq, Clone, DisplayTree, Display)]
#[allow(dead_code)]
pub enum UnaryOperator {
    Complement,
    Negation
}

impl UnaryOperator {
    fn parse(tokens: &mut VecDeque<TokenKind>) -> Self {
        let token = tokens.pop_front().unwrap();

        match token {
            TokenKind::Complement => Self::Complement,
            TokenKind::Minus => Self::Negation,
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
    fn parse_expression_factor_constant() {
        let mut tokens = VecDeque::from([TokenKind::Constant(3)]);
        assert_eq!(Expression::parse_factor(&mut tokens), Expression::Constant(3));
        assert!(tokens.is_empty());
    }

    #[test]
    fn parse_expression_factor_unary() {
        let mut tokens = VecDeque::from([
            TokenKind::Minus, TokenKind::Constant(2),
        ]);

        let expr = Expression::parse_factor(&mut tokens);
        let expected = Expression::Unary(
            UnaryOperator::Negation, Box::new(Expression::Constant(2))
        );
        assert_eq!(expr, expected);
        assert!(tokens.is_empty());
    }

    #[test]
    fn parse_expression_unary_nested() {
        let mut tokens = VecDeque::from([
            TokenKind::Complement, TokenKind::ParenOpen, TokenKind::Minus,
            TokenKind::Constant(4), TokenKind::ParenClose
        ]);

        let expr = Expression::parse_factor(&mut tokens);
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
            TokenKind::Complement, TokenKind::Minus
        ]);
        assert_eq!(UnaryOperator::parse(&mut tokens), UnaryOperator::Complement);
        assert_eq!(UnaryOperator::parse(&mut tokens), UnaryOperator::Negation);
        assert!(tokens.is_empty());
    }
}

