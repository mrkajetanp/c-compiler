use core::panic;

use regex::Regex;
use strum::IntoEnumIterator;
use strum_macros::{EnumIs, EnumIter};

static IDENTIFIER_RE: &str = r"[[:alpha:]_]\w*\b";
static CONSTANT_RE: &str = r"[[:digit:]]+\b";
static RETURN_RE: &str = r"return\b";
static INT_RE: &str = r"int\b";
static VOID_RE: &str = r"void\b";
static DO_RE: &str = r"do\b";
static WHILE_RE: &str = r"while\b";
static FOR_RE: &str = r"for\b";
static BREAK_RE: &str = r"break\b";
static CONTINUE_RE: &str = r"continue\b";
static PAREN_OPEN_RE: &str = r"\(";
static PAREN_CLOSE_RE: &str = r"\)";
static BRACE_OPEN_RE: &str = r"\{";
static BRACE_CLOSE_RE: &str = r"\}";
static SEMICOLON_RE: &str = r";";
static COMPLEMENT_RE: &str = r"~";
static DECREMENT_RE: &str = r"--";
static MINUS_RE: &str = r"-";
static PLUS_RE: &str = r"\+";
static ASTERISK_RE: &str = r"\*";
static SLASH_RE: &str = r"/";
static PERCENT_RE: &str = r"%";
static AND_RE: &str = r"&&";
static OR_RE: &str = r"\|\|";
static EQUAL_RE: &str = r"==";
static NOT_EQUAL_RE: &str = r"!=";
static LESS_EQUAL_THAN_RE: &str = r"<=";
static GREATER_EQUAL_THAN_RE: &str = r">=";
static NOT_RE: &str = r"!";
static LESS_THAN_RE: &str = r"<";
static GREATER_THAN_RE: &str = r">";
static ASSIGNMENT_RE: &str = r"=";
static IF_RE: &str = r"if\b";
static ELSE_RE: &str = r"else\b";
static QUESTION_RE: &str = r"\?";
static COLON_RE: &str = r":";
static COMMA_RE: &str = r"\,";

// NOTE: The tokenizer will try tokens in-order based on this list
// It *must* be ordered longest-match first
#[derive(EnumIter, EnumIs, Debug, strum_macros::Display, PartialEq, Clone)]
pub enum TokenKind {
    Return,
    Int,
    Void,
    Do,
    While,
    For,
    Break,
    Continue,
    ParenOpen,
    ParenClose,
    BraceOpen,
    BraceClose,
    Semicolon,
    Complement,
    Decrement,
    Minus,
    Plus,
    Asterisk,
    Slash,
    Percent,
    And,
    Or,
    Equal,
    NotEqual,
    LessEqualThan,
    GreaterEqualThan,
    Not,
    LessThan,
    GreaterThan,
    Assignment,
    If,
    Else,
    Question,
    Colon,
    Comma,
    Identifier(String),
    Constant(i64),
}

impl TokenKind {
    fn is_full_match(input: &str, pattern: &str) -> bool {
        if let Some(m) = Regex::new(pattern).unwrap().find(input) {
            m.start() == 0 && m.end() == input.len()
        } else {
            false
        }
    }

    pub fn from_str(input: &str) -> Option<TokenKind> {
        let input = input.trim();

        // log::trace!("Trying to tokenize input {:?}", input);

        match input {
            input if TokenKind::is_full_match(input, RETURN_RE) => Some(Self::Return),
            input if TokenKind::is_full_match(input, INT_RE) => Some(Self::Int),
            input if TokenKind::is_full_match(input, VOID_RE) => Some(Self::Void),
            input if TokenKind::is_full_match(input, DO_RE) => Some(Self::Do),
            input if TokenKind::is_full_match(input, WHILE_RE) => Some(Self::While),
            input if TokenKind::is_full_match(input, FOR_RE) => Some(Self::For),
            input if TokenKind::is_full_match(input, BREAK_RE) => Some(Self::Break),
            input if TokenKind::is_full_match(input, CONTINUE_RE) => Some(Self::Continue),
            input if TokenKind::is_full_match(input, PAREN_CLOSE_RE) => Some(Self::ParenClose),
            input if TokenKind::is_full_match(input, PAREN_OPEN_RE) => Some(Self::ParenOpen),
            input if TokenKind::is_full_match(input, BRACE_OPEN_RE) => Some(Self::BraceOpen),
            input if TokenKind::is_full_match(input, BRACE_CLOSE_RE) => Some(Self::BraceClose),
            input if TokenKind::is_full_match(input, SEMICOLON_RE) => Some(Self::Semicolon),
            input if TokenKind::is_full_match(input, DECREMENT_RE) => Some(Self::Decrement),
            input if TokenKind::is_full_match(input, COMPLEMENT_RE) => Some(Self::Complement),
            input if TokenKind::is_full_match(input, MINUS_RE) => Some(Self::Minus),
            input if TokenKind::is_full_match(input, PLUS_RE) => Some(Self::Plus),
            input if TokenKind::is_full_match(input, ASTERISK_RE) => Some(Self::Asterisk),
            input if TokenKind::is_full_match(input, SLASH_RE) => Some(Self::Slash),
            input if TokenKind::is_full_match(input, PERCENT_RE) => Some(Self::Percent),
            input if TokenKind::is_full_match(input, NOT_EQUAL_RE) => Some(Self::NotEqual),
            input if TokenKind::is_full_match(input, NOT_RE) => Some(Self::Not),
            input if TokenKind::is_full_match(input, AND_RE) => Some(Self::And),
            input if TokenKind::is_full_match(input, OR_RE) => Some(Self::Or),
            input if TokenKind::is_full_match(input, EQUAL_RE) => Some(Self::Equal),
            input if TokenKind::is_full_match(input, GREATER_EQUAL_THAN_RE) => {
                Some(Self::GreaterEqualThan)
            }
            input if TokenKind::is_full_match(input, LESS_EQUAL_THAN_RE) => {
                Some(Self::LessEqualThan)
            }
            input if TokenKind::is_full_match(input, LESS_THAN_RE) => Some(Self::LessThan),
            input if TokenKind::is_full_match(input, GREATER_THAN_RE) => Some(Self::GreaterThan),
            input if TokenKind::is_full_match(input, ASSIGNMENT_RE) => Some(Self::Assignment),
            input if TokenKind::is_full_match(input, IF_RE) => Some(Self::If),
            input if TokenKind::is_full_match(input, ELSE_RE) => Some(Self::Else),
            input if TokenKind::is_full_match(input, QUESTION_RE) => Some(Self::Question),
            input if TokenKind::is_full_match(input, COLON_RE) => Some(Self::Colon),
            input if TokenKind::is_full_match(input, COMMA_RE) => Some(Self::Comma),
            input if TokenKind::is_full_match(input, CONSTANT_RE) => {
                Some(Self::Constant(input.parse::<i64>().unwrap()))
            }
            input if TokenKind::is_full_match(input, IDENTIFIER_RE) => {
                Some(Self::Identifier(input.to_string()))
            }
            _ => None,
        }
    }

    pub fn to_regex(&self) -> Regex {
        match self {
            Self::Identifier(_) => Regex::new(IDENTIFIER_RE),
            Self::Constant(_) => Regex::new(CONSTANT_RE),
            Self::Return => Regex::new(RETURN_RE),
            Self::Int => Regex::new(INT_RE),
            Self::Void => Regex::new(VOID_RE),
            Self::Do => Regex::new(DO_RE),
            Self::While => Regex::new(WHILE_RE),
            Self::For => Regex::new(FOR_RE),
            Self::Break => Regex::new(BREAK_RE),
            Self::Continue => Regex::new(CONTINUE_RE),
            Self::ParenOpen => Regex::new(PAREN_OPEN_RE),
            Self::ParenClose => Regex::new(PAREN_CLOSE_RE),
            Self::BraceOpen => Regex::new(BRACE_OPEN_RE),
            Self::BraceClose => Regex::new(BRACE_CLOSE_RE),
            Self::Semicolon => Regex::new(SEMICOLON_RE),
            Self::Complement => Regex::new(COMPLEMENT_RE),
            Self::Decrement => Regex::new(DECREMENT_RE),
            Self::Minus => Regex::new(MINUS_RE),
            Self::Plus => Regex::new(PLUS_RE),
            Self::Asterisk => Regex::new(ASTERISK_RE),
            Self::Slash => Regex::new(SLASH_RE),
            Self::Percent => Regex::new(PERCENT_RE),
            Self::And => Regex::new(AND_RE),
            Self::Or => Regex::new(OR_RE),
            Self::NotEqual => Regex::new(NOT_EQUAL_RE),
            Self::Not => Regex::new(NOT_RE),
            Self::GreaterThan => Regex::new(GREATER_THAN_RE),
            Self::LessThan => Regex::new(LESS_THAN_RE),
            Self::GreaterEqualThan => Regex::new(GREATER_EQUAL_THAN_RE),
            Self::LessEqualThan => Regex::new(LESS_EQUAL_THAN_RE),
            Self::Equal => Regex::new(EQUAL_RE),
            Self::Assignment => Regex::new(ASSIGNMENT_RE),
            Self::If => Regex::new(IF_RE),
            Self::Else => Regex::new(ELSE_RE),
            Self::Question => Regex::new(QUESTION_RE),
            Self::Colon => Regex::new(COLON_RE),
            Self::Comma => Regex::new(COMMA_RE),
        }
        .unwrap()
    }

    pub fn precedence(&self) -> u32 {
        match self {
            &TokenKind::Asterisk | &TokenKind::Slash | &TokenKind::Percent => 50,
            &TokenKind::Plus | &TokenKind::Minus => 45,
            &TokenKind::LessThan
            | &TokenKind::GreaterThan
            | &TokenKind::LessEqualThan
            | &TokenKind::GreaterEqualThan => 35,
            &TokenKind::Equal | &TokenKind::NotEqual => 30,
            &TokenKind::And => 10,
            &TokenKind::Or => 5,
            &TokenKind::Question => 3,
            &TokenKind::Assignment => 1,
            _ => panic!("Token {:?} does not support precedence.", self),
        }
    }

    pub fn is_binary_operator(&self) -> bool {
        match self {
            &TokenKind::Asterisk
            | &TokenKind::Slash
            | &TokenKind::Percent
            | &TokenKind::LessThan
            | &TokenKind::GreaterThan
            | &TokenKind::LessEqualThan
            | &TokenKind::GreaterEqualThan
            | &TokenKind::Equal
            | &TokenKind::NotEqual
            | &TokenKind::And
            | &TokenKind::Or
            | &TokenKind::Question
            | &TokenKind::Assignment
            | &TokenKind::Plus
            | &TokenKind::Minus => true,
            _ => false,
        }
    }

    pub fn is_unary_operator(&self) -> bool {
        match self {
            &TokenKind::Complement | &TokenKind::Minus | &TokenKind::Not => true,
            _ => false,
        }
    }
}

struct Lexer<'a> {
    source: &'a str,
}

impl<'a> Lexer<'a> {
    pub fn new(source: &str) -> Lexer {
        Lexer {
            source: source.trim(),
        }
    }

    pub fn tokenize(&mut self) -> Vec<TokenKind> {
        let mut tokens = vec![];

        let patterns: Vec<Regex> = TokenKind::iter().map(|t| t.to_regex()).collect();

        while !&self.source.is_empty() {
            let mut token_found = false;

            for pattern in &patterns {
                if let Some(mat) = pattern.find(&self.source) {
                    if mat.start() == 0 {
                        tokens.push(TokenKind::from_str(mat.as_str()).unwrap());
                        self.source = &self.source[mat.end()..].trim();
                        token_found = true;
                    }
                }
            }

            if !token_found {
                log::debug!("Parsed tokens: {:?}", tokens);
                log::error!("Syntax Error: Could not parse token");
                let error_source: String = self
                    .source
                    .to_owned()
                    .lines()
                    .take(2)
                    .collect::<Vec<&str>>()
                    .join("\n");
                log::error!("At source: \n{}", error_source);
                panic!("Syntax Error");
            }
        }
        tokens
    }
}

pub fn run_lexer(source: String) -> Vec<TokenKind> {
    let mut lexer = Lexer::new(&source.trim());
    lexer.tokenize()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn tokenize_identifier() {
        assert_eq!(
            TokenKind::from_str("test"),
            Some(TokenKind::Identifier("test".to_owned()))
        );
        assert_eq!(
            TokenKind::from_str("main"),
            Some(TokenKind::Identifier("main".to_owned()))
        );
        assert_eq!(
            TokenKind::from_str("ma_n"),
            Some(TokenKind::Identifier("ma_n".to_owned()))
        );
        assert_eq!(TokenKind::from_str("53main"), None);
        assert_eq!(TokenKind::from_str("ma.in"), None);
    }

    #[test]
    fn tokenize_constant() {
        assert_eq!(TokenKind::from_str("66"), Some(TokenKind::Constant(66)));
        assert_eq!(TokenKind::from_str("32"), Some(TokenKind::Constant(32)));
    }

    #[test]
    fn tokenize_void() {
        assert_eq!(TokenKind::from_str("void").unwrap(), TokenKind::Void);
    }

    #[test]
    fn tokenize_return() {
        assert_eq!(TokenKind::from_str("return").unwrap(), TokenKind::Return);
    }

    #[test]
    fn tokenize_decrement() {
        assert_eq!(TokenKind::from_str("--").unwrap(), TokenKind::Decrement);
    }

    #[test]
    fn tokenize_complement() {
        assert_eq!(TokenKind::from_str("~").unwrap(), TokenKind::Complement);
    }

    #[test]
    fn tokenize_semicolon() {
        assert_eq!(TokenKind::from_str(";").unwrap(), TokenKind::Semicolon);
    }

    #[test]
    fn tokenize_brace_close() {
        assert_eq!(TokenKind::from_str("}").unwrap(), TokenKind::BraceClose);
    }

    #[test]
    fn tokenize_brace_open() {
        assert_eq!(TokenKind::from_str("{").unwrap(), TokenKind::BraceOpen);
    }

    #[test]
    fn tokenize_paren_close() {
        assert_eq!(TokenKind::from_str(")").unwrap(), TokenKind::ParenClose);
    }

    #[test]
    fn tokenize_paren_open() {
        assert_eq!(TokenKind::from_str("(").unwrap(), TokenKind::ParenOpen);
    }

    #[test]
    fn tokenize_minus() {
        assert_eq!(TokenKind::from_str("-").unwrap(), TokenKind::Minus);
    }

    #[test]
    fn tokenize_plus() {
        assert_eq!(TokenKind::from_str("+").unwrap(), TokenKind::Plus);
    }

    #[test]
    fn tokenize_asterisk() {
        assert_eq!(TokenKind::from_str("*").unwrap(), TokenKind::Asterisk);
    }

    #[test]
    fn tokenize_slash() {
        assert_eq!(TokenKind::from_str("/").unwrap(), TokenKind::Slash);
    }

    #[test]
    fn tokenize_percent() {
        assert_eq!(TokenKind::from_str("%").unwrap(), TokenKind::Percent);
    }
}
