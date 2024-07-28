use core::panic;

use regex::Regex;
use strum::IntoEnumIterator;
use strum_macros::{EnumIter, EnumIs};

static IDENTIFIER_RE: &str  = r"[[:alpha:]_]\w*\b";
static CONSTANT_RE: &str    = r"[[:digit:]]+\b";
static RETURN_RE: &str      = r"return\b";
static INT_RE: &str         = r"int\b";
static VOID_RE: &str        = r"void\b";
static PAREN_OPEN_RE: &str  = r"\(";
static PAREN_CLOSE_RE: &str = r"\)";
static BRACE_OPEN_RE: &str  = r"\{";
static BRACE_CLOSE_RE: &str = r"\}";
static SEMICOLON_RE: &str   = r";";
static COMPLEMENT_RE: &str  = r"~";
static NEGATION_RE: &str    = r"-";
static DECREMENT_RE: &str   = r"--";

#[derive(EnumIter, EnumIs, Debug, strum_macros::Display, PartialEq, Clone)]
pub enum TokenKind {
    Identifier(String),
    Constant(i64),

    Return,

    Int,
    Void,

    ParenOpen,
    ParenClose,
    BraceOpen,
    BraceClose,
    Semicolon,

    Complement,
    Negation,
    Decrement
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

        log::trace!("Trying to tokenize input {:?}", input);

        match input {
            input if TokenKind::is_full_match(input, INT_RE) =>
                Some(Self::Int),
            input if TokenKind::is_full_match(input, VOID_RE) =>
                Some(Self::Void),
            input if TokenKind::is_full_match(input, RETURN_RE) =>
                Some(Self::Return),
            input if TokenKind::is_full_match(input, PAREN_CLOSE_RE) =>
                Some(Self::ParenClose),
            input if TokenKind::is_full_match(input, PAREN_OPEN_RE) =>
                Some(Self::ParenOpen),
            input if TokenKind::is_full_match(input, BRACE_OPEN_RE) =>
                Some(Self::BraceOpen),
            input if TokenKind::is_full_match(input, BRACE_CLOSE_RE) =>
                Some(Self::BraceClose),
            input if TokenKind::is_full_match(input, SEMICOLON_RE) =>
                Some(Self::Semicolon),
            input if TokenKind::is_full_match(input, DECREMENT_RE) =>
                Some(Self::Decrement),
            input if TokenKind::is_full_match(input, COMPLEMENT_RE) =>
                Some(Self::Complement),
            input if TokenKind::is_full_match(input, NEGATION_RE) =>
                Some(Self::Negation),
            input if TokenKind::is_full_match(input, CONSTANT_RE) =>
                Some(Self::Constant(input.parse::<i64>().unwrap())),
            input if TokenKind::is_full_match(input, IDENTIFIER_RE) =>
                Some(Self::Identifier(input.to_string())),
            _ => None
        }
    }

    pub fn to_regex(&self) -> Regex {
        match self {
            Self::Identifier(_) => Regex::new(IDENTIFIER_RE).unwrap(),
            Self::Constant(_) => Regex::new(CONSTANT_RE).unwrap(),

            Self::Return => Regex::new(RETURN_RE).unwrap(),

            Self::Int => Regex::new(INT_RE).unwrap(),
            Self::Void => Regex::new(VOID_RE).unwrap(),

            Self::ParenOpen => Regex::new(PAREN_OPEN_RE).unwrap(),
            Self::ParenClose => Regex::new(PAREN_CLOSE_RE).unwrap(),
            Self::BraceOpen => Regex::new(BRACE_OPEN_RE).unwrap(),
            Self::BraceClose => Regex::new(BRACE_CLOSE_RE).unwrap(),
            Self::Semicolon => Regex::new(SEMICOLON_RE).unwrap(),

            Self::Complement => Regex::new(COMPLEMENT_RE).unwrap(),
            Self::Negation => Regex::new(NEGATION_RE).unwrap(),
            Self::Decrement => Regex::new(DECREMENT_RE).unwrap(),
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
                let error_source: String = self.source.to_owned()
                    .lines().take(2).collect::<Vec<&str>>().join("\n");
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
        assert_eq!(TokenKind::from_str("test"), Some(TokenKind::Identifier("test".to_owned())));
        assert_eq!(TokenKind::from_str("main"), Some(TokenKind::Identifier("main".to_owned())));
        assert_eq!(TokenKind::from_str("ma_n"), Some(TokenKind::Identifier("ma_n".to_owned())));
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
    fn tokenize_negation() {
        assert_eq!(TokenKind::from_str("-").unwrap(), TokenKind::Negation);
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
}
