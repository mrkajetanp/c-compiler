use core::panic;

use regex::Regex;
use strum::IntoEnumIterator;
use strum_macros::{EnumIter, EnumIs};

static IDENTIFIER_RE: &str  = r"[a-zA-Z_]\w*\b";
static CONSTANT_RE: &str    = r"[0-9]+\b";
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
    pub fn from_str(input: &str) -> Option<TokenKind> {
        let input = input.trim();

        match input {
            input if Regex::new(INT_RE).unwrap()
                .is_match(input) => Some(Self::Int),
            input if Regex::new(VOID_RE).unwrap()
                .is_match(input) => Some(Self::Void),
            input if Regex::new(RETURN_RE).unwrap()
                .is_match(input) => Some(Self::Return),
            input if Regex::new(PAREN_OPEN_RE).unwrap()
                .is_match(input) => Some(Self::ParenOpen),
            input if Regex::new(PAREN_CLOSE_RE).unwrap()
                .is_match(input) => Some(Self::ParenClose),
            input if Regex::new(BRACE_OPEN_RE).unwrap()
                .is_match(input) => Some(Self::BraceOpen),
            input if Regex::new(BRACE_CLOSE_RE).unwrap()
                .is_match(input) => Some(Self::BraceClose),
            input if Regex::new(SEMICOLON_RE).unwrap()
                .is_match(input) => Some(Self::Semicolon),
            input if Regex::new(DECREMENT_RE).unwrap()
                .is_match(input) => Some(Self::Decrement),
            input if Regex::new(COMPLEMENT_RE).unwrap()
                .is_match(input) => Some(Self::Complement),
            input if Regex::new(NEGATION_RE).unwrap()
                .is_match(input) => Some(Self::Negation),
            input if Regex::new(CONSTANT_RE).unwrap()
                .is_match(input) => Some(Self::Constant(input.parse::<i64>().unwrap())),
            input if Regex::new(IDENTIFIER_RE).unwrap()
                .is_match(input) => Some(Self::Identifier(input.to_string())),
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
                println!("parsed tokens: {:?}", tokens);
                panic!("Syntax Error: No tokens matching source \n{:?}", self.source);
            }
        }
        tokens
    }
}

pub fn run_lexer(source: String) -> Vec<TokenKind> {
    let mut lexer = Lexer::new(&source.trim());
    lexer.tokenize()
}
