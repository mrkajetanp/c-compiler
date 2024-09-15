use crate::codegen::x86_64::CodegenError;
use crate::lexer::LexerError;
use crate::parser::ast::ParserError;
use crate::semantic::resolution::SemanticError;
use crate::semantic::typecheck::TypeCheckError;
use thiserror::Error;

#[derive(Error, Debug)]
pub enum ErrorKind {
    #[error("Lexer Failed\n{0}")]
    LexerError(LexerError),
    #[error("AST Parsing Failed\n{0}")]
    ParserError(ParserError),
    #[error("Semantic Analysis Failed\n{0}")]
    SemanticError(SemanticError),
    #[error("Type Checking Failed\n{0}")]
    TypeCheckError(TypeCheckError),
    #[error("Codegen Failed\n{0}")]
    CodegenError(CodegenError),
    #[error("IO Error\n{0}")]
    IOError(std::io::Error),
}

impl From<LexerError> for ErrorKind {
    fn from(error: LexerError) -> Self {
        Self::LexerError(error)
    }
}

impl From<ParserError> for ErrorKind {
    fn from(error: ParserError) -> Self {
        Self::ParserError(error)
    }
}

impl From<SemanticError> for ErrorKind {
    fn from(error: SemanticError) -> Self {
        Self::SemanticError(error)
    }
}

impl From<TypeCheckError> for ErrorKind {
    fn from(error: TypeCheckError) -> Self {
        Self::TypeCheckError(error)
    }
}

impl From<CodegenError> for ErrorKind {
    fn from(error: CodegenError) -> Self {
        Self::CodegenError(error)
    }
}

impl From<std::io::Error> for ErrorKind {
    fn from(error: std::io::Error) -> Self {
        Self::IOError(error)
    }
}
