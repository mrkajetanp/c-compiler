extern crate rcc;

use rcc::lexer::TokenKind;
use rcc::*;
use serial_test::serial;
use std::fs;

static BASIC_SAMPLE: &str = "samples/basic.c";
static UNARY_SAMPLE: &str = "samples/unary.c";

#[test]
#[serial]
fn test_basic_lex() {
    let driver = Driver::new(BASIC_SAMPLE);
    let preprocessed_path = driver.preprocess();
    let source = fs::read_to_string(preprocessed_path).unwrap();

    let tokens_expected: Vec<TokenKind> = vec![
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
    ];

    let tokens = driver.lex(source);
    assert_eq!(tokens, tokens_expected);
}

#[test]
#[serial]
fn test_basic_parse() {
    let driver = Driver::new(BASIC_SAMPLE);
    driver.compile(CompileStage::Parse, true, false).unwrap();
}

#[test]
#[serial]
fn test_basic_ir() {
    let driver = Driver::new(BASIC_SAMPLE);
    driver.compile(CompileStage::IR, true, false).unwrap();
}

#[test]
#[serial]
fn test_basic_codegen() {
    let driver = Driver::new(BASIC_SAMPLE);
    driver.compile(CompileStage::Codegen, true, false).unwrap();
}

#[test]
#[serial]
fn test_basic_full() {
    let driver = Driver::new(BASIC_SAMPLE);
    driver.compile(CompileStage::Full, true, false).unwrap();
    driver.clean_binary().unwrap();
}

#[test]
#[serial]
fn test_unary_lex() {
    let driver = Driver::new(UNARY_SAMPLE);
    let preprocessed_path = driver.preprocess();
    let source = fs::read_to_string(preprocessed_path).unwrap();

    let tokens_expected: Vec<TokenKind> = vec![
        TokenKind::Int,
        TokenKind::Identifier("main".to_owned()),
        TokenKind::ParenOpen,
        TokenKind::Void,
        TokenKind::ParenClose,
        TokenKind::BraceOpen,
        TokenKind::Return,
        TokenKind::Complement,
        TokenKind::ParenOpen,
        TokenKind::Minus,
        TokenKind::Constant(2),
        TokenKind::ParenClose,
        TokenKind::Semicolon,
        TokenKind::BraceClose,
    ];

    let tokens = driver.lex(source);
    assert_eq!(tokens, tokens_expected);
}

#[test]
#[serial]
fn test_unary_parse() {
    let driver = Driver::new(UNARY_SAMPLE);
    driver.compile(CompileStage::Parse, true, false).unwrap();
}

#[test]
#[serial]
fn test_unary_ir() {
    let driver = Driver::new(UNARY_SAMPLE);
    driver.compile(CompileStage::IR, true, false).unwrap();
}

#[test]
#[serial]
fn test_unary_codegen() {
    let driver = Driver::new(UNARY_SAMPLE);
    driver.compile(CompileStage::Codegen, true, false).unwrap();
}

#[test]
#[serial]
fn test_unary_full() {
    let driver = Driver::new(UNARY_SAMPLE);
    driver.compile(CompileStage::Full, true, false).unwrap();
    driver.clean_binary().unwrap();
}
