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
