#[cfg(test)]
use crate::lexer::{Lexer, tokens::TokenType};

#[test]
fn test_punctuator_lparen() {
    let lexer = Lexer::from_text("(", "");
    let mut tokens = lexer.map(|t| t.unwrap()).collect::<Vec<_>>().into_iter();
    assert_eq!(tokens.next().unwrap().type_, TokenType::LParen);
}

#[test]
fn test_punctuator_rparen() {
    let lexer = Lexer::from_text(")", "");
    let mut tokens = lexer.map(|t| t.unwrap()).collect::<Vec<_>>().into_iter();
    assert_eq!(tokens.next().unwrap().type_, TokenType::RParen);
}

#[test]
fn test_punctuator_lbrack() {
    let lexer = Lexer::from_text("[1, 2, 3]", "");
    let mut tokens = lexer.map(|t| t.unwrap()).collect::<Vec<_>>().into_iter();
    assert_eq!(tokens.next().unwrap().type_, TokenType::LBrack);
}

#[test]
fn test_punctuator_rbrack() {
    let lexer = Lexer::from_text("]", "");
    let mut tokens = lexer.map(|t| t.unwrap()).collect::<Vec<_>>().into_iter();
    assert_eq!(tokens.next().unwrap().type_, TokenType::RBrack);
}

#[test]
fn test_punctuator_lbrace() {
    let lexer = Lexer::from_text("{", "");
    let mut tokens = lexer.map(|t| t.unwrap()).collect::<Vec<_>>().into_iter();
    assert_eq!(tokens.next().unwrap().type_, TokenType::LBrace);
}

#[test]
fn test_punctuator_rbrace() {
    let lexer = Lexer::from_text("}", "");
    let mut tokens = lexer.map(|t| t.unwrap()).collect::<Vec<_>>().into_iter();
    assert_eq!(tokens.next().unwrap().type_, TokenType::RBrace);
}

#[test]
fn test_punctuator_colon() {
    let lexer = Lexer::from_text(":", "");
    let mut tokens = lexer.map(|t| t.unwrap()).collect::<Vec<_>>().into_iter();
    assert_eq!(tokens.next().unwrap().type_, TokenType::Colon);
}

#[test]
fn test_punctuator_semicolon() {
    let lexer = Lexer::from_text(";", "");
    let mut tokens = lexer.map(|t| t.unwrap()).collect::<Vec<_>>().into_iter();
    assert_eq!(tokens.next().unwrap().type_, TokenType::Semicolon);
}

#[test]
fn test_punctuator_comma() {
    let lexer = Lexer::from_text(",", "");
    let mut tokens = lexer.map(|t| t.unwrap()).collect::<Vec<_>>().into_iter();
    assert_eq!(tokens.next().unwrap().type_, TokenType::Comma);
}

#[test]
fn test_punctuator_dot() {
    let lexer = Lexer::from_text(".", "");
    let mut tokens = lexer.map(|t| t.unwrap()).collect::<Vec<_>>().into_iter();
    assert_eq!(tokens.next().unwrap().type_, TokenType::Dot);
}

#[test]
fn test_punctuator_arrow() {
    let lexer = Lexer::from_text("-> i32", "");
    let mut tokens = lexer.map(|t| t.unwrap()).collect::<Vec<_>>().into_iter();
    assert_eq!(tokens.next().unwrap().type_, TokenType::Arrow);
}