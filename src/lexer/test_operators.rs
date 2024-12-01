#[cfg(test)]
use crate::lexer::{Lexer, tokens::TokenType};

#[test]
fn test_operator_plus() {
    let lexer = Lexer::from_text("x + y", "");
    let mut tokens = lexer.map(|t| t.unwrap()).collect::<Vec<_>>().into_iter();
    assert_eq!(tokens.next().unwrap().type_, TokenType::Identifier("x".to_string()));
    assert_eq!(tokens.next().unwrap().type_, TokenType::Plus);
    assert_eq!(tokens.next().unwrap().type_, TokenType::Identifier("y".to_string()));
}

#[test]
fn test_operator_minus() {
    let lexer = Lexer::from_text("a - b", "");
    let mut tokens = lexer.map(|t| t.unwrap()).collect::<Vec<_>>().into_iter();
    assert_eq!(tokens.next().unwrap().type_, TokenType::Identifier("a".to_string()));
    assert_eq!(tokens.next().unwrap().type_, TokenType::Minus);
    assert_eq!(tokens.next().unwrap().type_, TokenType::Identifier("b".to_string()));
}

#[test]
fn test_operator_mul() {
    let lexer = Lexer::from_text("a * b", "");
    let mut tokens = lexer.map(|t| t.unwrap()).collect::<Vec<_>>().into_iter();
    assert_eq!(tokens.next().unwrap().type_, TokenType::Identifier("a".to_string()));
    assert_eq!(tokens.next().unwrap().type_, TokenType::Mul);
    assert_eq!(tokens.next().unwrap().type_, TokenType::Identifier("b".to_string()));
}

#[test]
fn test_operator_div() {
    let lexer = Lexer::from_text("a / b", "");
    let mut tokens = lexer.map(|t| t.unwrap()).collect::<Vec<_>>().into_iter();
    assert_eq!(tokens.next().unwrap().type_, TokenType::Identifier("a".to_string()));
    assert_eq!(tokens.next().unwrap().type_, TokenType::Div);
    assert_eq!(tokens.next().unwrap().type_, TokenType::Identifier("b".to_string()));
}

#[test]
fn test_operator_equal() {
    let lexer = Lexer::from_text("a == b", "");
    let mut tokens = lexer.map(|t| t.unwrap()).collect::<Vec<_>>().into_iter();
    assert_eq!(tokens.next().unwrap().type_, TokenType::Identifier("a".to_string()));
    assert_eq!(tokens.next().unwrap().type_, TokenType::Equal);
    assert_eq!(tokens.next().unwrap().type_, TokenType::Identifier("b".to_string()));
}

#[test]
fn test_operator_not_equal() {
    let lexer = Lexer::from_text("a != b", "");
    let mut tokens = lexer.map(|t| t.unwrap()).collect::<Vec<_>>().into_iter();
    assert_eq!(tokens.next().unwrap().type_, TokenType::Identifier("a".to_string()));
    assert_eq!(tokens.next().unwrap().type_, TokenType::NotEq);
    assert_eq!(tokens.next().unwrap().type_, TokenType::Identifier("b".to_string()));
}

#[test]
fn test_operator_less_than() {
    let lexer = Lexer::from_text("a < b", "");
    let mut tokens = lexer.map(|t| t.unwrap()).collect::<Vec<_>>().into_iter();
    assert_eq!(tokens.next().unwrap().type_, TokenType::Identifier("a".to_string()));
    assert_eq!(tokens.next().unwrap().type_, TokenType::Less);
    assert_eq!(tokens.next().unwrap().type_, TokenType::Identifier("b".to_string()));
}

#[test]
fn test_operator_greater_than() {
    let lexer = Lexer::from_text("a > b", "");
    let mut tokens = lexer.map(|t| t.unwrap()).collect::<Vec<_>>().into_iter();
    assert_eq!(tokens.next().unwrap().type_, TokenType::Identifier("a".to_string()));
    assert_eq!(tokens.next().unwrap().type_, TokenType::Greater);
    assert_eq!(tokens.next().unwrap().type_, TokenType::Identifier("b".to_string()));
}

#[test]
fn test_operator_less_than_equal() {
    let lexer = Lexer::from_text("a <= b", "");
    let mut tokens = lexer.map(|t| t.unwrap()).collect::<Vec<_>>().into_iter();
    assert_eq!(tokens.next().unwrap().type_, TokenType::Identifier("a".to_string()));
    assert_eq!(tokens.next().unwrap().type_, TokenType::LessEq);
    assert_eq!(tokens.next().unwrap().type_, TokenType::Identifier("b".to_string()));
}

#[test]
fn test_operator_greater_than_equal() {
    let lexer = Lexer::from_text("a >= b", "");
    let mut tokens = lexer.map(|t| t.unwrap()).collect::<Vec<_>>().into_iter();
    assert_eq!(tokens.next().unwrap().type_, TokenType::Identifier("a".to_string()));
    assert_eq!(tokens.next().unwrap().type_, TokenType::GreaterEq);
    assert_eq!(tokens.next().unwrap().type_, TokenType::Identifier("b".to_string()));
}

#[test]
fn test_operator_not() {
    let lexer = Lexer::from_text("!a", "");
    let mut tokens = lexer.map(|t| t.unwrap()).collect::<Vec<_>>().into_iter();
    assert_eq!(tokens.next().unwrap().type_, TokenType::Not);
    assert_eq!(tokens.next().unwrap().type_, TokenType::Identifier("a".to_string()));
}

#[test]
fn test_operator_negate() {
    let lexer = Lexer::from_text("-1", "");
    let mut tokens = lexer.map(|t| t.unwrap()).collect::<Vec<_>>().into_iter();
    assert_eq!(tokens.next().unwrap().type_, TokenType::Minus);
    assert_eq!(tokens.next().unwrap().type_, TokenType::Int(1));
}

// sneaked this into this file
#[test]
fn test_comment() {
    let lexer = Lexer::from_text("# a == b", "");
    let mut tokens = lexer.map(|t| t.unwrap()).collect::<Vec<_>>().into_iter();
    assert!(tokens.next().is_none());
}