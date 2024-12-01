#[cfg(test)]
use crate::lexer::{Lexer, tokens::TokenType};

#[test]
fn test_keyword_if() {
    let lexer = Lexer::from_text("if", "");
    let mut tokens = lexer.map(|t| t.unwrap()).collect::<Vec<_>>().into_iter();
    assert_eq!(tokens.next().unwrap().type_, TokenType::If);
}

#[test]
fn test_keyword_else() {
    let lexer = Lexer::from_text("else", "");
    let mut tokens = lexer.map(|t| t.unwrap()).collect::<Vec<_>>().into_iter();
    assert_eq!(tokens.next().unwrap().type_, TokenType::Else);
}

#[test]
fn test_keyword_let() {
    let lexer = Lexer::from_text("let", "");
    let mut tokens = lexer.map(|t| t.unwrap()).collect::<Vec<_>>().into_iter();
    assert_eq!(tokens.next().unwrap().type_, TokenType::Let);
}

#[test]
fn test_keyword_fn() {
    let lexer = Lexer::from_text("fn", "");
    let mut tokens = lexer.map(|t| t.unwrap()).collect::<Vec<_>>().into_iter();
    assert_eq!(tokens.next().unwrap().type_, TokenType::Fn);
}

#[test]
fn test_keyword_def() {
    let lexer = Lexer::from_text("def", "");
    let mut tokens = lexer.map(|t| t.unwrap()).collect::<Vec<_>>().into_iter();
    assert_eq!(tokens.next().unwrap().type_, TokenType::Def);
}

#[test]
fn test_keyword_do() {
    let lexer = Lexer::from_text("do", "");
    let mut tokens = lexer.map(|t| t.unwrap()).collect::<Vec<_>>().into_iter();
    assert_eq!(tokens.next().unwrap().type_, TokenType::Do);
}

#[test]
fn test_keyword_end() {
    let lexer = Lexer::from_text("end", "");
    let mut tokens = lexer.map(|t| t.unwrap()).collect::<Vec<_>>().into_iter();
    assert_eq!(tokens.next().unwrap().type_, TokenType::End);
}

#[test]
fn test_keyword_then() {
    let lexer = Lexer::from_text("then", "");
    let mut tokens = lexer.map(|t| t.unwrap()).collect::<Vec<_>>().into_iter();
    assert_eq!(tokens.next().unwrap().type_, TokenType::Then);
}

#[test]
fn test_keyword_use() {
    let lexer = Lexer::from_text("use", "");
    let mut tokens = lexer.map(|t| t.unwrap()).collect::<Vec<_>>().into_iter();
    assert_eq!(tokens.next().unwrap().type_, TokenType::Use);
}

#[test]
fn test_keyword_struct() {
    let lexer = Lexer::from_text("struct", "");
    let mut tokens = lexer.map(|t| t.unwrap()).collect::<Vec<_>>().into_iter();
    assert_eq!(tokens.next().unwrap().type_, TokenType::Struct);
}

#[test]
fn test_keyword_extern() {
    let lexer = Lexer::from_text("extern", "");
    let mut tokens = lexer.map(|t| t.unwrap()).collect::<Vec<_>>().into_iter();
    assert_eq!(tokens.next().unwrap().type_, TokenType::Extern);
}