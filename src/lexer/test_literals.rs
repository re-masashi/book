#[cfg(test)]
use crate::lexer::{tokens::TokenType, Lexer};

#[test]
fn test_identifier() {
    let lexer = Lexer::from_text("let x = 10;", "");
    let mut tokens = lexer.map(|t| t.unwrap()).collect::<Vec<_>>().into_iter();
    assert_eq!(tokens.next().unwrap().type_, TokenType::Let);
    assert_eq!(
        tokens.next().unwrap().type_,
        TokenType::Identifier("x".to_string())
    );
}

#[test]
fn test_integer_literal() {
    let lexer = Lexer::from_text("42", "");
    let tokens = lexer.map(|t| t.unwrap()).collect::<Vec<_>>();
    assert_eq!(tokens.first().unwrap().type_, TokenType::Int(42));
}

#[test]
fn test_integer_literal_underscores() {
    let lexer = Lexer::from_text("1_000_000", "");
    let tokens = lexer.map(|t| t.unwrap()).collect::<Vec<_>>();
    assert_eq!(tokens.first().unwrap().type_, TokenType::Int(1_000_000));
}

#[test]
fn test_float_literal() {
    let lexer = Lexer::from_text("42.0", "");
    let tokens = lexer.map(|t| t.unwrap()).collect::<Vec<_>>();
    assert_eq!(tokens.first().unwrap().type_, TokenType::Float(42.0));
}

#[test]
fn test_float_literal_underscores() {
    let lexer = Lexer::from_text("1_000_000.212", "");
    let tokens = lexer.map(|t| t.unwrap()).collect::<Vec<_>>();
    assert_eq!(
        tokens.first().unwrap().type_,
        TokenType::Float(1_000_000.212)
    );
}

#[test]
fn test_boolean_literals() {
    let lexer = Lexer::from_text("true false", "");
    let mut tokens = lexer.map(|t| t.unwrap()).collect::<Vec<_>>().into_iter();
    assert_eq!(tokens.next().unwrap().type_, TokenType::True);
    assert_eq!(tokens.next().unwrap().type_, TokenType::False);
}

#[test]
fn test_string_literals() {
    let lexer = Lexer::from_text("\"aaaaa\"", "");
    let tokens = lexer.map(|t| t.unwrap()).collect::<Vec<_>>();
    assert_eq!(
        tokens.first().unwrap().type_,
        TokenType::String("aaaaa".to_string())
    );
}

#[test]
#[should_panic]
fn test_invalid_input() {
    let lexer = Lexer::from_text("@#%", "");
    lexer.for_each(|t| {
        t.unwrap();
    });
}
