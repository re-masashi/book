use crate::lexer::Lexer;
use crate::parser::*;

macro_rules! parse {
    ($source_path: expr) => {
        let lexer = Lexer::from_file($source_path).unwrap();
        let mut tokens = lexer.map(|t| t.unwrap()).collect::<Vec<_>>();
        tokens.push(Token {
            type_: TokenType::Int(0),
            pos: 1,
            line_no: 0,
            file: $source_path.to_string(),
        }); // HACK: adding a blank expression to the end of the parser. else, the last expression isn't parsed
        assert!(Parser::new(tokens.into_iter().peekable(), $source_path).parse_program().is_ok())
    }
}

#[test]
fn test_comments() {
    parse!("test_inputs/test_comments.bk");
}

#[test]
fn test_expressions() {
    parse!("test_inputs/test_expressions.bk");
}

#[test]
fn test_function() {
    parse!("test_inputs/test_function.bk");
}

#[test]
fn test_structs() {
    parse!("test_inputs/test_structs.bk");
}

#[test]
#[should_panic]
fn test_invalid_if() {
    parse!("test_inputs/test_invalid_if.bk");
}
