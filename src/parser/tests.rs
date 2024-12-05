use crate::codegen::Literal;
use crate::lexer::Lexer;
use crate::parser::*;

macro_rules! parse {
    ($source: expr, $expected: expr) => {
        let lexer = Lexer::from_text($source, "##test##");
        let mut tokens = lexer.map(|t| t.unwrap()).collect::<Vec<_>>();
        tokens.push(Token {
            type_: TokenType::Int(0),
            span: Span((1, 0), (1, 0)),
            file: "##test##".to_string(),
        }); // HACK: adding a blank expression to the end of the parser. else, the last expression isn't parsed
        assert_eq!(Parser::new(tokens.into_iter().peekable(), "##test##").parse_expression().unwrap(), $expected)
    };
    ($source_path: expr) => {
        let lexer = Lexer::from_file($source_path).unwrap();
        let mut tokens = lexer.map(|t| t.unwrap()).collect::<Vec<_>>();
        tokens.push(Token {
            type_: TokenType::Int(0),
            span: Span((1, 0), (1, 0)),
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

#[test]
fn test_tuples() {
    parse!(
        "(1,2,3)",
        Expr::Tuple(vec![
            Expr::Literal(Literal::Int(1).into()),
            Expr::Literal(Literal::Int(2).into()),
            Expr::Literal(Literal::Int(3).into()),
        ])
    );
}

#[test]
fn test_call() {
    parse!(
        "x(1,)()()",
        Expr::Call(
            Box::new(Expr::Call(
                Box::new(Expr::Call(
                    Box::new(Expr::Variable("x".into())),
                    vec![Expr::Literal(Literal::Int(1).into()),]
                )),
                vec![]
            )),
            vec![]
        )
    );
}

#[test]
fn test_index() {
    parse!(
        "x[0]",
        Expr::Index(
            Box::new(Expr::Variable("x".into())),
            Box::new(Expr::Literal(Literal::Int(0).into()),),
        )
    );
}

#[test]
fn test_array() {
    parse!(
        "[1,2,3,]",
        Expr::Array(vec![
            Expr::Literal(Literal::Int(1).into()),
            Expr::Literal(Literal::Int(2).into()),
            Expr::Literal(Literal::Int(3).into()),
        ])
    );
    parse!("[]", Expr::Array(vec![]));
    parse!(
        "[1,2,3]",
        Expr::Array(vec![
            Expr::Literal(Literal::Int(1).into()),
            Expr::Literal(Literal::Int(2).into()),
            Expr::Literal(Literal::Int(3).into()),
        ])
    );
    parse!(
        "[1,]",
        Expr::Array(vec![Expr::Literal(Literal::Int(1).into()),])
    );
    parse!(
        "[21213]",
        Expr::Array(vec![Expr::Literal(Literal::Int(21213).into()),])
    );
}
