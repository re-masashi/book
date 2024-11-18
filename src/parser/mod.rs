use crate::lexer::tokens::{Token, TokenType};
use crate::interpreter::Expr;

use std::iter::Peekable;
use std::vec::IntoIter;
use std::marker::PhantomData;
use std::cell::RefCell;

pub mod expression;
pub mod function;
pub mod program;

type TokenIter = Peekable<IntoIter<Token>>;

#[derive(Debug, Clone)]
pub struct NodePosition {
    pub pos: usize,
    pub line_no: usize,
    pub file: String,
}

// 'def' name (args) '->' return_type { expressions}
#[derive(Debug)]
pub struct Function<'a> {
    pub name: String,
    pub args: Vec<(String, String)>,
    pub expression: Expr<'a>,
    pub return_type: String,
}

// 'class' name { functions }
#[derive(Debug)]
pub struct Class<'a> {
    pub name: String,
    pub fns: Vec<(Function<'a>, NodePosition)>,
}

/// A parser that generates an abstract syntax tree (AST).
pub struct Parser<'a> {
    tokens: TokenIter,
    pos: i32,
    line_no: i32,
    file: String,
    phantom: PhantomData<&'a i32>,
}

impl Parser<'_> {
    pub fn new(tokens: TokenIter, file_path: &str) -> Self {
        Parser {
            tokens: tokens,
            pos: -1,
            line_no: 1,
            file: file_path.to_string(),
            phantom: PhantomData
        }
    }

    pub fn get_tok_precedence(&mut self, tok: TokenType) -> i32 {
        match tok {
            TokenType::Equal
            | TokenType::NotEq
            | TokenType::Greater
            | TokenType::GreaterEq
            | TokenType::Less
            | TokenType::LessEq => 0,
            TokenType::Minus | TokenType::Plus => 1,
            TokenType::DivEq | TokenType::Mul => 2,
            any => panic!("Bad operator! Unknown {:?}", any),
        }
    }

    fn advance(&mut self) -> Token {  // Make `self` mutable
        //  Use `as_ref()` to get shared references inside the closure:
        let next_token = self.tokens.next().expect("Reached end of tokens unexpectedly"); // Expect instead of panic, with better error
        self.pos = next_token.pos; // Assign directly
        self.line_no = next_token.line_no;
        next_token
    }
}

#[derive(Debug)]
pub struct ParserError {
    pos: NodePosition,
    line_no: usize,
    file: String,
    message: String
}

impl ParserError {
    fn new(line_no: usize, pos: NodePosition, file: String, message: String) -> ParserError {
        ParserError { line_no, pos, file, message }
    }
}
