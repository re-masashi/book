use crate::lexer::tokens::{Token, TokenType};

use std::iter::Peekable;
use std::vec::IntoIter;

pub mod expression;
pub mod program;

type TokenIter = Peekable<IntoIter<Token>>;

#[derive(Debug)]
pub struct NodePosition {
    pub pos: i32,
    pub line_no: i32,
    pub file: String,
}

// the top-level
#[derive(Debug)]
pub enum AstNode {
    FunctionDef(Function),
    Class(Class),
    Expression(ExprValue),
}

// todo: remove the clone 
#[derive(Debug, Clone)]
pub enum ExprValue {
    FnCall(String, Vec<ExprValue>),
    UnOp(Box<TokenType>, Box<ExprValue>),
    BinOp(Box<ExprValue>, Box<TokenType>, Box<ExprValue>),
    Boolean(bool),
    Integer(i64),
    Float(f64),
    String(String),
    Identifier(String),
    IfElse {
        cond: Box<ExprValue>,
        if_: Box<ExprValue>,
        else_: Box<ExprValue>,
        type_: String,
    },
    Assign {
        name: String,
        value: Box<ExprValue>,
    },
    Use(String),
    Array(Vec<ExprValue>, String),
    Do(Vec<ExprValue>),
    Null // not available in the runtime
}

// 'def' name (args) '->' return_type { expressions}
#[derive(Debug)]
pub struct Function {
    pub name: String,
    pub args: Vec<(String, String)>,
    pub expression: ExprValue,
    pub return_type: String,
}

// 'class' name { functions }
#[derive(Debug)]
pub struct Class {
    pub name: String,
    pub fns: Vec<(Function, NodePosition)>,
}

/// A parser that generates an abstract syntax tree (AST).
pub struct Parser {
    tokens: TokenIter,
    pos: i32,
    line_no: i32,
    file: String,
}

impl Parser {
    pub fn new(tokens: TokenIter, file_path: &str) -> Self {
        Parser {
            tokens,
            pos: -1,
            line_no: 1,
            file: file_path.to_string(),
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

    fn advance(&mut self) {
        self.pos = match self.tokens.peek() {
            Some(t) => t,
            None => panic!("Dunno"),
        }
        .pos;
        self.line_no = match self.tokens.peek() {
            Some(t) => t,
            None => panic!("Dunno"),
        }
        .line_no;
        // self.tokens.next().expect("sumn went wrong idk why")
    }

    fn parser_error(&self, cause: &str) -> String {
//         format!(
//             "
// {text}
// {pointy}
// {cause}

//     at {line}:{pos} in file `{file}`.",
//             text = read_to_string(self.file.clone())
//                 .unwrap()
//                 .lines()
//                 .collect::<Vec<_>>()[(self.line_no - 1) as usize],
//             pointy = ("~".repeat(self.pos as usize) + "^"),
//             cause = cause,
//             line = self.line_no,
//             pos = self.pos,
//             file = self.file
//         )
//         .to_string()
		cause.to_string()
    }
}