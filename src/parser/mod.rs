use owo_colors::OwoColorize;

use crate::interpreter::Expr;
use crate::lexer::tokens::{Token, TokenType};

use std::iter::Peekable;
use std::marker::PhantomData;
use std::vec::IntoIter;
use std::fs::read_to_string;

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
            tokens,
            pos: -1,
            line_no: 1,
            file: file_path.to_string(),
            phantom: PhantomData,
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

    fn advance(&mut self) -> Token {
        let next_token = self
            .tokens
            .next()
            .expect("Reached end of tokens unexpectedly");
        self.pos = next_token.pos; // Assign directly
        self.line_no = next_token.line_no;
        next_token
    }

    pub fn error(&self, message: String) {
        let file_contents = match read_to_string(&self.file){
            Ok(f)=>f,
            Err(e)=>{
                eprintln!("Error: Could not open file: {}. {}", self.file.green(), e);
                return
            }
        };

        let file_lines: Vec<_> = file_contents.lines().collect();
        let line_index = self.line_no as usize - 1;

        // More concise and safe way to get lines before/after
        let pre_line = if line_index > 1{
                format!(
                    "{} | {}\n",
                    line_index,
                    file_lines[line_index]
                )
        }else{
            "".to_string()
        };
        let current_line = file_lines.get(line_index).unwrap_or(&"");
        let post_line = file_lines.get(line_index + 1).unwrap_or(&"");


        let pointy_binding = format!("{:~<width$}^", "", width = (self.pos+1) as usize);
        let pointy = pointy_binding.red();

        let error_message = format!(
            "\n\
             {}\
             {} | {}\n\
             {}\n\
             {} | {}\n\
             {}: {}\n\
             at {}:{} in file `{}`.",
            pre_line.yellow(),
            self.line_no.green(), current_line.yellow(),
            pointy,
            (self.line_no + 1).green(), post_line.yellow(),
            "[Syntax Error]".red(),
            message,
            self.line_no.green(), self.pos.green(), self.file.green(),

        );

        eprintln!("{}", error_message);

    }
}
