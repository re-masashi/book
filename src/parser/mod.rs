use owo_colors::OwoColorize;

use crate::codegen::Expr;
use crate::lexer::tokens::{Span, Token, TokenType};

use std::fs::File;
use std::io::{BufRead, BufReader};
use std::iter::Peekable;
use std::marker::PhantomData;
use std::vec::IntoIter;

pub mod expression;
pub mod function;
pub mod program;

#[cfg(test)]
pub mod tests;

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
    span: Span,
    file: String,
    phantom: PhantomData<&'a i32>,
}

impl Parser<'_> {
    pub fn new(tokens: TokenIter, file_path: &str) -> Self {
        Parser {
            tokens,
            span: Span((-1, -1), (-1, -1)),
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
        self.span = next_token.span.clone();
        next_token
    }

    pub fn error(&self, message: String) {
        match File::open(&self.file) {
            Ok(file) => {
                let reader = BufReader::new(file);
                let lines: Vec<String> = reader
                    .lines()
                    .map(|l| l.expect("Could not read line"))
                    .collect();

                let (start_line, start_col) = self.span.0;
                let (end_line, end_col) = self.span.1;

                // Adjust for 0-based indexing (lines and columns are 1-indexed)
                let start_line = (start_line - 1) as usize;
                let start_col = start_col as usize;
                let end_line = (end_line - 1) as usize;
                let end_col = end_col as usize;

                // Validate line numbers and columns
                if start_line >= lines.len()
                    || end_line >= lines.len()
                    || start_col > lines[start_line].len()
                    || end_col > lines[end_line].len()
                {
                    eprintln!(
                        "Invalid span: Line or column numbers out of bounds. {:?} {}",
                        self.span,
                        lines.len()
                    );
                    return;
                }

                // Extract relevant lines
                let pre_line = if start_line == 0 {
                    Some("".to_string())
                } else {
                    lines
                        .get(start_line - 1)
                        .map(|l| format!("{} | {}", start_line, l))
                };
                let current_line = &lines[start_line];
                let post_line = lines
                    .get(start_line + 1)
                    .map(|l| format!("{} | {}", start_line + 2, l));

                // Create pointy indicators
                let start_pointy = format!("{:~<width$}^", "", width = start_col + 1);
                let end_pointy = format!("{:^<width$}^", "", width = end_col - start_col);
                let padding = format!(
                    "{: <width$}  ",
                    "",
                    width = (start_line + 1).ilog10() as usize
                );

                // Construct the error message
                let error_message = format!(
                    "\n\
                     {}\n\
                     {} | {}\n\
                     {}{}{}\n\
                     {}\n\
                     {}: {}\n\
                     at line {}:{} in file `{}`.",
                    pre_line.unwrap_or_default().yellow(),
                    (start_line + 1).green(),
                    current_line.yellow(),
                    padding,
                    start_pointy.red(),
                    end_pointy.red(),
                    post_line.unwrap_or_default().yellow(),
                    "[Syntax Error]".red(),
                    message,
                    (start_line + 1).green(),
                    (start_col + 1).green(),
                    self.file.green(),
                );

                eprintln!("{}", error_message);
            }
            Err(error) => {
                eprintln!(
                    "Error: Could not open file: {}. {}",
                    self.file.green(),
                    error
                );
            }
        }
    }
}
