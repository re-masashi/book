use crate::interpreter::Node;
use crate::lexer::tokens::TokenType;
use crate::parser::Parser;
use crate::{unwrap_some, Result};

impl<'a> Parser<'_> {
    pub fn parse_program(&'a mut self) -> Result<Node<'a>> {
        let mut vals = vec![];

        loop {
            match self.parse_program_() {
                Ok(result) => vals.push(result),
                Err(s) if s == *"EOF" => {
                    break;
                }
                Err(e) => return Err(e),
            }
        }
        Ok(Node::Program(vals))
    }

    pub fn parse_program_(&mut self) -> Result<Node<'a>> {
        match unwrap_some!(self.tokens.peek()).type_ {
            TokenType::Def => self.parse_function(),
            _ => Ok(Node::Expr(Box::new(self.parse_expression()?))),
        }
        .clone()
    }
}
