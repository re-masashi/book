use crate::lexer::tokens::TokenType;
use crate::parser::{Function, NodePosition, Parser};
use crate::{unwrap_some, Result};
use crate::interpreter::{TypedNode, Node, Expr, Literal};

impl<'a> Parser<'_> {
    pub fn parse_program(&'a mut self) -> Result<Node<'a>> {
        let mut retval = Node::Program(vec![]);
        let mut vals = vec![];

        loop {
            match self.parse_program_(){
                Ok(result)=>vals.push(result),
                Err(s) if s == "EOF".to_string()=>{
                    break;
                }
                Err(e)=>return Err(e)
            }
        };
        Ok(Node::Program(vals))
    }

    pub fn parse_program_(&mut self) -> Result<Node<'a>> {
        match unwrap_some!(self.tokens.peek()).type_ {
            TokenType::Def=>self.parse_function(),
            _=>Ok(Node::Expr(Box::new(
                self.parse_expression()?
            )))
        }.clone()
    }
}
