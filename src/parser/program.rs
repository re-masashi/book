use crate::lexer::tokens::TokenType;
use crate::parser::{AstNode, NodePosition, Parser, Function};
use crate::{unwrap_some, Result};

impl Parser {
    pub fn parse_program(&mut self) -> Result<Vec<(AstNode, NodePosition)>> {
        let mut ast: Vec<(AstNode, NodePosition)> = Vec::new();
        loop {
            match self.tokens.peek() {
                Some(s) => match &s.type_ {
                    TokenType::Identifier(_f)=>{
                        let (func, pos) = self.parse_function().unwrap();
                        ast.push((AstNode::FunctionDef(func), pos));
                    }
                    x=>panic!("{:?}", x)
                },
                None => return Ok(ast),
            }
        }
    }

    pub fn parse_function(&mut self) -> Result<(Function, NodePosition)>{
        self.advance();
        let f;
        if let TokenType::Identifier(i) = self.tokens.next().unwrap().type_ {
            f=i;
        }else{
            unreachable!()
        }

        let mut args = vec![];
        // println!("{:?}", function_name);
        while unwrap_some!(self.tokens.peek()).type_!=TokenType::Assign {
            self.advance();
            match unwrap_some!(self.tokens.next()).type_ {
                TokenType::Identifier(a) => {
                    println!("arg: {:?}", a);
                    args.push((a, "unavailable".to_string()))
                },
                TokenType::Assign=>{}
                _=>todo!()
            }
        }
        // println!("{:?}", self.tokens.peek());
        self.advance(); 
        self.tokens.next(); // eat '='

        match self.parse_expression() {
            Ok((expression, pos))=>
                Ok((Function {
                    name: f.to_string(),
                    args,
                    expression,
                    return_type: "unavailable".to_string()
                }, pos)),
            Err(e) => Err(e)
        }
    }
}
