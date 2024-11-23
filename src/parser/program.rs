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
            TokenType::Struct => self.parse_struct(),
            _ => Ok(Node::Expr(Box::new(self.parse_expression()?))),
        }
        .clone()
    }

    pub fn parse_struct(&mut self) -> Result<Node<'a>> {
        self.advance(); // eat 'struct'

        let name = match self.advance().type_ {
            TokenType::Identifier(i) => i.to_string(),
            ref x => {
                return Err(format!(
                    "expected identifier after struct keyword. found `{}`",
                    x.to_string()
                ))
            }
        };

        let mut generics = vec![];

        if unwrap_some!(self.tokens.peek()).type_ == TokenType::Less {
            self.advance(); // eat '<'
            loop {
                match unwrap_some!(self.tokens.peek()).type_ {
                    TokenType::Identifier(ref argname) => {
                        generics.push(std::borrow::Cow::Owned(argname.clone()));
                        self.advance();
                    }
                    ref x => {
                        return Err(format!(
                            "expected identifier in generic. found {}",
                            x.to_string()
                        ))
                    }
                }
                if unwrap_some!(self.tokens.peek()).type_ == TokenType::Comma {
                    self.advance(); // Eat ','
                    continue;
                }
                if unwrap_some!(self.tokens.peek()).type_ == TokenType::Greater {
                    self.advance(); // eat '>'
                    break;
                }
            }
        }

        let mut fields = vec![];
        loop {
            match unwrap_some!(self.tokens.peek()).type_ {
                TokenType::End => {
                    self.advance(); // eat 'end'
                    break;
                }
                TokenType::Identifier(ref i) => {
                    let field_name = i.clone();
                    self.advance(); // eat 'identifier'
                                    // match unwrap_some!(self.tokens.peek()).type_ {
                                    //     TokenType::Colon=>{}
                                    //     _=>return Err()
                                    // }

                    let field_type = self.parse_type()?;

                    fields.push((std::borrow::Cow::Owned(field_name), field_type));
                }
                ref x => {
                    return Err(format!(
                        "expected identifier in field of struct `{name}`'s definition. found {}",
                        x.to_string()
                    ))
                }
            }
        }

        Ok(Node::Struct(name.into(), generics, fields))
    }
}
