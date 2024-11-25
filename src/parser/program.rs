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
            TokenType::Extern => self.parse_extern(),
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
                    x
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
                            x
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
                        x
                    ))
                }
            }
        }

        Ok(Node::Struct(name.into(), generics, fields))
    }

    pub fn parse_extern(&mut self) -> Result<Node<'a>> {

        self.advance(); // eat 'extern'
        
        let mut args = vec![];
        
        let token = self.advance();
        let name = match token.type_ {
            TokenType::Identifier(ref n) => {
                n.clone()
            }
            ref x => {
                return Err(format!(
                    "expected a function name after 'extern'. found {}",
                    x
                ))
            }
        };
        // println!("def after name {:?}", self.tokens.peek());

        let token = self.advance();
        match token.type_ {
            TokenType::LParen => {
                if unwrap_some!(self.tokens.peek()).type_ == TokenType::RParen {
                    self.advance(); // Eat ')'
                } else {
                    loop {
                        args.push(self.parse_type()?);
                        if unwrap_some!(self.tokens.peek()).type_ == TokenType::Comma {
                            self.advance(); // Eat ','
                            if unwrap_some!(self.tokens.peek()).type_ == TokenType::RParen {
                                self.advance(); // eat ')'
                                break;
                            }
                            continue;
                        }else {
                            return Err(format!("Expected ',' after type in `extern` definition. found {}", unwrap_some!(self.tokens.peek()).type_))
                        }
                    }
                    if unwrap_some!(self.tokens.peek()).type_ == TokenType::Comma {
                        self.advance(); // eat trailing ','
                    }
                }
            }
            ref x => {
                return Err(format!(
                    "expected arguments after function name. found {}",
                    x
                ))
            }
        };

        let ret_type = if unwrap_some!(self.tokens.peek()).type_ == TokenType::Arrow {
            self.advance();
            self.parse_type()?
        }else{
            return Err(format!("Expected an arrow followed by a return type after extern definition. found {}", unwrap_some!(self.tokens.peek()).type_))
        };

        Ok(Node::Extern(
            std::borrow::Cow::Owned(name),
            args,
            ret_type,
        ))
    }
}
