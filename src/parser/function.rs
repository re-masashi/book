use crate::interpreter::{Node, TypeAnnot};
use crate::lexer::tokens::TokenType;
use crate::parser::Parser;
use crate::{unwrap_some, Result};

impl<'a> Parser<'_> {
    pub fn parse_type(&mut self) -> Result<TypeAnnot> {
        let type_name = match self.advance().type_ {
            TokenType::Identifier(ref i) => i.to_string(),
            x => {
                return Err(format!(
                    "invalid type without an identifier. found `{}`",
                    x.to_string()
                ))
            }
        };
        let mut generics = vec![];
        if unwrap_some!(self.tokens.peek()).type_ == TokenType::Less {
            // parse for generics
            self.advance(); // eat '<'
            loop {
                match unwrap_some!(self.tokens.peek()).type_ {
                    TokenType::Identifier(ref argname) => {
                        generics.push(argname.to_string());
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
        Ok(TypeAnnot {
            name: type_name,
            generics,
            traits: vec![],
        })
    }

    pub fn parse_function(&mut self) -> Result<Node<'a>> {
        // always returns a function node
        // println!("def {:?}", self.tokens.peek());
        // println!("toks {:#?}", self.tokens);

        self.advance(); // eat 'def'
                        // let mut name = "".to_string();
        let mut ret_type = None;
        let mut args = vec![];
        // println!("def after def {:?}", self.tokens.peek());

        let token = self.advance();
        let name = match token.type_ {
            TokenType::Identifier(ref n) => {
                n.clone()
                // println!("found a function named {:?}", n);
            }
            ref x => {
                return Err(format!(
                    "expected a function name after 'def'. found {}",
                    x.to_string()
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
                        // println!("def after '(' {:?}", self.tokens.peek());
                        match unwrap_some!(self.tokens.peek()).type_ {
                            TokenType::Identifier(ref argname) => {
                                let argname_clone = std::borrow::Cow::Owned(argname.clone());
                                self.advance();
                                if unwrap_some!(self.tokens.peek()).type_ == TokenType::Colon {
                                    self.advance(); // eat ':'
                                    args.push((argname_clone, Some(self.parse_type()?)));
                                } else {
                                    args.push((argname_clone, None));
                                }
                            }
                            ref x => {
                                return Err(format!("expected identifier. found {}", x.to_string()))
                            }
                        }
                        if unwrap_some!(self.tokens.peek()).type_ == TokenType::Comma {
                            self.advance(); // Eat ','
                            continue;
                        }
                        if unwrap_some!(self.tokens.peek()).type_ == TokenType::RParen {
                            self.advance(); // eat ')'
                            break;
                        }
                    }
                }
            }
            ref x => {
                return Err(format!(
                    "expected arguments after function name. found {}",
                    x.to_string()
                ))
            }
        };

        if unwrap_some!(self.tokens.peek()).type_ == TokenType::Arrow {
            self.advance();
            ret_type = Some(self.parse_type()?);
        };
        // println!("function body {:?}", self.tokens.peek());

        Ok(Node::Function(
            std::borrow::Cow::Owned(name),
            args,
            Box::new(self.parse_expression()?),
            ret_type,
        ))
    }
}
