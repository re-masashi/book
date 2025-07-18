#![allow(clippy::needless_lifetimes)]
// clippy goes nuts for no reason at all over false positives.
// see https://github.com/rust-lang/rust-clippy/issues/740
// and https://github.com/rust-lang/rust-clippy/issues/13749 maybe

use crate::codegen::{Node, TypeAnnot};
use crate::lexer::tokens::{Span, TokenType};
use crate::parser::Parser;
use crate::{unwrap_some, Result};

impl<'a> Parser<'_> {
    pub fn parse_type(&mut self) -> Result<TypeAnnot> {
        let type_name = match self.advance().type_ {
            TokenType::Identifier(ref i) => i.to_string(),
            TokenType::LParen => {
                let mut vals = vec![];
                loop {
                    if unwrap_some!(self.tokens.peek()).type_ == TokenType::RParen {
                        self.advance(); // eat ')'
                        break;
                    }
                    vals.push(self.parse_type()?);
                    if unwrap_some!(self.tokens.peek()).type_ == TokenType::RParen {
                        self.advance(); // eat ')'
                        break;
                    }
                    if unwrap_some!(self.tokens.peek()).type_ == TokenType::Comma {
                        self.advance(); // eat ','
                    } else {
                        return Err(format!(
                            "expected ',' or `)` in tuple type definition found {} instead",
                            self.advance().type_
                        ));
                    }
                }
                println!("{vals:?}");
                todo!()
            }
            x => return Err(format!("invalid type without an identifier. found `{x}`")),
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
                    ref x => return Err(format!("expected identifier in generic. found {x}")),
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

    pub fn parse_function(&mut self) -> Result<(Node<'a>, Span)> {
        // always returns a function node
        // println!("def {:?}", self.tokens.peek());
        // println!("toks {:#?}", self.tokens);

        self.advance(); // eat 'def'
        let mut span = self.span;
        let mut ret_type = None;
        let mut args = vec![];

        let token = self.advance();
        let name = match token.type_ {
            TokenType::Identifier(ref n) => n.clone(),
            ref x => return Err(format!("expected a function name after 'def'. found {x}")),
        };

        let token = self.advance();
        match token.type_ {
            TokenType::LParen => {
                if unwrap_some!(self.tokens.peek()).type_ == TokenType::RParen {
                    self.advance(); // Eat ')'
                } else {
                    loop {
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
                            ref x => return Err(format!("expected identifier. found {x}")),
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
            ref x => return Err(format!("expected arguments after function name. found {x}",)),
        };

        if unwrap_some!(self.tokens.peek()).type_ == TokenType::Arrow {
            self.advance();
            ret_type = Some(self.parse_type()?);
        };
        // println!("function body {:?}", self.tokens.peek());
        span.1 = self.span.1;

        Ok((
            Node::Function(
                std::borrow::Cow::Owned(name),
                args,
                Box::new(self.parse_expression()?.0),
                ret_type,
            ),
            span,
        ))
    }
}
