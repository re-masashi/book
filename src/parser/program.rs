use crate::codegen::Node;
use crate::lexer::tokens::{Span, Token, TokenType};
use crate::lexer::Lexer;
use crate::parser::Parser;
use crate::{unwrap_some, Result};

impl<'a> Parser<'_> {
    pub fn parse_program(&mut self) -> Result<(Node<'a>, Span, String)> {
        // node, span, file
        let mut vals = vec![];
        let span = self.span;
        let file = self.file.clone();
        loop {
            match self.parse_program_() {
                Ok((Node::Program(p), _)) => vals.append(&mut p.clone()), // unreachable i guess?
                Ok((result, _)) => vals.push((result, span, file.clone())), // todo: make it correct
                Err(s) if s == *"EOF" => {
                    break;
                }
                Err(e) => return Err(e),
            }
            if unwrap_some!(self.tokens.peek()).type_ == TokenType::Semicolon {
                self.advance();
            }
        }
        Ok((Node::Program(vals), span, file))
    }

    pub fn parse_program_(&mut self) -> Result<(Node<'a>, Span)> {
        match unwrap_some!(self.tokens.peek()).type_ {
            TokenType::Def => self.parse_function(),
            TokenType::Struct => self.parse_struct(),
            TokenType::Extern => self.parse_extern(),
            TokenType::Use => self.parse_use(),
            _ => {
                let (expr, span) = self.parse_expression()?;
                Ok((Node::Expr(Box::new(expr)), span))
            }
        }
        .clone()
    }

    pub fn parse_struct(&mut self) -> Result<(Node<'a>, Span)> {
        let mut span = self.span;

        self.advance(); // eat 'struct'

        let _file = self.file.clone();

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
                    ref x => return Err(format!("expected identifier in generic. found {}", x)),
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

        span.1 = self.span.1;

        Ok((Node::Struct(name.into(), generics, fields), span))
    }

    pub fn parse_extern(&mut self) -> Result<(Node<'a>, Span)> {
        let mut span = self.span;

        self.advance(); // eat 'extern'

        let mut args = vec![];

        let token = self.advance();
        let name = match token.type_ {
            TokenType::Identifier(ref n) => n.clone(),
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
                        } else {
                            return Err(format!(
                                "Expected ',' after type in `extern` definition. found {}",
                                unwrap_some!(self.tokens.peek()).type_
                            ));
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
        } else {
            return Err(format!(
                "Expected an arrow followed by a return type after extern definition. found {}",
                unwrap_some!(self.tokens.peek()).type_
            ));
        };

        span.1 = self.span.1;

        Ok((
            Node::Extern(std::borrow::Cow::Owned(name), args, ret_type),
            span,
        ))
    }

    pub fn parse_use(&mut self) -> Result<(Node<'a>, Span)> {
        let mut span = self.span;

        self.advance(); // eat 'use'

        let _file = self.file.clone();

        let modulename = match self.advance().type_ {
            TokenType::String(s) => s.to_string(),
            TokenType::Identifier(s) => s.to_string(),
            ref x => return Err(format!("invalid `use` module. {x}")),
        };
        let lexer = Lexer::from_file(&modulename).unwrap();
        let prevstate = (self.span, self.file.clone(), self.tokens.clone());
        let mut tokens = lexer.map(|t| t.unwrap()).collect::<Vec<_>>();
        tokens.push(Token {
            type_: TokenType::Int(0),
            span: Span((0, 1), (0, 1)),
            file: modulename.to_string(),
        });
        self.tokens = tokens.into_iter().peekable();
        self.file = modulename.to_string();
        self.span = Span((0, -1), (0, -1));
        let (Node::Program(ast), _, _) = self.parse_program()? else {
            unreachable!()
        };

        self.span = prevstate.0;
        self.file = prevstate.1;
        self.tokens = prevstate.2;

        span.1 = self.span.1;

        Ok((Node::Program(ast.clone()), span))
    }
}
