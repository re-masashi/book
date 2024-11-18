use crate::lexer::tokens::TokenType;
use crate::parser::Parser;
use crate::interpreter::{TypeAnnot, Node, Expr, Literal};
use crate::{unwrap_some, Result};

impl<'a> Parser<'_> {

    pub fn parse_type(&mut self) -> Result<TypeAnnot>{
        let type_name = match self.advance().type_ {
            TokenType::Identifier(ref i)=>{
                i.to_string()
            }
            x=>return Err(format!("invalid type without an identifier. found `{:?}`", x))
        };
        let mut generics = vec![];
        match unwrap_some!(self.tokens.peek()).type_ {
            TokenType::Less => { // parse for generics
                self.advance(); // eat '<'
                loop {
                    match unwrap_some!(self.tokens.peek()).type_ {
                        TokenType::Identifier(ref argname)=>{
                            generics.push(argname.to_string());
                            self.advance();
                        }
                        ref x=>return Err(format!("expected identifier in generic. found {:?}. line: {}, pos: {}", x, self.line_no, self.pos))
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
            _=>{}
        }
        return Ok(TypeAnnot{
            name: type_name,
            generics: generics,
            traits: vec![]
        })
    }
    
    pub fn parse_function(&mut self) -> Result<Node<'a>> { // always returns a function node
        println!("def {:?}", self.tokens.peek());
        println!("toks {:#?}", self.tokens);

        self.advance(); // eat 'def'
        let mut name = "".to_string();
        let mut ret_type = None;
        let mut args = vec![];
        println!("def after def {:?}", self.tokens.peek());

        let token = self.advance();
        match token.type_ {
            TokenType::Identifier(ref n)=>{
                name = n.clone();
                println!("found a function named {:?}", n);
            }
            _=>return Err("expected a function name after 'def'".to_string())
        }
        println!("def after name {:?}", self.tokens.peek());

        let token = self.advance();
        match token.type_ {
            TokenType::LParen=>{
                if unwrap_some!(self.tokens.peek()).type_ == TokenType::RParen {
                    self.advance(); // Eat ')'
                } else {
                    loop {
                        println!("def after '(' {:?}", self.tokens.peek());
                        match unwrap_some!(self.tokens.peek()).type_ {
                            TokenType::Identifier(ref argname)=>{
                                let argname_clone = std::borrow::Cow::Owned(argname.clone());
                                self.advance();
                                if unwrap_some!(self.tokens.peek()).type_ == TokenType::Colon {
                                    self.advance(); // eat ':'
                                    args.push((argname_clone, Some(self.parse_type().unwrap())));
                                }else{
                                    args.push((argname_clone, None));
                                }
                            }
                            _=>return Err("expected identifier".to_string())
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
            _=>return Err("expected arguments after function name".to_string())
        };

        match unwrap_some!(self.tokens.peek()).type_ {
            TokenType::Arrow =>{
                self.advance();
                ret_type = Some(self.parse_type().unwrap());
            }
            _=>{}
        };
        println!("function body {:?}", self.tokens.peek());

        Ok(Node::Function(
            std::borrow::Cow::Owned(name),
            args,
            Box::new(self.parse_expression().unwrap()),
            ret_type
        ))
    }
}
