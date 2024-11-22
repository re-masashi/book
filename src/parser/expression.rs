use crate::interpreter::{BinaryOperator, Expr, Literal, UnaryOperator};
use crate::lexer::tokens::TokenType;
use crate::parser::Parser;
use crate::{unwrap_some, Result};
// use log::trace;

fn tokentype_to_binop(tok: TokenType) -> BinaryOperator {
    match tok {
        TokenType::Plus => BinaryOperator::Add,
        TokenType::Minus => BinaryOperator::Sub,
        TokenType::Mul => BinaryOperator::Mul,
        TokenType::Div => BinaryOperator::Div,
        TokenType::Equal => BinaryOperator::Equal,
        TokenType::Greater => BinaryOperator::Greater,
        TokenType::Less => BinaryOperator::Less,
        TokenType::GreaterEq => BinaryOperator::GreaterEqual,
        TokenType::LessEq => BinaryOperator::LessEqual,
        TokenType::NotEq => BinaryOperator::NotEqual,
        TokenType::And => BinaryOperator::And,
        TokenType::Or => BinaryOperator::Or,
        _ => panic!("invalid"),
    }
}

fn tokentype_to_unop(tok: TokenType) -> UnaryOperator {
    match tok {
        TokenType::Minus => UnaryOperator::Negate,
        TokenType::Not => UnaryOperator::Not,
        _ => panic!("invalid"),
    }
}

impl<'a> Parser<'_> {
    pub fn parse_expression(&mut self) -> Result<Expr<'a>> {
        // println!("do {:?}", self.tokens.peek());

        let mut l_value = match self.advance().type_ {
            TokenType::Do => self.parse_do().unwrap(),
            TokenType::Int(i) => Expr::Literal(Literal::Int(i).into()),
            TokenType::Float(f) => Expr::Literal(Literal::Float(f).into()),
            TokenType::True => Expr::Literal(Literal::Boolean(true).into()),
            TokenType::False => Expr::Literal(Literal::Boolean(false).into()),
            TokenType::String(s) => {
                Expr::Literal(Literal::String(std::borrow::Cow::Owned(s)).into())
            }
            TokenType::Identifier(i) => Expr::Variable(std::borrow::Cow::Owned(i)),
            TokenType::LBrack => self.parse_array().unwrap(),
            TokenType::If => self.parse_if().unwrap(),
            TokenType::Let => self.parse_let().unwrap(),
            TokenType::While => {
                let cond = self.parse_expression().unwrap();
                match self.advance().type_ {
                    TokenType::Then => {}
                    ref x => {
                        panic!(
                            "expected `then` after while-loop condition. found `{:?}`",
                            x
                        );
                    }
                }
                let body = self.parse_expression().unwrap();
                Expr::While(Box::new(cond), Box::new(body))
            }
            TokenType::Fn => self.parse_lambda().unwrap(),
            TokenType::Minus => Expr::UnaryOp(
                UnaryOperator::Negate,
                Box::new(self.parse_expression().unwrap()),
            ),
            TokenType::Not => Expr::UnaryOp(
                UnaryOperator::Not,
                Box::new(self.parse_expression().unwrap()),
            ),
            ref x => panic!("found {:?}. line: {}, pos: {}", x, self.line_no, self.pos),
        };

        loop {
            if let TokenType::LBrack = unwrap_some!(self.tokens.peek()).type_ {
                // array index
                self.advance(); // eat '['
                let index = self.parse_expression();
                l_value = Expr::Index(Box::new(l_value), Box::new(index.unwrap()))
            } else {
                break;
            }
            if let TokenType::RBrack = unwrap_some!(self.tokens.peek()).type_ {
                self.advance(); // eat ']'
            }
        }
        if let TokenType::Comma = unwrap_some!(self.tokens.peek()).type_ {
            self.advance(); // eat trailing ','
        }

        loop {
            if let TokenType::LParen = unwrap_some!(self.tokens.peek()).type_ {
                // call
                self.advance(); // eat '('
                let mut args = vec![];
                // let index = self.parse_expression();
                loop {
                    if unwrap_some!(self.tokens.peek()).type_ == TokenType::RParen {
                        break;
                    }
                    args.push(Box::new(self.parse_expression().unwrap()));
                    if unwrap_some!(self.tokens.peek()).type_ == TokenType::Comma {
                        self.advance(); // Eat ','
                        continue;
                    }
                }
                l_value = Expr::Call(Box::new(l_value), args);
            } else {
                break;
            }

            if let TokenType::RParen = unwrap_some!(self.tokens.peek()).type_ {
                self.advance(); // eat ')'
            } else {
                // println!("{:?}", self.tokens.peek());
            }
        }
        if let TokenType::Comma = unwrap_some!(self.tokens.peek()).type_ {
            self.advance(); // eat trailing ','
        }

        loop {
            let mut op = tokentype_to_binop(TokenType::Plus);
            match unwrap_some!(self.tokens.peek()).type_ {
                TokenType::Plus
                | TokenType::Minus
                | TokenType::Mul
                | TokenType::Div
                | TokenType::Equal
                | TokenType::Greater
                | TokenType::Less
                | TokenType::GreaterEq
                | TokenType::LessEq
                | TokenType::NotEq
                | TokenType::And
                | TokenType::Or => op = tokentype_to_binop(self.advance().type_),
                _ => {
                    break;
                }
            }
            let r_value = self.parse_expression().unwrap();
            l_value = Expr::BinaryOp(Box::new(l_value), op, Box::new(r_value))
        }

        Ok(l_value)

        // unimplemented!();
    }

    fn parse_do(&mut self) -> Result<Expr<'a>> {
        // println!("do {:?}", self.tokens.peek());
        let mut expressions = vec![];
        if unwrap_some!(self.tokens.peek()).type_ == TokenType::End {
            panic!("cannot have an empty `do` expression");
        }
        loop {
            if unwrap_some!(self.tokens.peek()).type_ == TokenType::End {
                self.advance(); // Eat ','
                break;
            } else {
                // println!("do-body {:?}", self.tokens.peek());
                expressions.push(self.parse_expression().unwrap());
            }
        }

        Ok(Expr::Do(expressions))
    }

    fn parse_array(&mut self) -> Result<Expr<'a>> {
        let retval = Expr::Literal(Literal::Boolean(false).into());
        let mut args = vec![];
        loop {
            if unwrap_some!(self.tokens.peek()).type_ == TokenType::End {
                self.advance(); // Eat ','
                break;
            } else {
                args.push(self.parse_expression().unwrap());
            }
        }
        Ok(Expr::Array(args))
    }

    fn parse_if(&mut self) -> Result<Expr<'a>> {
        let cond = self.parse_expression().unwrap();
        match unwrap_some!(self.tokens.peek()).type_ {
            TokenType::Then => {}
            ref x => {
                return Err(format!(
                    "Expected `then` after `if` condition. found {:?}. line: {}, pos: {}",
                    x, self.line_no, self.pos
                ))
            }
        }
        self.advance(); // eat 'then'
        let if_branch = self.parse_expression().unwrap();
        match unwrap_some!(self.tokens.peek()).type_ {
            TokenType::Else => {
                self.advance();
                let else_branch = self.parse_expression().unwrap();
                return Ok(Expr::If(
                    Box::new(cond),
                    Box::new(if_branch),
                    Some(Box::new(else_branch)),
                ));
            }
            _ => return Ok(Expr::If(Box::new(cond), Box::new(if_branch), None)),
        }
    }

    fn parse_let(&mut self) -> Result<Expr<'a>> {
        let identifier: std::borrow::Cow<'_, str> = match self.advance().type_ {
            TokenType::Identifier(ref i) => std::borrow::Cow::Owned(i.clone()),
            _ => return Err("Expected `identifier` after `let` keyword".to_string()),
        };
        let type_ = match unwrap_some!(self.tokens.peek()).type_ {
            TokenType::Colon => {
                self.advance(); // eat ':'
                Some(self.parse_type().unwrap())
            }
            _ => None,
        };
        match self.advance().type_ {
            TokenType::Assign => {}
            _ => return Err("expected `=` after `let` ".to_string()),
        }
        let val = self.parse_expression().unwrap();
        Ok(Expr::Let(identifier, type_, Box::new(val)))
    }

    fn parse_lambda(&mut self) -> Result<Expr<'a>> {
        let mut args = vec![];
        let token = self.advance(); // Call advance and store the token
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
                                    args.push((argname_clone, Some(self.parse_type().unwrap())));
                                } else {
                                    args.push((argname_clone, None));
                                }
                            }
                            _ => return Err("expected identifier".to_string()),
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
            _ => return Err("expected arguments after function name".to_string()),
        };
        Ok(Expr::Lambda(
            args,
            Box::new(self.parse_expression().unwrap()),
        ))
    }
}
