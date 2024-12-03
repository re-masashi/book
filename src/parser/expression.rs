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
        _ => panic!("invalid binary operator"),
    }
}

impl<'a> Parser<'_> {
    pub fn parse_expression(&mut self) -> Result<Expr<'a>> {
        // println!("do {:?}", self.tokens.peek());

        let mut l_value = match self.advance().type_ {
            TokenType::Do => self.parse_do()?,
            TokenType::Int(i) => Expr::Literal(Literal::Int(i).into()),
            TokenType::Float(f) => Expr::Literal(Literal::Float(f).into()),
            TokenType::True => Expr::Literal(Literal::Boolean(true).into()),
            TokenType::False => Expr::Literal(Literal::Boolean(false).into()),
            TokenType::String(s) => {
                Expr::Literal(Literal::String(std::borrow::Cow::Owned(s)).into())
            }
            TokenType::Identifier(i) => Expr::Variable(std::borrow::Cow::Owned(i)),
            TokenType::LBrack => self.parse_array()?,
            TokenType::If => self.parse_if()?,
            TokenType::Let => self.parse_let()?,
            TokenType::LParen => {
                let mut v = self.parse_expression()?;
                println!("{:?}", v);
                match self.advance().type_ {
                    TokenType::RParen => {}
                    TokenType::Comma => v = self.parse_tuple(v)?,
                    ref x => {
                        return Err(format!(
                            "unclosed delimiter. expected ')' or ','. found {x}"
                        ))
                    }
                }
                v
            }
            TokenType::While => {
                let cond = self.parse_expression()?;
                match self.advance().type_ {
                    TokenType::Then => {}
                    ref x => {
                        return Err(format!(
                            "expected `then` after while-loop condition. found `{}`",
                            x
                        ))
                    }
                }
                let body = self.parse_expression()?;
                Expr::While(Box::new(cond), Box::new(body))
            }
            TokenType::Fn => self.parse_lambda()?,
            TokenType::Minus => {
                Expr::UnaryOp(UnaryOperator::Negate, Box::new(self.parse_expression()?))
            }
            TokenType::Break => return Ok(Expr::Break),
            TokenType::Continue => return Ok(Expr::Continue),
            TokenType::Return => return Ok(Expr::Return(Box::new(self.parse_expression()?))),
            TokenType::Not => Expr::UnaryOp(UnaryOperator::Not, Box::new(self.parse_expression()?)),
            ref x => return Err(format!("expected a valid expression. found `{}`.", x)),
        };

        while let TokenType::LBrack = unwrap_some!(self.tokens.peek()).type_ {
            // array index
            self.advance(); // eat '['
            let index = self.parse_expression();
            l_value = Expr::Index(Box::new(l_value), Box::new(index?));
            if let TokenType::RBrack = unwrap_some!(self.tokens.peek()).type_ {
                self.advance(); // eat ']'
            } else {
                return Err("unclosed delimiter ']' after array index.".to_string());
            }
        }

        if let TokenType::Comma = unwrap_some!(self.tokens.peek()).type_ {
            self.advance(); // eat trailing ','
        }

        while let TokenType::LParen = unwrap_some!(self.tokens.peek()).type_ {
            // call
            self.advance(); // eat '('
            let mut args = vec![];
            // let index = self.parse_expression();
            loop {
                if unwrap_some!(self.tokens.peek()).type_ == TokenType::RParen {
                    break;
                }
                args.push(Box::new(self.parse_expression()?));
                if unwrap_some!(self.tokens.peek()).type_ == TokenType::Comma {
                    self.advance(); // Eat ','
                    continue;
                }
            }
            l_value = Expr::Call(Box::new(l_value), args);

            if let TokenType::RParen = unwrap_some!(self.tokens.peek()).type_ {
                self.advance(); // eat ')'
            }
        }

        if let TokenType::Comma = unwrap_some!(self.tokens.peek()).type_ {
            self.advance(); // eat trailing ','
        }

        while let TokenType::Dot = unwrap_some!(self.tokens.peek()).type_ {
            self.advance(); // eat '.'
            let field = match self.advance().type_ {
                TokenType::Identifier(i) => i,
                x => {
                    return Err(format!(
                        "expected identifier after '.' for struct field access. found {}",
                        x
                    ))
                }
            };
            l_value = Expr::StructAccess(Box::new(l_value), field.into())
        }

        while let TokenType::Assign = unwrap_some!(self.tokens.peek()).type_ {
            match l_value {
                Expr::Variable(..) | Expr::StructAccess(..) | Expr::Index(..) => {
                    self.advance(); // eat '='
                    l_value = Expr::Assign(Box::new(l_value), Box::new(self.parse_expression()?));
                }
                _ => return Err("invalid expression on LHS of assignment.".to_string()),
            }
        }

        while let TokenType::Plus
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
        | TokenType::Or = unwrap_some!(self.tokens.peek()).type_
        {
            let op = tokentype_to_binop(self.advance().type_);
            let r_value = self.parse_expression()?;
            l_value = Expr::BinaryOp(Box::new(l_value), op, Box::new(r_value))
        }

        Ok(l_value)

        // unimplemented!();
    }

    fn parse_do(&mut self) -> Result<Expr<'a>> {
        // println!("do {:?}", self.tokens.peek());
        let mut expressions = vec![];
        if unwrap_some!(self.tokens.peek()).type_ == TokenType::End {
            return Err("cannot have an empty `do` expression".to_string());
        }
        loop {
            if unwrap_some!(self.tokens.peek()).type_ == TokenType::End {
                self.advance(); // Eat 'end'
                break;
            } else {
                // println!("do-body {:?}", self.tokens.peek());
                expressions.push(self.parse_expression()?);
            }
        }

        Ok(Expr::Do(expressions))
    }

    fn parse_array(&mut self) -> Result<Expr<'a>> {
        let mut args = vec![];
        println!("{:?}", self.tokens.peek());
        loop {
            if unwrap_some!(self.tokens.peek()).type_ == TokenType::RBrack {
                break;
            }

            args.push(self.parse_expression()?);
            if unwrap_some!(self.tokens.peek()).type_ == TokenType::Comma {
                self.advance(); // Eat ','
                continue;
            }
        }
        if let TokenType::RBrack = unwrap_some!(self.tokens.peek()).type_ {
            self.advance(); // eat ']'
        }

        Ok(Expr::Array(args))
    }

    fn parse_tuple(&mut self, expr: Expr<'a>) -> Result<Expr<'a>> {
        let mut vals = vec![expr];
        loop {
            if unwrap_some!(self.tokens.peek()).type_ == TokenType::RParen {
                self.advance(); // eat ')'
                break;
            }
            vals.push(self.parse_expression()?);
            if unwrap_some!(self.tokens.peek()).type_ == TokenType::Comma {
                self.advance(); // eat ','
            } else {
                return Err(format!(
                    "expected ',' or `)` in tuple found {} instead",
                    self.advance().type_
                ));
            }
        }
        // todo!()
        Ok(Expr::Tuple(vals))
    }

    fn parse_if(&mut self) -> Result<Expr<'a>> {
        let cond = self.parse_expression()?;
        match unwrap_some!(self.tokens.peek()).type_ {
            TokenType::Then => {}
            ref x => {
                return Err(format!(
                    "Expected `then` after `if` condition. 
                    found `{}`.",
                    x
                ))
            }
        }
        self.advance(); // eat 'then'
        let if_branch = self.parse_expression()?;
        match unwrap_some!(self.tokens.peek()).type_ {
            TokenType::Else => {
                self.advance();
                let else_branch = self.parse_expression()?;
                Ok(Expr::If(
                    Box::new(cond),
                    Box::new(if_branch),
                    Some(Box::new(else_branch)),
                ))
            }
            _ => Ok(Expr::If(Box::new(cond), Box::new(if_branch), None)),
        }
    }

    fn parse_let(&mut self) -> Result<Expr<'a>> {
        let identifier: std::borrow::Cow<'_, str> = match self.advance().type_ {
            TokenType::Identifier(ref i) => std::borrow::Cow::Owned(i.clone()),
            ref x => {
                return Err(format!(
                    "Expected `identifier` after `let` keyword. Found `{}`",
                    x
                ))
            }
        };
        let type_ = match unwrap_some!(self.tokens.peek()).type_ {
            TokenType::Colon => {
                self.advance(); // eat ':'
                Some(self.parse_type()?)
            }
            _ => None,
        };
        match self.advance().type_ {
            TokenType::Assign => {}
            ref x => return Err(format!("expected `=` after `let`. Found `{}`", x)),
        }
        let val = self.parse_expression()?;
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
                                    args.push((argname_clone, Some(self.parse_type()?)));
                                } else {
                                    args.push((argname_clone, None));
                                }
                            }
                            ref x => return Err(format!("expected identifier. found `{}`", x)),
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
                    "expected arguments after function name. found `{}`",
                    x
                ))
            }
        };
        Ok(Expr::Lambda(args, Box::new(self.parse_expression()?)))
    }
}
