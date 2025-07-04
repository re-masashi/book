use crate::codegen::{BinaryOperator, Expr, Literal, UnaryOperator};
use crate::lexer::tokens::{Span, TokenType};
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
    pub fn parse_expression(&mut self) -> Result<(Expr<'a>, Span)> {
        // println!("do {:?}", self.tokens.peek());
        // println!("{:?}", self.tokens.peek());
        // println!("{:#?}", self.tokens);

        let mut span = self.span;

        let mut l_value = match self.advance().type_ {
            TokenType::Do => self.parse_do()?.0,
            TokenType::Int(i) => {
                Expr::Literal(Literal::Int(i).into(), self.span, self.file.clone())
            }
            TokenType::Float(f) => {
                Expr::Literal(Literal::Float(f).into(), self.span, self.file.clone())
            }
            TokenType::True => {
                Expr::Literal(Literal::Boolean(true).into(), self.span, self.file.clone())
            }
            TokenType::False => {
                Expr::Literal(Literal::Boolean(false).into(), self.span, self.file.clone())
            }
            TokenType::String(s) => Expr::Literal(
                Literal::String(std::borrow::Cow::Owned(s)).into(),
                self.span,
                self.file.clone(),
            ),
            TokenType::Identifier(i) => {
                Expr::Variable(std::borrow::Cow::Owned(i), self.span, self.file.clone())
            }
            TokenType::LBrack => self.parse_array()?.0,
            TokenType::If => self.parse_if()?.0,
            TokenType::Let => self.parse_let()?.0,
            TokenType::LParen => {
                let mut v = self.parse_expression()?.0;
                // println!("{:?}", v);
                // println!("{:?}", self.tokens.peek());
                match self.advance().type_ {
                    TokenType::RParen => {}
                    TokenType::Comma => v = self.parse_tuple(v)?.0,
                    ref x => {
                        return Err(format!(
                            "unclosed delimiter. expected ')' or ','. found {x}"
                        ))
                    }
                }
                v
            }
            TokenType::While => {
                let cond = self.parse_expression()?.0;
                match self.advance().type_ {
                    TokenType::Then => {}
                    ref x => {
                        return Err(format!(
                            "expected `then` after while-loop condition. found `{x}`"
                        ))
                    }
                }
                let body = self.parse_expression()?.0;
                Expr::While(Box::new(cond), Box::new(body), self.span, self.file.clone())
            }
            TokenType::Fn => self.parse_lambda()?.0,
            TokenType::Minus => Expr::UnaryOp(
                UnaryOperator::Negate,
                Box::new(self.parse_expression()?.0),
                self.span,
                self.file.clone(),
            ),
            TokenType::Break => return Ok((Expr::Break(self.span, self.file.clone()), span)),
            TokenType::Continue => return Ok((Expr::Continue(self.span, self.file.clone()), span)),
            TokenType::Return => {
                return Ok((
                    Expr::Return(
                        Box::new(self.parse_expression()?.0),
                        span,
                        self.file.clone(),
                    ),
                    span,
                ))
            }
            TokenType::Not => Expr::UnaryOp(
                UnaryOperator::Not,
                Box::new(self.parse_expression()?.0),
                self.span,
                self.file.clone(),
            ),
            ref x => return Err(format!("expected a valid expression. found `{x}`.")),
        };

        loop {
            // postfix operators
            match unwrap_some!(self.tokens.peek()).type_ {
                TokenType::LBrack => {
                    self.advance(); // eat '['
                    let index = self.parse_expression();
                    span.1 = self.span.1;

                    l_value = Expr::Index(
                        Box::new(l_value),
                        Box::new(index?.0),
                        span,
                        self.file.clone(),
                    );
                    if let TokenType::RBrack = unwrap_some!(self.tokens.peek()).type_ {
                        self.advance(); // eat ']'
                    } else {
                        return Err("unclosed delimiter ']' after array index.".to_string());
                    }

                    if let TokenType::Comma = unwrap_some!(self.tokens.peek()).type_ {
                        self.advance(); // eat trailing ','
                    }

                    span.1 = self.span.1;
                }
                TokenType::LParen => {
                    // call
                    self.advance(); // eat '('
                    let mut args = vec![];
                    span.1 = self.span.1;

                    loop {
                        if unwrap_some!(self.tokens.peek()).type_ == TokenType::RParen {
                            break;
                        }
                        args.push(self.parse_expression()?.0);
                        span.1 = self.span.1;

                        if unwrap_some!(self.tokens.peek()).type_ == TokenType::Comma {
                            self.advance(); // Eat ','
                            continue;
                        }
                    }

                    l_value = Expr::Call(Box::new(l_value), args, span, self.file.clone());

                    if let TokenType::Comma = unwrap_some!(self.tokens.peek()).type_ {
                        self.advance(); // eat trailing ','
                    }

                    if let TokenType::RParen = unwrap_some!(self.tokens.peek()).type_ {
                        self.advance(); // eat ')'
                    }

                    span.1 = self.span.1;
                }
                TokenType::Dot => {
                    self.advance(); // eat '.'

                    let field = match self.advance().type_ {
                        TokenType::Identifier(i) => i,
                        x => {
                            return Err(format!(
                                "expected identifier after '.' for struct field access. found {x}"
                            ))
                        }
                    };
                    l_value =
                        Expr::StructAccess(Box::new(l_value), field.into(), span, self.file.clone())
                }
                TokenType::Assign => {
                    match l_value {
                        Expr::Variable(..) | Expr::StructAccess(..) | Expr::Index(..) => {
                            self.advance(); // eat '='
                            span.1 = self.span.1;
                            l_value = Expr::Assign(
                                Box::new(l_value),
                                Box::new(self.parse_expression()?.0),
                                span,
                                self.file.clone(),
                            );
                        }
                        _ => return Err("invalid expression on LHS of assignment.".to_string()),
                    }
                }
                _ => {
                    break;
                }
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
            let r_value = self.parse_expression()?.0;
            span.1 = self.span.1;

            l_value = Expr::BinaryOp(
                Box::new(l_value),
                op,
                Box::new(r_value),
                span,
                self.file.clone(),
            )
        }

        span.1 = self.span.1;

        Ok((l_value, span))
    }

    fn parse_do(&mut self) -> Result<(Expr<'a>, Span)> {
        let mut expressions = vec![];

        let mut span = self.span;

        let _file = self.file.clone();

        if unwrap_some!(self.tokens.peek()).type_ == TokenType::End {
            return Err("cannot have an empty `do` expression".to_string());
        }
        loop {
            if unwrap_some!(self.tokens.peek()).type_ == TokenType::End {
                self.advance(); // Eat 'end'
                break;
            } else {
                // println!("do-body {:?}", self.tokens.peek());
                expressions.push(self.parse_expression()?.0);
                span.1 = self.span.1;
            }
            if unwrap_some!(self.tokens.peek()).type_ == TokenType::Semicolon {
                self.advance();
            }
        }
        span.1 = self.span.1;

        Ok((Expr::Do(expressions, span, self.file.clone()), span))
    }

    fn parse_array(&mut self) -> Result<(Expr<'a>, Span)> {
        let mut args = vec![];

        let mut span = self.span;

        let _file = self.file.clone();

        // println!("{:?}", self.tokens.peek());
        loop {
            if unwrap_some!(self.tokens.peek()).type_ == TokenType::RBrack {
                self.advance(); // eat ']'
                break;
            }

            args.push(self.parse_expression()?.0);
            span.1 = self.span.1;
            if unwrap_some!(self.tokens.peek()).type_ == TokenType::Comma {
                self.advance(); // Eat ','
                continue;
            }
            if unwrap_some!(self.tokens.peek()).type_ == TokenType::RBrack {
                self.advance(); // eat ']'
                break;
            } else {
                return Err(format!(
                    "Expected ']' or ','. unclosed delimiter in array. found {}",
                    self.advance().type_
                ));
            }
        }
        span.1 = self.span.1;

        Ok((Expr::Array(args, span, self.file.clone()), span))
    }

    fn parse_tuple(&mut self, expr: Expr<'a>) -> Result<(Expr<'a>, Span)> {
        let mut vals = vec![expr];

        let mut span = self.span;

        let _file = self.file.clone();

        loop {
            if unwrap_some!(self.tokens.peek()).type_ == TokenType::RParen {
                self.advance(); // eat ')'
                break;
            }
            vals.push(self.parse_expression()?.0);
            span.1 = self.span.1;
            if unwrap_some!(self.tokens.peek()).type_ == TokenType::RParen {
                self.advance(); // eat ')'
                break;
            }
            if unwrap_some!(self.tokens.peek()).type_ == TokenType::Comma {
                self.advance(); // eat ','
            } else {
                return Err(format!(
                    "expected ',' or `)` in tuple found {} instead. unclosed delimiter in tuple.",
                    self.advance().type_
                ));
            }
        }
        span.1 = self.span.1;

        Ok((Expr::Tuple(vals, span, self.file.clone()), span))
    }

    fn parse_if(&mut self) -> Result<(Expr<'a>, Span)> {
        let mut span = self.span;

        let _file = self.file.clone();

        let cond = self.parse_expression()?.0;

        match unwrap_some!(self.tokens.peek()).type_ {
            TokenType::Then => {}
            ref x => {
                return Err(format!(
                    "Expected `then` after `if` condition. 
                    found `{x}`."
                ))
            }
        }
        self.advance(); // eat 'then'

        let if_branch = self.parse_expression()?.0;
        span.1 = self.span.1;

        match unwrap_some!(self.tokens.peek()).type_ {
            TokenType::Else => {
                self.advance();
                let else_branch = self.parse_expression()?.0;
                span.1 = self.span.1;

                Ok((
                    Expr::If(
                        Box::new(cond),
                        Box::new(if_branch),
                        Some(Box::new(else_branch)),
                        span,
                        self.file.clone(),
                    ),
                    span,
                ))
            }
            _ => Ok((
                Expr::If(
                    Box::new(cond),
                    Box::new(if_branch),
                    None,
                    span,
                    self.file.clone(),
                ),
                span,
            )),
        }
    }

    fn parse_let(&mut self) -> Result<(Expr<'a>, Span)> {
        let mut span = self.span;

        let _file = self.file.clone();

        let identifier: std::borrow::Cow<'_, str> = match self.advance().type_ {
            TokenType::Identifier(ref i) => std::borrow::Cow::Owned(i.clone()),
            ref x => {
                return Err(format!(
                    "Expected `identifier` after `let` keyword. Found `{x}`"
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
            ref x => return Err(format!("expected `=` after `let`. Found `{x}`")),
        }

        let val = self.parse_expression()?.0;
        span.1 = self.span.1;

        Ok((
            Expr::Let(identifier, type_, Box::new(val), span, self.file.clone()),
            span,
        ))
    }

    fn parse_lambda(&mut self) -> Result<(Expr<'a>, Span)> {
        let mut args = vec![];

        let mut span = self.span;

        let _file = self.file.clone();

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
                            ref x => return Err(format!("expected identifier. found `{x}`")),
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
                    "expected arguments after function name. found `{x}`"
                ))
            }
        };
        span.1 = self.span.1;

        Ok((
            Expr::Lambda(
                args,
                Box::new(self.parse_expression()?.0),
                span,
                self.file.clone(),
            ),
            span,
        ))
    }
}
