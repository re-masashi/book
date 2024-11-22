pub mod tokens;

use crate::lexer::tokens::{Token, TokenType};
use crate::Result;
use std::iter::Peekable;
use std::vec::IntoIter;
use std::{fs, io};

pub struct Lexer {
    raw_data: Peekable<IntoIter<char>>,
    pos: i32,
    line_no: i32,
    file: String,
}

impl Lexer {
    /// Create a lexer from a program file given the path to the file.
    ///
    /// # Arguments
    /// * `file_path` - The path to the program file.
    pub fn from_file(file_path: &str) -> io::Result<Self> {
        let mut text = &fs::read_to_string(file_path)?;
        let binding = text.to_owned() + "";
        text = &binding;
        Ok(Self::from_text(text, file_path))
    }

    /// Create a lexer with the program data in plain text.
    ///
    /// # Arguments
    /// * `text` - The raw program.
    pub fn from_text(text: &str, file_path: &str) -> Self {
        Lexer {
            raw_data: text.chars().collect::<Vec<_>>().into_iter().peekable(),
            pos: -1,
            line_no: 1,
            file: file_path.to_string(),
        }
    }

    /// Create a token by eating characters while a condition is met.
    ///
    /// # Arguments
    /// * `raw_token` - The raw string token to append characters to.
    /// * `cond` - The condition that must be met.
    fn get_next_char_while(&mut self, raw_token: &mut String, cond: fn(char) -> bool) {
        loop {
            match self.raw_data.peek() {
                Some(c) if cond(*c) => {
                    if *c != '\n' {
                        self.pos += 1;
                    } else {
                        self.line_no += 1;
                        self.pos = 0;
                    };
                    raw_token.push(*c);
                    self.raw_data.next();
                }
                _ => {
                    break;
                }
            }
        }
    }

    /// Check if a character is a part of an identifier.
    ///
    /// Identifiers must start with an alphabetic character or underscore, and then can have
    /// alphanumeric characters and underscores.
    ///
    fn is_in_identifier(c: char) -> bool {
        c.is_ascii_alphanumeric() || c == '_'
    }
}

impl Iterator for Lexer {
    type Item = Result<Token>;

    /// Identifies the next token
    fn next(&mut self) -> Option<Self::Item> {
        let token: Result<TokenType>;
        let current_char: char;
        // Find first non-whitespace character
        loop {
            match self.raw_data.next() {
                Some(' ') | Some('\t') => {
                    self.pos += 1;
                    continue;
                }
                Some('\n') => {
                    self.line_no += 1;
                    self.pos = 0;
                    continue;
                }
                // Comment
                Some('#') => {
                    let mut dump = String::new();
                    self.get_next_char_while(&mut dump, |c| c != '\n');
                    // println!("Lexing comment");
                    continue;
                }
                Some(c) => {
                    current_char = c;
                    self.pos += 1;
                    break;
                }
                None => return None,
            }
        }

        // println!("First char: {}", current_char);

        // Identifier
        if Self::is_in_identifier(current_char) && !current_char.is_numeric() {
            let mut name = current_char.to_string();
            self.get_next_char_while(&mut name, Self::is_in_identifier);
            // we check for identifiers.
            // if the identifier is a keyword, we add it as a keyword
            // else we treat it like a regular identifier
            match name {
                s if *"let" == s => token = Ok(TokenType::Let),
                s if *"fn" == s => token = Ok(TokenType::Fn),
                s if *"if" == s => token = Ok(TokenType::If),
                s if *"then" == s => token = Ok(TokenType::Then),
                s if *"else" == s => token = Ok(TokenType::Else),
                // s if *"for" == s => token = Ok(TokenType::For),
                s if *"while" == s => token = Ok(TokenType::While),
                s if *"in" == s => token = Ok(TokenType::In),
                s if *"as" == s => token = Ok(TokenType::As),
                s if *"do" == s => token = Ok(TokenType::Do),
                s if *"def" == s => token = Ok(TokenType::Def),
                s if *"end" == s => token = Ok(TokenType::End),
                s if *"use" == s => token = Ok(TokenType::Use),
                s if *"true" == s => token = Ok(TokenType::True),
                s if *"false" == s => token = Ok(TokenType::False),
                s => token = Ok(TokenType::Identifier(s)),
            };
        }
        // Integer Literal
        else if current_char.is_numeric() {
            let mut value = current_char.to_string();
            self.get_next_char_while(&mut value, |c| c.is_numeric());

            // println!("{:?}", current_char);

            if self.raw_data.peek() == Some(&'.') {
                println!("float!");
                value += ".";
                self.raw_data.next(); // eat '.'
                self.get_next_char_while(&mut value, |c| c.is_numeric());
                token = match value.parse() {
                    Ok(i) => Ok(TokenType::Float(i)),
                    Err(_) => Err(format!("Integer literal {} is invalid", value)),
                }
            } else {
                token = match value.parse() {
                    Ok(i) => Ok(TokenType::Int(i)),
                    Err(_) => Err(format!("Integer literal {} is invalid", value)),
                }
            }
        }
        // String Literal
        else if current_char == '"' {
            let mut value = String::new();

            self.get_next_char_while(&mut value, |c| c != '"');
            self.raw_data.next(); // Eat trailing "

            token = Ok(TokenType::String(value));
        } else if current_char == '\'' {
            let mut value = String::new();

            self.get_next_char_while(&mut value, |c| c != '\'');
            self.raw_data.next(); // Eat trailing "

            token = Ok(TokenType::String(value));
        } else if current_char == '`' {
            let mut value = String::new();

            self.get_next_char_while(&mut value, |c| c != '`');
            self.raw_data.next(); // Eat trailing "

            token = Ok(TokenType::String(value));
        } else if current_char == '|' {
            if self.raw_data.peek() == Some(&'>') {
                self.raw_data.next(); // Eat =
                token = Ok(TokenType::PlusEq);
            } else {
                token = Ok(TokenType::Unknown);
            }
            // todo: panic?
        }
        // Semicolon
        else if current_char == ';' {
            token = Ok(TokenType::Semicolon);
        }
        // Colon
        else if current_char == ':' {
            token = Ok(TokenType::Colon);
        }
        // Dot
        else if current_char == '.' {
            token = Ok(TokenType::Dot);
        }
        // Comma
        else if current_char == ',' {
            token = Ok(TokenType::Comma);
        }
        // LParen
        else if current_char == '(' {
            token = Ok(TokenType::LParen);
        }
        // RParen
        else if current_char == ')' {
            token = Ok(TokenType::RParen);
        }
        // LBrack
        else if current_char == '[' {
            token = Ok(TokenType::LBrack);
        }
        // RBrack
        else if current_char == ']' {
            token = Ok(TokenType::RBrack);
        }
        // LBrace
        else if current_char == '{' {
            token = Ok(TokenType::LBrace);
        }
        // RBrace
        else if current_char == '}' {
            token = Ok(TokenType::RBrace);
        }
        // Plus and PlusEq
        else if current_char == '+' {
            if self.raw_data.peek() == Some(&'=') {
                self.raw_data.next(); // Eat =
                token = Ok(TokenType::PlusEq);
            } else {
                token = Ok(TokenType::Plus);
            }
        }
        // Minus, Arrow and MinusEq
        else if current_char == '-' {
            if self.raw_data.peek() == Some(&'=') {
                self.raw_data.next(); // Eat =
                token = Ok(TokenType::MinusEq);
            } else if self.raw_data.peek() == Some(&'>') {
                self.raw_data.next();
                token = Ok(TokenType::Arrow);
            } else {
                token = Ok(TokenType::Minus);
            }
        }
        // Mul and MulEq
        else if current_char == '*' {
            if self.raw_data.peek() == Some(&'=') {
                self.raw_data.next(); // Eat =
                token = Ok(TokenType::MulEq);
            } else {
                token = Ok(TokenType::Mul);
            }
        }
        // Div and DivEq
        else if current_char == '/' {
            if self.raw_data.peek() == Some(&'=') {
                self.raw_data.next(); // Eat =
                token = Ok(TokenType::DivEq);
            } else {
                token = Ok(TokenType::Div);
            }
        }
        // Less and LessEq
        else if current_char == '<' {
            if self.raw_data.peek() == Some(&'=') {
                self.raw_data.next(); // Eat =
                token = Ok(TokenType::LessEq);
            } else if self.raw_data.peek() == Some(&'-') {
                self.raw_data.next(); // Eat -
                token = Ok(TokenType::Comprehension);
            } else {
                token = Ok(TokenType::Less);
            }
        }
        // Greater and GreaterEq
        else if current_char == '>' {
            if self.raw_data.peek() == Some(&'=') {
                self.raw_data.next(); // Eat =
                token = Ok(TokenType::GreaterEq);
            } else {
                token = Ok(TokenType::Greater);
            }
        }
        // Assign and Equal
        else if current_char == '=' {
            if self.raw_data.peek() == Some(&'=') {
                self.raw_data.next(); // Eat =
                token = Ok(TokenType::Equal);
            } else {
                token = Ok(TokenType::Assign);
            }
        }
        // Not and NotEq
        else if current_char == '!' {
            if self.raw_data.peek() == Some(&'=') {
                self.raw_data.next(); // Eat =
                token = Ok(TokenType::NotEq);
            } else {
                token = Ok(TokenType::Not);
            }
        } else {
            token = Ok(TokenType::Unknown)
        }

        Some(Ok(Token {
            type_: token.unwrap(),
            pos: self.pos,
            line_no: self.line_no,
            file: self.file.clone(),
        }))
    }
}
