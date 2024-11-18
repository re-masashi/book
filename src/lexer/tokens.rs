#[derive(Debug, PartialEq, Clone)]
pub enum TokenType {
    Int(i64),
    Float(f64),
    String(String),
    Identifier(String),

    // keywords
    Let,
    Fn,
    If,
    Else,
    // For,
    While,
    In,
    As,
    Def,
    Do,
    End,
    Then,
    Use,
    True,
    False,

    // did not add True and False as a Boolean(b) variant instead because
    // keeping keywords together is more neat and sane (imo)

    // operators
    Plus,
    Minus,
    Mul,
    Div,
    PlusEq,
    MinusEq,
    MulEq,
    DivEq,
    Pipe,
    Comprehension,
    Assign,

    And,
    Or,
    Not,
    Equal,
    NotEq,
    Greater,
    Less,
    GreaterEq,
    LessEq,

    // punctuators
    LParen,
    RParen,
    LBrack,
    RBrack,
    LBrace,
    RBrace,
    Colon,
    Semicolon,
    Comma,
    Dot,
    Arrow,

    Unknown,
}

#[derive(Debug, Clone)]
pub struct Token {
    pub type_: TokenType,
    pub pos: i32,
    pub line_no: i32,
    pub file: String,
}
