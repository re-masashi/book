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
}

impl TokenType {
    pub fn to_string(&self) -> String {
        match self {
            TokenType::Int(i) => i.to_string(),
            TokenType::Float(f) => f.to_string(),
            TokenType::String(s) => s.to_string(),
            TokenType::Identifier(i) => i.to_string(),

            // keywords
            TokenType::Let => "let".to_string(),
            TokenType::Fn => "fn".to_string(),
            TokenType::If => "if".to_string(),
            TokenType::Else => "else".to_string(),
            TokenType::While => "while".to_string(),
            TokenType::In => "in".to_string(),
            TokenType::As => "as".to_string(),
            TokenType::Def => "def".to_string(),
            TokenType::Do => "do".to_string(),
            TokenType::End => "end".to_string(),
            TokenType::Then => "then".to_string(),
            TokenType::Use => "use".to_string(),
            TokenType::True => "true".to_string(),
            TokenType::False => "false".to_string(),

            TokenType::Plus => "+".to_string(),
            TokenType::Minus => "-".to_string(),
            TokenType::Mul => "*".to_string(),
            TokenType::Div => "/".to_string(),
            TokenType::PlusEq => "+=".to_string(),
            TokenType::MinusEq => "-=".to_string(),
            TokenType::MulEq => "*=".to_string(),
            TokenType::DivEq => "/=".to_string(),
            TokenType::Pipe => "|>".to_string(),
            TokenType::Comprehension => "<-".to_string(),

            TokenType::Assign => "=".to_string(),
            TokenType::And => "and".to_string(),
            TokenType::Or => "or".to_string(),
            TokenType::Not => "not".to_string(),
            TokenType::Equal => "==".to_string(),
            TokenType::NotEq => "!=".to_string(),
            TokenType::Greater => ">".to_string(),
            TokenType::Less => "<".to_string(),
            TokenType::GreaterEq => ">=".to_string(),
            TokenType::LessEq => "<=".to_string(),

            TokenType::LParen => "(".to_string(),
            TokenType::RParen => ")".to_string(),
            TokenType::LBrack => "[".to_string(),
            TokenType::RBrack => "]".to_string(),
            TokenType::LBrace => "{".to_string(),
            TokenType::RBrace => "}".to_string(),
            TokenType::Colon => ":".to_string(),
            TokenType::Semicolon => ";".to_string(),
            TokenType::Comma => ",".to_string(),
            TokenType::Dot => ".".to_string(),
            TokenType::Arrow => "->".to_string(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Token {
    pub type_: TokenType,
    pub pos: i32,
    pub line_no: i32,
    pub file: String,
}
