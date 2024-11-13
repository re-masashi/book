pub mod lexer;
pub mod parser;
pub mod interpreter;

pub type Result<T> = std::result::Result<T, String>;

#[macro_export]
macro_rules! unwrap_some {
    ($val:expr) => {
        match $val {
            Some(s) => s,
            None => return Err("EOF".to_string()),
        }
    };
}