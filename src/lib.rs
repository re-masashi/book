pub mod interpreter;
pub mod lexer;
pub mod parser;

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
