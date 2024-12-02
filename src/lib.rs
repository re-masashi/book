use clap::{Parser, Subcommand};
use std::path::PathBuf;

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

#[derive(Parser)]
#[command(version, about, long_about = None)]
pub struct Cli {
    /// todo: Sets a custom config file
    #[arg(short, long, value_name = "CONFIG")]
    pub config: Option<PathBuf>,

    /// todo: Turn debugging information on
    #[arg(short, long, action = clap::ArgAction::Count)]
    pub debug: u8,

    #[command(subcommand)]
    pub command: Commands,
}

#[derive(Subcommand)]
pub enum Commands {
    /// builds and runs a file
    Run { file: String },
    /// builds a file
    Build { file: String },
}
