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
#[command(version = "0.0.1", about, long_about = "A compiler for Book.")]
pub struct Cli {
    /// todo: Sets a custom config file
    #[arg(short, long, value_name = "CONFIG")]
    pub config: Option<PathBuf>,

    #[arg(short, long, action = clap::ArgAction::Count)]
    pub debug: u8,

    #[arg(short = 'O', long, default_value_t = 2)]
    pub optimisation_level: u8,

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
