use book::{
    interpreter::{codegen::IRGenerator, TypeEnv},
    lexer::{tokens::Span, tokens::Token, tokens::TokenType, Lexer},
    parser::Parser,
    Cli, Commands,
};
use log::{trace, LevelFilter};
use owo_colors::OwoColorize;
use std::{
    collections::HashMap,
    process::{self, Command},
};

fn main() -> Result<(), Box<dyn std::error::Error>> {
    use clap::Parser;

    let cli = Cli::parse();

    if let Some(_config_path) = cli.config.as_deref() {
        todo!("config files not supported yet")
        // println!("Value for config: {}", config_path.display());
    }

    env_logger::builder()
        .format_timestamp(None)
        .format_module_path(false)
        .filter_level(match cli.debug {
            0 => LevelFilter::Warn,
            1 => LevelFilter::Debug,
            _ => LevelFilter::Trace,
        })
        .init();

    match &cli.command {
        Commands::Run { file } => {
            build_file(file)?;
            run_file(file)?;
        }
        Commands::Build { file } => {
            build_file(file)?;
        }
    }

    Ok(())
}

fn build_file(source_path: &String) -> Result<(), Box<dyn std::error::Error>> {
    trace!("`{source_path}` File is being read");
    let lexer = Lexer::from_file(source_path)?;
    let mut tokens = lexer.map(|t| t.unwrap()).collect::<Vec<_>>();
    tokens.push(Token {
        type_: TokenType::Int(0),
        span: Span((0, 1), (0, 1)),
        file: source_path.to_string(),
    }); // HACK: adding a blank expression to the end of the parser. else, the last expression isn't parsed
    let mut parser = Parser::new(tokens.into_iter().peekable(), source_path);

    let mut env = TypeEnv(HashMap::new());
    let mut substitutions = HashMap::new();

    trace!("Parsing started");
    let ast = match parser.parse_program() {
        Ok(ast) => ast,
        Err(e) => {
            parser.error(e);
            process::exit(0);
        }
    };
    trace!("Typechecking started");
    let typed_ast = env.node_to_type(&ast, &mut substitutions);

    trace!("Creating inkwell context");
    let context = inkwell::context::Context::create();
    let mut generator = IRGenerator::new(&context, source_path.to_string());

    trace!("Starting codegen");
    generator.gen_program(&typed_ast)?;
    let exec_name = &(source_path.to_owned()[..=source_path.len() - 4].to_string() + ".out");
    println!("executable `{}` created successfully", exec_name.green());
    Ok(())
}

fn run_file(source_path: &str) -> Result<(), Box<dyn std::error::Error>> {
    let exec_name = &(source_path.to_owned()[..=source_path.len() - 4].to_string() + ".out");
    println!("running `{}`", exec_name.green());
    Command::new(exec_name)
        .status()
        .expect("failed to execute process");
    Ok(())
}
