use book::{
    interpreter::{codegen::IRGenerator, TypeEnv},
    lexer::{tokens::Token, tokens::TokenType, Lexer},
    parser::Parser,
    Cli, Commands,
};
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

    match cli.debug {
        0 => {}
        _ => todo!(),
    }

    match &cli.command {
        Some(Commands::Run { file }) => {
            build_file(file)?;
            run_file(file)?;
        }
        Some(Commands::Build { file }) => {
            build_file(file)?;
        }
        None => {}
    }

    // println!("Size of Literal: {:?}", std::mem::size_of::<Literal>());
    // println!("Size of TypedNode: {:?}", std::mem::size_of::<TypedNode>());

    Ok(())
}

fn build_file(source_path: &String) -> Result<(), Box<dyn std::error::Error>> {
    let lexer = Lexer::from_file(source_path)?;
    let mut tokens = lexer.map(|t| t.unwrap()).collect::<Vec<_>>();
    tokens.push(Token {
        type_: TokenType::Int(0),
        pos: 1,
        line_no: 0,
        file: source_path.to_string(),
    }); // HACK: adding a blank expression to the end of the parser. else, the last expression isn't parsed
    let mut parser = Parser::new(tokens.into_iter().peekable(), source_path);

    let mut env = TypeEnv(HashMap::new());
    let mut substitutions = HashMap::new();

    let ast = match parser.parse_program() {
        Ok(ast) => ast,
        Err(e) => {
            parser.error(e);
            // eprintln!("parser error encountered. quitting!");
            process::exit(0);
        }
    };
    let typed_ast = env.node_to_type(&ast, &mut substitutions);
    // println!("AST (pre-opt imization): {:#?}", ast);

    // optimise_ast::optimise_node(&mut ast);
    //println!("AST (post-optimization): {:#?}", ast);

    let context = inkwell::context::Context::create();
    let mut generator = IRGenerator::new(&context, source_path.to_string());
    generator.gen_program(&typed_ast)?;
    Ok(())
}

fn run_file(source_path: &str) -> Result<(), Box<dyn std::error::Error>> {
    let exec_name = &(source_path.to_owned()[..=source_path.len() - 4].to_string() + ".out");
    Command::new(exec_name)
        .status()
        .expect("failed to execute process");
    Ok(())
}
