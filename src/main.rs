use book::{
    interpreter::{codegen::IRGenerator, TypeEnv},
    lexer::{tokens::Token, tokens::TokenType, Lexer},
    parser::Parser,
};
use std::{collections::HashMap, process};

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let source_path = "examples/example_struct.bk";
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

    // println!("Size of Literal: {:?}", std::mem::size_of::<Literal>());
    // println!("Size of TypedNode: {:?}", std::mem::size_of::<TypedNode>());

    Ok(())
}
