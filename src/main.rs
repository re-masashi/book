use book::interpreter::TypeEnv;
use book::lexer::Lexer;
use book::parser::Parser;

use std::collections::HashMap;

fn main() {
    let mut parser = Parser::new(
        Lexer::from_file("examples/first.bk")
            .unwrap()
            .map(|t| t.unwrap())
            .collect::<Vec<_>>()
            .into_iter()
            .peekable(),
        "",
    );
    // let mut ast = vec![];

    let mut env = TypeEnv(HashMap::new());
    let mut substitutions = HashMap::new();

    let binding = parser.parse_program().unwrap();
    println!("ast: {:#?}", binding);
    let mut node_ = &mut env.node_to_type(&binding, &mut substitutions);
    book::interpreter::optimise_ast::optimise_node(&mut node_);
    // println!("ast: {:#?}", node_);
    println!("\n\n\n\n");

    let binding = inkwell::context::Context::create();
    let mut generator = book::interpreter::translate::Generator::new(&binding, "");
    generator.generate_program(node_).unwrap();

    println!(
        "size_of Literal: {:?}",
        std::mem::size_of::<book::interpreter::Literal>()
    );
    println!(
        "size_of AST: {:?}",
        std::mem::size_of::<book::interpreter::TypedNode>()
    );
}
