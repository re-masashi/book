use book::lexer::Lexer;
use book::parser::Parser;

fn main() {
    println!(
        "{:#?}",
        Parser::new(
            Lexer::from_text(r#"
                myfn x y z = do 
                    x+y+z
                    "hello"
                    1+1.0
                    hello(1,2);
                    true
                    false
                    a=1
                    b=2
                    [i32 1,z]
                    use "myfile"
                    if true 2 else 2
                end
            "#, "")
                .map(|t| t.unwrap())
                .collect::<Vec<_>>()
                .into_iter()
                .peekable(),
            ""
        )
        .parse_program()
    );
    println!("size_of Literal: {:?}", std::mem::size_of::<book::interpreter::Literal>());
    println!("size_of AST: {:?}", std::mem::size_of::<book::interpreter::TypedNode>());
    book::interpreter::dosumn();
    book::interpreter::insert_types();
}

