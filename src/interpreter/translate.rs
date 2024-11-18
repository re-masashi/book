use crate::interpreter::{BinaryOperator, Literal, Type, TypeConstructor, TypedExpr, TypedNode};
use crate::tconst;
use std::collections::HashMap;
use std::sync::Arc;

#[derive(Debug)]
pub struct Generator<'a> {
    file: String,
    variables: HashMap<String, TypedExpr<'a>>,
    
    /// Variables in the current scope
    scope_var_names: Vec<Vec<String>>,
    functions: Vec<TypedNode<'a>>,
}

impl Generator<'_> {
    pub fn new() -> Self {
        Generator{
            file: "".to_string(),
            variables: HashMap::new(),
            scope_var_names: vec![],
            functions: vec![]
        }
    }

    pub fn compile(&mut self, program: &TypedNode) {
        self.file = include_str!("starting.c").to_string();
        let mut main_content = "".to_string();
        match program {
            TypedNode::Program(nodes) => {
                for node in nodes {
                    match node {
                        TypedNode::Expr(expr, _)=>{
                            main_content=main_content+&self.compile_expr(expr)+";";
                            // top level expressions go in main
                        }
                        TypedNode::Function(name, args, expr, ty)=>{
                            let mut typename = "".to_string();
                            
                            let stringname1 = "i32".to_string();

                            if let Type::Constructor(TypeConstructor {
                                name: stringname1,
                                ..
                            }) = ty.as_ref() {
                                typename="int".to_string();
                            }

                            let stringname2 = "float".to_string();
                            if let Type::Constructor(TypeConstructor {
                                name: stringname2,
                                ..
                            }) = ty.as_ref() {
                                typename="float".to_string();
                            }
                            
                            self.file = self.file.clone()+&format!(r#"
                                {typename} {name}({}){}
                                    {}   
                                {}
                            "#,
                            args.into_iter().map(|(argname, _)|{format!("int {argname}")}).collect::<Vec<_>>().join(", "),
                            "{",
                            self.compile_expr(expr),
                            "}",
                            );
                        }
                        _=>todo!()
                    }
                }
                self.file=self.file.clone()+&main_content;
            },
            _=>unreachable!()
        }
        println!("{}", self.file);
    }

    fn compile_expr(&mut self, expr: &TypedExpr) -> String {
        match expr {
            TypedExpr::Let(name, ref expr, ty) => {
                let stringname1 = "i32".to_string();
                if let Type::Constructor(TypeConstructor {
                    name: stringname1,
                    ..
                }) = ty.as_ref() {
                    return format!("int {:?} = {:?}", name, self.compile_expr(expr));
                }

                let stringname2 = "float".to_string();
                if let Type::Constructor(TypeConstructor {
                    name: stringname2,
                    ..
                }) = ty.as_ref() {
                    return format!("float {:?} = {:?}", name, self.compile_expr(expr));
                }
                
                panic!("erm idk how to implement that type yet. ");
            },
            TypedExpr::Variable(name, _) => {
                return name.to_string()
            }
            TypedExpr::Lambda(_, expr, _) => todo!(),
            TypedExpr::Literal(lit, _) => {
                return match lit.as_ref() {
                    Literal::Int(i) => format!("{:?}", i),
                    Literal::Float(f) => format!("{:?}", f),
                    Literal::Boolean(b) => format!("{:?}", b),
                    Literal::String(s) => format!("{:?}", s),
                }
            }
            TypedExpr::If(cond, if_, else_, ty) => {
                let mut typename = "".to_string();
                
                let stringname1 = "i32".to_string();

                if let Type::Constructor(TypeConstructor {
                    name: stringname1,
                    ..
                }) = ty.as_ref() {
                    typename="i32".to_string();
                }

                let stringname2 = "float".to_string();
                if let Type::Constructor(TypeConstructor {
                    name: stringname2,
                    ..
                }) = ty.as_ref() {
                    typename="i32".to_string();
                }

                return format!(
                    "
                    ({})?({}):({})
                    ",
                    self.compile_expr(&**cond),
                    
                    self.compile_expr(&**if_),
                    match else_ {
                        Some(s) => self.compile_expr(&**s),
                        None => "".to_string(),
                    },
                    
                )
            }
            _=>todo!()
        };
        "".to_string()
    }
}