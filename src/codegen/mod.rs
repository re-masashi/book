#![allow(dead_code)]

pub mod cfg;
pub mod optimise_ast;
// pub mod utils;
pub mod generator;
pub mod typechecker;

#[cfg(test)]
pub mod tests;

use std::borrow::Cow;
use std::collections::HashMap;
use std::iter::zip;
use std::sync::Arc;

#[macro_export]
macro_rules! tvar {
    ($i:expr) => {
        Arc::new(Type::Variable(TypeVariable($i)))
    };
}

#[macro_export]
macro_rules! tconst {
    // ($name:expr,$($generic:expr),*,$($trait_:expr),*) => {
    //     Arc::new(Type::Constructor(TypeConstructor {
    //         name: $name.to_string(),
    //         generics: vec![$($generic),*],
    //         traits: vec![$($trait_),*],
    //     }))
    // };
    ($name:expr,$($generic:expr),*) => {
        Arc::new(Type::Constructor(TypeConstructor {
            name: $name.to_string(),
            generics: vec![$($generic),*],
            traits: vec![],
        }))
    };
    ($name:expr) => { tconst!($name,) };
}

#[macro_export]
macro_rules! t_int {
    () => {
        Arc::new(Type::Constructor(TypeConstructor {
            name: "int".to_string(),
            generics: vec![],
            traits: vec![
                "Number".to_string(),
                "Printable".to_string(),
                "Simple".to_string(),
            ],
        }))
    };
}

#[macro_export]
macro_rules! t_float {
    () => {
        Arc::new(Type::Constructor(TypeConstructor {
            name: "float".to_string(),
            generics: vec![],
            traits: vec![
                "Number".to_string(),
                "Printable".to_string(),
                "Simple".to_string(),
            ],
        }))
    };
}

#[macro_export]
macro_rules! t_str {
    () => {
        Arc::new(Type::Constructor(TypeConstructor {
            name: "str".to_string(),
            generics: vec![],
            traits: vec!["Printable".to_string(), "Simple".to_string()],
        }))
    };
}

#[macro_export]
macro_rules! t_bool {
    () => {
        Arc::new(Type::Constructor(TypeConstructor {
            name: "bool".to_string(),
            generics: vec![],
            traits: vec!["Printable".to_string(), "Simple".to_string()],
        }))
    };
}

#[derive(Debug, Clone)]
pub enum TypedNode<'a> {
    Function(
        Cow<'a, str>,
        // Vec<Arc<Type>>, // generics
        Vec<(Cow<'a, str>, Arc<Type>)>,
        Box<TypedExpr<'a>>,
        Arc<Type>,
    ),
    Struct(
        Cow<'a, str>,
        Vec<Cow<'a, str>>,              // generics
        Vec<(Cow<'a, str>, Arc<Type>)>, // fields
    ),
    Expr(Box<TypedExpr<'a>>, Arc<Type>),
    Error(String),
    Program(Vec<TypedNode<'a>>),
    Extern(Cow<'a, str>, Vec<Arc<Type>>, Arc<Type>),
}

#[derive(Debug, Clone)]
pub enum TypedExpr<'a> {
    Let(Cow<'a, str>, Box<TypedExpr<'a>>, Arc<Type>), // name, value
    Variable(Cow<'a, str>, Arc<Type>),                // name
    Lambda(
        Vec<(Cow<'a, str>, Arc<Type>)>,
        Box<TypedExpr<'a>>,
        Arc<Type>,
    ), // args, expression (aka body)
    Literal(Arc<Literal<'a>>, Arc<Type>),             // literal
    If(
        Box<TypedExpr<'a>>,
        Box<TypedExpr<'a>>,
        Option<Box<TypedExpr<'a>>>,
        Arc<Type>,
    ), // condition, if_branch, else_branch
    Call(Box<TypedExpr<'a>>, Vec<TypedExpr<'a>>, Arc<Type>), // value to call, arguments
    While(Box<TypedExpr<'a>>, Box<TypedExpr<'a>>, Arc<Type>), // condition, body
    Break,
    Continue,
    BinaryOp(
        Box<TypedExpr<'a>>,
        &'a BinaryOperator,
        Box<TypedExpr<'a>>,
        Arc<Type>,
    ), // lhs, op, rhs
    UnaryOp(&'a UnaryOperator, Box<TypedExpr<'a>>, Arc<Type>), // op, val
    Array(Vec<TypedExpr<'a>>, Arc<Type>),
    Do(Vec<TypedExpr<'a>>, Arc<Type>),
    Index(Box<TypedExpr<'a>>, Box<TypedExpr<'a>>, Arc<Type>),
    StructAccess(Box<TypedExpr<'a>>, Cow<'a, str>, Arc<Type>),
    Return(Box<TypedExpr<'a>>, Arc<Type>),
    Tuple(Vec<TypedExpr<'a>>, Arc<Type>),
    Assign(Box<TypedExpr<'a>>, Box<TypedExpr<'a>>, Arc<Type>), // variable name, expression, type
}

pub struct TypeEnv(pub HashMap<String, Arc<Type>>);

#[derive(Debug, Clone)]
pub enum Node<'a> {
    Function(
        Cow<'a, str>,
        // Vec<String>,
        Vec<(Cow<'a, str>, Option<TypeAnnot>)>,
        Box<Expr<'a>>,
        Option<TypeAnnot>,
    ),
    Struct(
        Cow<'a, str>,
        Vec<Cow<'a, str>>,              // generics
        Vec<(Cow<'a, str>, TypeAnnot)>, // fields
    ),
    Expr(Box<Expr<'a>>),
    Error(String),
    Program(Vec<Node<'a>>),
    Extern(Cow<'a, str>, Vec<TypeAnnot>, TypeAnnot),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr<'a> {
    Let(Cow<'a, str>, Option<TypeAnnot>, Box<Expr<'a>>), // name, value
    Variable(Cow<'a, str>),                              // name
    Lambda(Vec<(Cow<'a, str>, Option<TypeAnnot>)>, Box<Expr<'a>>), // args, expression (aka body)
    Literal(Arc<Literal<'a>>),                           // literal
    If(Box<Expr<'a>>, Box<Expr<'a>>, Option<Box<Expr<'a>>>), // condition, if_branch, else_branch
    Call(Box<Expr<'a>>, Vec<Expr<'a>>),                  // value to call, arguments
    While(Box<Expr<'a>>, Box<Expr<'a>>),                 // condition, body
    Break,
    Continue,
    BinaryOp(Box<Expr<'a>>, BinaryOperator, Box<Expr<'a>>), // lhs, op, rhs
    UnaryOp(UnaryOperator, Box<Expr<'a>>),                  // op, val
    Array(Vec<Expr<'a>>),
    Do(Vec<Expr<'a>>),
    Index(Box<Expr<'a>>, Box<Expr<'a>>), // value, index
    StructAccess(Box<Expr<'a>>, Cow<'a, str>),
    Return(Box<Expr<'a>>),
    Tuple(Vec<Expr<'a>>),
    Assign(Box<Expr<'a>>, Box<Expr<'a>>), // variable name, expression
}

#[derive(Debug, PartialEq)]
pub enum Literal<'a> {
    Boolean(bool),
    Int(i64),
    Float(f64),
    String(Cow<'a, str>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum BinaryOperator {
    Add,
    Sub,
    Mul,
    Div,
    And,
    Or,
    Equal,
    NotEqual,
    Less,
    Greater,
    LessEqual,
    GreaterEqual,
}

#[derive(Debug, Clone, PartialEq)]
pub enum UnaryOperator {
    Negate,
    Not,
}

#[derive(Debug, Clone, PartialEq)]
pub struct TypeAnnot {
    pub name: String,
    pub generics: Vec<String>,
    pub traits: Vec<String>,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Type {
    Constructor(TypeConstructor),
    Variable(TypeVariable),
    Trait(String), // a specified trait bound
    Function(Vec<Arc<Type>>, Arc<Type>),
    Struct(String, Vec<String>, Vec<(String, Arc<Type>)>),
    Tuple(Vec<Arc<Type>>),
}

impl Type {
    fn substitute(&self, substitutions: &HashMap<TypeVariable, Arc<Type>>) -> Arc<Type> {
        match self {
            Type::Constructor(TypeConstructor {
                name,
                generics,
                traits,
            }) => Arc::new(Type::Constructor(TypeConstructor {
                name: name.clone(),
                generics: generics
                    .iter()
                    .map(|t| t.substitute(substitutions))
                    .collect(),
                traits: traits.clone(),
            })),
            Type::Variable(TypeVariable(i)) => {
                if let Some(t) = substitutions.get(&TypeVariable(*i)) {
                    t.substitute(substitutions)
                } else {
                    Arc::new(self.clone())
                }
            }
            Type::Function(args, ret) => Arc::new(Type::Function(
                args.iter().map(|t| t.substitute(substitutions)).collect(),
                ret.substitute(substitutions),
            )),
            Type::Struct(..) => self.clone().into(),
            Type::Trait(_) => self.clone().into(),
            Type::Tuple(_) => self.clone().into(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TypeConstructor {
    name: String,
    generics: Vec<Arc<Type>>,
    traits: Vec<String>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TypeVariable(usize);

impl TypeVariable {
    fn occurs_in(&self, ty: Arc<Type>, substitutions: &HashMap<TypeVariable, Arc<Type>>) -> bool {
        match ty.as_ref() {
            Type::Variable(v @ TypeVariable(i)) => {
                if let Some(substitution) = substitutions.get(v) {
                    if substitution.as_ref() != &Type::Variable(*v) {
                        return self.occurs_in(substitution.clone(), substitutions);
                    }
                }

                self.0 == *i
            }
            Type::Constructor(TypeConstructor { generics, .. }) => {
                for generic in generics {
                    if self.occurs_in(generic.clone(), substitutions) {
                        return true;
                    }
                }

                false
            }
            Type::Function(_, _) => false,
            // Type::Struct(_, _, _) => todo!(),
            x => todo!("{:?}", x),
        }
    }
}

fn unify(left: Arc<Type>, right: Arc<Type>, substitutions: &mut HashMap<TypeVariable, Arc<Type>>) {
    match (left.as_ref(), right.as_ref()) {
        (
            Type::Constructor(TypeConstructor {
                name: name1,
                generics: generics1,
                traits: _traits1,
            }),
            Type::Constructor(TypeConstructor {
                name: name2,
                generics: generics2,
                traits: _traits2,
            }),
        ) => {
            assert_eq!(name1, name2);
            assert_eq!(generics1.len(), generics2.len());

            for (left, right) in zip(generics1, generics2) {
                unify(left.clone(), right.clone(), substitutions);
            }
        }
        (Type::Variable(v1 @ TypeVariable(..)), Type::Variable(v2 @ TypeVariable(..)))
            if v1 == v2 =>
        {
            // both are equal
        }
        (_, Type::Variable(v @ TypeVariable(..))) => {
            if let Some(substitution) = substitutions.get(v) {
                unify(left, substitution.clone(), substitutions);
                return;
            }

            assert!(!v.occurs_in(left.clone(), substitutions));
            substitutions.insert(*v, left);
        }
        (Type::Variable(v @ TypeVariable(..)), _) => {
            if let Some(substitution) = substitutions.get(v) {
                unify(right, substitution.clone(), substitutions);
                return;
            }

            assert!(!v.occurs_in(right.clone(), substitutions));
            substitutions.insert(*v, right);
        }
        (Type::Function(a1, _r1), Type::Function(a2, _r2)) => {
            if a1.len() != a2.len() {
                panic!("invalid number of args");
            }
        }
        (Type::Struct(name, generics, fields), Type::Struct(name2, generics2, fields2)) => {
            assert!(name == name2);
            assert!(generics.len() == generics2.len());
            assert!(fields.len() == fields2.len());
            for (i, (_, field)) in fields.iter().enumerate() {
                unify(field.clone(), fields2[i].1.clone(), substitutions)
            }
        }
        (Type::Tuple(fields), Type::Tuple(fields2)) => {
            assert!(fields.len() == fields2.len());
            for (i, field) in fields.iter().enumerate() {
                unify(field.clone(), fields2[i].clone(), substitutions)
            }
        }
        (_, Type::Struct(..)) | (Type::Struct(..), _) => {
            panic!("invalid");
        }
        (_, Type::Function(_, _)) => {
            panic!("invalid type");
        }
        (Type::Function(_, _), _) => {
            panic!("invalid type");
        }
        _ => todo!(),
    }
}

pub fn dosumn() {
    let mut substitutions = HashMap::new();

    unify(
        tvar!(1),
        tconst!("Map", tvar!(2), tvar!(3)),
        &mut substitutions,
    );
    unify(tvar!(2), tconst!("str"), &mut substitutions);
    unify(tvar!(4), tconst!("Map"), &mut substitutions);
    unify(tvar!(5), tvar!(2), &mut substitutions);
    unify(
        tvar!(6),
        tconst!("Map", tvar!(5), tvar!(3)),
        &mut substitutions,
    );

    println!();

    for i in 1..=6 {
        println!(
            "{}: {:?}",
            i,
            match &(*Type::Variable(TypeVariable(i)).substitute(&substitutions)) {
                Type::Constructor(c) => c.name.clone(),
                Type::Variable(_) => format!("T{}", i),
                Type::Function(_, _) => format!("fn{}", i),
                Type::Struct(..) => "struct".to_string(),
                Type::Trait(t) => format!("Trait {t}"),
                Type::Tuple(..) => "tuple".to_string(),
            }
        );
    }
}

fn sample_ast() -> Expr<'static> {
    Expr::Let(
        std::borrow::Cow::Borrowed("hello"),
        None,
        Box::new(Expr::BinaryOp(
            Box::new(Expr::BinaryOp(
                Box::new(Expr::Literal(Literal::Int(1).into())),
                BinaryOperator::Add,
                Box::new(Expr::Literal(Literal::Int(2).into())),
            )),
            BinaryOperator::Mul,
            Box::new(Expr::Literal(Literal::Int(10).into())),
            // Box::new(
            //     Expr::BinaryOp(
            //         Box::new(Expr::Literal(Literal::Int(1).into())),
            //         BinaryOperator::Add,
            //         Box::new(Expr::Literal(Literal::Int(2).into())),
            //     )
            // ),
            // BinaryOperator::Equal,
            // Box::new(Expr::Literal(Literal::Int(3).into())),
        )),
    )
}

pub fn insert_types() {
    let mut env = TypeEnv(HashMap::new());
    let ast = sample_ast();
    let (mut typed_ast, _) = env.expr_to_type(&ast, &mut HashMap::new());
    crate::codegen::optimise_ast::optimise(&mut typed_ast);
    let mut cfg = crate::codegen::cfg::ControlFlowGraph::new();
    // println!("{:#?}", typed_ast);
    let binding = Expr::If(
        Box::new(Expr::Literal(Literal::Boolean(true).into())),
        Box::new(Expr::BinaryOp(
            Box::new(Expr::BinaryOp(
                Box::new(Expr::Literal(Literal::Int(1).into())),
                BinaryOperator::Add,
                Box::new(Expr::Literal(Literal::Int(2).into())),
            )),
            BinaryOperator::Mul,
            Box::new(Expr::Literal(Literal::Int(10).into())),
        )),
        Some(Box::new(Expr::Literal(Literal::Int(0).into()))),
    );
    cfg.build_from_expr(
        env.expr_to_type(&binding, &mut HashMap::new()).0,
        &mut vec![],
    );
    // println!("{:?}", typed_ast);
    // cfg.emit_graphviz("sample.graphviz");
}
