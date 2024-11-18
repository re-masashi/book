#![allow(dead_code)]

pub mod cfg;
pub mod optimise_ast;
pub mod program;
pub mod utils;
pub mod translate;

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

#[derive(Debug, Clone)]
pub enum TypedNode<'a> {
    Function(
        Cow<'a, str>,
        Vec<(Cow<'a, str>, Arc<Type>)>,
        Box<TypedExpr<'a>>,
        Arc<Type>,
    ),
    // Struct(usize),
    Expr(Box<TypedExpr<'a>>, Arc<Type>),
    Error(String),
    Program(Vec<TypedNode<'a>>),
}

#[derive(Debug, Clone)]
pub enum TypedExpr<'a> {
    Let(Cow<'a, str>, Box<TypedExpr<'a>>, Arc<Type>), // name, value
    Variable(Cow<'a, str>, Arc<Type>),                // name
    Lambda(Vec<(Cow<'a, str>, Arc<Type>)>, Box<TypedExpr<'a>>, Arc<Type>), // args, expression (aka body)
    Literal(Arc<Literal<'a>>, Arc<Type>),             // literal
    If(
        Box<TypedExpr<'a>>,
        Box<TypedExpr<'a>>,
        Option<Box<TypedExpr<'a>>>,
        Arc<Type>,
    ), // condition, if_branch, else_branch
    Call(Box<TypedExpr<'a>>, Vec<Box<TypedExpr<'a>>>, Arc<Type>), // value to call, arguments
    While(Box<TypedExpr<'a>>, Box<TypedExpr<'a>>, Arc<Type>), // condition, body
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
}

pub struct TypeEnv(pub HashMap<String, Type>);

impl<'a> TypeEnv {
    fn expr_to_type(
        &self,
        expr: &'a Expr,
        substitutions: &mut HashMap<TypeVariable, Arc<Type>>,
    ) -> (TypedExpr<'a>, Arc<Type>) {
        match expr {
            Expr::Let(name, ty, value) => match ty {
                Some(ty) => {
                    let annoted_type = Type::Constructor(TypeConstructor {
                        name: ty.name.to_string(),
                        generics: ty
                            .generics
                            .clone()
                            .into_iter()
                            .map(|generic| tconst!(generic))
                            .collect(),
                        traits: vec![],
                    });
                    let (expr_typed, type_) = self.expr_to_type(&value, substitutions);
                    unify(annoted_type.into(), type_.clone().into(), substitutions);
                    (
                        TypedExpr::Let(
                            std::borrow::Cow::Borrowed(name),
                            Box::new(expr_typed),
                            type_.clone(),
                        ),
                        type_,
                    )
                }
                None => {
                    let (expr_typed, type_) = self.expr_to_type(&value, substitutions);
                    (
                        TypedExpr::Let(
                            std::borrow::Cow::Borrowed(name),
                            Box::new(expr_typed),
                            type_.clone(),
                        ),
                        type_,
                    )
                }
            },
            Expr::Variable(name) => (
                TypedExpr::Variable(std::borrow::Cow::Borrowed(name), tvar!(self.0.len() + 1)),
                tvar!(self.0.len() + 1),
            ),
            Expr::Lambda(args, expression) => {
                let (expression_typed, type_) = self.expr_to_type(expression, substitutions);
                let type__ = Type::Function(
                    args.into_iter()
                        .map(|(arg, type_)| {
                            (
                                match type_ {
                                    Some(t) => {
                                        Type::Constructor(TypeConstructor {
                                            name: t.name.to_string(),
                                            generics: t
                                                .generics
                                                .clone()
                                                .into_iter()
                                                .map(|generic| tconst!(generic))
                                                .collect(),
                                            traits: vec![],
                                        }).into()
                                    },
                                    None => {
                                        tvar!(self.0.len()+1)
                                    },
                                }
                            )
                        })
                        .collect(),
                    type_.clone(),
                );
                (
                    TypedExpr::Lambda(
                        args
                        .into_iter()
                        .map(|(arg, type__)|{
                            (
                                arg.clone(),
                                match type__ {
                                    Some(t) => {
                                        Type::Constructor(TypeConstructor {
                                            name: t.name.to_string(),
                                            generics: t
                                                .generics
                                                .clone()
                                                .into_iter()
                                                .map(|generic| tconst!(generic))
                                                .collect(),
                                            traits: vec![],
                                        }).into()
                                    },
                                    None => {
                                        tvar!(self.0.len()+1)
                                    },
                                }
                            )
                        })
                        .collect(),
                        Box::new(expression_typed),
                        type__.clone().into(),
                    ),
                    type__.into(),
                )
            }
            Expr::Literal(lit) => {
                let type_ = match lit.as_ref() {
                    Literal::Boolean(_) => tconst!("bool"),
                    Literal::Int(_) => tconst!("int"),
                    Literal::Float(_) => tconst!("float"),
                    Literal::String(_) => tconst!("str"),
                };
                (TypedExpr::Literal(lit.clone(), type_.clone()), type_)
            }
            Expr::If(cond, if_branch, else_branch) => {
                let (cond_, cond_type) = self.expr_to_type(cond, substitutions);
                unify(tconst!("bool"), cond_type.clone(), substitutions);
                let (if_, if_type) = self.expr_to_type(if_branch, substitutions);
                if let Some(pat) = else_branch {
                    let (else_, else_type) = self.expr_to_type(pat, substitutions);
                    unify(if_type.clone(), else_type, substitutions);
                    (
                        TypedExpr::If(
                            Box::new(cond_),
                            Box::new(if_),
                            Some(Box::new(else_)),
                            if_type.clone(),
                        ),
                        if_type,
                    )
                }else{
                    (
                        TypedExpr::If(
                            Box::new(cond_),
                            Box::new(if_),
                            None,
                            if_type.clone(),
                        ),
                        if_type,
                    )
                }
            }
            Expr::Call(value, args) => {
                let (value_, value_type) = self.expr_to_type(value, substitutions);
                match value_type.clone().as_ref() {
                    Type::Variable(_) => {
                        let new_args = args
                            .into_iter()
                            .map(|arg| Box::new(self.expr_to_type(arg, substitutions).0))
                            .collect::<Vec<_>>();

                        (
                            TypedExpr::Call(Box::new(value_), new_args, tvar!(self.0.len() + 1)),
                            tvar!(self.0.len()),
                        )
                    }
                    Type::Function(_fn_args, _ret_type) => {
                        let type_ = Type::Function(
                            args.into_iter()
                                .map(|_arg| tvar!(self.0.len() + 1))
                                .collect(),
                            tvar!(self.0.len() + 1),
                        );
                        unify(value_type, type_.into(), substitutions);
                        let new_args = args
                            .into_iter()
                            .map(|arg| Box::new(self.expr_to_type(arg, substitutions).0))
                            .collect::<Vec<_>>();

                        (
                            TypedExpr::Call(Box::new(value_), new_args, tvar!(self.0.len() + 1)),
                            tvar!(self.0.len()),
                        )
                    }
                    Type::Constructor(..) => {
                        panic!("invalid function");
                    }
                }
            }
            Expr::While(cond, body) => {
                let (cond, cond_type) = self.expr_to_type(cond, substitutions);
                unify(tconst!("bool"), cond_type.clone(), substitutions);
                let (body, _body_type) = self.expr_to_type(body, substitutions);
                (
                    TypedExpr::While(Box::new(cond), Box::new(body), tconst!("bool")),
                    tconst!("bool"),
                )
            }
            Expr::BinaryOp(lhs, op, rhs) => {
                let (lhs, mut lhs_type) = self.expr_to_type(lhs, substitutions);
                let (rhs, mut rhs_type) = self.expr_to_type(rhs, substitutions);
                unify(lhs_type.clone(), rhs_type.clone(), substitutions);

                match (lhs_type.as_ref(), rhs_type.as_ref()) {
                    (_, Type::Variable(v @ TypeVariable(..))) => {
                        rhs_type = lhs_type.clone();
                    }
                    (Type::Variable(v @ TypeVariable(..)), _) => {
                        lhs_type = rhs_type.clone();
                    }
                    _=>{}
                }

                match op {
                    BinaryOperator::Add
                    | BinaryOperator::Sub
                    | BinaryOperator::Div
                    | BinaryOperator::Mul => {
                        (
                            TypedExpr::BinaryOp(Box::new(lhs), op, Box::new(rhs), lhs_type.clone()),
                            lhs_type,
                        ) // both sides must have the same time for now
                    }
                    BinaryOperator::Equal
                    | BinaryOperator::NotEqual
                    | BinaryOperator::Less
                    | BinaryOperator::Greater
                    | BinaryOperator::LessEqual
                    | BinaryOperator::GreaterEqual
                    | BinaryOperator::And
                    | BinaryOperator::Or => {
                        (
                            TypedExpr::BinaryOp(Box::new(lhs), op, Box::new(rhs), tconst!("bool")),
                            tconst!("bool"),
                        ) // both sides must have the same time
                    }
                }
            }
            Expr::UnaryOp(op, val) => {
                let (val, val_type) = self.expr_to_type(val, substitutions);
                match op {
                    UnaryOperator::Negate => (
                        TypedExpr::UnaryOp(op, Box::new(val), val_type.clone()),
                        val_type,
                    ),
                    UnaryOperator::Not => {
                        unify(val_type.clone(), tconst!("bool"), substitutions);
                        (
                            TypedExpr::UnaryOp(op, Box::new(val), val_type.clone()),
                            val_type,
                        )
                    }
                }
            }
            Expr::Array(vals) => {
                if vals.len() == 0 {
                    let type_ = tconst!("Array", tvar!(self.0.len() + 1));
                    return (TypedExpr::Array(vec![], type_.clone()), type_);
                }
                if vals.len() == 1 {
                    let (val, val_type) = self.expr_to_type(&vals[0], substitutions);
                    let type_ = tconst!("Array", val_type);
                    return (TypedExpr::Array(vec![val], type_.clone()), type_);
                } else {
                    let (_val, mut val_type) = self.expr_to_type(&vals[0], substitutions);
                    let mut typed_vals = vec![];
                    for val in &vals[1..] {
                        let (typed_val, val_type_) = self.expr_to_type(&val, substitutions);
                        unify(val_type, val_type_.clone(), substitutions);
                        val_type = val_type_;
                        typed_vals.push(typed_val);
                    }
                    return (TypedExpr::Array(typed_vals, val_type.clone()), val_type);
                }
            }
            Expr::Do(expressions) => {
                if expressions.len() == 0 {
                    let type_ = tvar!(self.0.len() + 1);
                    return (TypedExpr::Do(vec![], type_.clone()), type_);
                }
                if expressions.len() == 1 {
                    let (val, val_type) = self.expr_to_type(&expressions[0], substitutions);
                    let type_ = val_type;
                    return (TypedExpr::Do(vec![val], type_.clone()), type_);
                } else {
                    let (_val, mut val_type) = self.expr_to_type(&expressions[0], substitutions);
                    let mut typed_vals = vec![];
                    for val in &expressions[1..] {
                        let (typed_val, val_type_) = self.expr_to_type(&val, substitutions);
                        val_type = val_type_;
                        typed_vals.push(typed_val);
                    }
                    return (TypedExpr::Do(typed_vals, val_type.clone()), val_type);
                }
            }
            Expr::Index(expression, index) => {
                let (expr, _expr_type) = self.expr_to_type(expression, substitutions);
                let (index, _index_type) = self.expr_to_type(index, substitutions);
                return (TypedExpr::Index(Box::new(expr), Box::new(index), tvar!(self.0.len()+1)), tvar!(self.0.len()))
            }
        }
    }

    pub fn node_to_type(
        &self,
        node: &'a Node,
        substitutions: &mut HashMap<TypeVariable, Arc<Type>>,
    ) -> TypedNode<'a> {
        match node {
            Node::Function(name, args, ret, ty) => match ty {
                Some(ty) => {
                    let (ret, ret_type) = self.expr_to_type(ret, substitutions);
                    let annoted_type = Type::Constructor(TypeConstructor {
                            name: ty.name.to_string(),
                            generics: ty
                                .generics
                                .clone()
                                .into_iter()
                                .map(|generic| tconst!(generic))
                                .collect(),
                            traits: vec![],
                        });
                    let type_ = Type::Function(
                        args.into_iter()
                            .map(|(arg, argtype)| {
                                match argtype {
                                    Some(ty) => {
                                        Type::Constructor(TypeConstructor {
                                            name: ty.name.to_string(),
                                            generics: ty
                                                .generics
                                                .clone()
                                                .into_iter()
                                                .map(|generic| tconst!(generic))
                                                .collect(),
                                            traits: vec![],
                                        }).into()
                                    },
                                    None => tvar!(self.0.len() + 1),
                                }
                            })
                            .collect(),
                        annoted_type
                        .clone()
                        .into(),
                    );
                    unify(annoted_type.into(), ret_type, substitutions);
                    TypedNode::Function(
                        std::borrow::Cow::Borrowed(name), 
                        args
                        .into_iter()
                        .map(|(arg, type__)|{
                            (
                                arg.clone(),
                                match type__ {
                                    Some(t) => {
                                        Type::Constructor(TypeConstructor {
                                            name: t.name.to_string(),
                                            generics: t
                                                .generics
                                                .clone()
                                                .into_iter()
                                                .map(|generic| tconst!(generic))
                                                .collect(),
                                            traits: vec![],
                                        }).into()
                                    },
                                    None => {
                                        tvar!(self.0.len()+1)
                                    },
                                }
                            )
                        })
                        .collect(), 
                        Box::new(ret), 
                        type_.into()
                    )
                }
                None => {
                    let ret_type = tvar!(self.0.len() + 1);
                    let (expr, expr_type) = self.expr_to_type(ret, substitutions);
                    unify(ret_type, expr_type.clone(), substitutions);

                    let type_ = Type::Function(
                        args.into_iter()
                            .map(|_arg| tvar!(self.0.len() + 1))
                            .collect(),
                        tvar!(self.0.len() + 1),
                    );
                    TypedNode::Function(
                        std::borrow::Cow::Borrowed(name), 
                        args
                        .into_iter()
                        .map(|(arg, type__)|{
                            (
                                arg.clone(),
                                match type__ {
                                    Some(t) => {
                                        Type::Constructor(TypeConstructor {
                                            name: t.name.to_string(),
                                            generics: t
                                                .generics
                                                .clone()
                                                .into_iter()
                                                .map(|generic| tconst!(generic))
                                                .collect(),
                                            traits: vec![],
                                        }).into()
                                    },
                                    None => {
                                        tvar!(self.0.len()+1)
                                    },
                                }
                            )
                        })
                        .collect(), 
                        Box::new(expr), 
                        expr_type.into()
                    )
                }
            },
            Node::Expr(e) => {
                let (e,t) = self.expr_to_type(e, substitutions);
                TypedNode::Expr(Box::new(e), t)
            }
            Node::Error(e) => panic!("{:?}", e),
            Node::Program(nodes)=>{
                TypedNode::Program(nodes.into_iter().map(|node|{
                    self.node_to_type(node, substitutions)
                })
                .collect())
            }
        }
    }
}

#[derive(Debug, Clone)]
pub enum Node<'a> {
    Function(
        Cow<'a, str>,
        Vec<(Cow<'a, str>, Option<TypeAnnot>)>,
        Box<Expr<'a>>,
        Option<TypeAnnot>,
    ),
    // Struct(usize),
    Expr(Box<Expr<'a>>),
    Error(String),
    Program(Vec<Node<'a>>),
}

#[derive(Debug, Clone)]
pub enum Expr<'a> {
    Let(Cow<'a, str>, Option<TypeAnnot>, Box<Expr<'a>>), // name, value
    Variable(Cow<'a, str>),                              // name
    Lambda(Vec<(Cow<'a, str>, Option<TypeAnnot>)>, Box<Expr<'a>>),            // args, expression (aka body)
    Literal(Arc<Literal<'a>>),                           // literal
    If(Box<Expr<'a>>, Box<Expr<'a>>, Option<Box<Expr<'a>>>),     // condition, if_branch, else_branch
    Call(Box<Expr<'a>>, Vec<Box<Expr<'a>>>),             // value to call, arguments
    While(Box<Expr<'a>>, Box<Expr<'a>>),                 // condition, body
    BinaryOp(Box<Expr<'a>>, BinaryOperator, Box<Expr<'a>>), // lhs, op, rhs
    UnaryOp(UnaryOperator, Box<Expr<'a>>),               // op, val
    Array(Vec<Expr<'a>>),
    Do(Vec<Expr<'a>>),
    Index(Box<Expr<'a>>, Box<Expr<'a>>),       // value, index
}

#[derive(Debug)]
pub enum Literal<'a> {
    Boolean(bool),
    Int(i64),
    Float(f64),
    String(Cow<'a, str>),
}

#[derive(Debug, Clone)]
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

#[derive(Debug, Clone)]
pub enum UnaryOperator {
    Negate,
    Not,
}

#[derive(Debug, Clone)]
pub struct TypeAnnot {
    pub name: String,
    pub generics: Vec<String>,
    pub traits: Vec<String>,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Type {
    Constructor(TypeConstructor),
    Variable(TypeVariable),
    Function(Vec<Arc<Type>>, Arc<Type>),
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
                if let Some(substitution) = substitutions.get(&v) {
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
            Type::Function(_, _) => todo!(),
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
        (_, Type::Variable(v @ TypeVariable(..))) => {
            if let Some(substitution) = substitutions.get(&v) {
                unify(left, substitution.clone(), substitutions);
                return;
            }

            assert!(!v.occurs_in(left.clone(), substitutions));
            substitutions.insert(*v, left);
        }
        (Type::Variable(v @ TypeVariable(..)), _) => {
            if let Some(substitution) = substitutions.get(&v) {
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
        (_, Type::Function(_, _)) => {
            panic!("invalid type");
        }
        (Type::Function(_, _), _) => {
            panic!("invalid type");
        }
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

    println!("");

    for i in 1..=6 {
        println!(
            "{}: {:?}",
            i,
            match &(*Type::Variable(TypeVariable(i)).substitute(&substitutions)) {
                Type::Constructor(c) => c.name.clone(),
                Type::Variable(_) => format!("T{}", i),
                Type::Function(_, _) => format!("fn{}", i),
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
    let env = TypeEnv(HashMap::new());
    let ast = sample_ast();
    let (mut typed_ast, _) = env.expr_to_type(&ast, &mut HashMap::new());
    crate::interpreter::optimise_ast::optimise(&mut typed_ast);
    let mut cfg = crate::interpreter::cfg::ControlFlowGraph::new();
    println!("{:#?}", typed_ast);
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
        Some(Box::new(Expr::Literal(Literal::Int(0).into())))
    );
    cfg.build_from_expr(env.expr_to_type(&binding, &mut HashMap::new()).0, &mut vec![]);
    // println!("{:?}", typed_ast);
    cfg.emit_graphviz("sample.graphviz");
}
