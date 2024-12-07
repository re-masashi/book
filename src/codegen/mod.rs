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

use owo_colors::OwoColorize;

use crate::lexer::tokens::Span;

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
    Program(Vec<(TypedNode<'a>, Span, String)>),
    Extern(Cow<'a, str>, Vec<Arc<Type>>, Arc<Type>),
}

#[derive(Debug, Clone)]
pub enum TypedExpr<'a> {
    Let(Cow<'a, str>, Box<TypedExpr<'a>>, Arc<Type>, Span, String), // name, value, span
    Variable(Cow<'a, str>, Arc<Type>, Span, String),                // name, span
    Lambda(
        Vec<(Cow<'a, str>, Arc<Type>)>,
        Box<TypedExpr<'a>>,
        Arc<Type>,
        Span,
        String,
    ), // args, expression (aka body), span
    Literal(Arc<Literal<'a>>, Arc<Type>, Span, String),             // literal, span
    If(
        Box<TypedExpr<'a>>,
        Box<TypedExpr<'a>>,
        Option<Box<TypedExpr<'a>>>,
        Arc<Type>,
        Span,
        String,
    ), // condition, if_branch, else_branch, span
    Call(
        Box<TypedExpr<'a>>,
        Vec<TypedExpr<'a>>,
        Arc<Type>,
        Span,
        String,
    ), // value to call, arguments, span
    While(
        Box<TypedExpr<'a>>,
        Box<TypedExpr<'a>>,
        Arc<Type>,
        Span,
        String,
    ), // condition, body, span
    Break(Span, String),
    Continue(Span, String),
    BinaryOp(
        Box<TypedExpr<'a>>,
        &'a BinaryOperator,
        Box<TypedExpr<'a>>,
        Arc<Type>,
        Span,
        String,
    ), // lhs, op, rhs, span
    UnaryOp(
        &'a UnaryOperator,
        Box<TypedExpr<'a>>,
        Arc<Type>,
        Span,
        String,
    ), // op, val, span
    Array(Vec<TypedExpr<'a>>, Arc<Type>, Span, String),
    Do(Vec<TypedExpr<'a>>, Arc<Type>, Span, String),
    Index(
        Box<TypedExpr<'a>>,
        Box<TypedExpr<'a>>,
        Arc<Type>,
        Span,
        String,
    ),
    StructAccess(Box<TypedExpr<'a>>, Cow<'a, str>, Arc<Type>, Span, String),
    Return(Box<TypedExpr<'a>>, Arc<Type>, Span, String),
    Tuple(Vec<TypedExpr<'a>>, Arc<Type>, Span, String),
    Assign(
        Box<TypedExpr<'a>>,
        Box<TypedExpr<'a>>,
        Arc<Type>,
        Span,
        String,
    ), // variable name, expression, type
}

pub struct TypeEnv(pub HashMap<String, Arc<Type>>);

impl TypeEnv {
    pub fn error(&self, err: &TypeError) {
        let (message, span, file) = match err {
            TypeError::TypeMismatch {
                expected,
                found,
                span,
                file,
            } => (
                format!(
                    "Type Mismatch. Expected {}, found {} instead.",
                    expected.blue(),
                    found.blue()
                ),
                span,
                file,
            ),
            TypeError::OccursCheckFailed { span, file } => {
                ("Type occurs in itself!".to_string(), span, file)
            }
            TypeError::UnhandledType {
                left,
                right,
                span,
                file,
            } => (format!("Unhandled type: {left} vs {right}"), span, file),
        };

        println!(
            "{} at {:?}: 
            {} 
            file `{}`",
            "[TypeError]".red(),
            span,
            message,
            file.green()
        );
    }
}

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
    Program(Vec<(Node<'a>, Span, String)>),
    Extern(Cow<'a, str>, Vec<TypeAnnot>, TypeAnnot),
}

#[derive(Debug, Clone)]
pub enum Expr<'a> {
    Let(Cow<'a, str>, Option<TypeAnnot>, Box<Expr<'a>>, Span, String), // name, value, span
    Variable(Cow<'a, str>, Span, String),                              // name, span
    Lambda(
        Vec<(Cow<'a, str>, Option<TypeAnnot>)>,
        Box<Expr<'a>>,
        Span,
        String,
    ), // args, expression (aka body), span
    Literal(Arc<Literal<'a>>, Span, String),                           // literal, span
    If(
        Box<Expr<'a>>,
        Box<Expr<'a>>,
        Option<Box<Expr<'a>>>,
        Span,
        String,
    ), // condition, if_branch, else_branch, span
    Call(Box<Expr<'a>>, Vec<Expr<'a>>, Span, String), // value to call, arguments, span
    While(Box<Expr<'a>>, Box<Expr<'a>>, Span, String), // condition, body, span
    Break(Span, String),
    Continue(Span, String),
    BinaryOp(Box<Expr<'a>>, BinaryOperator, Box<Expr<'a>>, Span, String), // lhs, op, rhs, span
    UnaryOp(UnaryOperator, Box<Expr<'a>>, Span, String),                  // op, val, span
    Array(Vec<Expr<'a>>, Span, String),
    Do(Vec<Expr<'a>>, Span, String),
    Index(Box<Expr<'a>>, Box<Expr<'a>>, Span, String), // value, index, span
    StructAccess(Box<Expr<'a>>, Cow<'a, str>, Span, String), // struct, field, span
    Return(Box<Expr<'a>>, Span, String),
    Tuple(Vec<Expr<'a>>, Span, String),
    Assign(Box<Expr<'a>>, Box<Expr<'a>>, Span, String), // variable name, expression, span
}

impl PartialEq for Expr<'_> {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Expr::Let(name1, ty1, val1, _, _), Expr::Let(name2, ty2, val2, _, _)) => {
                name1 == name2 && ty1 == ty2 && *val1 == *val2
            }
            (Expr::Variable(name1, _, _), Expr::Variable(name2, _, _)) => name1 == name2,
            (Expr::Lambda(args1, body1, _, _), Expr::Lambda(args2, body2, _, _)) => {
                args1 == args2 && *body1 == *body2
            }
            (Expr::Literal(lit1, _, _), Expr::Literal(lit2, _, _)) => lit1 == lit2,
            (Expr::If(cond1, if1, else1, _, _), Expr::If(cond2, if2, else2, _, _)) => {
                cond1 == cond2 && *if1 == *if2 && else1 == else2
            }
            (Expr::Call(val1, args1, _, _), Expr::Call(val2, args2, _, _)) => {
                val1 == val2 && args1 == args2
            }
            (Expr::While(cond1, body1, _, _), Expr::While(cond2, body2, _, _)) => {
                cond1 == cond2 && *body1 == *body2
            }
            (Expr::Break(..), Expr::Break(..)) => true,
            (Expr::Continue(..), Expr::Continue(..)) => true,
            (Expr::BinaryOp(lhs1, op1, rhs1, _, _), Expr::BinaryOp(lhs2, op2, rhs2, _, _)) => {
                op1 == op2 && *lhs1 == *lhs2 && *rhs1 == *rhs2
            }
            (Expr::UnaryOp(op1, val1, _, _), Expr::UnaryOp(op2, val2, _, _)) => {
                op1 == op2 && *val1 == *val2
            }
            (Expr::Array(vals1, _, _), Expr::Array(vals2, _, _)) => vals1 == vals2,
            (Expr::Do(vals1, _, _), Expr::Do(vals2, _, _)) => vals1 == vals2,
            (Expr::Index(val1, idx1, _, _), Expr::Index(val2, idx2, _, _)) => {
                val1 == val2 && *idx1 == *idx2
            }
            (Expr::StructAccess(val1, field1, _, _), Expr::StructAccess(val2, field2, _, _)) => {
                val1 == val2 && field1 == field2
            }
            (Expr::Return(val1, _, _), Expr::Return(val2, _, _)) => *val1 == *val2,
            (Expr::Tuple(vals1, _, _), Expr::Tuple(vals2, _, _)) => vals1 == vals2,
            (Expr::Assign(lhs1, val1, _, _), Expr::Assign(lhs2, val2, _, _)) => {
                *lhs1 == *lhs2 && *val1 == *val2
            }
            _ => false,
        }
    }
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

#[derive(Debug)]
pub enum TypeError {
    TypeMismatch {
        expected: String,
        found: String,
        span: Span,
        file: String,
    },
    OccursCheckFailed {
        span: Span,
        file: String,
    },
    UnhandledType {
        left: String,
        right: String,
        span: Span,
        file: String,
    },
}

fn unify(
    left: Arc<Type>,
    right: Arc<Type>,
    substitutions: &mut HashMap<TypeVariable, Arc<Type>>,
    span: &Span,
    file: &String,
) -> Result<(), TypeError> {
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
            if name1 != name2 {
                return Err(TypeError::TypeMismatch {
                    expected: format!("'{}'", name1),
                    found: format!("'{}'", name2),
                    span: *span,
                    file: file.clone(),
                });
            }
            if generics1.len() != generics2.len() {
                return Err(TypeError::TypeMismatch {
                    expected: format!("{} generics", generics1.len()),
                    found: format!("{} generics", generics2.len()),
                    span: *span,
                    file: file.clone(),
                });
            }

            for (left, right) in zip(generics1, generics2) {
                unify(left.clone(), right.clone(), substitutions, span, file)?;
            }
            Ok(())
        }
        (Type::Variable(v1 @ TypeVariable(..)), Type::Variable(v2 @ TypeVariable(..)))
            if v1 == v2 =>
        {
            Ok(())
        }
        (_, Type::Variable(v @ TypeVariable(..))) => {
            if let Some(substitution) = substitutions.get(v) {
                unify(left, substitution.clone(), substitutions, span, file)?;
                return Ok(());
            }

            if v.occurs_in(left.clone(), substitutions) {
                return Err(TypeError::OccursCheckFailed {
                    span: *span,
                    file: file.clone(),
                });
            }
            substitutions.insert(*v, left);
            Ok(())
        }
        (Type::Variable(v @ TypeVariable(..)), _) => {
            if let Some(substitution) = substitutions.get(v) {
                unify(right, substitution.clone(), substitutions, span, file)?;
                return Ok(());
            }

            if v.occurs_in(right.clone(), substitutions) {
                return Err(TypeError::OccursCheckFailed {
                    span: *span,
                    file: file.clone(),
                });
            }
            substitutions.insert(*v, right);
            Ok(())
        }
        (Type::Function(a1, _r1), Type::Function(a2, _r2)) => {
            if a1.len() != a2.len() {
                return Err(TypeError::TypeMismatch {
                    expected: format!("{} arguments", a1.len()),
                    found: format!("{} arguments", a2.len()),
                    span: *span,
                    file: file.clone(),
                });
            }
            Ok(())
        }
        (Type::Struct(name, generics, fields), Type::Struct(name2, generics2, fields2)) => {
            if name != name2 {
                return Err(TypeError::TypeMismatch {
                    expected: format!("struct '{}'", name),
                    found: format!("struct '{}'", name2),
                    span: *span,
                    file: file.clone(),
                });
            }
            if generics.len() != generics2.len() {
                return Err(TypeError::TypeMismatch {
                    expected: format!("{} generics", generics.len()),
                    found: format!("{} generics", generics2.len()),
                    span: *span,
                    file: file.clone(),
                });
            }
            if fields.len() != fields2.len() {
                return Err(TypeError::TypeMismatch {
                    expected: format!("{} fields", fields.len()),
                    found: format!("{} fields", fields2.len()),
                    span: *span,
                    file: file.clone(),
                });
            }
            for (i, (_, field)) in fields.iter().enumerate() {
                unify(
                    field.clone(),
                    fields2[i].1.clone(),
                    substitutions,
                    span,
                    file,
                )?;
            }
            Ok(())
        }
        (Type::Tuple(fields), Type::Tuple(fields2)) => {
            if fields.len() != fields2.len() {
                return Err(TypeError::TypeMismatch {
                    expected: format!("{} tuple elements", fields.len()),
                    found: format!("{} tuple elements", fields2.len()),
                    span: *span,
                    file: file.clone(),
                });
            }
            for (i, field) in fields.iter().enumerate() {
                unify(field.clone(), fields2[i].clone(), substitutions, span, file)?;
            }
            Ok(())
        }
        (_, Type::Struct(..)) | (Type::Struct(..), _) => Err(TypeError::TypeMismatch {
            expected: "struct".to_string(),
            found: "non-struct type".to_string(),
            span: *span,
            file: file.clone(),
        }),
        (_, Type::Function(_, _)) => Err(TypeError::TypeMismatch {
            expected: "function type".to_string(),
            found: "non-function type".to_string(),
            span: *span,
            file: file.clone(),
        }),
        (Type::Function(_, _), _) => Err(TypeError::TypeMismatch {
            expected: "function type".to_string(),
            found: "non-function type".to_string(),
            span: *span,
            file: file.clone(),
        }),
        _ => Err(TypeError::UnhandledType {
            left: format!("{:?}", left),
            right: format!("{:?}", right),
            span: *span,
            file: file.clone(),
        }),
    }
}
