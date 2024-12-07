use crate::codegen::*;
use crate::{t_int, t_str, tconst, tvar};

use log::trace;

impl<'a> TypeEnv {
    pub fn expr_to_type(
        &mut self,
        expr: &'a Expr,
        _span: &Span,
        _file: &str,
        substitutions: &mut HashMap<TypeVariable, Arc<Type>>,
    ) -> Result<(TypedExpr<'a>, Arc<Type>), TypeError> {
        let (typed_expr, ty) = match expr {
            Expr::Let(name, ty, value, span, file) => match ty {
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
                    let (expr_typed, type_) =
                        self.expr_to_type(value, span, file, substitutions)?;
                    unify(
                        annoted_type.into(),
                        type_.clone(),
                        substitutions,
                        span,
                        file,
                    )?;
                    self.0.insert(name.to_string(), type_.clone());
                    (
                        TypedExpr::Let(
                            std::borrow::Cow::Borrowed(name),
                            Box::new(expr_typed),
                            type_.clone(),
                            *span,
                            file.clone(),
                        ),
                        type_,
                    )
                }
                None => {
                    let (expr_typed, type_) =
                        self.expr_to_type(value, span, file, substitutions)?;
                    // println!("{:?}", type_);
                    self.0.insert(name.to_string(), type_.clone());
                    (
                        TypedExpr::Let(
                            std::borrow::Cow::Borrowed(name),
                            Box::new(expr_typed),
                            type_.clone(),
                            *span,
                            file.clone(),
                        ),
                        type_,
                    )
                }
            },
            Expr::Variable(name, span, file) => match self.0.get(&name.to_string()) {
                Some(t) => (
                    TypedExpr::Variable(
                        std::borrow::Cow::Borrowed(name),
                        t.clone(),
                        *span,
                        file.clone(),
                    ),
                    t.clone(),
                ),
                None => {
                    // println!("unknown type {name}");
                    self.0
                        .insert(name.to_string(), tvar!(self.0.len() + 1).clone());
                    (
                        TypedExpr::Variable(
                            std::borrow::Cow::Borrowed(name),
                            tvar!(self.0.len()),
                            *span,
                            file.clone(),
                        ),
                        tvar!(self.0.len()),
                    )
                }
            },
            Expr::Lambda(args, expression, span, file) => {
                let (expression_typed, type_) =
                    self.expr_to_type(expression, span, file, substitutions)?;
                let type__ = Type::Function(
                    args.iter()
                        .map(|(_arg, type_)| match type_ {
                            Some(t) => Type::Constructor(TypeConstructor {
                                name: t.name.to_string(),
                                generics: t
                                    .generics
                                    .clone()
                                    .into_iter()
                                    .map(|generic| tconst!(generic))
                                    .collect(),
                                traits: vec![],
                            })
                            .into(),
                            None => {
                                tvar!(self.0.len() + 1)
                            }
                        })
                        .collect(),
                    type_.clone(),
                );
                (
                    TypedExpr::Lambda(
                        args.iter()
                            .map(|(arg, type__)| {
                                (
                                    arg.clone(),
                                    match type__ {
                                        Some(t) => Type::Constructor(TypeConstructor {
                                            name: t.name.to_string(),
                                            generics: t
                                                .generics
                                                .clone()
                                                .into_iter()
                                                .map(|generic| tconst!(generic))
                                                .collect(),
                                            traits: vec![],
                                        })
                                        .into(),
                                        None => {
                                            tvar!(self.0.len() + 1)
                                        }
                                    },
                                )
                            })
                            .collect(),
                        Box::new(expression_typed),
                        type__.clone().into(),
                        *span,
                        file.clone(),
                    ),
                    type__.into(),
                )
            }
            Expr::Literal(lit, span, file) => {
                let type_ = match lit.as_ref() {
                    Literal::Boolean(_) => tconst!("bool"),
                    Literal::Int(_) => tconst!("int"),
                    Literal::Float(_) => tconst!("float"),
                    Literal::String(_) => tconst!("str"),
                };
                (
                    TypedExpr::Literal(lit.clone(), type_.clone(), *span, file.clone()),
                    type_,
                )
            }
            Expr::If(cond, if_branch, else_branch, span, file) => {
                let (cond_, cond_type) = self.expr_to_type(cond, span, file, substitutions)?;
                unify(
                    tconst!("bool"),
                    cond_type.clone(),
                    substitutions,
                    span,
                    file,
                )?;
                let (if_, if_type) = self.expr_to_type(if_branch, span, file, substitutions)?;
                if let Some(pat) = else_branch {
                    let (else_, else_type) = self.expr_to_type(pat, span, file, substitutions)?;
                    unify(if_type.clone(), else_type, substitutions, span, file)?;
                    (
                        TypedExpr::If(
                            Box::new(cond_),
                            Box::new(if_),
                            Some(Box::new(else_)),
                            if_type.clone(),
                            *span,
                            file.clone(),
                        ),
                        if_type,
                    )
                } else {
                    (
                        TypedExpr::If(
                            Box::new(cond_),
                            Box::new(if_),
                            None,
                            if_type.clone(),
                            *span,
                            file.clone(),
                        ),
                        if_type,
                    )
                }
            }
            Expr::Call(value, args, span, file) => {
                let (value_, value_type) = self.expr_to_type(value, span, file, substitutions)?;

                match value_type.clone().as_ref() {
                    Type::Variable(_) => {
                        let new_args = args
                            .iter()
                            .map(|arg| self.expr_to_type(arg, span, file, substitutions).unwrap().0)
                            .collect::<Vec<_>>();
                        (
                            TypedExpr::Call(
                                Box::new(value_),
                                new_args,
                                value_type.clone(),
                                *span,
                                file.clone(),
                            ),
                            value_type.clone(),
                        )
                    }
                    Type::Function(_fn_args, ret_type) => {
                        let new_args = args
                            .iter()
                            .map(|arg| self.expr_to_type(arg, span, file, substitutions).unwrap().0)
                            .collect::<Vec<_>>();
                        (
                            TypedExpr::Call(
                                Box::new(value_),
                                new_args,
                                ret_type.clone(),
                                *span,
                                file.clone(),
                            ),
                            ret_type.clone(),
                        )
                    }
                    Type::Constructor(..) => {
                        panic!("invalid function {:?}", value_);
                    }
                    Type::Struct(..) => {
                        todo!()
                    }
                    Type::Trait(..) => todo!(),
                    Type::Tuple(..) => todo!(),
                }
            }
            Expr::While(cond, body, span, file) => {
                let (cond, cond_type) = self.expr_to_type(cond, span, file, substitutions)?;
                unify(
                    tconst!("bool"),
                    cond_type.clone(),
                    substitutions,
                    span,
                    file,
                )?;
                let (body, _body_type) = self.expr_to_type(body, span, file, substitutions)?;
                (
                    TypedExpr::While(
                        Box::new(cond),
                        Box::new(body),
                        tconst!("bool"),
                        *span,
                        file.clone(),
                    ),
                    tconst!("bool"),
                )
            }
            Expr::BinaryOp(lhs, op, rhs, span, file) => {
                let (lhs, mut lhs_type) = self.expr_to_type(lhs, span, file, substitutions)?;
                let (rhs, rhs_type) = self.expr_to_type(rhs, span, file, substitutions)?;

                unify(
                    lhs_type.clone(),
                    rhs_type.clone(),
                    substitutions,
                    span,
                    file,
                )?;

                match (lhs_type.as_ref(), rhs_type.as_ref()) {
                    (_, Type::Variable(TypeVariable(..))) => {
                        // rhs_type = lhs_type.clone();
                    }
                    (Type::Variable(TypeVariable(..)), _) => {
                        lhs_type = rhs_type.clone();
                    }
                    _ => {}
                }

                match op {
                    BinaryOperator::Add
                    | BinaryOperator::Sub
                    | BinaryOperator::Div
                    | BinaryOperator::Mul => {
                        (
                            TypedExpr::BinaryOp(
                                Box::new(lhs),
                                op,
                                Box::new(rhs),
                                lhs_type.clone(),
                                *span,
                                file.clone(),
                            ),
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
                            TypedExpr::BinaryOp(
                                Box::new(lhs),
                                op,
                                Box::new(rhs),
                                tconst!("bool"),
                                *span,
                                file.clone(),
                            ),
                            tconst!("bool"),
                        ) // both sides must have the same time
                    }
                }
            }
            Expr::UnaryOp(op, val, span, file) => {
                let (val, val_type) = self.expr_to_type(val, span, file, substitutions)?;
                match op {
                    UnaryOperator::Negate => (
                        TypedExpr::UnaryOp(
                            op,
                            Box::new(val),
                            val_type.clone(),
                            *span,
                            file.clone(),
                        ),
                        val_type,
                    ),
                    UnaryOperator::Not => {
                        unify(val_type.clone(), tconst!("bool"), substitutions, span, file)?;
                        (
                            TypedExpr::UnaryOp(
                                op,
                                Box::new(val),
                                val_type.clone(),
                                *span,
                                file.clone(),
                            ),
                            val_type,
                        )
                    }
                }
            }
            Expr::Array(vals, span, file) => {
                if vals.is_empty() {
                    let type_ = tconst!("Array", tvar!(self.0.len() + 1));
                    return Ok((
                        TypedExpr::Array(vec![], type_.clone(), *span, file.clone()),
                        type_,
                    ));
                }
                if vals.len() == 1 {
                    let (val, val_type) = self.expr_to_type(&vals[0], span, file, substitutions)?;
                    let type_ = tconst!("Array", val_type);
                    return Ok((
                        TypedExpr::Array(vec![val], type_.clone(), *span, file.clone()),
                        type_,
                    ));
                } else {
                    let (val, mut val_type) =
                        self.expr_to_type(&vals[0], span, file, substitutions)?;
                    let type_ = tconst!("Array", val_type.clone());
                    let mut typed_vals = vec![val];
                    for val in &vals[1..] {
                        let (typed_val, val_type_) =
                            self.expr_to_type(val, span, file, substitutions)?;
                        unify(val_type, val_type_.clone(), substitutions, span, file)?;
                        val_type = val_type_;
                        typed_vals.push(typed_val);
                    }
                    return Ok((
                        TypedExpr::Array(typed_vals, type_.clone(), *span, file.clone()),
                        type_,
                    ));
                }
            }
            Expr::Do(expressions, span, file) => {
                if expressions.is_empty() {
                    let type_ = tvar!(self.0.len() + 1);
                    return Ok((
                        TypedExpr::Do(vec![], type_.clone(), *span, file.clone()),
                        type_,
                    ));
                }
                if expressions.len() == 1 {
                    let (val, val_type) =
                        self.expr_to_type(&expressions[0], span, file, substitutions)?;
                    let type_ = val_type;
                    return Ok((
                        TypedExpr::Do(vec![val], type_.clone(), *span, file.clone()),
                        type_,
                    ));
                } else {
                    let mut typed_vals = vec![];
                    let (val, mut val_type) =
                        self.expr_to_type(&expressions[0], span, file, substitutions)?;
                    typed_vals.push(val);
                    for val in &expressions[1..] {
                        let (typed_val, val_type_) =
                            self.expr_to_type(val, span, file, substitutions)?;
                        val_type = val_type_;
                        typed_vals.push(typed_val);
                    }
                    return Ok((
                        TypedExpr::Do(typed_vals, val_type.clone(), *span, file.clone()),
                        val_type,
                    ));
                }
            }
            Expr::Index(expression, index, span, file) => {
                let (expr, expr_type) = self.expr_to_type(expression, span, file, substitutions)?;
                let (index, _index_type) = self.expr_to_type(index, span, file, substitutions)?;
                // println!("{:?}", expr);
                let ty = match expr_type.as_ref() {
                    Type::Constructor(c) if c.name == "Array" => {
                        c.generics[0].clone() // always works
                    }
                    ref c => {
                        todo!("add custom indexing types. {:?}", c)
                    }
                };
                return Ok((
                    TypedExpr::Index(
                        Box::new(expr),
                        Box::new(index),
                        ty.clone(),
                        *span,
                        file.clone(),
                    ),
                    ty,
                ));
            }
            Expr::StructAccess(struct_, field, span, file) => (
                TypedExpr::StructAccess(
                    Box::new(self.expr_to_type(struct_, span, file, substitutions)?.0),
                    std::borrow::Cow::Borrowed(field),
                    tvar!(self.0.len()),
                    *span,
                    file.clone(),
                ),
                tvar!(self.0.len()),
            ),
            Expr::Return(expr, span, file) => {
                let (val, ty) = self.expr_to_type(expr, span, file, substitutions)?;
                (
                    TypedExpr::Return(Box::new(val), ty.clone(), *span, file.clone()),
                    ty.clone(),
                )
            }
            Expr::Tuple(vals, span, file) => {
                if vals.is_empty() {
                    let type_ = tconst!("Array", tvar!(self.0.len() + 1));
                    return Ok((
                        TypedExpr::Tuple(vec![], type_.clone(), *span, file.clone()),
                        type_,
                    ));
                }
                if vals.len() == 1 {
                    let (val, val_type) = self.expr_to_type(&vals[0], span, file, substitutions)?;
                    let type_ = tconst!("Array", val_type);
                    (
                        TypedExpr::Tuple(vec![val], type_.clone(), *span, file.clone()),
                        type_,
                    )
                } else {
                    let (_val, mut val_type) =
                        self.expr_to_type(&vals[0], span, file, substitutions)?;
                    let mut typed_vals = vec![];
                    for val in &vals[1..] {
                        let (typed_val, val_type_) =
                            self.expr_to_type(val, span, file, substitutions)?;
                        unify(val_type, val_type_.clone(), substitutions, span, file)?;
                        val_type = val_type_;
                        typed_vals.push(typed_val);
                    }
                    (
                        TypedExpr::Tuple(typed_vals, val_type.clone(), *span, file.clone()),
                        val_type,
                    )
                }
            }
            Expr::Assign(lhs, val, span, file) => {
                let (val, val_type) = self.expr_to_type(val, span, file, substitutions)?;
                let (lhs, lhs_type) = self.expr_to_type(lhs, span, file, substitutions)?;
                unify(val_type.clone(), lhs_type, substitutions, span, file)?;
                (
                    TypedExpr::Assign(
                        Box::new(lhs),
                        Box::new(val),
                        val_type.clone(),
                        *span,
                        file.clone(),
                    ),
                    val_type,
                )
            }
            Expr::Break(span, file) => (TypedExpr::Break(*span, file.clone()), t_int!()),
            Expr::Continue(span, file) => (TypedExpr::Continue(*span, file.clone()), t_int!()),
        };
        // let substituted_expr = self.substitute_type_vars_in_typed_expr(typed_expr, substitutions);
        // let substituted_ty = Self::substitute_type_vars(ty, substitutions);
        Ok((typed_expr, ty))
    }

    pub fn node_to_type(
        &mut self,
        node: &'a Node,
        span: &Span,
        file: &String,
        substitutions: &mut HashMap<TypeVariable, Arc<Type>>,
    ) -> Result<TypedNode<'a>, TypeError> {
        self.0.insert(
            "type".to_string(),
            Type::Function(vec![tvar!(self.0.len() + 1)], t_str!()).into(),
        );
        self.0.insert(
            "print".to_string(),
            Type::Function(vec![t_str!()], t_int!()).into(),
        );
        self.0.insert(
            "println".to_string(),
            Type::Function(vec![t_str!()], t_int!()).into(),
        );
        let typed_node = match node {
            Node::Function(name, args, ret, ty) => match ty {
                Some(ty) => {
                    trace!("{:?}", ty);
                    let mut is_poly = false;
                    for (_, argtype) in args {
                        match argtype {
                            Some(_) => {}
                            None => {
                                is_poly = true;
                                break;
                            }
                        }
                    }
                    if is_poly {
                        let type_ = Type::Function(
                            args.iter()
                                .map(|(arg, _argtype)| {
                                    let type_: Arc<Type> = tvar!(self.0.len() + 1);
                                    self.0.insert(arg.to_string(), type_.clone());
                                    type_
                                })
                                .collect(),
                            tvar!(self.0.len() + 1),
                        );
                        self.0.insert(name.to_string(), type_.clone().into());
                        let (ret, _ret_type) = self.expr_to_type(ret, span, file, substitutions)?;
                        // unify(tvar!(self.0.len()+1), ret_type, substitutions);
                        return Ok(TypedNode::Function(
                            std::borrow::Cow::Borrowed(name),
                            args.iter()
                                .map(|(arg, _argtype)| {
                                    let type_: Arc<Type> = tvar!(self.0.len() + 1);
                                    self.0.insert(arg.to_string(), type_.clone());
                                    (arg.clone(), type_)
                                })
                                .collect(),
                            Box::new(ret),
                            type_.clone().into(),
                        ));
                    }
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
                        args.iter()
                            .map(|(arg, argtype)| match argtype {
                                Some(ty) => {
                                    let type_: Arc<Type> = Type::Constructor(TypeConstructor {
                                        name: ty.name.to_string(),
                                        generics: ty
                                            .generics
                                            .clone()
                                            .into_iter()
                                            .map(|generic| tconst!(generic))
                                            .collect(),
                                        traits: vec![],
                                    })
                                    .into();
                                    self.0.insert(arg.to_string(), type_.clone());
                                    type_.clone()
                                }
                                None => {
                                    let var = tvar!(self.0.len() + 1);
                                    self.0.insert(arg.to_string(), var.clone());
                                    var
                                }
                            })
                            .collect(),
                        annoted_type.clone().into(),
                    );
                    self.0.insert(name.to_string(), type_.clone().into());
                    let (ret, ret_type) = self.expr_to_type(ret, span, file, substitutions)?;
                    unify(annoted_type.into(), ret_type, substitutions, span, file)?;
                    let retval = TypedNode::Function(
                        std::borrow::Cow::Borrowed(name),
                        args.iter()
                            .map(|(arg, type__)| {
                                (
                                    arg.clone(),
                                    match type__ {
                                        Some(t) => Type::Constructor(TypeConstructor {
                                            name: t.name.to_string(),
                                            generics: t
                                                .generics
                                                .clone()
                                                .into_iter()
                                                .map(|generic| tconst!(generic))
                                                .collect(),
                                            traits: vec![],
                                        })
                                        .into(),
                                        None => {
                                            tvar!(self.0.len() + 1)
                                        }
                                    },
                                )
                            })
                            .collect(),
                        Box::new(ret),
                        type_.clone().into(),
                    );
                    retval
                }
                None => {
                    // let ret_type = tvar!(self.0.len() + 1);
                    let (expr, expr_type) = self.expr_to_type(ret, span, file, substitutions)?;
                    // unify(expr_type.clone(), ret_type, substitutions);

                    let type_ = Type::Function(
                        args.iter().map(|_arg| tvar!(self.0.len() + 1)).collect(),
                        expr_type.clone(),
                    );
                    self.0.insert(name.to_string(), type_.clone().into());
                    TypedNode::Function(
                        std::borrow::Cow::Borrowed(name),
                        args.iter()
                            .map(|(arg, type__)| {
                                (
                                    arg.clone(),
                                    match type__ {
                                        Some(t) => Type::Constructor(TypeConstructor {
                                            name: t.name.to_string(),
                                            generics: t
                                                .generics
                                                .clone()
                                                .into_iter()
                                                .map(|generic| tconst!(generic))
                                                .collect(),
                                            traits: vec![],
                                        })
                                        .into(),
                                        None => {
                                            let var = tvar!(self.0.len() + 1);
                                            self.0.insert(arg.to_string(), var.clone());
                                            var
                                        }
                                    },
                                )
                            })
                            .collect(),
                        Box::new(expr),
                        type_.into(),
                    )
                }
            },
            Node::Expr(e) => {
                let (e, t) = self.expr_to_type(e, span, file, substitutions)?;
                TypedNode::Expr(Box::new(e), t)
            }
            Node::Error(e) => panic!("{:?}", e),
            Node::Program(nodes) => {
                let mut nodes_ = vec![];
                for (node, span, file) in nodes {
                    nodes_.push((
                        self.node_to_type(node, span, file, substitutions)?,
                        *span,
                        file.clone(),
                    ));
                }
                TypedNode::Program(nodes_)
            }
            Node::Struct(name, generics, fields) => TypedNode::Struct(
                std::borrow::Cow::Borrowed(name),
                generics.to_vec(),
                fields
                    .iter()
                    .map(|(name, ty)| {
                        (
                            name.clone(),
                            Type::Constructor(TypeConstructor {
                                name: ty.name.to_string(),
                                generics: ty
                                    .generics
                                    .clone()
                                    .into_iter()
                                    .map(|generic| tconst!(generic))
                                    .collect(),
                                traits: vec![],
                            })
                            .into(),
                        )
                    })
                    .collect::<Vec<(_, Arc<Type>)>>(),
            ),
            Node::Extern(name, args, ret_type) => {
                let annoted_type: Arc<Type> = Type::Constructor(TypeConstructor {
                    name: ret_type.name.to_string(),
                    generics: ret_type
                        .generics
                        .clone()
                        .into_iter()
                        .map(|generic| tconst!(generic))
                        .collect(),
                    traits: vec![],
                })
                .into();
                let mut typed_args = vec![];
                let type_ = Type::Function(
                    args.iter()
                        .map(|argtype| {
                            let type_: Arc<_> = Type::Constructor(TypeConstructor {
                                name: argtype.name.to_string(),
                                generics: argtype
                                    .generics
                                    .clone()
                                    .into_iter()
                                    .map(|generic| tconst!(generic))
                                    .collect(),
                                traits: vec![],
                            })
                            .into();
                            typed_args.push(type_.clone());
                            type_.clone()
                        })
                        .collect(),
                    annoted_type.clone(),
                );
                self.0.insert(name.to_string(), type_.into());
                TypedNode::Extern(name.clone(), typed_args, annoted_type.clone())
            }
        };
        Ok(typed_node)
        // let x = self.substitute_type_vars_in_typed_node(typed_node, substitutions);
        // println!("{:#?}", x);
        // x
    }

    fn substitute_type_vars(
        // &mut self,
        ty: Arc<Type>,
        substitutions: &mut HashMap<TypeVariable, Arc<Type>>,
    ) -> Arc<Type> {
        match ty.as_ref() {
            Type::Variable(tv) => {
                if let Some(subst) = substitutions.get(tv) {
                    Self::substitute_type_vars(subst.clone(), substitutions) // Recursively substitute
                } else {
                    ty.clone() // No substitution found
                }
            }
            Type::Function(args, ret) => {
                // let new_generics = generics.iter().map(|g| substitute_type_vars(g.clone(), substitutions)).collect::<Vec<_>>();
                let new_args = args
                    .iter()
                    .map(|arg| Self::substitute_type_vars(arg.clone(), substitutions))
                    .collect::<Vec<_>>();
                let new_ret = Self::substitute_type_vars(ret.clone(), substitutions);
                Type::Function(new_args, new_ret).into()
            }
            Type::Constructor(tc) => {
                let new_generics = tc
                    .generics
                    .iter()
                    .map(|g| Self::substitute_type_vars(g.clone(), substitutions))
                    .collect::<Vec<_>>();
                Type::Constructor(TypeConstructor {
                    name: tc.name.clone(),
                    generics: new_generics,
                    traits: tc.traits.clone(), // You might need to substitute traits as well, if they can contain type variables
                })
                .into()
            }
            Type::Struct(..) => ty,
            Type::Trait(..) => ty,
            Type::Tuple(vals) => {
                let mut newvals = vec![];
                for val in vals {
                    newvals.push(Self::substitute_type_vars(val.clone(), substitutions))
                }
                Type::Tuple(newvals).into()
            }
        }
    }

    // pub fn substitute_type_vars_in_typed_expr(
    //     &mut self,
    //     typed_expr: TypedExpr<'a>,
    //     substitutions: &mut HashMap<TypeVariable, Arc<Type>>,
    // ) -> TypedExpr<'a> {
    //     match typed_expr {
    //         TypedExpr::Literal(lit, ty) => {
    //             let new_ty = Self::substitute_type_vars(ty, substitutions);
    //             TypedExpr::Literal(lit, new_ty)
    //         }
    //         TypedExpr::Variable(name, ty) => {
    //             match self.0.get(&name.to_string()) {
    //                 Some(t) => {
    //                     unify(ty, t.clone(), substitutions, span, file);
    //                     TypedExpr::Variable(
    //                         std::borrow::Cow::Owned(name.to_string()),
    //                         Self::substitute_type_vars(t.clone(), substitutions),
    //                     )
    //                 }
    //                 None => {
    //                     // println!("unknown type {name}");
    //                     self.0
    //                         .insert(name.to_string(), tvar!(self.0.len() + 1).clone());

    //                     TypedExpr::Variable(
    //                         std::borrow::Cow::Owned(name.to_string()),
    //                         tvar!(self.0.len()),
    //                     )
    //                 }
    //             }
    //         }
    //         TypedExpr::Lambda(args, body, ret_ty) => {
    //             let new_args = args
    //                 .into_iter()
    //                 .map(|(name, ty)| (name, Self::substitute_type_vars(ty, substitutions)))
    //                 .collect();
    //             let new_body =
    //                 Box::new(self.substitute_type_vars_in_typed_expr(*body, substitutions));
    //             let new_ret_ty = Self::substitute_type_vars(ret_ty, substitutions);
    //             TypedExpr::Lambda(new_args, new_body, new_ret_ty)
    //         }
    //         TypedExpr::Let(name, expr, ty) => {
    //             let new_expr =
    //                 Box::new(self.substitute_type_vars_in_typed_expr(*expr, substitutions));
    //             let new_ty = Self::substitute_type_vars(ty, substitutions);
    //             TypedExpr::Let(name, new_expr, new_ty)
    //         }
    //         TypedExpr::If(cond, then_branch, else_branch, ty) => {
    //             let new_cond =
    //                 Box::new(self.substitute_type_vars_in_typed_expr(*cond, substitutions));
    //             let new_then_branch =
    //                 Box::new(self.substitute_type_vars_in_typed_expr(*then_branch, substitutions));
    //             let new_else_branch = else_branch.map(|eb| {
    //                 Box::new(self.substitute_type_vars_in_typed_expr(*eb, substitutions))
    //             });
    //             let new_ty = Self::substitute_type_vars(ty, substitutions);

    //             TypedExpr::If(new_cond, new_then_branch, new_else_branch, new_ty)
    //         }
    //         TypedExpr::Call(func, args, ret_ty) => {
    //             let new_func =
    //                 Box::new(self.substitute_type_vars_in_typed_expr(*func, substitutions));
    //             let new_args = args
    //                 .into_iter()
    //                 .map(|arg| self.substitute_type_vars_in_typed_expr(arg, substitutions))
    //                 .collect();
    //             let new_ret_ty = Self::substitute_type_vars(ret_ty, substitutions);
    //             TypedExpr::Call(new_func, new_args, new_ret_ty)
    //         }
    //         TypedExpr::While(cond, body, ty) => {
    //             let new_cond =
    //                 Box::new(self.substitute_type_vars_in_typed_expr(*cond, substitutions));
    //             let new_body =
    //                 Box::new(self.substitute_type_vars_in_typed_expr(*body, substitutions));
    //             let new_ty = Self::substitute_type_vars(ty, substitutions);
    //             TypedExpr::While(new_cond, new_body, new_ty)
    //         }
    //         TypedExpr::BinaryOp(lhs, op, rhs, ty) => {
    //             let new_lhs =
    //                 Box::new(self.substitute_type_vars_in_typed_expr(*lhs, substitutions));
    //             let new_rhs =
    //                 Box::new(self.substitute_type_vars_in_typed_expr(*rhs, substitutions));
    //             let new_ty = Self::substitute_type_vars(ty, substitutions);
    //             TypedExpr::BinaryOp(new_lhs, op, new_rhs, new_ty)
    //         }
    //         TypedExpr::UnaryOp(op, expr, ty) => {
    //             let new_expr =
    //                 Box::new(self.substitute_type_vars_in_typed_expr(*expr, substitutions));
    //             let new_ty = Self::substitute_type_vars(ty, substitutions);
    //             TypedExpr::UnaryOp(op, new_expr, new_ty)
    //         }
    //         TypedExpr::Array(values, ty) => {
    //             let new_vals = values
    //                 .into_iter()
    //                 .map(|val| self.substitute_type_vars_in_typed_expr(val, substitutions))
    //                 .collect();
    //             let new_ty = Self::substitute_type_vars(ty, substitutions);
    //             TypedExpr::Array(new_vals, new_ty)
    //         }
    //         TypedExpr::Do(exprs, ty) => {
    //             let new_exprs = exprs
    //                 .into_iter()
    //                 .map(|expr| self.substitute_type_vars_in_typed_expr(expr, substitutions))
    //                 .collect();
    //             let new_ty = Self::substitute_type_vars(ty, substitutions);
    //             TypedExpr::Do(new_exprs, new_ty)
    //         }
    //         TypedExpr::Index(arr, index, ty) => {
    //             let new_arr =
    //                 Box::new(self.substitute_type_vars_in_typed_expr(*arr, substitutions));
    //             let new_idx =
    //                 Box::new(self.substitute_type_vars_in_typed_expr(*index, substitutions));
    //             let new_ty = Self::substitute_type_vars(ty, substitutions);

    //             TypedExpr::Index(new_arr, new_idx, new_ty)
    //         }
    //         TypedExpr::StructAccess(_, _, _) => typed_expr,
    //         TypedExpr::Return(expr, ty) => TypedExpr::Return(
    //             Box::new(self.substitute_type_vars_in_typed_expr(*expr, substitutions)),
    //             Self::substitute_type_vars(ty, substitutions),
    //         ),
    //         TypedExpr::Tuple(values, ty) => {
    //             let new_vals = values
    //                 .into_iter()
    //                 .map(|val| self.substitute_type_vars_in_typed_expr(val, substitutions))
    //                 .collect();
    //             let new_ty = Self::substitute_type_vars(ty, substitutions);
    //             TypedExpr::Tuple(new_vals, new_ty)
    //         }
    //         TypedExpr::Assign(name, expr, ty) => TypedExpr::Assign(
    //             name,
    //             Box::new(self.substitute_type_vars_in_typed_expr(*expr, substitutions)),
    //             Self::substitute_type_vars(ty, substitutions),
    //         ),
    //         TypedExpr::Break | TypedExpr::Continue => typed_expr,
    //     }
    // }

    // pub fn substitute_type_vars_in_typed_node(
    //     &mut self,
    //     typed_node: TypedNode<'a>,
    //     substitutions: &mut HashMap<TypeVariable, Arc<Type>>,
    // ) -> TypedNode<'a> {
    //     match typed_node {
    //         TypedNode::Expr(expr, ty) => {
    //             let new_expr =
    //                 Box::new(self.substitute_type_vars_in_typed_expr(*expr, substitutions));
    //             let new_ty = Self::substitute_type_vars(ty, substitutions);
    //             TypedNode::Expr(new_expr, new_ty)
    //         }
    //         TypedNode::Function(name, args, body, ret_ty) => {
    //             let new_args = args
    //                 .into_iter()
    //                 .map(|(name, ty)| (name, Self::substitute_type_vars(ty, substitutions)))
    //                 .collect();
    //             let new_body =
    //                 Box::new(self.substitute_type_vars_in_typed_expr(*body, substitutions));
    //             let new_ret_ty = Self::substitute_type_vars(ret_ty, substitutions);

    //             TypedNode::Function(name, new_args, new_body, new_ret_ty)
    //         }
    //         TypedNode::Program(nodes) => {
    //             let new_nodes = nodes
    //                 .into_iter()
    //                 .map(|(n, span, file)| (self.substitute_type_vars_in_typed_node(n, substitutions), span, file))
    //                 .collect();
    //             TypedNode::Program(new_nodes)
    //         }

    //         _ => typed_node,
    //     }
    // }
}

fn get_type_from_typed_expr(expr: &TypedExpr) -> Arc<Type> {
    match expr {
        TypedExpr::Literal(_, ty, ..) => ty.clone(),
        TypedExpr::Variable(_, ty, ..) => ty.clone(),
        TypedExpr::Lambda(_, _, ty, ..) => ty.clone(),
        TypedExpr::Let(_, _, ty, ..) => ty.clone(),
        TypedExpr::If(_, _, _, ty, ..) => ty.clone(),
        TypedExpr::Call(_, _, ty, ..) => ty.clone(),
        TypedExpr::While(_, _, ty, ..) => ty.clone(),
        TypedExpr::BinaryOp(_, _, _, ty, ..) => ty.clone(),
        TypedExpr::UnaryOp(_, _, ty, ..) => ty.clone(),
        TypedExpr::Array(_, ty, ..) => ty.clone(),
        TypedExpr::Do(_, ty, ..) => ty.clone(),
        TypedExpr::Index(_, _, ty, ..) => ty.clone(),
        TypedExpr::StructAccess(_, _, ty, ..) => ty.clone(),
        TypedExpr::Return(_, ty, ..) => ty.clone(),
        TypedExpr::Tuple(_, ty, ..) => ty.clone(),
        TypedExpr::Assign(_, _, ty, ..) => ty.clone(),
        TypedExpr::Break(..) | TypedExpr::Continue(..) => t_int!(),
    }
}
