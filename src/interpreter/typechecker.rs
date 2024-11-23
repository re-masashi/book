use crate::interpreter::*;
use crate::{tconst, tvar};

impl<'a> TypeEnv {
    pub fn expr_to_type(
        &mut self,
        expr: &'a Expr,
        substitutions: &mut HashMap<TypeVariable, Arc<Type>>,
    ) -> (TypedExpr<'a>, Arc<Type>) {
        let (typed_expr, ty) = match expr {
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
                    let (expr_typed, type_) = self.expr_to_type(value, substitutions);
                    unify(annoted_type.into(), type_.clone(), substitutions);
                    self.0.insert(name.to_string(), type_.clone());
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
                    let (expr_typed, type_) = self.expr_to_type(value, substitutions);
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
            Expr::Variable(name) => match self.0.get(&name.to_string()) {
                Some(t) => (
                    TypedExpr::Variable(std::borrow::Cow::Borrowed(name), t.clone()),
                    t.clone(),
                ),
                None => {
                    self.0
                        .insert(name.to_string(), tvar!(self.0.len() + 1).clone());
                    (
                        TypedExpr::Variable(std::borrow::Cow::Borrowed(name), tvar!(self.0.len())),
                        tvar!(self.0.len()),
                    )
                }
            },
            Expr::Lambda(args, expression) => {
                let (expression_typed, type_) = self.expr_to_type(expression, substitutions);
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
                } else {
                    (
                        TypedExpr::If(Box::new(cond_), Box::new(if_), None, if_type.clone()),
                        if_type,
                    )
                }
            }
            Expr::Call(value, args) => {
                let (value_, value_type) = self.expr_to_type(value, substitutions);
                match value_type.clone().as_ref() {
                    Type::Variable(_) => {
                        let new_args = args
                            .iter()
                            .map(|arg| Box::new(self.expr_to_type(arg, substitutions).0))
                            .collect::<Vec<_>>();
                        (
                            TypedExpr::Call(Box::new(value_), new_args, value_type.clone()),
                            value_type.clone(),
                        )
                    }
                    Type::Function(_fn_args, _ret_type) => {
                        let type_ = Type::Function(
                            args.iter().map(|_arg| tvar!(self.0.len() + 1)).collect(),
                            tvar!(self.0.len() + 1),
                        );
                        unify(value_type, type_.into(), substitutions);
                        let new_args = args
                            .iter()
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
                    Type::Struct(..) => {
                        todo!()
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
                let (rhs, rhs_type) = self.expr_to_type(rhs, substitutions);
                unify(lhs_type.clone(), rhs_type.clone(), substitutions);

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
                if vals.is_empty() {
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
                        let (typed_val, val_type_) = self.expr_to_type(val, substitutions);
                        unify(val_type, val_type_.clone(), substitutions);
                        val_type = val_type_;
                        typed_vals.push(typed_val);
                    }
                    return (TypedExpr::Array(typed_vals, val_type.clone()), val_type);
                }
            }
            Expr::Do(expressions) => {
                if expressions.is_empty() {
                    let type_ = tvar!(self.0.len() + 1);
                    return (TypedExpr::Do(vec![], type_.clone()), type_);
                }
                if expressions.len() == 1 {
                    let (val, val_type) = self.expr_to_type(&expressions[0], substitutions);
                    let type_ = val_type;
                    return (TypedExpr::Do(vec![val], type_.clone()), type_);
                } else {
                    let mut typed_vals = vec![];
                    let (val, mut val_type) = self.expr_to_type(&expressions[0], substitutions);
                    typed_vals.push(val);
                    for val in &expressions[1..] {
                        let (typed_val, val_type_) = self.expr_to_type(val, substitutions);
                        val_type = val_type_;
                        typed_vals.push(typed_val);
                    }
                    return (TypedExpr::Do(typed_vals, val_type.clone()), val_type);
                }
            }
            Expr::Index(expression, index) => {
                let (expr, _expr_type) = self.expr_to_type(expression, substitutions);
                let (index, _index_type) = self.expr_to_type(index, substitutions);
                return (
                    TypedExpr::Index(Box::new(expr), Box::new(index), tvar!(self.0.len() + 1)),
                    tvar!(self.0.len()),
                );
            }
            Expr::StructAccess(struct_, field) => {
                return (
                    TypedExpr::StructAccess(
                        Box::new(self.expr_to_type(struct_, substitutions).0),
                        std::borrow::Cow::Borrowed(field),
                        tvar!(self.0.len()),
                    ),
                    tvar!(self.0.len()),
                )
            }
        };
        let substituted_expr = Self::substitute_type_vars_in_typed_expr(typed_expr, substitutions);
        let substituted_ty = Self::substitute_type_vars(ty, substitutions);
        (substituted_expr, substituted_ty)
    }

    pub fn node_to_type(
        &mut self,
        node: &'a Node,
        substitutions: &mut HashMap<TypeVariable, Arc<Type>>,
    ) -> TypedNode<'a> {
        let typed_node = match node {
            Node::Function(name, args, ret, ty) => match ty {
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
                                },
                                None => tvar!(self.0.len() + 1),
                            })
                            .collect(),
                        annoted_type.clone().into(),
                    );
                    self.0.insert(name.to_string(), type_.clone().into());
                    let (ret, ret_type) = self.expr_to_type(ret, substitutions);
                    unify(annoted_type.into(), ret_type, substitutions);
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
                    let (expr, expr_type) = self.expr_to_type(ret, substitutions);
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
                                            tvar!(self.0.len() + 1)
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
                let (e, t) = self.expr_to_type(e, substitutions);
                TypedNode::Expr(Box::new(e), t)
            }
            Node::Error(e) => panic!("{:?}", e),
            Node::Program(nodes) => TypedNode::Program(
                nodes
                    .iter()
                    .map(|node| self.node_to_type(node, substitutions))
                    .collect(),
            ),
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
        };
        Self::substitute_type_vars_in_typed_node(typed_node, substitutions)
    }

    fn substitute_type_vars(
        ty: Arc<Type>,
        substitutions: &HashMap<TypeVariable, Arc<Type>>,
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
        }
    }

    fn substitute_type_vars_in_typed_expr(
        typed_expr: TypedExpr<'a>,
        substitutions: &HashMap<TypeVariable, Arc<Type>>,
    ) -> TypedExpr<'a> {
        match typed_expr {
            TypedExpr::Literal(lit, ty) => {
                let new_ty = Self::substitute_type_vars(ty, substitutions);
                TypedExpr::Literal(lit, new_ty)
            }
            TypedExpr::Variable(name, ty) => {
                let new_ty = Self::substitute_type_vars(ty, substitutions);
                TypedExpr::Variable(name, new_ty)
            }
            TypedExpr::Lambda(args, body, ret_ty) => {
                let new_args = args
                    .into_iter()
                    .map(|(name, ty)| (name, Self::substitute_type_vars(ty, substitutions)))
                    .collect();
                let new_body = Box::new(Self::substitute_type_vars_in_typed_expr(
                    *body,
                    substitutions,
                ));
                let new_ret_ty = Self::substitute_type_vars(ret_ty, substitutions);
                TypedExpr::Lambda(new_args, new_body, new_ret_ty)
            }
            TypedExpr::Let(name, expr, ty) => {
                let new_expr = Box::new(Self::substitute_type_vars_in_typed_expr(
                    *expr,
                    substitutions,
                ));
                let new_ty = Self::substitute_type_vars(ty, substitutions);
                TypedExpr::Let(name, new_expr, new_ty)
            }
            TypedExpr::If(cond, then_branch, else_branch, ty) => {
                let new_cond = Box::new(Self::substitute_type_vars_in_typed_expr(
                    *cond,
                    substitutions,
                ));
                let new_then_branch = Box::new(Self::substitute_type_vars_in_typed_expr(
                    *then_branch,
                    substitutions,
                ));
                let new_else_branch = else_branch.map(|eb| {
                    Box::new(Self::substitute_type_vars_in_typed_expr(*eb, substitutions))
                });
                let new_ty = Self::substitute_type_vars(ty, substitutions);

                TypedExpr::If(new_cond, new_then_branch, new_else_branch, new_ty)
            }
            TypedExpr::Call(func, args, ret_ty) => {
                let new_func = Box::new(Self::substitute_type_vars_in_typed_expr(
                    *func,
                    substitutions,
                ));
                let new_args = args
                    .into_iter()
                    .map(|arg| {
                        Box::new(Self::substitute_type_vars_in_typed_expr(
                            *arg,
                            substitutions,
                        ))
                    })
                    .collect();
                let new_ret_ty = Self::substitute_type_vars(ret_ty, substitutions);
                TypedExpr::Call(new_func, new_args, new_ret_ty)
            }
            TypedExpr::While(cond, body, ty) => {
                let new_cond = Box::new(Self::substitute_type_vars_in_typed_expr(
                    *cond,
                    substitutions,
                ));
                let new_body = Box::new(Self::substitute_type_vars_in_typed_expr(
                    *body,
                    substitutions,
                ));
                let new_ty = Self::substitute_type_vars(ty, substitutions);
                TypedExpr::While(new_cond, new_body, new_ty)
            }
            TypedExpr::BinaryOp(lhs, op, rhs, ty) => {
                let new_lhs = Box::new(Self::substitute_type_vars_in_typed_expr(
                    *lhs,
                    substitutions,
                ));
                let new_rhs = Box::new(Self::substitute_type_vars_in_typed_expr(
                    *rhs,
                    substitutions,
                ));
                let new_ty = Self::substitute_type_vars(ty, substitutions);
                TypedExpr::BinaryOp(new_lhs, op, new_rhs, new_ty)
            }
            TypedExpr::UnaryOp(op, expr, ty) => {
                let new_expr = Box::new(Self::substitute_type_vars_in_typed_expr(
                    *expr,
                    substitutions,
                ));
                let new_ty = Self::substitute_type_vars(ty, substitutions);
                TypedExpr::UnaryOp(op, new_expr, new_ty)
            }
            TypedExpr::Array(values, ty) => {
                let new_vals = values
                    .into_iter()
                    .map(|val| Self::substitute_type_vars_in_typed_expr(val, substitutions))
                    .collect();
                let new_ty = Self::substitute_type_vars(ty, substitutions);
                TypedExpr::Array(new_vals, new_ty)
            }
            TypedExpr::Do(exprs, ty) => {
                let new_exprs = exprs
                    .into_iter()
                    .map(|expr| Self::substitute_type_vars_in_typed_expr(expr, substitutions))
                    .collect();
                let new_ty = Self::substitute_type_vars(ty, substitutions);
                TypedExpr::Do(new_exprs, new_ty)
            }
            TypedExpr::Index(arr, index, ty) => {
                let new_arr = Box::new(Self::substitute_type_vars_in_typed_expr(
                    *arr,
                    substitutions,
                ));
                let new_idx = Box::new(Self::substitute_type_vars_in_typed_expr(
                    *index,
                    substitutions,
                ));
                let new_ty = Self::substitute_type_vars(ty, substitutions);

                TypedExpr::Index(new_arr, new_idx, new_ty)
            }
            TypedExpr::StructAccess(_, _, _) => typed_expr,
        }
    }

    //And similarly for TypedNode:
    fn substitute_type_vars_in_typed_node(
        typed_node: TypedNode<'a>,
        substitutions: &HashMap<TypeVariable, Arc<Type>>,
    ) -> TypedNode<'a> {
        match typed_node {
            TypedNode::Expr(expr, ty) => {
                let new_expr = Box::new(Self::substitute_type_vars_in_typed_expr(
                    *expr,
                    substitutions,
                ));
                let new_ty = Self::substitute_type_vars(ty, substitutions);
                TypedNode::Expr(new_expr, new_ty)
            }
            TypedNode::Function(name, args, body, ret_ty) => {
                let new_args = args
                    .into_iter()
                    .map(|(name, ty)| (name, Self::substitute_type_vars(ty, substitutions)))
                    .collect();
                let new_body = Box::new(Self::substitute_type_vars_in_typed_expr(
                    *body,
                    substitutions,
                ));
                let new_ret_ty = Self::substitute_type_vars(ret_ty, substitutions);

                TypedNode::Function(name, new_args, new_body, new_ret_ty)
            }
            TypedNode::Program(nodes) => {
                let new_nodes = nodes
                    .into_iter()
                    .map(|n| Self::substitute_type_vars_in_typed_node(n, substitutions))
                    .collect();
                TypedNode::Program(new_nodes)
            }

            _ => typed_node,
        }
    }
}

fn get_type_from_typed_expr(expr: &TypedExpr) -> Arc<Type> {
    match expr {
        TypedExpr::Literal(_, ty) => ty.clone(),
        TypedExpr::Variable(_, ty) => ty.clone(),
        TypedExpr::Lambda(_, _, ty) => ty.clone(),
        TypedExpr::Let(_, _, ty) => ty.clone(),
        TypedExpr::If(_, _, _, ty) => ty.clone(),
        TypedExpr::Call(_, _, ty) => ty.clone(),
        TypedExpr::While(_, _, ty) => ty.clone(),
        TypedExpr::BinaryOp(_, _, _, ty) => ty.clone(),
        TypedExpr::UnaryOp(_, _, ty) => ty.clone(),
        TypedExpr::Array(_, ty) => ty.clone(),
        TypedExpr::Do(_, ty) => ty.clone(),
        TypedExpr::Index(_, _, ty) => ty.clone(),
        TypedExpr::StructAccess(_, _, ty) => ty.clone(),
    }
}
