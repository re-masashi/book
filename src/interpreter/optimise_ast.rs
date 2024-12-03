use crate::interpreter::{BinaryOperator, Literal, Type, TypeConstructor, TypedExpr, TypedNode};
use crate::tconst;
use std::collections::HashMap;
use std::sync::Arc;

pub fn optimise_node(node: &mut TypedNode) {
    match node {
        TypedNode::Function(_, _, expr, _) => optimise(&mut *expr),
        TypedNode::Expr(expr, _) => optimise(&mut *expr),
        TypedNode::Program(nodes) => {
            for node in nodes {
                optimise_node(node);
            }
        }
        _ => {}
    }
}

pub fn optimise(ast: &mut TypedExpr) {
    optimise_constant_fold(ast);
    optimise_constant_propagation(ast, &mut HashMap::new());
    optimise_const_branching_if(ast);
}

pub fn optimise_constant_fold(ast: &mut TypedExpr) {
    let mut temp: TypedExpr = TypedExpr::Literal(Literal::Int(0).into(), tconst!("int"));
    let mut to_swap = false;

    match ast {
        TypedExpr::Let(_, ref mut expr, _) => optimise_constant_fold(expr),
        TypedExpr::Variable(..) => {}
        TypedExpr::Assign(_name, expr, _) => {
            optimise_constant_fold(expr);
            // todo: handle this w care
        }
        TypedExpr::Lambda(_, expr, _) => optimise_constant_fold(expr),
        TypedExpr::Literal(_, _) => {} // skip
        TypedExpr::If(cond, if_, else_, _) => {
            optimise_constant_fold(cond);
            optimise_constant_fold(if_);
            if let Some(pat) = else_ {
                optimise_constant_fold(pat);
            }
        }
        TypedExpr::Call(_, args, _) => {
            args.iter_mut().for_each(|arg| {
                optimise_constant_fold(&mut *arg);
            });
        }
        TypedExpr::While(cond, expr, _) => {
            optimise_constant_fold(cond);
            optimise_constant_fold(expr);
        } // condition, body
        TypedExpr::BinaryOp(lhs, op, rhs, _) => {
            optimise_constant_fold(&mut *lhs);
            optimise_constant_fold(&mut *rhs);
            if let (TypedExpr::Literal(a, _), op, TypedExpr::Literal(b, _)) =
                (lhs.as_ref(), op, rhs.as_ref())
            {
                temp = match (a.as_ref(), op, b.as_ref()) {
                    (Literal::Int(a), BinaryOperator::Add, Literal::Int(b)) => {
                        let res = Literal::Int(a + b);
                        TypedExpr::Literal(res.into(), tconst!("int"))
                    }
                    (Literal::Int(a), BinaryOperator::Sub, Literal::Int(b)) => {
                        let res = Literal::Int(a - b);
                        TypedExpr::Literal(res.into(), tconst!("int"))
                    }
                    (Literal::Int(a), BinaryOperator::Mul, Literal::Int(b)) => {
                        let res = Literal::Int(a * b);
                        TypedExpr::Literal(res.into(), tconst!("int"))
                    }
                    (Literal::Int(a), BinaryOperator::Div, Literal::Int(b)) => {
                        if *b == 0 {
                            panic!("ATTEMPTED TO DIVIDE BY 0");
                        }
                        let res = Literal::Int(a / b);
                        TypedExpr::Literal(res.into(), tconst!("int"))
                    }

                    (Literal::Int(a), BinaryOperator::Equal, Literal::Int(b)) => {
                        let res = Literal::Boolean(a == b);
                        TypedExpr::Literal(res.into(), tconst!("bool"))
                    }
                    (Literal::Int(a), BinaryOperator::NotEqual, Literal::Int(b)) => {
                        let res = Literal::Boolean(a != b);
                        TypedExpr::Literal(res.into(), tconst!("bool"))
                    }
                    (Literal::Int(a), BinaryOperator::Greater, Literal::Int(b)) => {
                        let res = Literal::Boolean(a > b);
                        TypedExpr::Literal(res.into(), tconst!("bool"))
                    }
                    (Literal::Int(a), BinaryOperator::Less, Literal::Int(b)) => {
                        let res = Literal::Boolean(a < b);
                        TypedExpr::Literal(res.into(), tconst!("bool"))
                    }
                    (Literal::Int(a), BinaryOperator::GreaterEqual, Literal::Int(b)) => {
                        let res = Literal::Boolean(a >= b);
                        TypedExpr::Literal(res.into(), tconst!("bool"))
                    }
                    (Literal::Int(a), BinaryOperator::LessEqual, Literal::Int(b)) => {
                        let res = Literal::Boolean(a <= b);
                        TypedExpr::Literal(res.into(), tconst!("bool"))
                    }

                    // floats
                    (Literal::Float(a), BinaryOperator::Add, Literal::Float(b)) => {
                        let res = Literal::Float(a + b);
                        TypedExpr::Literal(res.into(), tconst!("int"))
                    }
                    (Literal::Float(a), BinaryOperator::Sub, Literal::Float(b)) => {
                        let res = Literal::Float(a - b);
                        TypedExpr::Literal(res.into(), tconst!("int"))
                    }
                    (Literal::Float(a), BinaryOperator::Mul, Literal::Float(b)) => {
                        let res = Literal::Float(a * b);
                        TypedExpr::Literal(res.into(), tconst!("int"))
                    }
                    (Literal::Float(a), BinaryOperator::Div, Literal::Float(b)) => {
                        if *b == 0.0 {
                            panic!("ATTEMPTED TO DIVIDE BY 0");
                        }
                        let res = Literal::Float(a / b);
                        TypedExpr::Literal(res.into(), tconst!("int"))
                    }

                    (Literal::Float(a), BinaryOperator::Equal, Literal::Float(b)) => {
                        let res = Literal::Boolean(a == b);
                        TypedExpr::Literal(res.into(), tconst!("bool"))
                    }
                    (Literal::Float(a), BinaryOperator::NotEqual, Literal::Float(b)) => {
                        let res = Literal::Boolean(a != b);
                        TypedExpr::Literal(res.into(), tconst!("bool"))
                    }
                    (Literal::Float(a), BinaryOperator::Greater, Literal::Float(b)) => {
                        let res = Literal::Boolean(a > b);
                        TypedExpr::Literal(res.into(), tconst!("bool"))
                    }
                    (Literal::Float(a), BinaryOperator::Less, Literal::Float(b)) => {
                        let res = Literal::Boolean(a < b);
                        TypedExpr::Literal(res.into(), tconst!("bool"))
                    }
                    (Literal::Float(a), BinaryOperator::GreaterEqual, Literal::Float(b)) => {
                        let res = Literal::Boolean(a >= b);
                        TypedExpr::Literal(res.into(), tconst!("bool"))
                    }
                    (Literal::Float(a), BinaryOperator::LessEqual, Literal::Float(b)) => {
                        let res = Literal::Boolean(a <= b);
                        TypedExpr::Literal(res.into(), tconst!("bool"))
                    }
                    _ => return,
                };
                to_swap = true;
            }
        } // lhs, op, rhs
        TypedExpr::UnaryOp(_op, expr, _) => {
            optimise_constant_fold(expr);
            // no constant folding (im tired)
        }
        TypedExpr::Array(elements, _) => {
            for element in elements.iter_mut() {
                optimise_constant_fold(element);
            }
        }
        TypedExpr::Do(expressions, _) => {
            for expression in expressions.iter_mut() {
                optimise_constant_fold(expression);
            }
        }
        TypedExpr::Index(expression, index, _) => {
            optimise_constant_fold(expression);
            optimise_constant_fold(index);
        }
        TypedExpr::StructAccess(_, _, _) => {} // nothing to do
        TypedExpr::Return(expr, _) => {
            optimise_constant_fold(expr);
        }
        TypedExpr::Tuple(elements, _) => {
            for element in elements.iter_mut() {
                optimise_constant_fold(element);
            }
        }
        TypedExpr::Break | TypedExpr::Continue => {}
    };
    if to_swap {
        // println!("swapped");
        *ast = temp;
    }
}

pub fn optimise_constant_propagation<'a>(
    ast: &mut TypedExpr<'a>,
    variables: &mut HashMap<String, Box<TypedExpr<'a>>>,
) {
    let mut temp = TypedExpr::Literal(Literal::Boolean(true).into(), tconst!("bool"));
    let mut to_swap = false;
    match ast {
        TypedExpr::Let(name, expr, _) => {
            // let mut exp = expr.clone();
            optimise_constant_propagation(expr, variables);
            variables.insert(name.to_string(), expr.clone());
        }
        TypedExpr::Variable(name, _) => {
            if let Some(expr) = variables.get(&name.to_string()) {
                if let TypedExpr::Literal(lit, t) = expr.as_ref() {
                    temp = TypedExpr::Literal(lit.clone(), t.clone());
                } else if let TypedExpr::Variable(other_name, annot) = expr.clone().as_ref() {
                    // Recursively propagate variables if they map to other variables.
                    temp = TypedExpr::Variable(other_name.clone(), annot.clone());
                    optimise_constant_propagation(&mut temp, variables);
                }
                to_swap = true;
            }
        }
        TypedExpr::Assign(_name, expr, _) => {
            optimise_constant_propagation(expr, variables);
        }
        TypedExpr::Lambda(_args, expr, _) => optimise_constant_propagation(expr, variables),
        TypedExpr::Literal(_, _) => {} // skip
        TypedExpr::If(cond, if_, else_, _) => {
            if let Some(pat) = else_ {
                optimise_constant_propagation(pat, variables);
            }
            optimise_constant_propagation(cond, variables);
            optimise_constant_propagation(if_, variables);
        }
        TypedExpr::Call(func, args, _) => {
            optimise_constant_propagation(func, variables);
            for arg in args.iter_mut() {
                optimise_constant_propagation(arg, variables);
            }
        }
        TypedExpr::While(cond, expr, _) => {
            optimise_constant_propagation(cond, variables);
            optimise_constant_propagation(expr, variables);
        }
        TypedExpr::BinaryOp(lhs, _, rhs, _) => {
            optimise_constant_propagation(lhs, variables);
            optimise_constant_propagation(rhs, variables);
        }
        TypedExpr::UnaryOp(_, expr, _) => optimise_constant_propagation(expr, variables),
        TypedExpr::Array(elements, _) => {
            for element in elements.iter_mut() {
                optimise_constant_propagation(element, variables);
            }
        }
        TypedExpr::Do(expressions, _) => {
            for expression in expressions.iter_mut() {
                optimise_constant_propagation(expression, variables);
            }
        }
        TypedExpr::Index(expression, index, _) => {
            optimise_constant_propagation(expression, variables);
            optimise_constant_propagation(index, variables);
        }
        TypedExpr::StructAccess(_, _, _) => {} // nothing to do
        TypedExpr::Return(expr, _) => {
            optimise_constant_propagation(expr, variables);
        }
        TypedExpr::Tuple(elements, _) => {
            for element in elements.iter_mut() {
                optimise_constant_propagation(element, variables);
            }
        }
        TypedExpr::Break | TypedExpr::Continue => {}
    }
    if to_swap {
        *ast = temp
    }
}

/// if a condition is true/false in an if-else condition the expression is changed is changed from an
/// if-else to the desired branch
pub fn optimise_const_branching_if(ast: &mut TypedExpr) {
    let mut temp: TypedExpr = TypedExpr::Literal(Literal::Int(0).into(), tconst!("int"));
    let mut to_swap = false;

    match ast {
        TypedExpr::If(cond, if_, else_, _) => match cond.as_ref() {
            TypedExpr::Literal(lit, _) => {
                if let Literal::Boolean(true) = lit.as_ref() {
                    temp = *(if_.clone());
                }
                if let Literal::Boolean(false) = lit.as_ref() {
                    if let Some(pat) = else_ {
                        temp = *(pat.clone());
                    }
                }
                to_swap = true;
            }
            _ => {
                optimise_const_branching_if(cond);
                optimise_const_branching_if(if_);
                if let Some(pat) = else_ {
                    optimise_const_branching_if(pat);
                }
            }
        },
        TypedExpr::Let(_, expr, _) => {
            // let mut exp = expr.clone();
            optimise_const_branching_if(expr);
        }
        TypedExpr::Variable(_, _) => {
            // no_op
        }
        TypedExpr::Assign(_name, expr, _) => {
            optimise_const_branching_if(expr);
        }
        TypedExpr::Lambda(_, expr, _) => optimise_const_branching_if(expr),
        TypedExpr::Literal(_, _) => {} // skip
        TypedExpr::Call(func, args, _) => {
            optimise_const_branching_if(func);
            for arg in args.iter_mut() {
                optimise_const_branching_if(arg);
            }
        }
        TypedExpr::While(cond, expr, _) => {
            optimise_const_branching_if(cond);
            optimise_const_branching_if(expr);
        }
        TypedExpr::BinaryOp(lhs, _, rhs, _) => {
            optimise_const_branching_if(lhs);
            optimise_const_branching_if(rhs);
        }
        TypedExpr::UnaryOp(_, expr, _) => optimise_const_branching_if(expr),
        TypedExpr::Array(elements, _) => {
            for element in elements.iter_mut() {
                optimise_const_branching_if(element);
            }
        }
        TypedExpr::Do(expressions, _) => {
            for expression in expressions.iter_mut() {
                optimise_const_branching_if(expression);
            }
        }
        TypedExpr::Index(expression, index, _) => {
            optimise_const_branching_if(expression);
            optimise_const_branching_if(index);
        }
        TypedExpr::StructAccess(_, _, _) => {} // nothing to do
        TypedExpr::Return(expr, _) => {
            optimise_const_branching_if(expr);
        }
        TypedExpr::Tuple(elements, _) => {
            for element in elements.iter_mut() {
                optimise_const_branching_if(element);
            }
        }
        TypedExpr::Break | TypedExpr::Continue => {}
    };
    if to_swap {
        // println!("swapped if-else");
        *ast = temp;
    }
}
