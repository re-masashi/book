use crate::interpreter::*;

#[test]
fn test_typechecker_inference() {
    let mut env = TypeEnv(HashMap::new());
    let ast = Expr::Let(
        std::borrow::Cow::Borrowed("hello"),
        None,
        Box::new(Expr::BinaryOp(
            Box::new(Expr::BinaryOp(
                Box::new(Expr::Literal(Literal::Int(1).into())),
                BinaryOperator::Add,
                Box::new(Expr::Literal(Literal::Int(2).into())),
            )),
            BinaryOperator::Mul,
            Box::new(Expr::Variable(std::borrow::Cow::Borrowed("x"))),
        )),
    );
    let _ = env.expr_to_type(&ast, &mut HashMap::new());
}

#[test]
#[should_panic]
fn test_typechecker_fail() {
    let mut env = TypeEnv(HashMap::new());
    let ast = Expr::BinaryOp(
        Box::new(Expr::Literal(Literal::Int(1).into())),
        BinaryOperator::Add,
        Box::new(Expr::Literal(Literal::Boolean(true).into())),
    );
    let _ = env.expr_to_type(&ast, &mut HashMap::new());
}

#[test]
fn test_typechecker_binop_int_add() {
    let mut env = TypeEnv(HashMap::new());
    let ast = Expr::BinaryOp(
        Box::new(Expr::Literal(Literal::Int(1).into())),
        BinaryOperator::Add,
        Box::new(Expr::Literal(Literal::Int(31).into())),
    );
    let _ = env.expr_to_type(&ast, &mut HashMap::new());
}

#[test]
fn test_typechecker_binop_int_sub() {
    let mut env = TypeEnv(HashMap::new());
    let ast = Expr::BinaryOp(
        Box::new(Expr::Literal(Literal::Int(121).into())),
        BinaryOperator::Sub,
        Box::new(Expr::Literal(Literal::Int(21).into())),
    );
    let _ = env.expr_to_type(&ast, &mut HashMap::new());
}

#[test]
fn test_typechecker_binop_int_mul() {
    let mut env = TypeEnv(HashMap::new());
    let ast = Expr::BinaryOp(
        Box::new(Expr::Literal(Literal::Int(100).into())),
        BinaryOperator::Mul,
        Box::new(Expr::Literal(Literal::Int(-1).into())),
    );
    let _ = env.expr_to_type(&ast, &mut HashMap::new());
}

#[test]
fn test_typechecker_binop_int_div() {
    let mut env = TypeEnv(HashMap::new());
    let ast = Expr::BinaryOp(
        Box::new(Expr::Literal(Literal::Int(333).into())),
        BinaryOperator::Div,
        Box::new(Expr::Literal(Literal::Int(37).into())),
    );
    let _ = env.expr_to_type(&ast, &mut HashMap::new());
}

#[test]
fn test_typechecker_binop_float_add() {
    let mut env = TypeEnv(HashMap::new());
    let ast = Expr::BinaryOp(
        Box::new(Expr::Literal(Literal::Float(1.0).into())),
        BinaryOperator::Add,
        Box::new(Expr::Literal(Literal::Float(31.0).into())),
    );
    let _ = env.expr_to_type(&ast, &mut HashMap::new());
}

#[test]
fn test_typechecker_binop_float_sub() {
    let mut env = TypeEnv(HashMap::new());
    let ast = Expr::BinaryOp(
        Box::new(Expr::Literal(Literal::Float(121.0).into())),
        BinaryOperator::Sub,
        Box::new(Expr::Literal(Literal::Float(21.0).into())),
    );
    let _ = env.expr_to_type(&ast, &mut HashMap::new());
}

#[test]
fn test_typechecker_binop_float_mul() {
    let mut env = TypeEnv(HashMap::new());
    let ast = Expr::BinaryOp(
        Box::new(Expr::Literal(Literal::Float(100.0).into())),
        BinaryOperator::Mul,
        Box::new(Expr::Literal(Literal::Float(-1.0).into())),
    );
    let _ = env.expr_to_type(&ast, &mut HashMap::new());
}

#[test]
fn test_typechecker_binop_float_div() {
    let mut env = TypeEnv(HashMap::new());
    let ast = Expr::BinaryOp(
        Box::new(Expr::Literal(Literal::Float(333.0).into())),
        BinaryOperator::Div,
        Box::new(Expr::Literal(Literal::Float(37.0).into())),
    );
    let _ = env.expr_to_type(&ast, &mut HashMap::new());
}

#[test]
fn test_typechecker_let() {
    let mut env = TypeEnv(HashMap::new());
    let ast = Expr::Let(
        std::borrow::Cow::Borrowed("myvar"),
        None,
        Box::new(Expr::Literal(Literal::Int(1).into())),
    );
    let _ = env.expr_to_type(&ast, &mut HashMap::new());
}

#[test]
#[should_panic]
fn test_typechecker_let_contradicting_types() {
    // contradicting types
    let mut env = TypeEnv(HashMap::new());
    let ast = Expr::Let(
        std::borrow::Cow::Borrowed("myvar"),
        Some(TypeAnnot {
            name: "int".to_string(),
            generics: vec![],
            traits: vec![],
        }),
        Box::new(Expr::BinaryOp(
            Box::new(Expr::Literal(Literal::Int(1).into())),
            BinaryOperator::Equal,
            Box::new(Expr::Literal(Literal::Int(3).into())),
        )),
    );
    let _ = env.expr_to_type(&ast, &mut HashMap::new());
}

#[test]
fn test_typechecker_variable() {
    let mut env = TypeEnv(HashMap::new());
    let ast = Expr::Variable(std::borrow::Cow::Borrowed("myvar"));
    let _ = env.expr_to_type(&ast, &mut HashMap::new());
}

#[test]
fn test_typechecker_lambda() {
    let mut env = TypeEnv(HashMap::new());
    let ast = Expr::Lambda(
        vec![(std::borrow::Cow::Borrowed("x"), None)],
        Box::new(Expr::BinaryOp(
            Box::new(Expr::Variable(std::borrow::Cow::Borrowed("x"))),
            BinaryOperator::Add,
            Box::new(Expr::Literal(Literal::Int(3).into())),
        )),
    );
    let _ = env.expr_to_type(&ast, &mut HashMap::new());
}

#[test]
fn test_typechecker_literal_int() {
    let mut env = TypeEnv(HashMap::new());
    let ast = Expr::Literal(Literal::Int(42).into());
    let _ = env.expr_to_type(&ast, &mut HashMap::new());
}

#[test]
fn test_typechecker_literal_float() {
    let mut env = TypeEnv(HashMap::new());
    let ast = Expr::Literal(Literal::Float(42.0).into());
    let _ = env.expr_to_type(&ast, &mut HashMap::new());
}

#[test]
fn test_typechecker_literal_boolean() {
    let mut env = TypeEnv(HashMap::new());
    let ast = Expr::Literal(Literal::Boolean(false).into());
    let _ = env.expr_to_type(&ast, &mut HashMap::new());
}

#[test]
fn test_typechecker_literal_string() {
    let mut env = TypeEnv(HashMap::new());
    let ast = Expr::Literal(Literal::String("hewwo".to_string().into()).into());
    let _ = env.expr_to_type(&ast, &mut HashMap::new());
}

#[test]
fn test_typechecker_array() {
    let mut env = TypeEnv(HashMap::new());
    let ast = Expr::Array(
        // Literal::Array(
        vec![
            Expr::Literal(Literal::Int(0).into()),
            Expr::Literal(Literal::Int(1).into()),
        ], // ).into()
    );
    let _ = env.expr_to_type(&ast, &mut HashMap::new());
}

#[test]
#[should_panic]
fn test_typechecker_array_fail() {
    let mut env = TypeEnv(HashMap::new());
    let ast = Expr::Array(
        // Literal::Array(
        vec![
            Expr::Literal(Literal::Int(0).into()),
            Expr::Literal(Literal::Int(1).into()),
            Expr::Literal(Literal::Float(2.).into()),
        ], // ).into()
    );
    let _ = env.expr_to_type(&ast, &mut HashMap::new());
}

// todo: call, while, unop, array

#[test]
fn test_typechecker_if() {
    let mut env = TypeEnv(HashMap::new());
    let ast = Expr::If(
        Box::new(Expr::Literal(Literal::Boolean(false).into())),
        Box::new(Expr::Literal(Literal::Int(1).into())),
        Some(Box::new(Expr::Literal(Literal::Int(0).into()))),
    );
    let _ = env.expr_to_type(&ast, &mut HashMap::new());
}

#[test]
#[should_panic]
fn test_typechecker_if_mismatch() {
    let mut env = TypeEnv(HashMap::new());
    let ast = Expr::If(
        Box::new(Expr::Literal(Literal::Boolean(false).into())),
        Box::new(Expr::Literal(Literal::Int(1).into())),
        Some(Box::new(Expr::Literal(
            Literal::String(Cow::Borrowed("ewwow")).into(),
        ))),
    );
    let _ = env.expr_to_type(&ast, &mut HashMap::new());
}
