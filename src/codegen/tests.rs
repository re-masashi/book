use crate::codegen::*;

#[test]
fn test_typechecker_inference() {
    let mut env = TypeEnv(HashMap::new(), HashMap::new());
    let ast = Expr::Let(
        std::borrow::Cow::Borrowed("hello"),
        None,
        Box::new(Expr::BinaryOp(
            Box::new(Expr::BinaryOp(
                Box::new(Expr::Literal(
                    Literal::Int(1).into(),
                    Span((0, 0), (0, 0)),
                    "".to_string(),
                )),
                BinaryOperator::Add,
                Box::new(Expr::Literal(
                    Literal::Int(2).into(),
                    Span((0, 0), (0, 0)),
                    "".to_string(),
                )),
                Span((0, 0), (0, 0)),
                "".to_string(),
            )),
            BinaryOperator::Mul,
            Box::new(Expr::Variable(
                std::borrow::Cow::Borrowed("x"),
                Span((0, 0), (0, 0)),
                "".to_string(),
            )),
            Span((0, 0), (0, 0)),
            "".to_string(),
        )),
        Span((0, 0), (0, 0)),
        "".to_string(),
    );
    let _ = env
        .expr_to_type(&ast, &Span((0, 0), (0, 0)), "", &mut HashMap::new())
        .unwrap();
}

#[test]
#[should_panic]
fn test_typechecker_fail() {
    let mut env = TypeEnv(HashMap::new(), HashMap::new());
    let ast = Expr::BinaryOp(
        Box::new(Expr::Literal(
            Literal::Int(1).into(),
            Span((0, 0), (0, 0)),
            "".to_string(),
        )),
        BinaryOperator::Add,
        Box::new(Expr::Literal(
            Literal::Boolean(true).into(),
            Span((0, 0), (0, 0)),
            "".to_string(),
        )),
        Span((0, 0), (0, 0)),
        "".to_string(),
    );
    let _ = env
        .expr_to_type(&ast, &Span((0, 0), (0, 0)), "", &mut HashMap::new())
        .unwrap();
}

#[test]
fn test_typechecker_binop_int_add() {
    let mut env = TypeEnv(HashMap::new(), HashMap::new());
    let ast = Expr::BinaryOp(
        Box::new(Expr::Literal(
            Literal::Int(1).into(),
            Span((0, 0), (0, 0)),
            "".to_string(),
        )),
        BinaryOperator::Add,
        Box::new(Expr::Literal(
            Literal::Int(31).into(),
            Span((0, 0), (0, 0)),
            "".to_string(),
        )),
        Span((0, 0), (0, 0)),
        "".to_string(),
    );
    let _ = env
        .expr_to_type(&ast, &Span((0, 0), (0, 0)), "", &mut HashMap::new())
        .unwrap();
}

#[test]
fn test_typechecker_binop_int_sub() {
    let mut env = TypeEnv(HashMap::new(), HashMap::new());
    let ast = Expr::BinaryOp(
        Box::new(Expr::Literal(
            Literal::Int(121).into(),
            Span((0, 0), (0, 0)),
            "".to_string(),
        )),
        BinaryOperator::Sub,
        Box::new(Expr::Literal(
            Literal::Int(21).into(),
            Span((0, 0), (0, 0)),
            "".to_string(),
        )),
        Span((0, 0), (0, 0)),
        "".to_string(),
    );
    let _ = env
        .expr_to_type(&ast, &Span((0, 0), (0, 0)), "", &mut HashMap::new())
        .unwrap();
}

#[test]
fn test_typechecker_binop_int_mul() {
    let mut env = TypeEnv(HashMap::new(), HashMap::new());
    let ast = Expr::BinaryOp(
        Box::new(Expr::Literal(
            Literal::Int(100).into(),
            Span((0, 0), (0, 0)),
            "".to_string(),
        )),
        BinaryOperator::Mul,
        Box::new(Expr::Literal(
            Literal::Int(-1).into(),
            Span((0, 0), (0, 0)),
            "".to_string(),
        )),
        Span((0, 0), (0, 0)),
        "".to_string(),
    );
    let _ = env
        .expr_to_type(&ast, &Span((0, 0), (0, 0)), "", &mut HashMap::new())
        .unwrap();
}

#[test]
fn test_typechecker_binop_int_div() {
    let mut env = TypeEnv(HashMap::new(), HashMap::new());
    let ast = Expr::BinaryOp(
        Box::new(Expr::Literal(
            Literal::Int(333).into(),
            Span((0, 0), (0, 0)),
            "".to_string(),
        )),
        BinaryOperator::Div,
        Box::new(Expr::Literal(
            Literal::Int(37).into(),
            Span((0, 0), (0, 0)),
            "".to_string(),
        )),
        Span((0, 0), (0, 0)),
        "".to_string(),
    );
    let _ = env
        .expr_to_type(&ast, &Span((0, 0), (0, 0)), "", &mut HashMap::new())
        .unwrap();
}

#[test]
fn test_typechecker_binop_float_add() {
    let mut env = TypeEnv(HashMap::new(), HashMap::new());
    let ast = Expr::BinaryOp(
        Box::new(Expr::Literal(
            Literal::Float(1.0).into(),
            Span((0, 0), (0, 0)),
            "".to_string(),
        )),
        BinaryOperator::Add,
        Box::new(Expr::Literal(
            Literal::Float(31.0).into(),
            Span((0, 0), (0, 0)),
            "".to_string(),
        )),
        Span((0, 0), (0, 0)),
        "".to_string(),
    );
    let _ = env
        .expr_to_type(&ast, &Span((0, 0), (0, 0)), "", &mut HashMap::new())
        .unwrap();
}

#[test]
fn test_typechecker_binop_float_sub() {
    let mut env = TypeEnv(HashMap::new(), HashMap::new());
    let ast = Expr::BinaryOp(
        Box::new(Expr::Literal(
            Literal::Float(121.0).into(),
            Span((0, 0), (0, 0)),
            "".to_string(),
        )),
        BinaryOperator::Sub,
        Box::new(Expr::Literal(
            Literal::Float(21.0).into(),
            Span((0, 0), (0, 0)),
            "".to_string(),
        )),
        Span((0, 0), (0, 0)),
        "".to_string(),
    );
    let _ = env
        .expr_to_type(&ast, &Span((0, 0), (0, 0)), "", &mut HashMap::new())
        .unwrap();
}

#[test]
fn test_typechecker_binop_float_mul() {
    let mut env = TypeEnv(HashMap::new(), HashMap::new());
    let ast = Expr::BinaryOp(
        Box::new(Expr::Literal(
            Literal::Float(100.0).into(),
            Span((0, 0), (0, 0)),
            "".to_string(),
        )),
        BinaryOperator::Mul,
        Box::new(Expr::Literal(
            Literal::Float(-1.0).into(),
            Span((0, 0), (0, 0)),
            "".to_string(),
        )),
        Span((0, 0), (0, 0)),
        "".to_string(),
    );
    let _ = env
        .expr_to_type(&ast, &Span((0, 0), (0, 0)), "", &mut HashMap::new())
        .unwrap();
}

#[test]
fn test_typechecker_binop_float_div() {
    let mut env = TypeEnv(HashMap::new(), HashMap::new());
    let ast = Expr::BinaryOp(
        Box::new(Expr::Literal(
            Literal::Float(333.0).into(),
            Span((0, 0), (0, 0)),
            "".to_string(),
        )),
        BinaryOperator::Div,
        Box::new(Expr::Literal(
            Literal::Float(37.0).into(),
            Span((0, 0), (0, 0)),
            "".to_string(),
        )),
        Span((0, 0), (0, 0)),
        "".to_string(),
    );
    let _ = env
        .expr_to_type(&ast, &Span((0, 0), (0, 0)), "", &mut HashMap::new())
        .unwrap();
}

#[test]
fn test_typechecker_let() {
    let mut env = TypeEnv(HashMap::new(), HashMap::new());
    let ast = Expr::Let(
        std::borrow::Cow::Borrowed("myvar"),
        None,
        Box::new(Expr::Literal(
            Literal::Int(1).into(),
            Span((0, 0), (0, 0)),
            "".to_string(),
        )),
        Span((0, 0), (0, 0)),
        "".to_string(),
    );
    let _ = env
        .expr_to_type(&ast, &Span((0, 0), (0, 0)), "", &mut HashMap::new())
        .unwrap();
}

#[test]
#[should_panic]
fn test_typechecker_let_contradicting_types() {
    // contradicting types
    let mut env = TypeEnv(HashMap::new(), HashMap::new());
    let ast = Expr::Let(
        std::borrow::Cow::Borrowed("myvar"),
        Some(TypeAnnot {
            name: "int".to_string(),
            generics: vec![],
            traits: vec![],
        }),
        Box::new(Expr::BinaryOp(
            Box::new(Expr::Literal(
                Literal::Int(1).into(),
                Span((0, 0), (0, 0)),
                "".to_string(),
            )),
            BinaryOperator::Equal,
            Box::new(Expr::Literal(
                Literal::Int(3).into(),
                Span((0, 0), (0, 0)),
                "".to_string(),
            )),
            Span((0, 0), (0, 0)),
            "".to_string(),
        )),
        Span((0, 0), (0, 0)),
        "".to_string(),
    );
    let _ = env
        .expr_to_type(&ast, &Span((0, 0), (0, 0)), "", &mut HashMap::new())
        .unwrap();
}

#[test]
fn test_typechecker_variable() {
    let mut env = TypeEnv(HashMap::new(), HashMap::new());
    let ast = Expr::Variable(
        std::borrow::Cow::Borrowed("myvar"),
        Span((0, 0), (0, 0)),
        "".to_string(),
    );
    let _ = env
        .expr_to_type(&ast, &Span((0, 0), (0, 0)), "", &mut HashMap::new())
        .unwrap();
}

#[test]
fn test_typechecker_lambda() {
    let mut env = TypeEnv(HashMap::new(), HashMap::new());
    let ast = Expr::Lambda(
        vec![(std::borrow::Cow::Borrowed("x"), None)],
        Box::new(Expr::BinaryOp(
            Box::new(Expr::Variable(
                std::borrow::Cow::Borrowed("x"),
                Span((0, 0), (0, 0)),
                "".to_string(),
            )),
            BinaryOperator::Add,
            Box::new(Expr::Literal(
                Literal::Int(3).into(),
                Span((0, 0), (0, 0)),
                "".to_string(),
            )),
            Span((0, 0), (0, 0)),
            "".to_string(),
        )),
        Span((0, 0), (0, 0)),
        "".to_string(),
    );
    let _ = env
        .expr_to_type(&ast, &Span((0, 0), (0, 0)), "", &mut HashMap::new())
        .unwrap();
}

#[test]
fn test_typechecker_literal_int() {
    let mut env = TypeEnv(HashMap::new(), HashMap::new());
    let ast = Expr::Literal(
        Literal::Int(42).into(),
        Span((0, 0), (0, 0)),
        "".to_string(),
    );
    let _ = env
        .expr_to_type(&ast, &Span((0, 0), (0, 0)), "", &mut HashMap::new())
        .unwrap();
}

#[test]
fn test_typechecker_literal_float() {
    let mut env = TypeEnv(HashMap::new(), HashMap::new());
    let ast = Expr::Literal(
        Literal::Float(42.0).into(),
        Span((0, 0), (0, 0)),
        "".to_string(),
    );
    let _ = env
        .expr_to_type(&ast, &Span((0, 0), (0, 0)), "", &mut HashMap::new())
        .unwrap();
}

#[test]
fn test_typechecker_literal_boolean() {
    let mut env = TypeEnv(HashMap::new(), HashMap::new());
    let ast = Expr::Literal(
        Literal::Boolean(false).into(),
        Span((0, 0), (0, 0)),
        "".to_string(),
    );
    let _ = env
        .expr_to_type(&ast, &Span((0, 0), (0, 0)), "", &mut HashMap::new())
        .unwrap();
}

#[test]
fn test_typechecker_literal_string() {
    let mut env = TypeEnv(HashMap::new(), HashMap::new());
    let ast = Expr::Literal(
        Literal::String("hewwo".to_string().into()).into(),
        Span((0, 0), (0, 0)),
        "".to_string(),
    );
    let _ = env
        .expr_to_type(&ast, &Span((0, 0), (0, 0)), "", &mut HashMap::new())
        .unwrap();
}

#[test]
fn test_typechecker_array() {
    let mut env = TypeEnv(HashMap::new(), HashMap::new());
    let ast = Expr::Array(
        // Literal::Array(
        vec![
            Expr::Literal(Literal::Int(0).into(), Span((0, 0), (0, 0)), "".to_string()),
            Expr::Literal(Literal::Int(1).into(), Span((0, 0), (0, 0)), "".to_string()),
        ], // ).into()
        Span((0, 0), (0, 0)),
        "".to_string(),
    );
    let _ = env
        .expr_to_type(&ast, &Span((0, 0), (0, 0)), "", &mut HashMap::new())
        .unwrap();
}

#[test]
#[should_panic]
fn test_typechecker_array_fail() {
    let mut env = TypeEnv(HashMap::new(), HashMap::new());
    let ast = Expr::Array(
        // Literal::Array(
        vec![
            Expr::Literal(Literal::Int(0).into(), Span((0, 0), (0, 0)), "".to_string()),
            Expr::Literal(Literal::Int(1).into(), Span((0, 0), (0, 0)), "".to_string()),
            Expr::Literal(
                Literal::Float(2.).into(),
                Span((0, 0), (0, 0)),
                "".to_string(),
            ),
        ], // ).into()
        Span((0, 0), (0, 0)),
        "".to_string(),
    );
    let _ = env
        .expr_to_type(&ast, &Span((0, 0), (0, 0)), "", &mut HashMap::new())
        .unwrap();
}

// todo: call, while, unop, array

#[test]
fn test_typechecker_if() {
    let mut env = TypeEnv(HashMap::new(), HashMap::new());
    let ast = Expr::If(
        Box::new(Expr::Literal(
            Literal::Boolean(false).into(),
            Span((0, 0), (0, 0)),
            "".to_string(),
        )),
        Box::new(Expr::Literal(
            Literal::Int(1).into(),
            Span((0, 0), (0, 0)),
            "".to_string(),
        )),
        Some(Box::new(Expr::Literal(
            Literal::Int(0).into(),
            Span((0, 0), (0, 0)),
            "".to_string(),
        ))),
        Span((0, 0), (0, 0)),
        "".to_string(),
    );
    let _ = env
        .expr_to_type(&ast, &Span((0, 0), (0, 0)), "", &mut HashMap::new())
        .unwrap();
}

#[test]
#[should_panic]
fn test_typechecker_if_mismatch() {
    let mut env = TypeEnv(HashMap::new(), HashMap::new());
    let ast = Expr::If(
        Box::new(Expr::Literal(
            Literal::Boolean(false).into(),
            Span((0, 0), (0, 0)),
            "".to_string(),
        )),
        Box::new(Expr::Literal(
            Literal::Int(1).into(),
            Span((0, 0), (0, 0)),
            "".to_string(),
        )),
        Some(Box::new(Expr::Literal(
            Literal::String(Cow::Borrowed("ewwow")).into(),
            Span((0, 0), (0, 0)),
            "".to_string(),
        ))),
        Span((0, 0), (0, 0)),
        "".to_string(),
    );
    let _ = env
        .expr_to_type(&ast, &Span((0, 0), (0, 0)), "", &mut HashMap::new())
        .unwrap();
}
