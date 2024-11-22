//doesnt compile but it's here cuz im abt to take a huge turn
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::types::{BasicMetadataTypeEnum, BasicType, BasicTypeEnum};
use inkwell::values::{
    AnyValueEnum, BasicMetadataValueEnum, BasicValue, BasicValueEnum, FunctionValue, PointerValue
};
use inkwell::basic_block::BasicBlock;
use inkwell::AddressSpace;
use inkwell::{FloatPredicate, IntPredicate};

use std::collections::HashMap;
use std::sync::Arc;

use crate::interpreter::{
    BinaryOperator, Literal, Type, TypeConstructor, TypedExpr, TypedNode, UnaryOperator,
};
use crate::tconst;

pub struct Generator<'ctx> {
    context: &'ctx Context,
    module: Module<'ctx>,
    builder: Builder<'ctx>,
    lambda_counter: i32,
    variables: HashMap<String, FunctionValue<'ctx>>
}

impl<'ctx> Generator<'ctx> {
    pub fn new(context: &'ctx Context, module_name: &str) -> Self {
        let module = context.create_module(module_name);
        let builder = context.create_builder();
        Self {
            context,
            module,
            builder,
            lambda_counter: 0,
            variables: HashMap::new()
        }
    }

    pub fn generate_program(&mut self, node: &TypedNode) -> Result<(), String> {
        match node {
            TypedNode::Program(nodes) => {
                let mut exprs = vec![];
                for node in nodes {
                    match node {
                        TypedNode::Function(..) => {
                            self.generate_function(node)?;
                        }
                        TypedNode::Expr(e, _) => {
                            exprs.push(*e.clone());
                        }
                        _ => unreachable!(),
                    }
                }
                exprs.push(TypedExpr::Literal(Literal::Int(0).into(), tconst!("int")));
                let main_fn = TypedNode::Function(
                    std::borrow::Cow::Borrowed("main"),
                    vec![],
                    Box::new(TypedExpr::Do(exprs, tconst!("int"))),
                    Type::Function(vec![], tconst!("int")).into(),
                );
                self.generate_function(&main_fn);
            }
            _ => unreachable!(),
        };
        self.print_ir();
        // println!("{:#?}\n====", node);
        Ok(())
    }

    fn generate_function(&mut self, node: &TypedNode) -> Result<FunctionValue<'ctx>, String> {
        match node {
            TypedNode::Function(name, args, expr, type_) => {
                let (arg_types, ret_type) = match type_.as_ref().clone() {
                    Type::Function(arg_types, ret_type) => {
                        let arg_types: Vec<BasicMetadataTypeEnum<'ctx>> = args
                            .iter()
                            .map(|(_name, type_)| self.type_to_llvm(type_.clone()).into())
                            .collect();
                        (arg_types, self.type_to_llvm(ret_type))
                    }
                    _ => todo!(),
                };

                if self.module.get_function(name).is_some() {
                    return Err("Function already declared.".to_string()); // maybe allow redeclaration?
                } else {
                    // println!("\n");
                    let fn_type = ret_type.fn_type(arg_types.as_slice(), false);
                    let function = self.module.add_function(&name, fn_type, None);
                    
                    self.variables.insert(
                        name.to_string(),
                        function
                    );

                    // println!("function {name} added to module");

                    let basic_block = self.context.append_basic_block(function, "entry");
                    self.builder.position_at_end(basic_block);

                    // Set up arguments
                    for (i, (arg_name, type_)) in args.iter().enumerate() {
                        let arg_value = function.get_nth_param(i as u32).unwrap();
                        let alloca = self.builder.build_alloca(arg_value.get_type(), arg_name).unwrap();
                        self.builder.build_store(alloca, arg_value);
                        // self.variables
                        //     .insert(arg_name.to_string(), argfn);
                    }

                    // println!("args generated for function {name}");

                    // println!("exp {:#?}", *expr.clone());

                    // println!("function {name} started building expressions");
                    let returned_val = self.generate_expression(*expr.clone(), function)?;
                    // println!("function {name} built return");

                    match returned_val.get_type() {
                        BasicTypeEnum::ArrayType(_) => {
                            self.builder.build_aggregate_return(&[returned_val])
                        }
                        _ => self.builder.build_return(Some(&returned_val)),
                    };

                    // Verify the function
                    if !function.verify(true) {
                        return Err("something went wrong during function generation".to_string());
                    }

                    Ok(function) // Return the function value
                }
            }
            _ => unreachable!(),
        }
    }

    fn generate_expression(
        &mut self,
        expression: TypedExpr,
        function: FunctionValue<'ctx>,
    ) -> Result<BasicValueEnum<'ctx>, String> {
        match expression {
            TypedExpr::Let(name, expr, type_) => {
                let function_name = format!("$lambda_var_{name}_{}", self.lambda_counter);
                self.lambda_counter += 1;

                let arg_types: Vec<BasicMetadataTypeEnum<'ctx>> = vec![]; 
                
                let fn_type = self.type_to_llvm(type_).fn_type(&arg_types, false);
                let function = self.module.add_function(&function_name, fn_type, None);

                let basic_block = self.context.append_basic_block(function, "entry");
                self.builder.position_at_end(basic_block);

                let returned_val = self.generate_expression(*expr, function)?;

                match returned_val.get_type() {
                    BasicTypeEnum::ArrayType(_) => {
                        self.builder.build_aggregate_return(&[returned_val])
                    }
                    _ => self.builder.build_return(Some(&returned_val)),
                };

                // Verify the function
                if !function.verify(true) {
                    return Err("something went wrong during lambda generation".to_string());
                }

                self.variables
                    .insert(
                        name.to_string(), 
                        function
                    );

                Ok(function.as_global_value().as_basic_value_enum()) // Return the function value
            }
            TypedExpr::Variable(name, type_) => {
                match self.variables.get(&name.to_string()) {
                    Some(val) => {
                        todo!()
                        // // Make sure *ty and the expected type_ match!
                        // if *ty != self.type_to_llvm(type_) {
                        //     return Err(format!("Type mismatch for variable {}", name));
                        // }

                        // let value = self.builder.build_load(*ty, *val, &name).unwrap();
                        // Ok(value.into())
                    }
                    None => {
                        return Err(format!("Undeclared variable: {}", name));
                    }
                }
            }
            TypedExpr::Lambda(args, body, return_type_) => {
                let function_name = format!("$lambda_{}", self.lambda_counter);
                self.lambda_counter += 1;

                let arg_types: Vec<BasicMetadataTypeEnum<'ctx>> = args
                    .iter()
                    .map(|(_name, type_)| self.type_to_llvm(type_.clone()).into())
                    .collect();
                let fn_type = self.type_to_llvm(return_type_).fn_type(&arg_types, false);
                let function = self.module.add_function(&function_name, fn_type, None);

                let basic_block = self.context.append_basic_block(function, "entry");
                self.builder.position_at_end(basic_block);

                // Set up arguments
                for (i, (arg_name, _)) in args.iter().enumerate() {
                    let arg_value = function.get_nth_param(i as u32).unwrap();
                    let alloca = self.builder.build_alloca(arg_value.get_type(), arg_name);
                    self.builder.build_store(alloca.unwrap(), arg_value);
                }

                let returned_val = self.generate_expression(*body, function)?;

                match returned_val.get_type() {
                    BasicTypeEnum::ArrayType(_) => {
                        self.builder.build_aggregate_return(&[returned_val])
                    }
                    _ => self.builder.build_return(Some(&returned_val)),
                };

                // Verify the function
                if !function.verify(true) {
                    return Err("something went wrong during lambda generation".to_string());
                }

                Ok(function.as_global_value().as_basic_value_enum()) // Return the function value
            }
            TypedExpr::Literal(lit, _) => self.generate_literal(&lit.as_ref()),
            TypedExpr::If(cond, if_, else_, type_) => {
                let cond_val = self.generate_expression(*cond, function)?;
                let then_bb = self.context.append_basic_block(function, "then");
                let merge_bb = self.context.append_basic_block(function, "merge");

                let else_bb = if let Some(ref else_expr) = else_ {
                    Some(self.context.append_basic_block(function, "else"))
                } else {
                    None
                };

                let pre_if_bb = self.builder.get_insert_block().unwrap();

                // Build the correct conditional branch *first*
                if let Some(else_bb) = else_bb {
                    self.builder.build_conditional_branch(
                        cond_val.into_int_value(),
                        then_bb,
                        else_bb,
                    );
                } else {
                    self.builder.build_conditional_branch(
                        cond_val.into_int_value(),
                        then_bb,
                        merge_bb,
                    );
                };

                // Then block
                self.builder.position_at_end(then_bb);
                let then_val = self.generate_expression(*if_, function)?;
                self.builder.build_unconditional_branch(merge_bb);

                // Else block (if present)
                let else_val = if let Some(else_bb) = else_bb {
                    self.builder.position_at_end(else_bb);
                    let else_expr = else_.unwrap(); //  <--- Add this line
                    let else_val = self.generate_expression(*else_expr, function)?;
                    self.builder.build_unconditional_branch(merge_bb);
                    else_val
                } else {
                    self.type_to_llvm(type_.clone()).const_zero() // Default value
                };

                // Merge block (Now this is always correct)
                self.builder.position_at_end(merge_bb);

                let phi = self
                    .builder
                    .build_phi(self.type_to_llvm(type_), "iftmp")
                    .unwrap();

                if let Some(else_bb) = else_bb {
                    phi.add_incoming(&[(&then_val, then_bb), (&else_val, else_bb)]);
                } else {
                    phi.add_incoming(&[(&then_val, then_bb), (&else_val, pre_if_bb)]);
                    // pre_if_bb is correct here
                };

                Ok(phi.as_basic_value().into())
            }
            TypedExpr::BinaryOp(lhs, op, rhs, type_) => {
                // ... (Get lhs_val and rhs_val)
                let lhs_val = self.generate_expression(*(lhs.clone()), function)?;
                let rhs_val = self.generate_expression(*(rhs.clone()), function)?;

                // Type checking (more sophisticated needed for mixed types)
                let lhs_type = lhs_val.get_type();
                let rhs_type = rhs_val.get_type();

                let result = match (op, lhs_type, rhs_type) {
                    (BinaryOperator::Add, BasicTypeEnum::IntType(_), BasicTypeEnum::IntType(_)) => {
                        let lhs_val = lhs_val.into_int_value();
                        let rhs_val = rhs_val.into_int_value();
                        self.builder
                            .build_int_add(lhs_val, rhs_val, "addtmp")
                            .unwrap()
                            .into()
                    }
                    (
                        BinaryOperator::Add,
                        BasicTypeEnum::FloatType(_),
                        BasicTypeEnum::FloatType(_),
                    ) => {
                        let lhs_val = lhs_val.into_float_value();
                        let rhs_val = rhs_val.into_float_value();
                        self.builder
                            .build_float_add(lhs_val, rhs_val, "faddtmp")
                            .unwrap()
                            .into()
                    }

                    (BinaryOperator::Sub, BasicTypeEnum::IntType(_), BasicTypeEnum::IntType(_)) => {
                        let lhs_val = lhs_val.into_int_value();
                        let rhs_val = rhs_val.into_int_value();
                        self.builder
                            .build_int_sub(lhs_val, rhs_val, "subtmp")
                            .unwrap()
                            .into()
                    }
                    (
                        BinaryOperator::Sub,
                        BasicTypeEnum::FloatType(_),
                        BasicTypeEnum::FloatType(_),
                    ) => {
                        let lhs_val = lhs_val.into_float_value();
                        let rhs_val = rhs_val.into_float_value();
                        self.builder
                            .build_float_sub(lhs_val, rhs_val, "fsubtmp")
                            .unwrap()
                            .into()
                    }

                    (BinaryOperator::Mul, BasicTypeEnum::IntType(_), BasicTypeEnum::IntType(_)) => {
                        let lhs_val = lhs_val.into_int_value();
                        let rhs_val = rhs_val.into_int_value();
                        self.builder
                            .build_int_mul(lhs_val, rhs_val, "multmp")
                            .unwrap()
                            .into()
                    }
                    (
                        BinaryOperator::Mul,
                        BasicTypeEnum::FloatType(_),
                        BasicTypeEnum::FloatType(_),
                    ) => {
                        let lhs_val = lhs_val.into_float_value();
                        let rhs_val = rhs_val.into_float_value();
                        self.builder
                            .build_float_mul(lhs_val, rhs_val, "fmultmp")
                            .unwrap()
                            .into()
                    }

                    (BinaryOperator::Div, BasicTypeEnum::IntType(_), BasicTypeEnum::IntType(_)) => {
                        let lhs_val = lhs_val.into_int_value();
                        let rhs_val = rhs_val.into_int_value();
                        self.builder
                            .build_int_signed_div(lhs_val, rhs_val, "divtmp")
                            .unwrap()
                            .into()
                    }
                    (
                        BinaryOperator::Div,
                        BasicTypeEnum::FloatType(_),
                        BasicTypeEnum::FloatType(_),
                    ) => {
                        let lhs_val = lhs_val.into_float_value();
                        let rhs_val = rhs_val.into_float_value();
                        self.builder
                            .build_float_div(lhs_val, rhs_val, "fdivtmp")
                            .unwrap()
                            .into()
                    }

                    (BinaryOperator::Or, _, _) => {
                        let lhs_val = self.generate_expression(*lhs, function)?;
                        let lhs_bool = self
                            .builder
                            .build_int_compare(
                                IntPredicate::NE,
                                lhs_val.into_int_value(),
                                self.context.i32_type().const_zero(),
                                "lhs_bool",
                            )
                            .unwrap();

                        let then_block = self.context.append_basic_block(function, "or_then");
                        let else_block = self.context.append_basic_block(function, "or_else");
                        let merge_block = self.context.append_basic_block(function, "or_merge");

                        self.builder
                            .build_conditional_branch(lhs_bool, then_block, else_block);

                        self.builder.position_at_end(else_block);
                        let rhs_bool = self.generate_expression(*rhs, function)?;
                        let rhs_bool = self
                            .builder
                            .build_int_compare(
                                IntPredicate::NE,
                                rhs_bool.into_int_value(),
                                self.context.i32_type().const_zero(),
                                "rhs_bool",
                            )
                            .unwrap(); // Cast to boolean if necessary.

                        self.builder.position_at_end(merge_block);

                        let phi = self
                            .builder
                            .build_phi(self.context.bool_type(), "or_result")
                            .unwrap();
                        phi.add_incoming(&[
                            (&(lhs_bool.as_basic_value_enum()), then_block),
                            (&(rhs_bool.as_basic_value_enum()), else_block),
                        ]);
                        phi.as_basic_value()
                    }

                    (BinaryOperator::And, _, _) => {
                        let lhs_val = self.generate_expression(*lhs, function)?;
                        let lhs_bool = self
                            .builder
                            .build_int_compare(
                                IntPredicate::NE,
                                lhs_val.into_int_value(), // Assuming i32 or i64; adjust as needed
                                self.context.i32_type().const_zero(), // Use the correct integer type's zero constant
                                "lhs_bool",
                            )
                            .unwrap();

                        let then_block = self.context.append_basic_block(function, "and_then");
                        let else_block = self.context.append_basic_block(function, "and_else");
                        let merge_block = self.context.append_basic_block(function, "and_merge");

                        self.builder
                            .build_conditional_branch(lhs_bool, then_block, else_block);

                        self.builder.position_at_end(then_block);
                        let rhs_val = self.generate_expression(*rhs, function)?;
                        let rhs_bool = self
                            .builder
                            .build_int_compare(
                                IntPredicate::NE,
                                rhs_val.into_int_value(),
                                self.context.i32_type().const_zero(),
                                "rhs_bool",
                            )
                            .unwrap(); // Cast to boolean if necessary
                        self.builder.build_unconditional_branch(merge_block);

                        self.builder.position_at_end(else_block);
                        self.builder.build_unconditional_branch(merge_block); // Short-circuit: if lhs is false, the result is false

                        self.builder.position_at_end(merge_block);
                        let phi = self
                            .builder
                            .build_phi(self.context.bool_type(), "and_result")
                            .unwrap();
                        phi.add_incoming(&[
                            (&(rhs_bool.as_basic_value_enum()), then_block), // rhs_val if lhs is true
                            (&(lhs_bool.as_basic_value_enum()), else_block), // false if lhs is false
                        ]);
                        phi.as_basic_value()
                    }

                    (
                        BinaryOperator::Equal,
                        BasicTypeEnum::IntType(_),
                        BasicTypeEnum::IntType(_),
                    ) => self
                        .builder
                        .build_int_compare(
                            IntPredicate::EQ,
                            lhs_val.into_int_value(),
                            rhs_val.into_int_value(),
                            "eqtmp",
                        )
                        .unwrap()
                        .into(),
                    (
                        BinaryOperator::Equal,
                        BasicTypeEnum::FloatType(_),
                        BasicTypeEnum::FloatType(_),
                    ) => self
                        .builder
                        .build_float_compare(
                            FloatPredicate::OEQ,
                            lhs_val.into_float_value(),
                            rhs_val.into_float_value(),
                            "feqtmp",
                        )
                        .unwrap()
                        .into(),

                    (
                        BinaryOperator::LessEqual,
                        BasicTypeEnum::IntType(_),
                        BasicTypeEnum::IntType(_),
                    ) => self
                        .builder
                        .build_int_compare(
                            IntPredicate::SLE,
                            lhs_val.into_int_value(),
                            rhs_val.into_int_value(),
                            "eqtmp",
                        )
                        .unwrap()
                        .into(),
                    (
                        BinaryOperator::LessEqual,
                        BasicTypeEnum::FloatType(_),
                        BasicTypeEnum::FloatType(_),
                    ) => self
                        .builder
                        .build_float_compare(
                            FloatPredicate::OLE,
                            lhs_val.into_float_value(),
                            rhs_val.into_float_value(),
                            "feqtmp",
                        )
                        .unwrap()
                        .into(),

                    (
                        BinaryOperator::GreaterEqual,
                        BasicTypeEnum::IntType(_),
                        BasicTypeEnum::IntType(_),
                    ) => self
                        .builder
                        .build_int_compare(
                            IntPredicate::SGE,
                            lhs_val.into_int_value(),
                            rhs_val.into_int_value(),
                            "eqtmp",
                        )
                        .unwrap()
                        .into(),
                    (
                        BinaryOperator::GreaterEqual,
                        BasicTypeEnum::FloatType(_),
                        BasicTypeEnum::FloatType(_),
                    ) => self
                        .builder
                        .build_float_compare(
                            FloatPredicate::OGE,
                            lhs_val.into_float_value(),
                            rhs_val.into_float_value(),
                            "feqtmp",
                        )
                        .unwrap()
                        .into(),

                    (
                        BinaryOperator::Less,
                        BasicTypeEnum::IntType(_),
                        BasicTypeEnum::IntType(_),
                    ) => self
                        .builder
                        .build_int_compare(
                            IntPredicate::SLT,
                            lhs_val.into_int_value(),
                            rhs_val.into_int_value(),
                            "eqtmp",
                        )
                        .unwrap()
                        .into(),
                    (
                        BinaryOperator::Less,
                        BasicTypeEnum::FloatType(_),
                        BasicTypeEnum::FloatType(_),
                    ) => self
                        .builder
                        .build_float_compare(
                            FloatPredicate::OLT,
                            lhs_val.into_float_value(),
                            rhs_val.into_float_value(),
                            "feqtmp",
                        )
                        .unwrap()
                        .into(),

                    (
                        BinaryOperator::Greater,
                        BasicTypeEnum::IntType(_),
                        BasicTypeEnum::IntType(_),
                    ) => self
                        .builder
                        .build_int_compare(
                            IntPredicate::SGT,
                            lhs_val.into_int_value(),
                            rhs_val.into_int_value(),
                            "eqtmp",
                        )
                        .unwrap()
                        .into(),
                    (
                        BinaryOperator::Greater,
                        BasicTypeEnum::FloatType(_),
                        BasicTypeEnum::FloatType(_),
                    ) => self
                        .builder
                        .build_float_compare(
                            FloatPredicate::OGT,
                            lhs_val.into_float_value(),
                            rhs_val.into_float_value(),
                            "feqtmp",
                        )
                        .unwrap()
                        .into(),

                    _ => {
                        return Err(format!(
                            "Unsupported operator or type combination: {:?} {:?} {:?}",
                            op, lhs_type, rhs_type
                        ))
                    }
                };
                Ok(result)
            }
            TypedExpr::Call(callee, args, _type_) => {
                let callee_val = self.generate_expression(*callee, function)?;
                let function_to_call= callee_val.into_pointer_value();
                let mut compiled_args: Vec<BasicValueEnum> = vec![];
                println!("{:?}", callee_val);

                args.iter().for_each(|arg| {
                    compiled_args.push(self.generate_expression(*arg.clone(), function).unwrap())
                });

                let argsv: Vec<BasicMetadataValueEnum> =
                    compiled_args.iter().map(|&val| val.into()).collect();

                match self.builder.build_call(
                    function_to_call, 
                    &argsv, 
                    "calltmp"
                ) {
                    Ok(call_site_value) => Ok(call_site_value.try_as_basic_value().left().unwrap()),
                    Err(e) => Err(e.to_string()),
                }
            }
            TypedExpr::While(cond, body, _type_) => {
                let loop_start_block = self.context.append_basic_block(function, "loop_start");
                let body_block = self.context.append_basic_block(function, "loop_body");
                let loop_end_block = self.context.append_basic_block(function, "loop_end");

                self.builder.build_unconditional_branch(loop_start_block);
                self.builder.position_at_end(loop_start_block);

                let cond_value = self.generate_expression(*cond, function)?.into_int_value();

                self.builder.build_conditional_branch(cond_value, body_block, loop_end_block);

                self.builder.position_at_end(body_block);
                self.generate_expression(*body, function)?;
                self.builder.build_unconditional_branch(loop_start_block);

                self.builder.position_at_end(loop_end_block);
                Ok(self.context.i32_type().const_zero().into())
            }
            TypedExpr::UnaryOp(op, expr, type_) => {
                let val = self.generate_expression(*expr, function)?;
                match op {
                    UnaryOperator::Negate if type_.as_ref() == tconst!("int").as_ref() => Ok(self
                        .builder
                        .build_int_neg(val.into_int_value(), "neg")
                        .unwrap()
                        .into()),
                    UnaryOperator::Not if type_.as_ref() == tconst!("bool").as_ref() => Ok(self
                        .builder
                        .build_not(val.into_int_value(), "not")
                        .unwrap()
                        .into()),
                    _ => {
                        return Err(format!(
                            "Unsupported unary operator or type: {:?} {:?}",
                            op, type_
                        ))
                    }
                }
            }
            TypedExpr::Array(elements, type_) => {
                let element_type = match type_.as_ref() {
                    Type::Constructor(TypeConstructor {
                        name,     // always equals array
                        generics, // always equals [T],
                        traits,
                    }) => generics[0].clone(),
                    _ => unreachable!(),
                };

                let llvm_element_type = self.type_to_llvm(element_type);
                let array_type = llvm_element_type.into_array_type();

                let ptr = self.builder.build_alloca(array_type, "tmparray").unwrap();

                elements.iter().enumerate().for_each(|(i, elem)| {
                    let expr = self.generate_expression(elem.clone(), function).unwrap();

                    let const_i = self.context.i64_type().const_int(i as u64, false);
                    let const_0 = self.context.i64_type().const_zero();

                    let inner_ptr = unsafe {
                        self.builder
                            .build_gep(
                                llvm_element_type,
                                ptr,
                                &[const_0, const_i],
                                format!("elem_{}", i).as_str(),
                            )
                            .unwrap()
                    };

                    self.builder.build_store(inner_ptr, expr);
                });

                Ok(ptr.into())
                // let global_array = self.module.add_global(array_type, Some(AddressSpace::Const), "array_global").unwrap();
                // global_array.set_initializer(&array_value);
                // Ok(global_array.as_pointer_value().as_basic_value_enum())
            }
            TypedExpr::Do(expressions, type_) => {
                // Generate code for each expression sequentially
                let mut exprs = vec![];
                let mut expressions_ =
                    vec![TypedExpr::Literal(Literal::Int(0).into(), tconst!("int"))];
                expressions_.append(&mut expressions.clone());

                expressions_
                    .iter()
                    .for_each(|expr| exprs.push(self.generate_expression(expr.clone(), function)));

                match exprs.last() {
                    None => Ok(self.context.i32_type().const_zero().into()),
                    Some(v) => v.clone(),
                }
            }
            // Handle array indexing
            TypedExpr::Index(array, index, type_) => {
                let array_val = self.generate_expression(*array, function)?;
                let index_val = self.generate_expression(*index, function)?;

                match array_val.get_type() {
                    BasicTypeEnum::ArrayType(_) => {
                        let indices = [
                            self.context.i32_type().const_zero(),
                            index_val.into_int_value(),
                        ];
                        Ok(unsafe {
                            self.builder.build_in_bounds_gep(
                                self.type_to_llvm(type_),
                                array_val.into_pointer_value(),
                                &indices,
                                "index_access",
                            ).unwrap()
                        }.into())
                    }
                    _ => Err(format!(
                        "Unsupported type for indexing: {:?}",
                        array_val.get_type()
                    )),
                }
            }
        }
    }

    fn generate_literal(&mut self, literal: &Literal) -> Result<BasicValueEnum<'ctx>, String> {
        match literal {
            Literal::Int(i) => Ok(self.context.i32_type().const_int(*i as u64, false).into()),
            Literal::Float(f) => Ok(self.context.f32_type().const_float(*f).into()),
            Literal::Boolean(b) => Ok(self
                .context
                .bool_type()
                .const_int(if *b { 1 } else { 0 }, false)
                .into()),
            Literal::String(s) => {
                let string_ptr = unsafe { self.builder.build_global_string_ptr(&s, "str") };
                Ok(string_ptr.unwrap().as_pointer_value().as_basic_value_enum())
            }
            _ => Err("Unsupported literal type".to_string()),
        }
    }

    fn type_to_llvm(&self, ty: Arc<Type>) -> BasicTypeEnum<'ctx> {
        match ty.as_ref() {
            Type::Constructor(TypeConstructor {
                name,
                generics,
                traits,
            }) => match name.as_str() {
                "int" => self.context.i32_type().into(),
                "long" => self.context.i64_type().into(),
                "float" => self.context.f32_type().into(),
                "double" => self.context.f64_type().into(),
                "str" => self
                    .context
                    .i8_type()
                    .ptr_type(AddressSpace::from(0))
                    .into(),
                _ => todo!(),
            },
            _ => panic!("{:?}", ty),
        }
    }

    fn get_module(&self) -> &Module<'ctx> {
        &self.module
    }

    fn print_ir(&self) {
        println!("{}", self.module.print_to_string().to_string());
    }
}
