use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::targets::{
    CodeModel, FileType, InitializationConfig, RelocMode, Target, TargetMachine,
};
use inkwell::types::{BasicMetadataTypeEnum, BasicType, BasicTypeEnum, StructType};
use inkwell::values::{BasicMetadataValueEnum, BasicValue, BasicValueEnum, FunctionValue};
use inkwell::AddressSpace;
use inkwell::OptimizationLevel;
use inkwell::{FloatPredicate, IntPredicate};

use std::collections::HashMap;
use std::path::Path;
use std::process::Command;
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
    variables: HashMap<String, (BasicValueEnum<'ctx>, Option<FunctionValue<'ctx>>, Arc<Type>)>,
    file: String,
    structs: HashMap<String, (StructType<'ctx>, FunctionValue<'ctx>)>,
}

impl<'ctx> Generator<'ctx> {
    pub fn new(context: &'ctx Context, file: String) -> Self {
        let module = context.create_module(&file);
        let builder = context.create_builder();
        Self {
            context,
            module,
            builder,
            lambda_counter: 0,
            variables: HashMap::new(),
            file,
            structs: HashMap::new(),
        }
    }

    pub fn generate_program(&mut self, node: &TypedNode) -> Result<(), String> {
        self.generate_extern("println".to_string(), vec![tconst!("int")], tconst!("int"))?;
        self.generate_extern("printstr".to_string(), vec![tconst!("str")], tconst!("int"))?;

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
                        TypedNode::Extern(name, args, ret) => {
                            self.generate_extern(name.to_string(), args.to_vec(), ret.clone())?;
                        }
                        TypedNode::Struct(name, generics, fields) => {
                            let mut field_types = vec![];
                            let mut field_metadata_types = vec![];

                            for (_name, field_type) in fields {
                                field_types.push(self.type_to_llvm(field_type.clone()));
                                field_metadata_types
                                    .push(self.type_to_llvm(field_type.clone()).into());
                            }

                            let struct_type = self.context.struct_type(&field_types, false);

                            let function_type = self
                                .context
                                .ptr_type(inkwell::AddressSpace::from(0))
                                .fn_type(&field_metadata_types, false);
                            let function = self.module.add_function(
                                &("create_".to_string() + name),
                                function_type,
                                None,
                            );

                            let entry_block = self.context.append_basic_block(function, "entry");
                            self.builder.position_at_end(entry_block);

                            let struct_ptr = self
                                .builder
                                .build_alloca(struct_type, "struct_ptr")
                                .unwrap();

                            for (i, (field_name, _field_type)) in fields.iter().enumerate() {
                                let field_ptr = unsafe {
                                    self.builder
                                        .build_gep(
                                            struct_type,
                                            struct_ptr,
                                            &[
                                                self.context.i32_type().const_zero(),
                                                self.context.i32_type().const_int(i as u64, false),
                                            ],
                                            field_name,
                                        )
                                        .unwrap()
                                };

                                let field_value = function.get_nth_param(i as u32).unwrap();
                                self.builder.build_store(field_ptr, field_value).unwrap();
                            }

                            self.builder.build_return(Some(&struct_ptr)).unwrap();

                            self.structs
                                .insert(name.to_string(), (struct_type, function));
                            self.variables.insert(
                                name.to_string(),
                                (
                                    struct_ptr.into(),
                                    Some(function),
                                    Type::Struct(
                                        name.to_string(),
                                        generics.iter().map(|g| g.to_string()).collect(),
                                        fields
                                            .iter()
                                            .map(|(name, ty)| (name.to_string(), ty.clone()))
                                            .collect(),
                                    )
                                    .into(),
                                ),
                            );
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
                self.generate_function(&main_fn)?;
            }
            _ => unreachable!(),
        };

        let default_triple = TargetMachine::get_default_triple();

        Target::initialize_x86(&InitializationConfig::default());

        let opt = OptimizationLevel::Aggressive;
        let reloc = RelocMode::PIC;
        let model = CodeModel::Default;
        let binding = self.file.clone() + ".o";
        let path = Path::new(&binding);
        let target = Target::from_name("x86-64").unwrap();
        let target_machine = target
            .create_target_machine(&default_triple, "x86-64", "+avx2", opt, reloc, model)
            .unwrap();

        match self.module.verify() {
            Ok(_) => {}
            Err(e) => return Err(format!("couldn't verify the module!. {}", e)),
        };
        self.module
            .run_passes(
                "tailcallelim,mem2reg,bdce,dce,dse",
                &target_machine,
                inkwell::passes::PassBuilderOptions::create(),
            )
            .unwrap();

        self.print_ir();

        let exec_name = &(self.file.clone()[..=self.file.len() - 4].to_string() + ".out");
        println!("emitting {:?}. binding {:?}", exec_name, binding);

        assert!(target_machine
            .write_to_file(&self.module, FileType::Object, path)
            .is_ok());
        // println!("{:#?}\n====", node);
        Command::new("gcc") // todo: make this better
            .args(["-O2", &(binding), "std.cc", "-o", exec_name])
            .output()
            .expect("failed to execute process");

        Ok(())
    }

    fn generate_function(&mut self, node: &TypedNode) -> Result<FunctionValue<'ctx>, String> {
        match node {
            TypedNode::Function(name, args, expr, type_) => {
                let (arg_types, ret_type) = match type_.as_ref().clone() {
                    Type::Function(_arg_types, ret_type) => {
                        let arg_types: Vec<BasicMetadataTypeEnum<'ctx>> = args
                            .iter()
                            .map(|(_name, type_)| self.type_to_llvm(type_.clone()).into())
                            .collect();
                        (arg_types, self.type_to_llvm(ret_type))
                    }
                    _ => todo!(),
                };

                if self.module.get_function(name).is_some() {
                    Err("Function already declared.".to_string())// maybe allow redeclaration?
                } else {
                    // println!("\n");
                    let fn_type = ret_type.fn_type(arg_types.as_slice(), false);
                    let function = self.module.add_function(name, fn_type, None);

                    self.variables.insert(
                        name.to_string(),
                        (
                            function.as_global_value().as_basic_value_enum(),
                            Some(function),
                            type_.clone(),
                        ),
                    );

                    // println!("function {name} added to module");

                    let basic_block = self.context.append_basic_block(function, "entry");
                    self.builder.position_at_end(basic_block);

                    // Set up arguments
                    for (i, (arg_name, type_)) in args.iter().enumerate() {
                        let arg_value = function.get_nth_param(i as u32).unwrap();
                        let alloca = self
                            .builder
                            .build_alloca(arg_value.get_type(), arg_name)
                            .unwrap();
                        self.builder.build_store(alloca, arg_value).unwrap();
                        self.variables
                            .insert(arg_name.to_string(), (alloca.into(), None, type_.clone()));
                    }

                    let (returned_val, _) = self.generate_expression(*expr.clone(), function)?;
                    // println!("function {name} built return");

                    if match returned_val.get_type() {
                        BasicTypeEnum::ArrayType(_) => {
                            self.builder.build_aggregate_return(&[returned_val])
                        }
                        _ => self.builder.build_return(Some(&returned_val)),
                    }.is_err()
                    {
                        return Err(
                            "something went wrong during function return generation.".to_string()
                        );
                    };

                    // Verify the function
                    if !function.verify(true) {
                        self.print_ir();
                        return Err(format!(
                            "something went wrong during function `{name}` generation"
                        ));
                    }

                    Ok(function) // Return the function value
                }
            }
            _ => unreachable!(),
        }
    }

    fn generate_extern(
        &mut self,
        name: String,
        args: Vec<Arc<Type>>,
        ret: Arc<Type>,
    ) -> Result<(), String> {
        let ret_type = self.type_to_llvm(ret.clone());
        let arg_types: Vec<BasicMetadataTypeEnum<'ctx>> = args
            .iter()
            .map(|type_| self.type_to_llvm(type_.clone()).into())
            .collect();
        let fn_type = ret_type.fn_type(arg_types.as_slice(), false);
        let function = self.module.add_function(&name, fn_type, None);

        self.variables.insert(
            name.to_string(),
            (
                function.as_global_value().as_basic_value_enum(),
                Some(function),
                Type::Function(args, ret).into(),
            ),
        );
        Ok(())
    }

    fn generate_expression(
        &mut self,
        expression: TypedExpr,
        function: FunctionValue<'ctx>,
    ) -> Result<(BasicValueEnum<'ctx>, Option<FunctionValue<'ctx>>), String> {
        match expression {
            TypedExpr::Let(name, expr, type_) => {
                let (val, fn_) = self.generate_expression(*expr, function)?;
                let alloca = self
                    .builder
                    .build_alloca(self.type_to_llvm(type_.clone()), &name)
                    .unwrap();
                self.builder.build_store(alloca, val).unwrap();
                self.variables
                    .insert(name.to_string(), (alloca.into(), fn_, type_));

                Ok((alloca.as_basic_value_enum(), None))
            }
            TypedExpr::Variable(name, _type_) => match self.variables.get(&name.to_string()) {
                Some((val, fn_, ty)) => {
                    if let Type::Function(..) = ty.as_ref() { return Ok((*val, *fn_)) }
                    if let Some(_) = self.structs.get(&name.to_string()) { return Ok((*val, *fn_)) }
                    let value = self
                        .builder
                        .build_load(
                            self.type_to_llvm(ty.clone()),
                            val.into_pointer_value(),
                            &name,
                        )
                        .unwrap();
                    Ok((value, *fn_))
                }
                None => Err(format!("Undeclared variable: {}", name)),
            },
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
                    self.builder
                        .build_store(alloca.unwrap(), arg_value)
                        .unwrap();
                }

                let (returned_val, _) = self.generate_expression(*body, function)?;

                if match returned_val.get_type() {
                    BasicTypeEnum::ArrayType(_) => {
                        self.builder.build_aggregate_return(&[returned_val])
                    }
                    _ => self.builder.build_return(Some(&returned_val)),
                }.is_err()
                {
                    return Err("something went wrong during lambda generation.".to_string());
                };

                // Verify the function
                if !function.verify(true) {
                    return Err("something went wrong during lambda generation".to_string());
                }

                Ok((function.as_global_value().as_basic_value_enum(), None)) // Return the function value
            }
            TypedExpr::Literal(lit, _) => Ok((self.generate_literal(lit.as_ref())?, None)),
            TypedExpr::If(cond, if_, else_, type_) => {
                let (cond_val, _) = self.generate_expression(*cond, function)?;
                let then_bb = self.context.append_basic_block(function, "then");
                let merge_bb = self.context.append_basic_block(function, "merge");

                let else_bb = if let Some(ref _else_expr) = else_ {
                    Some(self.context.append_basic_block(function, "else"))
                } else {
                    None
                };

                let pre_if_bb = self.builder.get_insert_block().unwrap();

                // Build the correct conditional branch *first*
                if let Some(else_bb) = else_bb {
                    match self.builder.build_conditional_branch(
                        cond_val.into_int_value(),
                        then_bb,
                        else_bb,
                    ) {
                        Ok(_) => {}
                        Err(e) => {
                            return Err(format!(
                                "something went wrong during if-else codegen. {}",
                                e
                            ))
                        }
                    };
                } else {
                    match self.builder.build_conditional_branch(
                        cond_val.into_int_value(),
                        then_bb,
                        merge_bb,
                    ) {
                        Ok(_) => {}
                        Err(e) => {
                            return Err(format!(
                                "something went wrong during if-else codegen. {}",
                                e
                            ))
                        }
                    };
                };

                // Then block
                self.builder.position_at_end(then_bb);
                let (then_val, _) = if let Some(_else_bb) = else_bb {
                    self.generate_expression(*if_, function)?
                } else {
                    self.generate_expression(*if_, function)?;
                    (
                        BasicValueEnum::IntValue(self.context.i32_type().const_zero()),
                        None,
                    )
                };

                match self.builder.build_unconditional_branch(merge_bb) {
                    Ok(_) => {}
                    Err(e) => {
                        return Err(format!("something went wrong during `if` codegen. {}", e))
                    }
                };

                // Else block (if present)
                let else_val = if let Some(else_bb) = else_bb {
                    self.builder.position_at_end(else_bb);
                    let else_expr = else_.unwrap();
                    let (else_val, _) = self.generate_expression(*else_expr, function)?;
                    match self.builder.build_unconditional_branch(merge_bb) {
                        Ok(_) => {}
                        Err(e) => {
                            return Err(format!(
                                "something went wrong during `if-else` codegen. {}",
                                e
                            ))
                        }
                    };
                    else_val
                } else {
                    BasicValueEnum::IntValue(self.context.i32_type().const_zero())
                    // Default value
                };

                // Merge block (Now this is always correct)
                self.builder.position_at_end(merge_bb);

                let phi = self
                    .builder
                    .build_phi(
                        match else_bb {
                            Some(_) => self.type_to_llvm(type_),
                            None => BasicTypeEnum::IntType(self.context.i32_type()),
                        },
                        "iftmp",
                    )
                    .unwrap();

                if let Some(else_bb) = else_bb {
                    phi.add_incoming(&[(&then_val, then_bb), (&else_val, else_bb)]);
                } else {
                    phi.add_incoming(&[(&then_val, then_bb), (&else_val, pre_if_bb)]);
                    // pre_if_bb is correct here
                };

                Ok((phi.as_basic_value(), None))
            }
            TypedExpr::BinaryOp(lhs, op, rhs, _type_) => {
                let (lhs_val, _) = self.generate_expression(*(lhs.clone()), function)?;
                let (rhs_val, _) = self.generate_expression(*(rhs.clone()), function)?;

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
                        let (lhs_val, _) = self.generate_expression(*lhs, function)?;
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

                        match self
                            .builder
                            .build_conditional_branch(lhs_bool, then_block, else_block)
                        {
                            Ok(_) => {}
                            Err(e) => {
                                return Err(format!(
                                    "something went wrong during `or` codegen. {}",
                                    e
                                ))
                            }
                        };

                        self.builder.position_at_end(else_block);
                        let (rhs_bool, _) = self.generate_expression(*rhs, function)?;
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
                        let (lhs_val, _) = self.generate_expression(*lhs, function)?;
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

                        match self
                            .builder
                            .build_conditional_branch(lhs_bool, then_block, else_block)
                        {
                            Ok(_) => {}
                            Err(e) => {
                                return Err(format!(
                                    "something went wrong during `and` codegen. {}",
                                    e
                                ))
                            }
                        };

                        self.builder.position_at_end(then_block);
                        let (rhs_val, _) = self.generate_expression(*rhs, function)?;
                        let rhs_bool = self
                            .builder
                            .build_int_compare(
                                IntPredicate::NE,
                                rhs_val.into_int_value(),
                                self.context.i32_type().const_zero(),
                                "rhs_bool",
                            )
                            .unwrap();

                        match self.builder.build_unconditional_branch(merge_block) {
                            Ok(_) => {}
                            Err(e) => {
                                return Err(format!(
                                    "something went wrong during `and` codegen. {}",
                                    e
                                ))
                            }
                        };

                        self.builder.position_at_end(else_block);

                        match self.builder.build_unconditional_branch(merge_block) {
                            Ok(_) => {}
                            Err(e) => {
                                return Err(format!(
                                    "something went wrong during `and` codegen. {}",
                                    e
                                ))
                            }
                        }; // Short-circuit: if lhs is false, the result is false

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
                Ok((result, None))
            }
            TypedExpr::Call(callee, args, _type_) => {
                // Generate the function call
                // todo!()
                match self.generate_expression(*callee, function)?.1 {
                    Some(fn_) => {
                        let function_to_call = fn_;
                        let mut compiled_args: Vec<BasicValueEnum> = vec![];

                        args.iter().for_each(|arg| {
                            // print!("arg: {:?}", arg);
                            compiled_args
                                .push(self.generate_expression(*arg.clone(), function).unwrap().0)
                        });

                        let argsv: Vec<BasicMetadataValueEnum> = compiled_args
                            .iter()
                            .by_ref()
                            .map(|&val| val.into())
                            .collect();

                        match self.builder.build_call(function_to_call, &argsv, "calltmp") {
                            Ok(call_site_value) => {
                                Ok((call_site_value.try_as_basic_value().left().unwrap(), None))
                            }
                            Err(e) => Err(e.to_string()),
                        }
                    }
                    None => panic!("invalid call!"),
                }
            }
            TypedExpr::While(cond, body, _type_) => {
                let loop_start_block = self.context.append_basic_block(function, "loop_start");
                let body_block = self.context.append_basic_block(function, "loop_body");
                let loop_end_block = self.context.append_basic_block(function, "loop_end");

                match self.builder.build_unconditional_branch(loop_start_block) {
                    Ok(_) => {}
                    Err(e) => {
                        return Err(format!(
                            "something went wrong during `while` codegen. {}",
                            e
                        ))
                    }
                };
                self.builder.position_at_end(loop_start_block);

                let cond_value = self
                    .generate_expression(*cond, function)?
                    .0
                    .into_int_value();

                match self
                    .builder
                    .build_conditional_branch(cond_value, body_block, loop_end_block)
                {
                    Ok(_) => {}
                    Err(e) => {
                        return Err(format!(
                            "something went wrong during `while` codegen. {}",
                            e
                        ))
                    }
                };

                self.builder.position_at_end(body_block);
                self.generate_expression(*body, function)?;
                match self.builder.build_unconditional_branch(loop_start_block) {
                    Ok(_) => {}
                    Err(e) => {
                        return Err(format!(
                            "something went wrong during `while` codegen. {}",
                            e
                        ))
                    }
                };

                self.builder.position_at_end(loop_end_block);
                Ok((self.context.i32_type().const_zero().into(), None))
            }
            TypedExpr::UnaryOp(op, expr, type_) => {
                let (val, _) = self.generate_expression(*expr, function)?;
                match op {
                    UnaryOperator::Negate if type_.as_ref() == tconst!("int").as_ref() => Ok((
                        self.builder
                            .build_int_neg(val.into_int_value(), "neg")
                            .unwrap()
                            .into(),
                        None,
                    )),
                    UnaryOperator::Not if type_.as_ref() == tconst!("bool").as_ref() => Ok((
                        self.builder
                            .build_not(val.into_int_value(), "not")
                            .unwrap()
                            .into(),
                        None,
                    )),
                    _ => {
                        Err(format!(
                            "Unsupported unary operator or type: {:?} {:?}",
                            op, type_
                        ))
                    }
                }
            }
            TypedExpr::Array(elements, type_) => {
                let element_type = match type_.as_ref() {
                    Type::Constructor(TypeConstructor {
                        name: _,  // always equals array
                        generics, // always equals [T],
                        traits: _,
                    }) => generics[0].clone(),
                    _ => unreachable!(),
                };

                let llvm_element_type = self.type_to_llvm(element_type);
                let array_type = llvm_element_type.into_array_type();

                let ptr = self.builder.build_alloca(array_type, "tmparray").unwrap();

                elements.iter().enumerate().for_each(|(i, elem)| {
                    let (expr, _) = self.generate_expression(elem.clone(), function).unwrap();

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

                    self.builder.build_store(inner_ptr, expr).unwrap();
                });

                Ok((ptr.into(), None))
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

                expressions_.iter().for_each(|expr| {
                    exprs.push(self.generate_expression(expr.clone(), function).unwrap().0)
                });

                match exprs.last() {
                    None => Ok((self.type_to_llvm(type_).const_zero(), None)),
                    Some(v) => Ok((*v, None)),
                }
            }
            TypedExpr::Index(array, index, type_) => {
                let (array_val, _) = self.generate_expression(*array, function)?;
                let (index_val, _) = self.generate_expression(*index, function)?;

                match array_val.get_type() {
                    BasicTypeEnum::ArrayType(_) => {
                        let indices = [
                            self.context.i32_type().const_zero(),
                            index_val.into_int_value(),
                        ];
                        Ok((
                            unsafe {
                                self.builder
                                    .build_in_bounds_gep(
                                        self.type_to_llvm(type_),
                                        array_val.into_pointer_value(),
                                        &indices,
                                        "index_access",
                                    )
                                    .unwrap()
                            }
                            .into(),
                            None,
                        ))
                    }
                    _ => Err(format!(
                        "Unsupported type for indexing: {:?}",
                        array_val.get_type()
                    )),
                }
            }
            TypedExpr::StructAccess(_structref, _field, _ty) => {
                todo!()
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
                let string_ptr = self.builder.build_global_string_ptr(s, "str");
                Ok(string_ptr.unwrap().as_pointer_value().as_basic_value_enum())
            }
        }
    }

    fn type_to_llvm(&self, ty: Arc<Type>) -> BasicTypeEnum<'ctx> {
        match ty.as_ref() {
            Type::Constructor(TypeConstructor {
                name,
                generics: _,
                traits: _,
            }) => match name.as_str() {
                "int" => self.context.i32_type().into(),
                "long" => self.context.i64_type().into(),
                "float" => self.context.f32_type().into(),
                "double" => self.context.f64_type().into(),
                "str" => self.context.ptr_type(AddressSpace::from(0)).into(),
                x => panic!("found unknown identifier {}", x),
            },
            Type::Struct(_name, _, _fields) => {
                // let field_types = fields.iter().map(|(_, ty)| self.type_to_llvm(ty.clone())).collect::<Vec<_>>();
                self.context.ptr_type(AddressSpace::from(0)).into() //.struct_type(&field_types, false).into()
            }
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
