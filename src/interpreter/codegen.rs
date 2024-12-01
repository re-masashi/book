use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::targets::{
    CodeModel, FileType, InitializationConfig, RelocMode, Target, TargetMachine,
};
use inkwell::types::{BasicMetadataTypeEnum, BasicType, BasicTypeEnum, StructType};
use inkwell::values::{
    BasicMetadataValueEnum, BasicValue, BasicValueEnum, FunctionValue, StructValue,
};
use inkwell::AddressSpace;
use inkwell::OptimizationLevel;
use inkwell::{FloatPredicate, IntPredicate};

use std::collections::HashMap;
use std::path::Path;
use std::process::Command;
use std::sync::Arc;

use crate::interpreter::{
    BinaryOperator, Literal, Type, TypeConstructor, TypeEnv, TypedExpr, TypedNode, UnaryOperator,
};
use crate::{t_float, t_int, t_str, tconst};

pub struct IRGenerator<'ctx> {
    context: &'ctx Context,
    module: Module<'ctx>,
    builder: Builder<'ctx>,
    lambda_counter: i32,
    variables: HashMap<String, (IRValue<'ctx>, IRType<'ctx>)>,
    structs: HashMap<String, StructType<'ctx>>,
    pos: i32,
    line_no: i32,
    file: String,
    polymorphic_functions: HashMap<String, Vec<PolyMorphicFunction<'ctx>>>,
    builtins: Vec<IRValue<'ctx>>,
}

pub struct PolyMorphicFunction<'ctx>(
    Vec<Arc<Type>>, // args
    Arc<Type>,      // ret
    IRValue<'ctx>,
    IRType<'ctx>,
);

#[derive(Clone)]
pub enum IRType<'ctx> {
    Function(Vec<(String, IRType<'ctx>)>, Box<IRType<'ctx>>),
    Struct(StructType<'ctx>, Vec<(String, IRType<'ctx>)>),
    Simple(BasicTypeEnum<'ctx>),
    PolyMorph, // no need to store all the data
    BuiltIn,   // functions such as type().
}

impl<'ctx> IRType<'ctx> {
    fn as_meta_enum(&self, context: &'ctx Context) -> BasicMetadataTypeEnum<'ctx> {
        match &self {
            IRType::Function(..) => todo!(),
            IRType::Struct(..) => context.ptr_type(AddressSpace::from(0)).into(),
            IRType::Simple(v) => (*v).into(),
            IRType::PolyMorph => todo!(),
            IRType::BuiltIn => todo!(),
        }
    }

    fn as_basic_enum(&self, context: &'ctx Context) -> BasicTypeEnum<'ctx> {
        match &self {
            IRType::Function(..) => todo!(),
            IRType::Struct(..) => context.ptr_type(AddressSpace::from(0)).into(),
            IRType::Simple(v) => *v,
            IRType::PolyMorph => todo!(),
            IRType::BuiltIn => todo!(),
        }
    }
}

#[derive(Clone)]
pub enum IRValue<'ctx> {
    Function(
        FunctionValue<'ctx>,
        Vec<(String, IRType<'ctx>)>,
        Box<IRType<'ctx>>,
    ), // function, args, ret
    Struct(StructValue<'ctx>, Vec<(String, IRType<'ctx>)>),
    Simple(BasicValueEnum<'ctx>),
    PolyMorph(
        String,
        Box<TypedExpr<'ctx>>,
        Vec<(String, Arc<Type>)>,
        Arc<Type>,
    ),
    BuiltIn(String),
}

impl<'ctx> IRValue<'ctx> {
    fn as_meta_enum(&self, _context: &'ctx Context) -> BasicMetadataValueEnum<'ctx> {
        match &self {
            IRValue::Function(..) => todo!(),
            IRValue::Struct(..) => todo!(),
            IRValue::Simple(v) => (*v).into(),
            IRValue::PolyMorph(..) => todo!(),
            IRValue::BuiltIn(..) => todo!(),
        }
    }

    fn as_basic_enum(&self, _context: &'ctx Context) -> BasicValueEnum<'ctx> {
        match &self {
            IRValue::Function(f, ..) => f.as_global_value().as_basic_value_enum(),
            IRValue::Struct(..) => todo!(),
            IRValue::Simple(v) => *v,
            IRValue::PolyMorph(..) => todo!(),
            IRValue::BuiltIn(..) => todo!(),
        }
    }
}

impl<'ctx> IRGenerator<'ctx> {
    pub fn new(context: &'ctx Context, file: String) -> Self {
        let module = context.create_module(&file);
        let builder = context.create_builder();
        Self {
            context,
            module,
            builder,
            lambda_counter: 0,
            variables: HashMap::new(),
            structs: HashMap::new(),
            pos: 0,
            line_no: 0,
            file,
            polymorphic_functions: HashMap::new(),
            builtins: vec![
                IRValue::BuiltIn("type".to_string()),
                IRValue::BuiltIn("print".to_string()),
            ],
        }
    }

    fn print_ir(&self) {
        println!("{}", self.module.print_to_string().to_string());
    }

    fn declare_gc_functions(&self) {
        // Declare the GC_malloc function
        let malloc_type = self
            .context
            .ptr_type(AddressSpace::from(0))
            .fn_type(&[self.context.i64_type().into()], false);
        let _malloc_func = self.module.add_function("GC_malloc", malloc_type, None);

        // Declare the GC_free function
        let free_type = self
            .context
            .void_type()
            .fn_type(&[self.context.i8_type().into()], false);
        let _free_func = self.module.add_function("GC_free", free_type, None);
    }

    pub fn gen_program(&mut self, node: &TypedNode<'ctx>) -> Result<(), String> {
        self.declare_gc_functions();
        self.variables.insert(
            "type".to_string(),
            (IRValue::BuiltIn("type".to_string()), IRType::BuiltIn),
        );
        self.variables.insert(
            "print".to_string(),
            (IRValue::BuiltIn("print".to_string()), IRType::BuiltIn),
        );

        self.gen_extern("println".to_string(), vec![t_int!()], t_int!())?;
        self.gen_extern("printstr".to_string(), vec![t_str!()], t_str!())?;

        self.gen_extern("int_to_str".to_string(), vec![t_int!()], t_str!())?;
        self.gen_extern("float_to_str".to_string(), vec![t_float!()], t_str!())?;
        self.gen_extern("bool_to_str".to_string(), vec![t_int!()], t_str!())?;

        match node {
            TypedNode::Program(nodes) => {
                let mut exprs = vec![];
                for node in nodes {
                    match node {
                        TypedNode::Function(name, args, expr, type_) => {
                            self.gen_function(
                                name.to_string(),
                                args.iter()
                                    .map(|(x, t)| (x.to_string(), t.clone()))
                                    .collect(),
                                expr,
                                type_.clone(),
                            )?;
                        }
                        TypedNode::Expr(e, _) => {
                            exprs.push(*e.clone());
                        }
                        TypedNode::Extern(name, args, ret) => {
                            self.gen_extern(name.to_string(), args.to_vec(), ret.clone())?;
                        }
                        TypedNode::Struct(name, _generics, fields) => {
                            let mut arg_types = vec![];
                            let mut field_types = vec![];
                            let mut field_metadata_types = vec![];

                            let malloc_func = self
                                .module
                                .get_function("GC_malloc")
                                .expect("malloc not found");

                            for (name, field_type) in fields {
                                arg_types.push((
                                    name.to_string(),
                                    self.type_to_llvm(field_type.clone()),
                                ));
                                field_types.push(
                                    self.type_to_llvm(field_type.clone())
                                        .as_basic_enum(self.context),
                                );
                                field_metadata_types.push(
                                    self.type_to_llvm(field_type.clone())
                                        .as_meta_enum(self.context),
                                );
                            }

                            let struct_type = self.context.struct_type(&field_types, false);
                            let struct_size = struct_type.size_of().unwrap();

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

                            // let struct_ptr = self.builder.build_alloca(struct_type.clone(), "struct_ptr").unwrap();
                            let struct_ptr = self
                                .builder
                                .build_call(malloc_func, &[struct_size.into()], "struct_ptr")
                                .unwrap()
                                .try_as_basic_value()
                                .left()
                                .unwrap()
                                .into_pointer_value();

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

                            self.structs.insert(name.to_string(), struct_type);
                            self.variables.insert(
                                name.to_string(),
                                (
                                    IRValue::Function(
                                        function,
                                        arg_types.clone(),
                                        Box::new(IRType::Struct(
                                            struct_type,
                                            fields
                                                .iter()
                                                .map(|(name, ty)| {
                                                    (
                                                        name.to_string(),
                                                        self.type_to_llvm(ty.clone()),
                                                    )
                                                })
                                                .collect(),
                                        )),
                                    ),
                                    IRType::Function(
                                        // function,
                                        arg_types,
                                        Box::new(IRType::Struct(
                                            struct_type,
                                            fields
                                                .iter()
                                                .map(|(name, ty)| {
                                                    (
                                                        name.to_string(),
                                                        self.type_to_llvm(ty.clone()),
                                                    )
                                                })
                                                .collect(),
                                        )),
                                    ),
                                ),
                            );
                        }
                        _ => unreachable!(),
                    }
                }
                exprs.push(TypedExpr::Literal(Literal::Int(0).into(), tconst!("int")));
                self.gen_function(
                    "main".to_string(),
                    vec![],
                    &Box::new(TypedExpr::Do(exprs, tconst!("int"))),
                    Type::Function(vec![], tconst!("int")).into(),
                )?;
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
            .args([
                "-O2",
                &(binding),
                "std.cc",
                "-o",
                exec_name,
                "-I/usr/lib/gc",
                "-lgc",
            ])
            .output()
            .expect("failed to execute process");

        Ok(())
    }

    fn gen_extern(
        &mut self,
        name: String,
        args: Vec<Arc<Type>>,
        ret: Arc<Type>,
    ) -> Result<(), String> {
        let ret_type = self.type_to_llvm(ret.clone());
        let arg_meta_types: Vec<BasicMetadataTypeEnum<'ctx>> = args
            .iter()
            .map(|type_| self.type_to_llvm(type_.clone()).as_meta_enum(self.context))
            .collect();
        let arg_types: Vec<(String, IRType<'ctx>)> = args
            .into_iter()
            .enumerate()
            .map(|(i, type_)| (i.to_string(), self.type_to_llvm(type_.clone()))) // false arg names
            .collect();
        let fn_type = ret_type
            .as_basic_enum(self.context)
            .fn_type(arg_meta_types.as_slice(), false);
        let function = self.module.add_function(&name, fn_type, None);

        self.variables.insert(
            name.to_string(),
            (
                IRValue::Function(function, arg_types.clone(), Box::new(ret_type.clone())),
                IRType::Function(arg_types.clone(), Box::new(ret_type.clone())),
            ),
        );
        Ok(())
    }

    pub fn gen_function(
        &mut self,
        name: String,
        args: Vec<(String, Arc<Type>)>,
        expr: &TypedExpr<'ctx>,
        type_: Arc<Type>,
    ) -> Result<(IRValue<'ctx>, IRType<'ctx>), String> {
        let (arg_meta_types, arg_types, ret_type) = match type_.as_ref().clone() {
            Type::Function(_arg_types, ret_type) => {
                let mut is_poly = false;
                for (_, arg_ty) in args.iter() {
                    // println!("{:?}", arg_ty);
                    if let Type::Variable(_) = arg_ty.as_ref() {
                        is_poly = true;
                        break;
                    }
                }
                if is_poly {
                    self.variables.insert(
                        name.to_string(),
                        (
                            IRValue::PolyMorph(
                                name.to_string(),
                                Box::new(expr.clone()),
                                args.clone(),
                                ret_type.clone(),
                            ),
                            IRType::PolyMorph,
                        ),
                    );
                    // self.polymorphic_functions.insert()
                    return Ok((
                        IRValue::PolyMorph(
                            name.to_string(),
                            Box::new(expr.clone()),
                            args,
                            ret_type.clone(),
                        ),
                        IRType::PolyMorph,
                    ));
                }
                let arg_meta_types: Vec<BasicMetadataTypeEnum<'ctx>> = args
                    .iter()
                    .map(|(_name, type_)| {
                        self.type_to_llvm(type_.clone()).as_meta_enum(self.context)
                    })
                    .collect();
                let arg_types: Vec<(String, IRType<'ctx>)> = args
                    .iter()
                    .map(|(name, type_)| (name.to_string(), self.type_to_llvm(type_.clone())))
                    .collect();
                (arg_meta_types, arg_types, self.type_to_llvm(ret_type))
            }
            _ => unreachable!(),
        };

        if self.module.get_function(&name).is_some() {
            Err("Function already declared.".to_string()) // maybe allow redeclaration?
        } else {
            // println!("\n");
            let fn_type = ret_type
                .as_basic_enum(self.context)
                .fn_type(arg_meta_types.as_slice(), false);
            let function = self.module.add_function(&name, fn_type, None);

            self.variables.insert(
                name.to_string(),
                (
                    IRValue::Function(function, arg_types.clone(), Box::new(ret_type.clone())),
                    IRType::Function(arg_types.clone(), Box::new(ret_type.clone())),
                ),
            );

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
                self.variables.insert(
                    arg_name.to_string(),
                    (
                        IRValue::Simple(alloca.into()),
                        self.type_to_llvm(type_.clone()),
                    ),
                );
            }

            let (returned_val, returned_type) = self.gen_expression(expr, function)?;

            if match returned_type {
                IRType::Simple(t) => match t {
                    BasicTypeEnum::ArrayType(_) => self
                        .builder
                        .build_aggregate_return(&[returned_val.as_basic_enum(self.context)]),
                    _ => self
                        .builder
                        .build_return(Some(&returned_val.as_basic_enum(self.context))),
                },
                _ => todo!(),
            }
            .is_err()
            {
                return Err("something went wrong during function return generation.".to_string());
            };

            // Verify the function
            if !function.verify(true) {
                self.print_ir();
                return Err(format!(
                    "something went wrong during function `{name}` generation"
                ));
            }

            Ok((
                IRValue::Function(function, arg_types.clone(), Box::new(ret_type.clone())),
                IRType::Function(arg_types.clone(), Box::new(ret_type.clone())),
            ))
        }
    }

    pub fn gen_expression(
        &mut self,
        expression: &TypedExpr<'ctx>,
        function: FunctionValue<'ctx>,
    ) -> Result<(IRValue<'ctx>, IRType<'ctx>), String> {
        match expression {
            TypedExpr::Let(name, expr, type_) => {
                let (val, ty) = self.gen_expression(expr, function)?;
                let alloca = self
                    .builder
                    .build_alloca(
                        self.type_to_llvm(type_.clone()).as_basic_enum(self.context),
                        name,
                    )
                    .unwrap();
                self.builder
                    .build_store(alloca, val.as_basic_enum(self.context))
                    .unwrap();
                self.variables.insert(
                    name.to_string(),
                    (IRValue::Simple(alloca.into()), ty.clone()),
                );
                Ok((IRValue::Simple(alloca.into()), ty))
            }
            TypedExpr::Variable(name, _type_) => match self.variables.get(&name.to_string()) {
                Some((value, type_)) => {
                    if let IRType::Function(..) = type_ {
                        return Ok((value.clone(), type_.clone()));
                    }
                    if let IRType::PolyMorph = type_ {
                        return Ok((value.clone(), type_.clone()));
                    }
                    if let IRType::BuiltIn = type_ {
                        return Ok((value.clone(), type_.clone()));
                    }
                    let value = self
                        .builder
                        .build_load(
                            type_.as_basic_enum(self.context),
                            value.as_basic_enum(self.context).into_pointer_value(),
                            name,
                        )
                        .unwrap();
                    Ok((IRValue::Simple(value), type_.clone()))
                }
                None => Err(format!("Variable '{}' not found", name)),
            },
            TypedExpr::Lambda(args, body, return_type_) => {
                // Create a new function for the lambda
                let lambda_name = format!("lambda_{}", self.lambda_counter);
                self.lambda_counter += 1;

                let (lambda_func, lambda_type) = self.gen_function(
                    lambda_name,
                    args.iter()
                        .map(|(name, type_)| (name.to_string(), type_.clone()))
                        .collect(),
                    body,
                    return_type_.clone(),
                )?;

                let IRValue::Function(lambda_func, _, _) = lambda_func else {
                    unreachable!()
                };

                Ok((
                    IRValue::Simple(lambda_func.as_global_value().as_basic_value_enum()),
                    lambda_type,
                ))
            }
            TypedExpr::Literal(lit, _) => Ok(self.gen_literal(lit)),
            TypedExpr::If(cond, if_, else_, type_) => {
                let (cond_val, _) = self.gen_expression(cond, function)?;
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
                        cond_val.as_basic_enum(self.context).into_int_value(),
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
                        cond_val.as_basic_enum(self.context).into_int_value(),
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
                let (then_val, then_ty) = if let Some(_else_bb) = else_bb {
                    self.gen_expression(if_, function)?
                } else {
                    self.gen_expression(if_, function)?;
                    (
                        IRValue::Simple(BasicValueEnum::IntValue(
                            self.context.i32_type().const_zero(),
                        )),
                        IRType::Simple(BasicTypeEnum::IntType(self.context.i32_type())),
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
                    let else_expr = else_.clone().unwrap();
                    let (else_val, _) = self.gen_expression(&else_expr, function)?;
                    match self.builder.build_unconditional_branch(merge_bb) {
                        Ok(_) => {}
                        Err(e) => {
                            return Err(format!(
                                "something went wrong during `if-else` codegen. {}",
                                e
                            ))
                        }
                    };
                    else_val.as_basic_enum(self.context)
                } else {
                    BasicValueEnum::IntValue(self.context.i32_type().const_zero())
                    // Default value
                };

                self.builder.position_at_end(merge_bb);

                let phi = self
                    .builder
                    .build_phi(
                        match else_bb {
                            Some(_) => self.type_to_llvm(type_.clone()).as_basic_enum(self.context),
                            None => BasicTypeEnum::IntType(self.context.i32_type()),
                        },
                        "iftmp",
                    )
                    .unwrap();

                if let Some(else_bb) = else_bb {
                    phi.add_incoming(&[
                        (&then_val.as_basic_enum(self.context), then_bb),
                        (&else_val, else_bb),
                    ]);
                } else {
                    phi.add_incoming(&[
                        (&then_val.as_basic_enum(self.context), then_bb),
                        (&else_val, pre_if_bb),
                    ]);
                };

                Ok((IRValue::Simple(phi.as_basic_value()), then_ty))
            }
            TypedExpr::BinaryOp(lhs, op, rhs, _type_) => {
                let (lhs_val, lhs_type) = self.gen_expression(&(lhs.clone()), function)?;
                let (rhs_val, rhs_type) = self.gen_expression(&(rhs.clone()), function)?;

                let lhs_type = lhs_type.as_basic_enum(self.context);
                let rhs_type = rhs_type.as_basic_enum(self.context);

                let lhs_val = lhs_val.as_basic_enum(self.context);
                let rhs_val = rhs_val.as_basic_enum(self.context);

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
                        let (lhs_val, lhs_type) = self.gen_expression(lhs, function)?;
                        let (lhs_val, _lhs_type) = (
                            lhs_val.as_basic_enum(self.context),
                            lhs_type.as_basic_enum(self.context),
                        );

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
                        let rhs_bool = self
                            .gen_expression(rhs, function)?
                            .0
                            .as_basic_enum(self.context);
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
                        let lhs_val = self
                            .gen_expression(lhs, function)?
                            .0
                            .as_basic_enum(self.context);
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
                        let rhs_val = self
                            .gen_expression(rhs, function)?
                            .0
                            .as_basic_enum(self.context);
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
                Ok((IRValue::Simple(result), IRType::Simple(lhs_type)))
            }
            TypedExpr::Call(callee, args, type_) => {
                match self.gen_expression(callee, function)? {
                    (IRValue::BuiltIn(name), IRType::BuiltIn) => {
                        match name.as_str() {
                            "type" => {
                                if args.len() != 1 {
                                    return Err("Invalid number of arguments to function `type`"
                                        .to_string());
                                }
                                let (_compiled_arg, ty) =
                                    self.gen_expression(&args[0], function)?;
                                let typestring = match ty {
                                    IRType::Simple(_) => {
                                        match get_type_from_typed_expr(&args[0]).as_ref() {
                                            Type::Constructor(c) => c.name.clone(), // todo: handle generics
                                            Type::Struct(name, ..) => name.clone(),
                                            Type::Function(..) => "<function>".to_string(),
                                            _ => unreachable!(),
                                        }
                                    }
                                    IRType::BuiltIn => format!("<builtin {name}>"),
                                    IRType::Struct(name, ..) => name.to_string(),
                                    IRType::Function(..) | IRType::PolyMorph => {
                                        "<function>".to_string()
                                    }
                                };
                                Ok((
                                    self.gen_literal(&Literal::String(typestring.into())).0,
                                    IRType::Simple(
                                        self.context.ptr_type(AddressSpace::from(0)).into(),
                                    ),
                                ))
                            }
                            "print" => {
                                let ret = tconst!("str");
                                if args.is_empty() {
                                    let string_ptr =
                                        self.builder.build_global_string_ptr("\n", "str");
                                    return match self.builder.build_call(
                                        self.module.get_function("printstr").unwrap(),
                                        &[string_ptr
                                            .unwrap()
                                            .as_pointer_value()
                                            .as_basic_value_enum()
                                            .into()],
                                        "calltmp",
                                    ) {
                                        Ok(call_site_value) => Ok((
                                            IRValue::Simple(
                                                call_site_value
                                                    .try_as_basic_value()
                                                    .left()
                                                    .unwrap(),
                                            ),
                                            self.type_to_llvm(ret.clone()),
                                        )),
                                        Err(e) => Err(e.to_string()),
                                    };
                                }
                                if args.len() != 1 {
                                    return Err("Invalid number of args.".to_string());
                                }
                                let arg = &args[0];
                                // println!("{:#?}", arg);
                                // for arg in args {
                                let (compiled_arg, ty) = self.gen_expression(arg, function)?;
                                let (IRValue::Simple(str_val), _) = match ty {
                                    IRType::Simple(_) => {
                                        match get_type_from_typed_expr(arg).as_ref() {
                                            Type::Constructor(c) if c.name == "int" => {
                                                match self.builder.build_call(
                                                    self.module.get_function("int_to_str").unwrap(),
                                                    &[compiled_arg.as_meta_enum(self.context)],
                                                    "calltmp",
                                                ) {
                                                    Ok(call_site_value) => Ok((
                                                        IRValue::Simple(
                                                            call_site_value
                                                                .try_as_basic_value()
                                                                .left()
                                                                .unwrap(),
                                                        ),
                                                        self.type_to_llvm(ret.clone()),
                                                    )),
                                                    Err(e) => Err(e.to_string()),
                                                }
                                            }
                                            Type::Constructor(c) if c.name == "float" => {
                                                match self.builder.build_call(
                                                    self.module
                                                        .get_function("float_to_str")
                                                        .unwrap(),
                                                    &[compiled_arg.as_meta_enum(self.context)],
                                                    "calltmp",
                                                ) {
                                                    Ok(call_site_value) => Ok((
                                                        IRValue::Simple(
                                                            call_site_value
                                                                .try_as_basic_value()
                                                                .left()
                                                                .unwrap(),
                                                        ),
                                                        self.type_to_llvm(ret.clone()),
                                                    )),
                                                    Err(e) => Err(e.to_string()),
                                                }
                                            }
                                            Type::Constructor(c) if c.name == "bool" => {
                                                match self.builder.build_call(
                                                    self.module
                                                        .get_function("bool_to_str")
                                                        .unwrap(),
                                                    &[compiled_arg.as_meta_enum(self.context)],
                                                    "calltmp",
                                                ) {
                                                    Ok(call_site_value) => Ok((
                                                        IRValue::Simple(
                                                            call_site_value
                                                                .try_as_basic_value()
                                                                .left()
                                                                .unwrap(),
                                                        ),
                                                        self.type_to_llvm(ret.clone()),
                                                    )),
                                                    Err(e) => Err(e.to_string()),
                                                }
                                            }
                                            Type::Constructor(c) if c.name == "str" => {
                                                Ok((compiled_arg.clone(), ty))
                                            }
                                            Type::Constructor(c) => Ok(self.gen_literal(
                                                &Literal::String(format!("<{}>", c.name).into()),
                                            )),
                                            Type::Struct(name, ..) => Ok(self.gen_literal(
                                                &Literal::String(format!("<{}>", name).into()),
                                            )),
                                            Type::Function(..) => Ok(self.gen_literal(
                                                &Literal::String("<function>".to_string().into()),
                                            )),
                                            Type::Variable(_v) => {
                                                todo!()
                                            }
                                            Type::Trait(_) => todo!(),
                                        }
                                    }
                                    IRType::BuiltIn => todo!(),
                                    IRType::Struct(name, ..) => Ok(self.gen_literal(
                                        &Literal::String(format!("<{}>", name).into()),
                                    )),
                                    IRType::Function(..) | IRType::PolyMorph => todo!(),
                                }?
                                else {
                                    unreachable!()
                                };
                                // };
                                // todo!()
                                match self.builder.build_call(
                                    self.module.get_function("printstr").unwrap(),
                                    &[str_val.into()],
                                    "calltmp",
                                ) {
                                    Ok(call_site_value) => Ok((
                                        IRValue::Simple(
                                            call_site_value.try_as_basic_value().left().unwrap(),
                                        ),
                                        self.type_to_llvm(ret.clone()),
                                    )),
                                    Err(e) => Err(e.to_string()),
                                }
                            }
                            "println" => {
                                let ret = tconst!("str");
                                if args.is_empty() {
                                    let string_ptr =
                                        self.builder.build_global_string_ptr("\n", "str");
                                    return match self.builder.build_call(
                                        self.module.get_function("printstr").unwrap(),
                                        &[string_ptr
                                            .unwrap()
                                            .as_pointer_value()
                                            .as_basic_value_enum()
                                            .into()],
                                        "calltmp",
                                    ) {
                                        Ok(call_site_value) => Ok((
                                            IRValue::Simple(
                                                call_site_value
                                                    .try_as_basic_value()
                                                    .left()
                                                    .unwrap(),
                                            ),
                                            self.type_to_llvm(ret.clone()),
                                        )),
                                        Err(e) => Err(e.to_string()),
                                    };
                                }
                                if args.len() != 1 {
                                    return Err("Invalid number of args.".to_string());
                                }
                                let arg = &args[0];
                                // println!("{:#?}", arg);
                                // for arg in args {
                                let (compiled_arg, ty) = self.gen_expression(arg, function)?;
                                let (IRValue::Simple(str_val), _) = match ty {
                                    IRType::Simple(_) => {
                                        match get_type_from_typed_expr(arg).as_ref() {
                                            Type::Constructor(c) if c.name == "int" => {
                                                match self.builder.build_call(
                                                    self.module.get_function("int_to_str").unwrap(),
                                                    &[compiled_arg.as_meta_enum(self.context)],
                                                    "calltmp",
                                                ) {
                                                    Ok(call_site_value) => Ok((
                                                        IRValue::Simple(
                                                            call_site_value
                                                                .try_as_basic_value()
                                                                .left()
                                                                .unwrap(),
                                                        ),
                                                        self.type_to_llvm(ret.clone()),
                                                    )),
                                                    Err(e) => Err(e.to_string()),
                                                }
                                            }
                                            Type::Constructor(c) if c.name == "float" => {
                                                match self.builder.build_call(
                                                    self.module
                                                        .get_function("float_to_str")
                                                        .unwrap(),
                                                    &[compiled_arg.as_meta_enum(self.context)],
                                                    "calltmp",
                                                ) {
                                                    Ok(call_site_value) => Ok((
                                                        IRValue::Simple(
                                                            call_site_value
                                                                .try_as_basic_value()
                                                                .left()
                                                                .unwrap(),
                                                        ),
                                                        self.type_to_llvm(ret.clone()),
                                                    )),
                                                    Err(e) => Err(e.to_string()),
                                                }
                                            }
                                            Type::Constructor(c) if c.name == "bool" => {
                                                match self.builder.build_call(
                                                    self.module
                                                        .get_function("bool_to_str")
                                                        .unwrap(),
                                                    &[compiled_arg.as_meta_enum(self.context)],
                                                    "calltmp",
                                                ) {
                                                    Ok(call_site_value) => Ok((
                                                        IRValue::Simple(
                                                            call_site_value
                                                                .try_as_basic_value()
                                                                .left()
                                                                .unwrap(),
                                                        ),
                                                        self.type_to_llvm(ret.clone()),
                                                    )),
                                                    Err(e) => Err(e.to_string()),
                                                }
                                            }
                                            Type::Constructor(c) if c.name == "str" => {
                                                Ok((compiled_arg.clone(), ty))
                                            }
                                            Type::Constructor(c) => Ok(self.gen_literal(
                                                &Literal::String(format!("<{}>", c.name).into()),
                                            )),
                                            Type::Struct(name, ..) => Ok(self.gen_literal(
                                                &Literal::String(format!("<{}>", name).into()),
                                            )),
                                            Type::Function(..) => Ok(self.gen_literal(
                                                &Literal::String("<function>".to_string().into()),
                                            )),
                                            Type::Variable(_v) => {
                                                todo!()
                                            }
                                            Type::Trait(_) => todo!(),
                                        }
                                    }
                                    IRType::BuiltIn => todo!(),
                                    IRType::Struct(name, ..) => Ok(self.gen_literal(
                                        &Literal::String(format!("<{}>", name).into()),
                                    )),
                                    IRType::Function(..) | IRType::PolyMorph => todo!(),
                                }?
                                else {
                                    unreachable!()
                                };
                                // };
                                // todo!()
                                match self.builder.build_call(
                                    self.module.get_function("printstrln").unwrap(),
                                    &[str_val.into()],
                                    "calltmp",
                                ) {
                                    Ok(call_site_value) => Ok((
                                        IRValue::Simple(
                                            call_site_value.try_as_basic_value().left().unwrap(),
                                        ),
                                        self.type_to_llvm(ret.clone()),
                                    )),
                                    Err(e) => Err(e.to_string()),
                                }
                            }
                            _ => unreachable!(),
                        }
                    }
                    (IRValue::PolyMorph(name, expr, polyargs, _ret), IRType::PolyMorph) => {
                        let mut poly_args = vec![];
                        let arg_exprs = args;
                        let continuation_bb = function.get_last_basic_block().unwrap();

                        if args.len() != polyargs.len() {
                            return Err("Invalid number of args".to_string());
                        }
                        let mut substitutions = HashMap::new();

                        for (i, arg) in args.iter().enumerate() {
                            let ty = get_type_from_typed_expr(arg);
                            if let Type::Variable(v) = polyargs[i].1.as_ref() {
                                substitutions.insert(*v, ty.clone());
                            }
                            poly_args.push((polyargs[i].0.clone().into(), ty));
                        }

                        let new_name = format!("{name}${}$", self.lambda_counter);
                        self.lambda_counter += 1;

                        let TypedNode::Function(_, args, expr, ret) =
                            TypeEnv::substitute_type_vars_in_typed_node(
                                TypedNode::Function(
                                    new_name.clone().into(),
                                    poly_args,
                                    expr,
                                    type_.clone(),
                                ),
                                &substitutions,
                            )
                        else {
                            unreachable!()
                        };

                        let (IRValue::Function(function_to_call, _, _), _fn_type) = self
                            .gen_function(
                                new_name.clone(),
                                args.iter()
                                    .map(|(x, t)| (x.to_string(), t.clone()))
                                    .collect(),
                                &expr,
                                Type::Function(
                                    args.iter().map(|(_, t)| (t.clone())).collect(),
                                    ret.clone(),
                                )
                                .into(),
                            )?
                        else {
                            unreachable!()
                        };

                        self.builder.position_at_end(continuation_bb);

                        // todo!()
                        // type check to see if all expressions are valid
                        // i'll add it later. let me just make a prototype.

                        /*
                        Then, we look up all the instances of this function in self.polymorphic_functions.
                        if the argument types match, we use that function.
                        else, we compile a new version.
                        I'll do it later. Compile everything freshly for now.
                        */

                        let mut compiled_args: Vec<BasicValueEnum> = vec![];

                        arg_exprs.iter().for_each(|arg| {
                            compiled_args.push(
                                self.gen_expression(arg, function)
                                    .unwrap()
                                    .0
                                    .as_basic_enum(self.context),
                            )
                        });

                        let argsv: Vec<BasicMetadataValueEnum> = compiled_args
                            .iter()
                            .by_ref()
                            .map(|&val| val.into())
                            .collect();

                        match self.builder.build_call(function_to_call, &argsv, "calltmp") {
                            Ok(call_site_value) => Ok((
                                IRValue::Simple(
                                    call_site_value.try_as_basic_value().left().unwrap(),
                                ),
                                self.type_to_llvm(ret),
                            )),
                            Err(e) => Err(e.to_string()),
                        }
                    }
                    (IRValue::Function(fn_, _, _), IRType::Function(_args_, ret)) => {
                        let function_to_call = fn_;
                        let mut compiled_args: Vec<BasicValueEnum> = vec![];

                        args.iter().for_each(|arg| {
                            compiled_args.push(
                                self.gen_expression(arg, function)
                                    .unwrap()
                                    .0
                                    .as_basic_enum(self.context),
                            )
                        });

                        let argsv: Vec<BasicMetadataValueEnum> = compiled_args
                            .iter()
                            .by_ref()
                            .map(|&val| val.into())
                            .collect();

                        match self.builder.build_call(function_to_call, &argsv, "calltmp") {
                            Ok(call_site_value) => Ok((
                                IRValue::Simple(
                                    call_site_value.try_as_basic_value().left().unwrap(),
                                ),
                                *ret,
                            )),
                            Err(e) => Err(e.to_string()),
                        }
                    }
                    _ => panic!("invalid call!"),
                }
            }
            TypedExpr::UnaryOp(op, expr, type_) => {
                let (val, val_type) = self.gen_expression(expr, function)?;
                let val = val.as_basic_enum(self.context);

                match op {
                    UnaryOperator::Negate if type_.as_ref() == tconst!("int").as_ref() => Ok((
                        IRValue::Simple(
                            self.builder
                                .build_int_neg(val.into_int_value(), "neg")
                                .unwrap()
                                .into(),
                        ),
                        val_type,
                    )),
                    UnaryOperator::Not if type_.as_ref() == tconst!("bool").as_ref() => Ok((
                        IRValue::Simple(
                            self.builder
                                .build_not(val.into_int_value(), "not")
                                .unwrap()
                                .into(),
                        ),
                        val_type,
                    )),
                    _ => Err(format!(
                        "Unsupported unary operator or type: {:?} {:?}",
                        op, type_
                    )),
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

                let ty = self.type_to_llvm(element_type.clone());
                let llvm_element_type = self.type_to_llvm(element_type).as_basic_enum(self.context);
                let array_type = llvm_element_type.into_array_type();

                let ptr = self.builder.build_alloca(array_type, "tmparray").unwrap();

                elements.iter().enumerate().for_each(|(i, elem)| {
                    let expr = self
                        .gen_expression(elem, function)
                        .unwrap()
                        .0
                        .as_basic_enum(self.context);

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

                Ok((IRValue::Simple(ptr.into()), ty))
                // let global_array = self.module.add_global(array_type, Some(AddressSpace::Const), "array_global").unwrap();
                // global_array.set_initializer(&array_value);
                // Ok(global_array.as_pointer_value().as_basic_value_enum())
            }
            TypedExpr::While(..) => {
                panic!("ive not decided if i want a `while` loop in this language yet")
            }
            TypedExpr::Do(expressions, type_) => {
                let mut exprs = vec![];
                let mut expressions_ =
                    vec![TypedExpr::Literal(Literal::Int(0).into(), tconst!("int"))];
                expressions_.append(&mut expressions.clone());

                for expr in expressions_ {
                    exprs.push(self.gen_expression(&expr, function)?.0)
                }

                match exprs.last() {
                    None => Ok((
                        IRValue::Simple(
                            self.type_to_llvm(type_.clone())
                                .as_basic_enum(self.context)
                                .const_zero(),
                        ),
                        self.type_to_llvm(type_.clone()),
                    )),
                    Some(v) => Ok((v.clone(), self.type_to_llvm(type_.clone()))),
                }
            }
            TypedExpr::Index(array, index, type_) => {
                let array_val = self
                    .gen_expression(array, function)?
                    .0
                    .as_basic_enum(self.context);
                let index_val = self
                    .gen_expression(index, function)?
                    .0
                    .as_basic_enum(self.context);

                match array_val.get_type() {
                    BasicTypeEnum::ArrayType(_) => {
                        let indices = [
                            self.context.i32_type().const_zero(),
                            index_val.into_int_value(),
                        ];
                        Ok((
                            IRValue::Simple(
                                unsafe {
                                    self.builder
                                        .build_in_bounds_gep(
                                            self.type_to_llvm(type_.clone())
                                                .as_basic_enum(self.context),
                                            array_val.into_pointer_value(),
                                            &indices,
                                            "index_access",
                                        )
                                        .unwrap()
                                }
                                .into(),
                            ),
                            self.type_to_llvm(type_.clone()),
                        ))
                    }
                    _ => Err(format!(
                        "Unsupported type for indexing: {:?}",
                        array_val.get_type()
                    )),
                }
            }
            TypedExpr::StructAccess(structref, field, _ty) => {
                let (structref, structty) = self.gen_expression(structref, function)?;
                let (struct_type, fields) = match structty {
                    IRType::Struct(struct_type, fields) => (struct_type, fields),
                    _ => return Err("AAAAAAAAA!!!!!!!!!! INVALID STRUCT ACCESSSS!!!!".to_string()),
                };
                for (i, (name, ty)) in fields.iter().enumerate() {
                    if name == field {
                        let field_ptr = self
                            .builder
                            .build_struct_gep(
                                struct_type,
                                structref.as_basic_enum(self.context).into_pointer_value(),
                                i as u32,
                                "field_ptr",
                            )
                            .unwrap();
                        let field_val = self
                            .builder
                            .build_load(ty.as_basic_enum(self.context), field_ptr, "field_val")
                            .unwrap();
                        return Ok((IRValue::Simple(field_val), ty.clone()));
                    }
                }
                Err("no such field found in the given struct".to_string())
            }
        }
    }

    fn gen_literal(&mut self, literal: &Literal) -> (IRValue<'ctx>, IRType<'ctx>) {
        match literal {
            Literal::Int(i) => (
                IRValue::Simple(self.context.i32_type().const_int(*i as u64, false).into()),
                IRType::Simple(self.context.i32_type().into()),
            ),
            Literal::Float(f) => (
                IRValue::Simple(self.context.f32_type().const_float(*f).into()),
                IRType::Simple(self.context.f32_type().into()),
            ),
            Literal::Boolean(b) => (
                IRValue::Simple(
                    self.context
                        .bool_type()
                        .const_int(if *b { 1 } else { 0 }, false)
                        .into(),
                ),
                IRType::Simple(self.context.bool_type().into()),
            ),
            Literal::String(s) => {
                let string_ptr = self.builder.build_global_string_ptr(s, "str");
                (
                    IRValue::Simple(string_ptr.unwrap().as_pointer_value().as_basic_value_enum()),
                    IRType::Simple(self.context.ptr_type(AddressSpace::from(0)).into()),
                )
            }
        }
    }

    fn type_to_llvm(&self, ty: Arc<Type>) -> IRType<'ctx> {
        match ty.as_ref() {
            Type::Constructor(TypeConstructor {
                name,
                generics: _,
                traits: _,
            }) => match name.as_str() {
                "ptr" => IRType::Simple(self.context.ptr_type(AddressSpace::from(0)).into()),
                "bool" => IRType::Simple(self.context.i32_type().into()),
                "int" => IRType::Simple(self.context.i32_type().into()),
                "long" => IRType::Simple(self.context.i64_type().into()),
                "float" => IRType::Simple(self.context.f32_type().into()),
                "double" => IRType::Simple(self.context.f64_type().into()),
                "str" => IRType::Simple(self.context.ptr_type(AddressSpace::from(0)).into()),
                x => match self.structs.get(x) {
                    Some(_v) => IRType::Simple(self.context.ptr_type(AddressSpace::from(0)).into()),
                    None => panic!("no such struct"),
                },
            },
            Type::Struct(_name, _, _fields) => {
                IRType::Simple(self.context.ptr_type(AddressSpace::from(0)).into())
            }
            _ => panic!("{:?}", ty),
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
