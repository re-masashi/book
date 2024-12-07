use crate::codegen::generator::{get_type_from_typed_expr, IRGenerator, IRType, IRValue};
use crate::codegen::{BinaryOperator, Literal, Type, TypeConstructor, TypedExpr, UnaryOperator};
use crate::tconst;

use inkwell::types::{BasicType, BasicTypeEnum};
use inkwell::values::{
    BasicMetadataValueEnum, BasicValue, BasicValueEnum, FunctionValue, InstructionOpcode,
};
use inkwell::AddressSpace;
use inkwell::{FloatPredicate, IntPredicate};

// use std::collections::HashMap;
use std::sync::Arc;

impl<'ctx> IRGenerator<'ctx> {
    pub fn gen_expression(
        &mut self,
        expression: &TypedExpr<'ctx>,
        function: FunctionValue<'ctx>,
    ) -> Result<(IRValue<'ctx>, IRType<'ctx>), String> {
        match expression {
            TypedExpr::Let(name, expr, type_, _span, _file) => {
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
            TypedExpr::Variable(name, _type_, _span, _file) => {
                match self.variables.get(&name.to_string()) {
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
                }
            }
            TypedExpr::Lambda(args, body, return_type_, _span, _file) => {
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
            TypedExpr::Literal(lit, _, _span, _file) => Ok(self.gen_literal(lit)),
            TypedExpr::If(cond, if_, else_, type_, _span, _file) => {
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

                match self
                    .builder
                    .get_insert_block()
                    .unwrap() // always exists
                    .get_last_instruction()
                {
                    None => {}
                    Some(ins) => {
                        if ins.get_opcode() == InstructionOpcode::Return
                            || ins.get_opcode() == InstructionOpcode::Br
                        {
                            // println!("brr");
                            // self.print_ir();
                            // return Ok((IRValue::Simple(self.context.i32_type().const_zero().into()), IRType::Simple(self.context.i32_type().into())))
                        } else {
                            match self.builder.build_unconditional_branch(merge_bb) {
                                Ok(_) => {}
                                Err(e) => {
                                    return Err(format!(
                                        "something went wrong during `if` codegen. {}",
                                        e
                                    ))
                                }
                            };
                        }
                    }
                }

                // Else block (if present)
                let else_val = if let Some(else_bb) = else_bb {
                    self.builder.position_at_end(else_bb);
                    let else_expr = else_.clone().unwrap();
                    let (else_val, _) = self.gen_expression(&else_expr, function)?;
                    match self
                        .builder
                        .get_insert_block()
                        .unwrap() // always exists
                        .get_last_instruction()
                    {
                        None => {}
                        Some(ins) => {
                            if ins.get_opcode() == InstructionOpcode::Return
                                || ins.get_opcode() == InstructionOpcode::Br
                            {
                                // println!("brr");
                                // self.print_ir();
                                // return Ok((IRValue::Simple(self.context.i32_type().const_zero().into()), IRType::Simple(self.context.i32_type().into())))
                            } else {
                                match self.builder.build_unconditional_branch(merge_bb) {
                                    Ok(_) => {}
                                    Err(e) => {
                                        return Err(format!(
                                            "something went wrong during `if` codegen. {}",
                                            e
                                        ))
                                    }
                                };
                            }
                        }
                    }
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
            TypedExpr::BinaryOp(lhs, op, rhs, _type_, _span, _file) => {
                let (lhs_val, lhs_type) = self.gen_expression(&(lhs.clone()), function)?;
                let (rhs_val, rhs_type) = self.gen_expression(&(rhs.clone()), function)?;

                if let (IRValue::Simple(_), IRValue::Simple(_)) = (lhs_val.clone(), rhs_val.clone())
                {
                    let argsv: [BasicMetadataValueEnum; 2] = [
                        lhs_val.as_meta_enum(self.context),
                        rhs_val.as_meta_enum(self.context),
                    ];
                    match (
                        get_type_from_typed_expr(&lhs.clone()).as_ref(),
                        get_type_from_typed_expr(&lhs.clone()).as_ref(),
                    ) {
                        (Type::Constructor(c1), Type::Constructor(c2))
                            if c1.name == "str" && c2.name == "str" =>
                        {
                            match op {
                                BinaryOperator::Equal => {
                                    let function_to_call =
                                        self.module.get_function("strcmp").unwrap();
                                    let lval = match self.builder.build_call(
                                        function_to_call,
                                        &argsv,
                                        "strcmptemp",
                                    ) {
                                        Ok(call_site_value) => {
                                            call_site_value.try_as_basic_value().left().unwrap()
                                        }
                                        Err(e) => return Err(e.to_string()),
                                    };
                                    return Ok((
                                        IRValue::Simple(
                                            self.builder
                                                .build_int_compare(
                                                    IntPredicate::EQ,
                                                    lval.into_int_value(),
                                                    self.context.i32_type().const_zero(),
                                                    "eqtmp",
                                                )
                                                .unwrap()
                                                .into(),
                                        ),
                                        IRType::Simple(self.context.i32_type().into()),
                                    ));
                                }
                                BinaryOperator::Add => {
                                    let function_to_call =
                                        self.module.get_function("concat").unwrap();
                                    return match self.builder.build_call(
                                        function_to_call,
                                        &argsv,
                                        "strcmptemp",
                                    ) {
                                        Ok(call_site_value) => Ok((
                                            IRValue::Simple(
                                                call_site_value
                                                    .try_as_basic_value()
                                                    .left()
                                                    .unwrap(),
                                            ),
                                            IRType::Simple(
                                                self.context.ptr_type(AddressSpace::from(0)).into(),
                                            ), // str
                                        )),
                                        Err(e) => Err(e.to_string()),
                                    };
                                }
                                // BinaryOperator::Less => {}
                                // BinaryOperator::Greater => {}
                                _ => return Err("invalid operator for strings.".to_string()),
                            }
                        }
                        (Type::Constructor(c1), Type::Constructor(c2))
                            if c1.name == "str" && c2.name == "int" =>
                        {
                            match op {
                                BinaryOperator::Mul => {
                                    let function_to_call =
                                        self.module.get_function("stringrepeat").unwrap();
                                    return match self.builder.build_call(
                                        function_to_call,
                                        &argsv,
                                        "strcmptemp",
                                    ) {
                                        Ok(call_site_value) => Ok((
                                            IRValue::Simple(
                                                call_site_value
                                                    .try_as_basic_value()
                                                    .left()
                                                    .unwrap(),
                                            ),
                                            IRType::Simple(
                                                self.context.ptr_type(AddressSpace::from(0)).into(),
                                            ), // str
                                        )),
                                        Err(e) => Err(e.to_string()),
                                    };
                                }
                                _ => return Err("invalid operator for string and int".to_string()),
                            }
                        }
                        _ => {}
                    }
                }

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
            TypedExpr::Call(callee, args, _type_, _span, _file) => {
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
                                    IRType::Returned(..) => "()".to_string(),
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
                                            Type::Tuple(_) => todo!(),
                                        }
                                    }
                                    IRType::BuiltIn => todo!(),
                                    IRType::Struct(name, ..) => Ok(self.gen_literal(
                                        &Literal::String(format!("<{}>", name).into()),
                                    )),
                                    IRType::Function(..)
                                    | IRType::PolyMorph
                                    | IRType::Returned(..) => todo!(),
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
                                            Type::Tuple(_) => todo!(),
                                        }
                                    }
                                    IRType::BuiltIn => todo!(),
                                    IRType::Struct(name, ..) => Ok(self.gen_literal(
                                        &Literal::String(format!("<{}>", name).into()),
                                    )),
                                    IRType::Function(..)
                                    | IRType::PolyMorph
                                    | IRType::Returned(..) => todo!(),
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
                            "str" => {
                                let ret = tconst!("str");
                                if args.len() != 1 {
                                    return Err("Invalid number of args.".to_string());
                                }
                                let arg = &args[0];
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
                                            Type::Tuple(_) => todo!(),
                                        }
                                    }
                                    IRType::BuiltIn => todo!(),
                                    IRType::Struct(name, ..) => Ok(self.gen_literal(
                                        &Literal::String(format!("<{}>", name).into()),
                                    )),
                                    IRType::Function(..)
                                    | IRType::PolyMorph
                                    | IRType::Returned(..) => todo!(),
                                }?
                                else {
                                    unreachable!()
                                };
                                Ok((
                                    IRValue::Simple(str_val),
                                    IRType::Simple(
                                        self.context.ptr_type(AddressSpace::from(0)).into(),
                                    ),
                                ))
                            }
                            _ => unreachable!(),
                        }
                    }
                    (IRValue::PolyMorph(_name, _expr, _polyargs, _ret), IRType::PolyMorph) => {
                        todo!("needs work")
                        // let mut new_typed_args = vec![];
                        // let arg_exprs = args;
                        // let continuation_bb = function.get_last_basic_block().unwrap();

                        // let mut typeenv = TypeEnv(HashMap::new());

                        // if args.len() != polyargs.len() {
                        //     return Err("Invalid number of args".to_string());
                        // }
                        // let mut substitutions = HashMap::new();

                        // for (i, arg) in args.iter().enumerate() {
                        //     let ty = get_type_from_typed_expr(arg);
                        //     if let Type::Variable(v) = polyargs[i].1.as_ref() {
                        //         substitutions.insert(*v, ty.clone());
                        //     }
                        //     new_typed_args.push((polyargs[i].0.clone().into(), ty));
                        // }

                        // let new_name = format!("{name}${}$", self.lambda_counter);
                        // self.lambda_counter += 1;

                        // let TypedNode::Function(_, args, expr, ret) = typeenv
                        //     .substitute_type_vars_in_typed_node(
                        //         TypedNode::Function(
                        //             new_name.clone().into(),
                        //             new_typed_args,
                        //             expr,
                        //             type_.clone(),
                        //         ),
                        //         &mut substitutions,
                        //     )
                        // else {
                        //     unreachable!()
                        // };
                        // println!("{:?}", substitutions);

                        // let (IRValue::Function(function_to_call, _, _), _fn_type) = self
                        //     .gen_function(
                        //         new_name.clone(),
                        //         args.iter()
                        //             .map(|(x, t)| (x.to_string(), t.clone()))
                        //             .collect(),
                        //         &typeenv
                        //             .substitute_type_vars_in_typed_expr(*expr, &mut substitutions),
                        //         Type::Function(
                        //             args.iter().map(|(_, t)| (t.clone())).collect(),
                        //             ret.clone(),
                        //         )
                        //         .into(),
                        //     )?
                        // else {
                        //     unreachable!()
                        // };

                        // self.builder.position_at_end(continuation_bb);

                        // // todo!()
                        // // type check to see if all expressions are valid
                        // // i'll add it later. let me just make a prototype.

                        // /*
                        // Then, we look up all the instances of this function in self.polymorphic_functions.
                        // if the argument types match, we use that function.
                        // else, we compile a new version.
                        // I'll do it later. Compile everything freshly for now.
                        // */

                        // let mut compiled_args: Vec<BasicValueEnum> = vec![];

                        // arg_exprs.iter().for_each(|arg| {
                        //     compiled_args.push(
                        //         self.gen_expression(arg, function)
                        //             .unwrap()
                        //             .0
                        //             .as_basic_enum(self.context),
                        //     )
                        // });

                        // let argsv: Vec<BasicMetadataValueEnum> = compiled_args
                        //     .iter()
                        //     .by_ref()
                        //     .map(|&val| val.into())
                        //     .collect();

                        // match self.builder.build_call(function_to_call, &argsv, "calltmp") {
                        //     Ok(call_site_value) => Ok((
                        //         IRValue::Simple(
                        //             call_site_value.try_as_basic_value().left().unwrap(),
                        //         ),
                        //         self.type_to_llvm(ret),
                        //     )),
                        //     Err(e) => Err(e.to_string()),
                        // }
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
            TypedExpr::UnaryOp(op, expr, type_, _span, _file) => {
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
            TypedExpr::Array(elements, type_, _span, _file) => {
                let element_type = match type_.as_ref() {
                    Type::Constructor(TypeConstructor {
                        name: _,  // always equals "Array"
                        generics, // always equals [T]
                        traits: _,
                    }) => generics[0].clone(),
                    _ => return Err("Invalid type for array".to_string()),
                };

                // Convert the element type to LLVM type
                let llvm_element_type = self.type_to_llvm(element_type);
                let llvm_element_type_enum = llvm_element_type.as_basic_enum(self.context);

                // Create array type
                let array_type = llvm_element_type_enum.array_type(elements.len() as u32);

                // Allocate memory for the array
                let ptr = self.builder.build_alloca(array_type, "tmparray").unwrap();

                // Iterate over elements and store them in the array
                for (i, elem) in elements.iter().enumerate() {
                    // Generate code for the element expression
                    let expr = self
                        .gen_expression(elem, function)?
                        .0
                        .as_basic_enum(self.context);

                    // Create constants for the GEP indexing (0 for the first dimension, i for the second dimension)
                    let const_0 = self.context.i64_type().const_zero();
                    let const_i = self.context.i64_type().const_int(i as u64, false);

                    let inner_ptr = unsafe {
                        self.builder
                            .build_gep(array_type, ptr, &[const_0, const_i], &format!("elem_{}", i))
                            .unwrap()
                    };

                    self.builder.build_store(inner_ptr, expr).unwrap();
                }

                Ok((
                    IRValue::Simple(ptr.into()),
                    IRType::Simple(self.context.ptr_type(AddressSpace::from(0)).into()),
                ))

                // let global_array = self.module.add_global(array_type, Some(AddressSpace::Const), "array_global").unwrap();
                // global_array.set_initializer(&array_value);
                // Ok(global_array.as_pointer_value().as_basic_value_enum())
            }
            TypedExpr::While(cond, body, _type_, _span, _file) => {
                // match **body {
                //     TypedExpr::Break => {
                //         return Ok((
                //             IRValue::Simple(self.context.i32_type().const_zero().into()),
                //             IRType::Simple(self.context.i32_type().into())
                //         ))
                //     }
                //     TypedExpr::Continue => return Err("created an infinite loop with no body".to_string()),
                //     _=>{}
                // }
                // Create basic blocks for the loop
                let cond_bb = self.context.append_basic_block(function, "while_cond");
                let body_bb = self.context.append_basic_block(function, "while_body");
                let after_bb = self.context.append_basic_block(function, "after_while");

                // Branch to the condition block
                self.builder.build_unconditional_branch(cond_bb).unwrap();

                // Condition block
                self.builder.position_at_end(cond_bb);
                let (cond_val, _) = self.gen_expression(cond, function)?;
                self.builder
                    .build_conditional_branch(
                        cond_val.as_basic_enum(self.context).into_int_value(),
                        body_bb,
                        after_bb,
                    )
                    .unwrap();

                // Loop body block
                self.builder.position_at_end(body_bb);
                let pre_loop_bbs = self.loop_bbs;
                self.loop_bbs = Some((body_bb, after_bb));
                self.gen_expression(body, function)?;

                match self
                    .builder
                    .get_insert_block()
                    .unwrap() // always exists
                    .get_last_instruction()
                {
                    None => {}
                    Some(ins) => {
                        if ins.get_opcode() == InstructionOpcode::Return
                            || ins.get_opcode() == InstructionOpcode::Br
                        {
                            // println!("brr");
                            // self.print_ir();
                            // return Ok((IRValue::Simple(self.context.i32_type().const_zero().into()), IRType::Simple(self.context.i32_type().into())))
                        } else {
                            self.builder.build_unconditional_branch(cond_bb).unwrap();
                            // Loop back to the condition
                        }
                    }
                }

                // After loop block
                self.builder.position_at_end(after_bb);
                self.loop_bbs = pre_loop_bbs;

                Ok((
                    IRValue::Simple(self.context.i32_type().const_zero().into()),
                    IRType::Simple(self.context.i32_type().into()),
                ))
            }
            TypedExpr::Do(expressions, type_, _span, _file) => {
                let mut exprs = vec![];
                let mut expressions_ = vec![];
                expressions_.append(&mut expressions.clone());

                for expr in expressions_ {
                    // debug!("{:?}", expr);
                    let compiled_expr = self.gen_expression(&expr, function)?;
                    match expr {
                        TypedExpr::Return(..) => return Ok(compiled_expr),
                        _ => {
                            exprs.push(compiled_expr.0);
                        }
                    }
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
            TypedExpr::Index(array_expr, index_expr, type_, _span, _file) => {
                // println!("1");
                let array_ptr = self
                    .gen_expression(array_expr, function)?
                    .0
                    .as_basic_enum(self.context)
                    .into_pointer_value();
                // println!("2");
                let index_value = self
                    .gen_expression(index_expr, function)?
                    .0
                    .as_basic_enum(self.context)
                    .into_int_value();
                // println!("3");
                // println!("4");
                let index_ptr = unsafe {
                    self.builder
                        .build_gep(
                            self.type_to_llvm(type_.clone()).as_basic_enum(self.context),
                            array_ptr,
                            &[index_value],
                            "element_ptr",
                        )
                        .unwrap()
                };
                let field_val = self
                    .builder
                    .build_load(
                        self.type_to_llvm(type_.clone()).as_basic_enum(self.context),
                        index_ptr,
                        "index_val",
                    )
                    .unwrap();
                Ok((IRValue::Simple(field_val), self.type_to_llvm(type_.clone())))
            }
            TypedExpr::StructAccess(structref, field, _ty, _span, _file) => {
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
            TypedExpr::Return(expr, _return_type, _span, _file) => {
                let (val, ty) = self.gen_expression(expr, function)?;
                match self
                    .builder
                    .get_insert_block()
                    .unwrap() // always exists
                    .get_last_instruction()
                {
                    None => {}
                    Some(ins) => {
                        if ins.get_opcode() == InstructionOpcode::Return {
                            return Ok((
                                IRValue::Returned(Box::new(val)),
                                IRType::Returned(Box::new(ty)),
                            ));
                        }
                    }
                }

                // Create a return instruction
                self.builder
                    .build_return(Some(&val.as_basic_enum(self.context)))
                    .unwrap();

                Ok((
                    IRValue::Returned(Box::new(val)),
                    IRType::Returned(Box::new(ty)),
                ))
            }
            TypedExpr::Tuple(..) => todo!(),
            TypedExpr::Assign(lhs, expr, _type_, _span, _file) => {
                let (val, ty) = self.gen_expression(&expr.clone(), function)?;

                match *lhs.clone() {
                    TypedExpr::Variable(name, _, _span, _file) => {
                        // Get the variable's alloca
                        let var_alloca = self
                            .variables
                            .get(&name.to_string())
                            .unwrap()
                            .0
                            .as_basic_enum(self.context)
                            .into_pointer_value();

                        // Store the value into the alloca
                        self.builder
                            .build_store(var_alloca, val.as_basic_enum(self.context))
                            .unwrap();
                        Ok((val, ty))
                    }
                    TypedExpr::Index(array_expr, index_expr, type_, _span, _file) => {
                        // println!("1");
                        let array_ptr = self
                            .gen_expression(&array_expr, function)?
                            .0
                            .as_basic_enum(self.context)
                            .into_pointer_value();
                        // println!("2");
                        let index_value = self
                            .gen_expression(&index_expr, function)?
                            .0
                            .as_basic_enum(self.context)
                            .into_int_value();
                        // println!("3");
                        // println!("4");
                        let index_ptr = unsafe {
                            self.builder
                                .build_gep(
                                    self.type_to_llvm(type_.clone()).as_basic_enum(self.context),
                                    array_ptr,
                                    &[index_value],
                                    "element_ptr",
                                )
                                .unwrap()
                        };
                        self.builder
                            .build_store(index_ptr, val.as_basic_enum(self.context))
                            .unwrap();
                        let field_val = self
                            .builder
                            .build_load(
                                self.type_to_llvm(type_.clone()).as_basic_enum(self.context),
                                index_ptr,
                                "index_val",
                            )
                            .unwrap();
                        Ok((IRValue::Simple(field_val), self.type_to_llvm(type_.clone())))
                    }
                    TypedExpr::StructAccess(structref, field, _, _span, _file) => {
                        let (structref, structty) = self.gen_expression(&structref, function)?;
                        let (struct_type, fields) = match structty {
                            IRType::Struct(struct_type, fields) => (struct_type, fields),
                            _ => {
                                return Err(
                                    "AAAAAAAAA!!!!!!!!!! INVALID STRUCT ACCESSSS!!!!".to_string()
                                )
                            }
                        };
                        for (i, (name, ty)) in fields.iter().enumerate() {
                            if *name == field {
                                let field_ptr = self
                                    .builder
                                    .build_struct_gep(
                                        struct_type,
                                        structref.as_basic_enum(self.context).into_pointer_value(),
                                        i as u32,
                                        "field_ptr",
                                    )
                                    .unwrap();
                                self.builder
                                    .build_store(field_ptr, val.as_basic_enum(self.context))
                                    .unwrap();
                                let field_val = self
                                    .builder
                                    .build_load(
                                        ty.as_basic_enum(self.context),
                                        field_ptr,
                                        "field_val",
                                    )
                                    .unwrap();

                                return Ok((IRValue::Simple(field_val), ty.clone()));
                            }
                        }
                        Err(format!("no such field {field} in struct."))
                    }
                    _ => Err("tried to assign to invalid value".to_string()),
                }
            }
            TypedExpr::Break(..) => {
                match self.loop_bbs {
                    Some((_, after_bb)) => {
                        self.builder.build_unconditional_branch(after_bb).unwrap();
                    }
                    None => return Err("Tried to use break outside of a loop.".to_string()),
                }
                Ok((
                    IRValue::Simple(self.context.i32_type().const_zero().into()),
                    IRType::Simple(self.context.i32_type().into()),
                ))
            }
            TypedExpr::Continue(..) => {
                match self.loop_bbs {
                    Some((body_bb, _)) => {
                        self.builder.build_unconditional_branch(body_bb).unwrap();
                    }
                    None => return Err("Tried to use continue outside of a loop.".to_string()),
                }
                Ok((
                    IRValue::Simple(self.context.i32_type().const_zero().into()),
                    IRType::Simple(self.context.i32_type().into()),
                ))
            }
        }
    }
}
