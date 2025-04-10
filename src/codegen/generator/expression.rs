use crate::codegen::generator::{IRGenerator, IRType, IRValue};
use crate::codegen::{Type, TypeConstructor, TypedExpr, UnaryOperator};
use crate::tconst;

use inkwell::types::{BasicType, BasicTypeEnum, StructType};
use inkwell::values::{BasicValue, BasicValueEnum, FunctionValue, InstructionOpcode};
use inkwell::AddressSpace;
use inkwell::IntPredicate;

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

                let else_bb = if else_.is_some() {
                    Some(self.context.append_basic_block(function, "else"))
                } else {
                    None
                };

                let current_bb = self.builder.get_insert_block().unwrap();

                // Build conditional branch
                self.builder
                    .build_conditional_branch(
                        cond_val.as_basic_enum(self.context).into_int_value(),
                        then_bb,
                        else_bb.unwrap_or(merge_bb),
                    )
                    .unwrap();

                // THEN
                self.builder.position_at_end(then_bb);
                let (then_val, ty) = self.gen_expression(if_, function)?;

                if self
                    .builder
                    .get_insert_block()
                    .unwrap()
                    .get_terminator()
                    .is_none()
                {
                    self.builder.build_unconditional_branch(merge_bb).unwrap();
                }

                // ELSE
                let else_val = if let Some(else_bb) = else_bb {
                    self.builder.position_at_end(else_bb);
                    let (else_val, _) = self.gen_expression(else_.as_ref().unwrap(), function)?;
                    if self
                        .builder
                        .get_insert_block()
                        .unwrap()
                        .get_terminator()
                        .is_none()
                    {
                        self.builder.build_unconditional_branch(merge_bb).unwrap();
                    }
                    else_val.as_basic_enum(self.context)
                } else {
                    // Use default if no else block
                    BasicValueEnum::IntValue(self.context.i32_type().const_zero())
                };

                self.builder.position_at_end(merge_bb);

                let phi_type = self.type_to_llvm(type_.clone()).as_basic_enum(self.context);
                let phi = self.builder.build_phi(phi_type, "iftmp").unwrap();

                phi.add_incoming(&[
                    (&then_val.as_basic_enum(self.context), then_bb),
                    (&else_val, else_bb.unwrap_or(current_bb)),
                ]);

                Ok((IRValue::Simple(phi.as_basic_value()), ty))
            }
            TypedExpr::BinaryOp(..) => self.gen_binop(expression, function),
            TypedExpr::Call(..) => self.gen_call(expression, function),
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
                    Type::Constructor(TypeConstructor { generics, .. }) => generics[0].clone(),
                    _ => return Err("Invalid array type".into()),
                };
                let _i8_ptr_type = self.context.ptr_type(AddressSpace::default());
                let array_struct_type = self.context.struct_type(
                    &[
                        self.context.ptr_type(AddressSpace::default()).into(), // data ptr
                        self.context.i32_type().into(),                        // len
                        self.context.i32_type().into(),                        // capacity
                    ],
                    false,
                );

                let llvm_elem_ty = self
                    .type_to_llvm(element_type.clone())
                    .as_basic_enum(self.context);
                let element_size = llvm_elem_ty.size_of().unwrap();

                let len = elements.len() as u64;
                let capacity = len.max(len * 2); // minimum starting capacity

                let total_size =
                    element_size.const_mul(self.context.i32_type().const_int(capacity, false));
                let malloc_fn = self.module.get_function("GC_malloc").unwrap(); // assumed available
                let raw_ptr = self
                    .builder
                    .build_call(malloc_fn, &[total_size.into()], "malloc_array")
                    .unwrap()
                    .try_as_basic_value()
                    .left()
                    .unwrap()
                    .into_pointer_value();

                // Cast malloced i8* to element pointer
                let data_ptr = self
                    .builder
                    .build_bit_cast(
                        raw_ptr,
                        self.context.ptr_type(AddressSpace::default()),
                        "casted_array_ptr",
                    )
                    .unwrap()
                    .into_pointer_value();

                for (i, elem) in elements.iter().enumerate() {
                    let (val, _) = self.gen_expression(elem, function)?;
                    let elem_val = val.as_basic_enum(self.context);

                    let gep = unsafe {
                        self.builder
                            .build_in_bounds_gep(
                                llvm_elem_ty,
                                data_ptr,
                                &[self.context.i64_type().const_int(i as u64, false)],
                                &format!("array_gep_{}", i),
                            )
                            .unwrap()
                    };
                    self.builder.build_store(gep, elem_val).unwrap();
                }

                // Now make the struct { ptr, len, cap }
                let array_alloca = self
                    .builder
                    .build_alloca(array_struct_type, "array_struct")
                    .unwrap();

                let ptr_field = self
                    .builder
                    .build_struct_gep(array_struct_type, array_alloca, 0, "ptr_field")
                    .unwrap();
                let len_field = self
                    .builder
                    .build_struct_gep(array_struct_type, array_alloca, 1, "len_field")
                    .unwrap();
                let cap_field = self
                    .builder
                    .build_struct_gep(array_struct_type, array_alloca, 2, "cap_field")
                    .unwrap();

                self.builder.build_store(ptr_field, raw_ptr).unwrap();
                self.builder
                    .build_store(len_field, self.context.i64_type().const_int(len, false))
                    .unwrap();
                self.builder
                    .build_store(
                        cap_field,
                        self.context.i64_type().const_int(capacity, false),
                    )
                    .unwrap();

                Ok((
                    IRValue::Simple(array_alloca.into()),
                    IRType::Simple(self.context.ptr_type(AddressSpace::from(0)).into()),
                ))
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
                let i32_type = self.context.i32_type();
                let array_struct_type = self.context.struct_type(
                    &[
                        self.context.ptr_type(AddressSpace::default()).into(), // data ptr
                        self.context.i32_type().into(),                        // len
                        self.context.i32_type().into(),                        // capacity
                    ],
                    false,
                );

                let array_ptr = self
                    .gen_expression(array_expr, function)?
                    .0
                    .as_basic_enum(self.context)
                    .into_pointer_value();
                let index_val = self
                    .gen_expression(index_expr, function)?
                    .0
                    .as_basic_enum(self.context)
                    .into_int_value();

                let len_ptr = self
                    .builder
                    .build_struct_gep(array_struct_type, array_ptr, 1, "len_ptr")
                    .unwrap();
                let len_val = self
                    .builder
                    .build_load(i32_type, len_ptr, "len_val")
                    .unwrap()
                    .into_int_value();

                let cmp = self
                    .builder
                    .build_int_compare(IntPredicate::ULT, index_val, len_val, "index_in_bounds")
                    .unwrap();

                let ok_bb = self.context.append_basic_block(function, "index_ok");
                let err_bb = self.context.append_basic_block(function, "index_oob");
                let cont_bb = self.context.append_basic_block(function, "index_cont");

                self.builder
                    .build_conditional_branch(cmp, ok_bb, err_bb)
                    .unwrap();

                // Error block
                self.builder.position_at_end(err_bb);
                let _puts_fn = self
                    .module
                    .get_function("puts")
                    .expect("puts() function not found");

                let format_str = self
                    .builder
                    .build_global_string_ptr("Index out of bounds: i=%ld, len=%ld\n", "err_str")
                    .unwrap();

                // Declare printf if not already declared
                let printf = self.module.get_function("printf").unwrap_or_else(|| {
                    let i8ptr = self.context.ptr_type(AddressSpace::from(0));
                    self.module.add_function(
                        "printf",
                        self.context.i32_type().fn_type(&[i8ptr.into()], true),
                        None,
                    )
                });

                self.builder
                    .build_call(
                        printf,
                        &[
                            format_str.as_pointer_value().into(),
                            index_val.into(),
                            len_val.into(),
                        ],
                        "call_printf",
                    )
                    .unwrap();

                self.builder.build_unreachable().unwrap();

                // Success path
                self.builder.position_at_end(ok_bb);

                let ptr_ptr = self
                    .builder
                    .build_struct_gep(array_struct_type, array_ptr, 0, "dataptr")
                    .unwrap();
                let raw_data = self
                    .builder
                    .build_load(
                        self.context.ptr_type(AddressSpace::default()),
                        ptr_ptr,
                        "load_ptr",
                    )
                    .unwrap()
                    .into_pointer_value();

                let llvm_elem_ty = self.type_to_llvm(type_.clone()).as_basic_enum(self.context);
                let gep = unsafe {
                    self.builder
                        .build_in_bounds_gep(llvm_elem_ty, raw_data, &[index_val], "data_gep")
                        .unwrap()
                };
                let val = self
                    .builder
                    .build_load(llvm_elem_ty, gep, "load_val")
                    .unwrap();

                self.builder.build_unconditional_branch(cont_bb).unwrap();

                self.builder.position_at_end(cont_bb);
                Ok((IRValue::Simple(val), self.type_to_llvm(type_.clone())))
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
                            .unwrap() // can panic
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
                        Ok((
                            IRValue::Simple(index_value.into()),
                            self.type_to_llvm(type_.clone()),
                        ))
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

    fn array_struct_type(&self, _elem_type: BasicTypeEnum<'ctx>) -> StructType<'ctx> {
        self.context.struct_type(
            &[
                self.context.ptr_type(AddressSpace::from(0)).into(),
                self.context.i64_type().into(),
            ],
            false,
        )
    }
}
