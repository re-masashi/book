use crate::codegen::generator::{IRGenerator, IRType, IRValue};
use crate::codegen::{Type, TypeConstructor, TypedExpr, UnaryOperator};
use crate::tconst;

use inkwell::types::{BasicType, BasicTypeEnum};
use inkwell::values::{
    BasicValue, BasicValueEnum, FunctionValue, InstructionOpcode,
};
use inkwell::AddressSpace;

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
            TypedExpr::BinaryOp(..) => {
                self.gen_binop(expression, function)
            }
            TypedExpr::Call(..) => {
                self.gen_call(expression, function)
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
