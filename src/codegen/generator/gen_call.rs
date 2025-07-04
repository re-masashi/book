use crate::codegen::generator::{get_type_from_typed_expr, IRGenerator, IRType, IRValue};
use crate::codegen::{Literal, Type, TypeConstructor, TypeEnv, TypedExpr, TypedNode};
use crate::tconst;
use inkwell::types::BasicType;
use inkwell::values::{BasicMetadataValueEnum, BasicValue, BasicValueEnum, FunctionValue};
use inkwell::AddressSpace;
use inkwell::IntPredicate;

use std::collections::HashMap;
use std::sync::Arc;

impl<'ctx> IRGenerator<'ctx> {
    pub fn gen_call(
        &mut self,
        expression: &TypedExpr<'ctx>,
        function: FunctionValue<'ctx>,
    ) -> Result<(IRValue<'ctx>, IRType<'ctx>), String> {
        match expression {
            TypedExpr::Call(callee, args, type_, _span, _file) => {
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
                                    return Err(format!(
                                        "Invalid number of args. expected 1. found {}",
                                        args.len()
                                    ));
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
                                                &Literal::String(format!("<{name}>").into()),
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
                                    IRType::Struct(name, ..) => Ok(self
                                        .gen_literal(&Literal::String(format!("<{name}>").into()))),
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
                                    // println!("{:#?}", args);
                                    return Err(format!(
                                        "Invalid number of args. expected 1. found {}",
                                        args.len()
                                    ));
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
                                                &Literal::String(format!("<{name}>").into()),
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
                                    IRType::Struct(v, ..) => {
                                        Ok(self
                                            .gen_literal(&Literal::String(format!("<{v}>").into())))
                                    }
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
                                    return Err(
                                        "Invalid number of args provided to `str`.".to_string()
                                    );
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
                                                &Literal::String(format!("<{name}>").into()),
                                            )),
                                            Type::Function(..) => Ok(self.gen_literal(
                                                &Literal::String("<function>".to_string().into()),
                                            )),
                                            Type::Variable(_v) => {
                                                // println!("{:#?}", expression);
                                                todo!()
                                            }
                                            Type::Trait(_) => todo!(),
                                            Type::Tuple(_) => todo!(),
                                        }
                                    }
                                    IRType::BuiltIn => todo!(),
                                    IRType::Struct(name, ..) => Ok(self
                                        .gen_literal(&Literal::String(format!("<{name}>").into()))),
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
                            "array" => {
                                if args.len() != 2 {
                                    return Err(
                                        "Invalid number of args provided to `array`.".to_string()
                                    );
                                }
                                if let Type::Constructor(TypeConstructor {
                                    name,
                                    generics: _,
                                    traits: _,
                                }) = get_type_from_typed_expr(&args[0]).as_ref()
                                {
                                    if name != "int" {
                                        return Err(format!(
                                            "Invalid length provided to `array`. Found {:?}",
                                            &args[0]
                                        ));
                                    }
                                } else {
                                    return Err(format!(
                                        "Invalid length provided to `array`. Found {:?}",
                                        &args[0]
                                    ));
                                }
                                let (compiled_len, _len_ty) =
                                    self.gen_expression(&args[0], function)?;
                                let (_compiled_val, val_ty) =
                                    self.gen_expression(&args[1], function)?;

                                // Get the size of the value type (in bits)
                                let type_size_in_bits = val_ty
                                    .as_basic_enum(self.context)
                                    .size_of()
                                    .expect("Failed to get size of type");
                                let malloc_func = self
                                    .module
                                    .get_function("GC_malloc")
                                    .expect("malloc not found");

                                let array_size = self
                                    .builder
                                    .build_int_mul(
                                        compiled_len.as_basic_enum(self.context).into_int_value(),
                                        type_size_in_bits,
                                        "array_size",
                                    )
                                    .unwrap();
                                let array_ptr = self
                                    .builder
                                    .build_call(malloc_func, &[array_size.into()], "array_alloc")
                                    .unwrap()
                                    .try_as_basic_value()
                                    .left()
                                    .unwrap();

                                Ok((
                                    IRValue::Simple(array_ptr),
                                    IRType::Simple(
                                        self.context.ptr_type(AddressSpace::from(0)).into(),
                                    ),
                                ))
                            }
                            "len" => {
                                if args.len() != 1 {
                                    return Err(
                                        "Invalid number of args provided to `len`.".to_string()
                                    );
                                }

                                let (compiled_array, _array_ty) =
                                    self.gen_expression(&args[0], function)?;

                                Ok((
                                    compiled_array,
                                    IRType::Simple(self.context.i32_type().into()),
                                ))
                            }
                            "push" => {
                                if args.len() != 2 {
                                    return Err(
                                        "`push` expects exactly 2 arguments: array and value."
                                            .to_string(),
                                    );
                                }

                                let (compiled_array, _array_ty) =
                                    self.gen_expression(&args[0], function)?;
                                let (compiled_value, val_ty) =
                                    self.gen_expression(&args[1], function)?;
                                // println!("{:?}", get_type_from_typed_expr(&args[0]));
                                let array_elem_ty =
                                    match get_type_from_typed_expr(&args[0]).as_ref() {
                                        Type::Constructor(c) if c.name == "Array" => {
                                            c.generics[0].clone()
                                        }
                                        _ => todo!(),
                                    };
                                let _array_elem_llvm_ty = self.type_to_llvm(array_elem_ty.clone());
                                // println!("{:?}", array_elem_llvm_ty);
                                let array_struct_type = self.context.struct_type(
                                    &[
                                        self.context.ptr_type(AddressSpace::default()).into(), // data ptr
                                        self.context.i32_type().into(), // len
                                        self.context.i32_type().into(), // capacity
                                    ],
                                    false,
                                );

                                let struct_ptr = compiled_array
                                    .as_basic_enum(self.context)
                                    .into_pointer_value();
                                let struct_type = array_struct_type; // struct_ptr.get_type(); //.get_inner_type().into_struct_type();

                                // Get field pointers
                                let ptr_ptr = self
                                    .builder
                                    .build_struct_gep(struct_type, struct_ptr, 0, "ptr_ptr")
                                    .unwrap();
                                let len_ptr = self
                                    .builder
                                    .build_struct_gep(struct_type, struct_ptr, 1, "len_ptr")
                                    .unwrap();
                                let cap_ptr = self
                                    .builder
                                    .build_struct_gep(struct_type, struct_ptr, 2, "cap_ptr")
                                    .unwrap();

                                let data_ptr = self
                                    .builder
                                    .build_load(
                                        self.context.ptr_type(AddressSpace::default()),
                                        ptr_ptr,
                                        "data_ptr",
                                    )
                                    .unwrap()
                                    .into_pointer_value();

                                let len_val = self
                                    .builder
                                    .build_load(self.context.i32_type(), len_ptr, "len")
                                    .unwrap()
                                    .into_int_value();

                                let cap_val = self
                                    .builder
                                    .build_load(self.context.i32_type(), cap_ptr, "cap")
                                    .unwrap()
                                    .into_int_value();

                                let type_size =
                                    val_ty.as_basic_enum(self.context).size_of().unwrap();
                                let type_size_i32 = self
                                    .builder
                                    .build_int_cast(
                                        type_size,
                                        self.context.i32_type(),
                                        "type_size_i32",
                                    )
                                    .unwrap();

                                let malloc_fn = self
                                    .module
                                    .get_function("GC_malloc")
                                    .expect("missing GC_malloc");

                                let len_eq_cap = self
                                    .builder
                                    .build_int_compare(
                                        IntPredicate::EQ,
                                        len_val,
                                        cap_val,
                                        "len_eq_cap",
                                    )
                                    .unwrap();

                                let grow_block = self.context.append_basic_block(function, "grow");
                                let push_block = self.context.append_basic_block(function, "push");
                                let end_block = self.context.append_basic_block(function, "end");

                                self.builder
                                    .build_conditional_branch(len_eq_cap, grow_block, push_block)
                                    .unwrap();

                                // === Grow block ===
                                self.builder.position_at_end(grow_block);
                                let new_cap = self
                                    .builder
                                    .build_int_mul(
                                        cap_val,
                                        self.context.i32_type().const_int(2, false),
                                        "new_cap",
                                    )
                                    .unwrap();

                                let malloc_size = self
                                    .builder
                                    .build_int_mul(new_cap, type_size_i32, "malloc_size")
                                    .unwrap();

                                let malloc_size_i64 = self
                                    .builder
                                    .build_int_z_extend(
                                        malloc_size,
                                        self.context.i64_type(),
                                        "malloc_size_i64",
                                    )
                                    .unwrap();

                                let new_arr = self
                                    .builder
                                    .build_call(malloc_fn, &[malloc_size_i64.into()], "new_arr")
                                    .unwrap()
                                    .try_as_basic_value()
                                    .left()
                                    .unwrap()
                                    .into_pointer_value();

                                // Copy old values to new array
                                let memcpy_fn = self
                                    .module
                                    .get_function("llvm.memcpy.p0.p0.i64")
                                    .unwrap_or_else(|| {
                                        let fn_ty = self.context.void_type().fn_type(
                                            &[
                                                self.context
                                                    .ptr_type(AddressSpace::default())
                                                    .into(), // dest
                                                self.context
                                                    .ptr_type(AddressSpace::default())
                                                    .into(), // src
                                                self.context.i64_type().into(), // size
                                                self.context.bool_type().into(), // is_volatile
                                            ],
                                            false,
                                        );
                                        self.module.add_function(
                                            "llvm.memcpy.p0.p0.i64",
                                            fn_ty,
                                            None,
                                        )
                                    });

                                let copy_size = self
                                    .builder
                                    .build_int_mul(len_val, type_size_i32, "copy_size")
                                    .unwrap();
                                let copy_size_i64 = self
                                    .builder
                                    .build_int_z_extend(
                                        copy_size,
                                        self.context.i64_type(),
                                        "copy_size_i64",
                                    )
                                    .unwrap();

                                self.builder
                                    .build_call(
                                        memcpy_fn,
                                        &[
                                            new_arr.into(),
                                            data_ptr.into(),
                                            copy_size_i64.into(),
                                            self.context.bool_type().const_zero().into(),
                                        ],
                                        "memcpy",
                                    )
                                    .unwrap();

                                // Write new value at index = len
                                let new_dest_ptr = unsafe {
                                    self.builder
                                        .build_gep(
                                            val_ty.as_basic_enum(self.context),
                                            new_arr,
                                            &[len_val],
                                            "new_dest_ptr",
                                        )
                                        .unwrap()
                                };
                                self.builder
                                    .build_store(
                                        new_dest_ptr,
                                        compiled_value.as_basic_enum(self.context),
                                    )
                                    .unwrap();

                                // Update array fields
                                self.builder.build_store(ptr_ptr, new_arr).unwrap();
                                self.builder.build_store(cap_ptr, new_cap).unwrap();

                                self.builder.build_unconditional_branch(end_block).unwrap();

                                // === Push block (no grow needed) ===
                                self.builder.position_at_end(push_block);
                                let dest_ptr = unsafe {
                                    self.builder
                                        .build_gep(
                                            val_ty.as_basic_enum(self.context),
                                            data_ptr,
                                            &[len_val],
                                            "dest_ptr",
                                        )
                                        .unwrap()
                                };
                                self.builder
                                    .build_store(
                                        dest_ptr,
                                        compiled_value.as_basic_enum(self.context),
                                    )
                                    .unwrap();

                                self.builder.build_unconditional_branch(end_block).unwrap();

                                // === End block ===
                                self.builder.position_at_end(end_block);
                                let new_len = self
                                    .builder
                                    .build_int_add(
                                        len_val,
                                        self.context.i32_type().const_int(1, false),
                                        "new_len",
                                    )
                                    .unwrap();
                                self.builder.build_store(len_ptr, new_len).unwrap();

                                Ok((
                                    IRValue::Simple(struct_ptr.into()),
                                    IRType::Simple(struct_ptr.get_type().into()),
                                ))
                            }
                            _ => unreachable!(),
                        }
                    }
                    (IRValue::PolyMorph(name, expr, polyargs, _ret), IRType::PolyMorph) => {
                        let mut new_typed_args = vec![];
                        let arg_exprs = args;
                        let continuation_bb = function.get_last_basic_block().unwrap();

                        let mut typeenv = TypeEnv(HashMap::new(), HashMap::new());

                        if args.len() != polyargs.len() {
                            return Err(format!("Invalid number of args provided to {name}"));
                        }
                        let mut substitutions = HashMap::new();

                        for (i, arg) in args.iter().enumerate() {
                            let ty = get_type_from_typed_expr(arg);
                            if let Type::Variable(v) = polyargs[i].1.as_ref() {
                                substitutions.insert(*v, ty.clone());
                            }
                            new_typed_args.push((polyargs[i].0.clone().into(), ty));
                        }

                        let new_name = format!("{name}${}$", self.lambda_counter);
                        self.lambda_counter += 1;

                        let TypedNode::Function(_, args, expr, ret) = typeenv
                            .substitute_type_vars_in_typed_node(
                                TypedNode::Function(
                                    new_name.clone().into(),
                                    new_typed_args,
                                    expr,
                                    type_.clone(),
                                ),
                                &mut substitutions,
                            )
                        else {
                            unreachable!()
                        };
                        println!("{substitutions:?}");

                        let (IRValue::Function(function_to_call, _, _), _fn_type) = self
                            .gen_function(
                                new_name.clone(),
                                args.iter()
                                    .map(|(x, t)| (x.to_string(), t.clone()))
                                    .collect(),
                                &typeenv
                                    .substitute_type_vars_in_typed_expr(*expr, &mut substitutions),
                                Type::Function(
                                    args.iter().map(|(_, t)| (t.clone())).collect(),
                                    ret.clone(),
                                )
                                .into(),
                            )
                            .unwrap()
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
            _ => unreachable!(),
        }
    }
}
