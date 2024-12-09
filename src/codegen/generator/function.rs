use crate::codegen::generator::{IRGenerator, IRType, IRValue};
use crate::codegen::{Type, TypedExpr};

use inkwell::types::{BasicMetadataTypeEnum, BasicType, BasicTypeEnum};

use std::sync::Arc;

impl<'ctx> IRGenerator<'ctx> {
    pub fn gen_extern(
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

            match returned_type {
                IRType::Simple(t) => {
                    if (match t {
                        BasicTypeEnum::ArrayType(_) => self
                            .builder
                            .build_aggregate_return(&[returned_val.as_basic_enum(self.context)]),
                        _ => self
                            .builder
                            .build_return(Some(&returned_val.as_basic_enum(self.context))),
                    })
                    .is_err()
                    {
                        return Err(
                            "something went wrong during function return generation.".to_string()
                        );
                    };
                }
                IRType::Returned(_) => {
                    // no-op
                }
                IRType::Struct(_t, _) => {
                    self.builder
                        .build_return(Some(&returned_val.as_basic_enum(self.context)))
                        .unwrap();
                }
                ref x => todo!("{:?}", x),
            }

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
}
