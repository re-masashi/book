use crate::codegen::generator::{get_type_from_typed_expr, IRGenerator, IRType, IRValue};
use crate::codegen::{BinaryOperator, Type, TypedExpr};

use inkwell::types::{BasicTypeEnum};
use inkwell::values::{
    BasicMetadataValueEnum, BasicValue, FunctionValue
};
use inkwell::AddressSpace;
use inkwell::{FloatPredicate, IntPredicate};

impl<'ctx> IRGenerator<'ctx> {
    pub fn gen_binop(
        &mut self,
        expression: &TypedExpr<'ctx>,
        function: FunctionValue<'ctx>,
    ) -> Result<(IRValue<'ctx>, IRType<'ctx>), String> {
        match expression {
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
            _=>unreachable!()
        }
    }
}