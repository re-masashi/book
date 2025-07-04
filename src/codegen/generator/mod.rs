use inkwell::basic_block::BasicBlock;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::targets::{
    CodeModel, FileType, InitializationConfig, RelocMode, Target, TargetMachine,
};
use inkwell::types::{BasicMetadataTypeEnum, BasicTypeEnum, StructType};
use inkwell::values::{
    BasicMetadataValueEnum, BasicValue, BasicValueEnum, FunctionValue, StructValue,
};
use inkwell::AddressSpace;
use inkwell::OptimizationLevel;
use log::debug;
use owo_colors::OwoColorize;

use std::collections::HashMap;
use std::env;
use std::fs::File;
use std::io::BufRead;
use std::io::BufReader;
use std::path::Path;
use std::process::Command;
use std::sync::Arc;

use crate::codegen::{Literal, Type, TypeConstructor, TypedExpr, TypedNode};
use crate::lexer::tokens::Span;
use crate::{t_float, t_int, t_str, tconst};

pub mod expression;
pub mod function;
pub mod gen_binop;
pub mod gen_call;

macro_rules! _write_std_cc {
    () => {{
        let project_root = env::var("CARGO_MANIFEST_DIR").unwrap(); // Get project root path
        let std_cc_path = Path::new(&project_root).join("src/std.cc"); // Get std.cc path

        // Create a temp file to write std.cc contents
        let temp_file = NamedTempFile::new().unwrap();
        let temp_path = temp_file.path().to_str().unwrap().to_string();

        // Copy std.cc contents to the temp file
        let std_cc_contents = std::fs::read_to_string(std_cc_path).unwrap();
        write(&temp_path, std_cc_contents).unwrap();

        temp_path
    }};
}

pub struct IRGenerator<'ctx> {
    context: &'ctx Context,
    module: Module<'ctx>,
    builder: Builder<'ctx>,
    lambda_counter: i32,
    variables: HashMap<String, (IRValue<'ctx>, IRType<'ctx>)>,
    structs: HashMap<String, (StructType<'ctx>, Vec<(String, IRType<'ctx>)>)>,
    pos: i32,
    line_no: i32,
    file: String,
    polymorphic_functions: HashMap<String, Vec<PolyMorphicFunction<'ctx>>>,
    builtins: Vec<IRValue<'ctx>>,
    loop_bbs: Option<(BasicBlock<'ctx>, BasicBlock<'ctx>)>,
    // generic_structs: HashMap<String, (Vec<String>, Vec<(String, Arc<Type>)>)>,
}

pub struct PolyMorphicFunction<'ctx>(
    Vec<Arc<Type>>, // args
    Arc<Type>,      // ret
    IRValue<'ctx>,
    IRType<'ctx>,
);

pub struct PolyMorphicStruct<'ctx>(
    Vec<Arc<Type>>, // args
    IRType<'ctx>,
);

#[derive(Clone, Debug)]
pub enum IRType<'ctx> {
    Function(Vec<(String, IRType<'ctx>)>, Box<IRType<'ctx>>),
    Struct(StructType<'ctx>, Vec<(String, IRType<'ctx>)>),
    Simple(BasicTypeEnum<'ctx>),
    PolyMorph, // no need to store all the data
    BuiltIn,   // functions such as type().
    Returned(Box<IRType<'ctx>>),
}

impl<'ctx> IRType<'ctx> {
    fn as_meta_enum(&self, context: &'ctx Context) -> BasicMetadataTypeEnum<'ctx> {
        match &self {
            IRType::Function(..) => todo!(),
            IRType::Struct(..) => context.ptr_type(AddressSpace::from(0)).into(),
            IRType::Simple(v) => (*v).into(),
            IRType::PolyMorph => todo!(),
            IRType::BuiltIn => todo!(),
            IRType::Returned(..) => todo!(),
        }
    }

    fn as_basic_enum(&self, context: &'ctx Context) -> BasicTypeEnum<'ctx> {
        match &self {
            IRType::Function(..) => todo!(),
            IRType::Struct(..) => context.ptr_type(AddressSpace::from(0)).into(),
            IRType::Simple(v) => *v,
            IRType::PolyMorph => todo!(),
            IRType::BuiltIn => todo!(),
            IRType::Returned(..) => todo!(),
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
    Returned(Box<IRValue<'ctx>>),
}

impl<'ctx> IRValue<'ctx> {
    fn as_meta_enum(&self, _context: &'ctx Context) -> BasicMetadataValueEnum<'ctx> {
        match &self {
            IRValue::Function(..) => todo!(),
            IRValue::Struct(..) => todo!(),
            IRValue::Simple(v) => (*v).into(),
            IRValue::PolyMorph(..) => todo!(),
            IRValue::BuiltIn(..) => todo!(),
            IRValue::Returned(..) => todo!(),
        }
    }

    fn as_basic_enum(&self, _context: &'ctx Context) -> BasicValueEnum<'ctx> {
        match &self {
            IRValue::Function(f, ..) => f.as_global_value().as_basic_value_enum(),
            IRValue::Struct(..) => todo!(),
            IRValue::Simple(v) => *v,
            IRValue::PolyMorph(..) => todo!(),
            IRValue::BuiltIn(..) => todo!(),
            IRValue::Returned(..) => todo!(),
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
                IRValue::BuiltIn("println".to_string()),
                IRValue::BuiltIn("str".to_string()),
                IRValue::BuiltIn("array".to_string()),
                IRValue::BuiltIn("push".to_string()),
                IRValue::BuiltIn("set".to_string()),
            ],
            loop_bbs: None,
            // generic_structs: HashMap::new(),
        }
    }

    fn print_ir(&self) {
        debug!("\n{}", self.module.print_to_string().to_string());
    }

    fn declare_gc_functions(&self) {
        // Declare the GC_malloc function
        let malloc_type = self
            .context
            .ptr_type(AddressSpace::from(0))
            .fn_type(&[self.context.i64_type().into()], false);
        let _malloc_func = self.module.add_function("GC_malloc", malloc_type, None);

        // Declare the GC_realloc function
        let realloc_type = self.context.ptr_type(AddressSpace::from(0)).fn_type(
            &[
                self.context.ptr_type(AddressSpace::from(0)).into(),
                self.context.i64_type().into(),
            ],
            false,
        ); // maybe not needed?
        let _realloc_func = self.module.add_function("GC_realloc", realloc_type, None);

        // Declare the GC_free function
        let free_type = self.context.void_type().fn_type(
            &[self.context.ptr_type(AddressSpace::from(0)).into()],
            false,
        ); // maybe not needed?
        let _free_func = self.module.add_function("GC_free", free_type, None);
    }

    pub fn declare_utility_functions(&self) {
        // Declare the exit function
        let exit_type = self
            .context
            .void_type()
            .fn_type(&[self.context.i32_type().into()], false); // maybe not needed?
        let _exit_func = self.module.add_function("exit", exit_type, None);

        // Declare the puts function
        let puts_type = self.context.i32_type().fn_type(
            &[self.context.ptr_type(AddressSpace::from(0)).into()],
            false,
        ); // maybe not needed?
        let _puts_func = self.module.add_function("puts", puts_type, None);

        let printf_type = self
            .context
            .i32_type()
            .fn_type(&[self.context.ptr_type(AddressSpace::from(0)).into()], true);
        let _print_func = self.module.add_function("printf", printf_type, None);
    }

    pub fn gen_program(
        &mut self,
        node: &TypedNode<'ctx>,
        _span: &Span,
        _file: String,
        optimisation_level: u8,
    ) -> Result<(), String> {
        self.declare_gc_functions();
        self.declare_utility_functions();

        self.variables.insert(
            "type".to_string(),
            (IRValue::BuiltIn("type".to_string()), IRType::BuiltIn),
        );
        self.variables.insert(
            "print".to_string(),
            (IRValue::BuiltIn("print".to_string()), IRType::BuiltIn),
        );
        self.variables.insert(
            "println".to_string(),
            (IRValue::BuiltIn("println".to_string()), IRType::BuiltIn),
        );
        self.variables.insert(
            "str".to_string(),
            (IRValue::BuiltIn("str".to_string()), IRType::BuiltIn),
        );
        self.variables.insert(
            "array".to_string(),
            (IRValue::BuiltIn("array".to_string()), IRType::BuiltIn),
        );
        self.variables.insert(
            "push".to_string(),
            (IRValue::BuiltIn("push".to_string()), IRType::BuiltIn),
        );

        self.gen_extern("printint".to_string(), vec![t_int!()], t_int!())?;
        self.gen_extern("printstr".to_string(), vec![t_str!()], t_int!())?;
        self.gen_extern("printstrln".to_string(), vec![t_str!()], t_int!())?;

        self.gen_extern("int_to_str".to_string(), vec![t_int!()], t_str!())?;
        self.gen_extern("float_to_str".to_string(), vec![t_float!()], t_str!())?;
        self.gen_extern("bool_to_str".to_string(), vec![t_int!()], t_str!())?;

        self.gen_extern("concat".to_string(), vec![t_str!(), t_str!()], t_str!())?;
        self.gen_extern("strcmp".to_string(), vec![t_str!(), t_str!()], t_int!())?;
        self.gen_extern(
            "stringrepeat".to_string(),
            vec![t_str!(), t_int!()],
            t_str!(),
        )?;

        let default_triple = TargetMachine::get_default_triple();

        Target::initialize_x86(&InitializationConfig::default());

        let opt = match optimisation_level {
            0 => OptimizationLevel::None,
            1 => OptimizationLevel::Less,
            2 => OptimizationLevel::Default,
            _ => OptimizationLevel::Aggressive,
        };
        let reloc = RelocMode::PIC;
        let model = CodeModel::Default;
        let binding = self.file.clone() + ".o";
        let path = Path::new(&binding);
        let target = Target::from_name("x86-64").unwrap();
        let target_machine = target
            .create_target_machine(&default_triple, "x86-64", "+avx2", opt, reloc, model)
            .unwrap();

        // let data_layout = target_machine.get_target_data();
        let data_layout = target_machine.get_target_data().get_data_layout();
        self.module.set_data_layout(&data_layout);

        match node {
            TypedNode::Program(nodes) => {
                let mut exprs = vec![];
                for (node, _span, _file) in nodes {
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
                        TypedNode::Struct(_name, generics, fields) => {
                            let _generic_params: Vec<String> =
                                generics.iter().map(|g| g.to_string()).collect();
                            let _field_types: Vec<(String, Arc<Type>)> = fields
                                .clone()
                                .iter()
                                .map(|(fname, ftype)| (fname.to_string(), ftype.clone()))
                                .collect();
                            // self.generic_structs
                            // .insert(name.to_string(), (generic_params, field_types));
                            // println!("structs {:#?}", self.generic_structs);
                            // let mut arg_types = vec![];
                            // let mut field_types = vec![];
                            // let mut field_metadata_types = vec![];

                            // let malloc_func = self
                            //     .module
                            //     .get_function("GC_malloc")
                            //     .expect("malloc not found");

                            // for (fname, field_type) in fields {
                            //     arg_types.push((
                            //         fname.to_string(),
                            //         self.type_to_llvm(field_type.clone()),
                            //     ));
                            //     field_types.push(
                            //         self.type_to_llvm(field_type.clone())
                            //             .as_basic_enum(self.context),
                            //     );
                            //     field_metadata_types.push(
                            //         self.type_to_llvm(field_type.clone())
                            //             .as_meta_enum(self.context),
                            //     );
                            // }
                            // // println!("");

                            // let struct_type = self.context.struct_type(&field_types, false);
                            // let struct_size = struct_type.size_of().unwrap();

                            // let function_type = self
                            //     .context
                            //     .ptr_type(inkwell::AddressSpace::from(0))
                            //     .fn_type(&field_metadata_types, false);
                            // let function = self.module.add_function(
                            //     &("create_".to_string() + name),
                            //     function_type,
                            //     None,
                            // );

                            // let entry_block = self.context.append_basic_block(function, "entry");
                            // self.builder.position_at_end(entry_block);

                            // // let struct_ptr = self.builder.build_alloca(struct_type.clone(), "struct_ptr").unwrap();
                            // let struct_ptr = self
                            //     .builder
                            //     .build_call(malloc_func, &[struct_size.into()], "struct_ptr")
                            //     .unwrap()
                            //     .try_as_basic_value()
                            //     .left()
                            //     .unwrap()
                            //     .into_pointer_value();

                            // for (i, (field_name, _field_type)) in fields.iter().enumerate() {
                            //     let field_ptr = unsafe {
                            //         self.builder
                            //             .build_gep(
                            //                 struct_type,
                            //                 struct_ptr,
                            //                 &[
                            //                     self.context.i32_type().const_zero(),
                            //                     self.context.i32_type().const_int(i as u64, false),
                            //                 ],
                            //                 field_name,
                            //             )
                            //             .unwrap()
                            //     };

                            //     let field_value = function.get_nth_param(i as u32).unwrap();
                            //     self.builder.build_store(field_ptr, field_value).unwrap();
                            // }

                            // self.builder.build_return(Some(&struct_ptr)).unwrap();

                            // self.structs.insert(
                            //     name.to_string(),
                            //     (
                            //         struct_type,
                            //         fields
                            //             .iter()
                            //             .map(|(name, ty)| {
                            //                 (name.to_string(), self.type_to_llvm(ty.clone()))
                            //             })
                            //             .collect(),
                            //     ),
                            // );
                            // self.variables.insert(
                            //     name.to_string(),
                            //     (
                            //         IRValue::Function(
                            //             function,
                            //             arg_types.clone(),
                            //             Box::new(IRType::Struct(
                            //                 struct_type,
                            //                 fields
                            //                     .iter()
                            //                     .map(|(name, ty)| {
                            //                         (
                            //                             name.to_string(),
                            //                             self.type_to_llvm(ty.clone()),
                            //                         )
                            //                     })
                            //                     .collect(),
                            //             )),
                            //         ),
                            //         IRType::Function(
                            //             // function,
                            //             arg_types,
                            //             Box::new(IRType::Struct(
                            //                 struct_type,
                            //                 fields
                            //                     .iter()
                            //                     .map(|(name, ty)| {
                            //                         (
                            //                             name.to_string(),
                            //                             self.type_to_llvm(ty.clone()),
                            //                         )
                            //                     })
                            //                     .collect(),
                            //             )),
                            //         ),
                            //     ),
                            // );
                        }
                        _ => unreachable!(),
                    }
                }
                exprs.push(TypedExpr::Literal(
                    Literal::Int(0).into(),
                    tconst!("int"),
                    Span((1, 0), (1, 0)),
                    self.file.clone(),
                ));
                self.gen_function(
                    "main".to_string(),
                    vec![],
                    &Box::new(TypedExpr::Do(
                        exprs,
                        tconst!("int"),
                        Span((1, 0), (1, 0)),
                        self.file.clone(),
                    )),
                    Type::Function(vec![], tconst!("int")).into(),
                )?;
            }
            _ => unreachable!(),
        };

        match self.module.verify() {
            Ok(_) => {}
            Err(e) => return Err(format!("couldn't verify the module!. {e}")),
        };
        if optimisation_level != 0 {
            self.module
                .run_passes(
                    "tailcallelim,\
                    mem2reg,\
                    bdce,\
                    dce,\
                    dse,\
                    instcombine,\
                    consthoist,\
                    loop-deletion,\
                    loop-data-prefetch,\
                    loop-distribute,\
                    loop-flatten,\
                    loop-fusion,\
                    loop-load-elim,\
                    loop-reduce,\
                    loop-unroll,\
                    loop-rotate,\
                    loop-simplify,\
                    loop-sink,\
                    loop-vectorize,\
                    unify-loop-exits",
                    &target_machine,
                    inkwell::passes::PassBuilderOptions::create(),
                )
                .unwrap();
        }

        self.print_ir();

        let exec_name = &(self.file.clone()[..=self.file.len() - 4].to_string() + ".out");
        // println!("emitting {:?}. binding {:?}", exec_name, binding);

        assert!(target_machine
            .write_to_file(&self.module, FileType::Object, path)
            .is_ok());

        // Get the value of the environment variable or use "std.cc" as a default
        let stdlib_path = env::var("BOOK_STDLIB_PATH").unwrap_or_else(|_| "std.cc".to_string());

        let result = Command::new("gcc") // todo: make this better
            .args([
                "-O3",
                &(binding),
                &stdlib_path, // Use the resolved path here
                "-o",
                exec_name,
                "-I/usr/lib/gc",
                "-lgc",
            ])
            .output()
            .expect("failed to execute process");

        if !result.status.success() {
            eprintln!("Build failed: {result:?}");
            return Err("build failed".to_string());
        }

        Ok(())
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
                "Array" => IRType::Simple(self.context.ptr_type(AddressSpace::from(0)).into()),
                x => match self.structs.get(x) {
                    Some((ref s, ref f)) => IRType::Struct(*s, f.to_vec()),
                    None => panic!("no such struct `{x}`"),
                },
            },
            Type::Struct(name, _generics, _fields) => {
                // println!("{:?}", generics);
                match self.structs.get(name) {
                    Some((ref s, ref f)) => IRType::Struct(*s, f.to_vec()),
                    None => panic!("no such struct `{name}`"),
                }
                // IRType::Simple(self.context.ptr_type(AddressSpace::from(0)).into())
            }
            _ => panic!("Type to llvm not possible for {ty:?}"),
        }
    }
}

fn get_type_from_typed_expr(expr: &TypedExpr) -> Arc<Type> {
    match expr {
        TypedExpr::Literal(_, ty, ..) => ty.clone(),
        TypedExpr::Variable(_, ty, ..) => ty.clone(),
        TypedExpr::Lambda(_, _, ty, ..) => ty.clone(),
        TypedExpr::Let(_, _, ty, ..) => ty.clone(),
        TypedExpr::If(_, _, _, ty, ..) => ty.clone(),
        TypedExpr::Call(_, _, ty, ..) => ty.clone(),
        TypedExpr::While(_, _, ty, ..) => ty.clone(),
        TypedExpr::BinaryOp(_, _, _, ty, ..) => ty.clone(),
        TypedExpr::UnaryOp(_, _, ty, ..) => ty.clone(),
        TypedExpr::Array(_, ty, ..) => ty.clone(),
        TypedExpr::Do(_, ty, ..) => ty.clone(),
        TypedExpr::Index(_, _, ty, ..) => ty.clone(),
        TypedExpr::StructAccess(_, _, ty, ..) => ty.clone(),
        TypedExpr::Return(_, ty, ..) => ty.clone(),
        TypedExpr::Tuple(_, ty, ..) => ty.clone(),
        TypedExpr::Assign(_, _, ty, ..) => ty.clone(),
        TypedExpr::Break(..) | TypedExpr::Continue(..) => t_int!(),
    }
}

pub fn error(filename: String, span: Span, message: String) {
    match File::open(&filename) {
        Ok(file) => {
            let reader = BufReader::new(file);
            let lines: Vec<String> = reader
                .lines()
                .map(|l| l.expect("Could not read line"))
                .collect();

            let (start_line, start_col) = span.0;
            let (end_line, end_col) = span.1;

            // Adjust for 0-based indexing (lines and columns are 1-indexed)
            let start_line = (start_line - 1) as usize;
            let start_col = start_col as usize;
            let end_line = (end_line - 1) as usize;
            let end_col = end_col as usize;

            // Validate line numbers and columns
            if start_line >= lines.len()
                || end_line >= lines.len()
                || start_col > lines[start_line].len()
                || end_col > lines[end_line].len()
            {
                eprintln!(
                    "Invalid span: Line or column numbers out of bounds. {span:?} {}",
                    lines.len()
                );
                return;
            }

            // Extract relevant lines
            let pre_line = if start_line == 0 {
                Some("".to_string())
            } else {
                lines
                    .get(start_line - 1)
                    .map(|l| format!("{start_line} | {l}"))
            };
            let current_line = &lines[start_line];
            let post_line = lines
                .get(start_line + 1)
                .map(|l| format!("{} | {l}", start_line + 2));

            // Create pointy indicators
            let start_pointy = format!("{:~<width$}^", "", width = start_col + 1);
            let end_pointy = format!(
                "{:^<width$}^",
                "",
                width = if start_col > end_col {
                    1
                } else {
                    end_col - start_col
                }
            );
            let padding = format!(
                "{: <width$}  ",
                "",
                width = (start_line + 1).ilog10() as usize
            );

            // Construct the error message
            let error_message = format!(
                "\n\
                 {}\n\
                 {} | {}\n\
                 {padding}{}{}\n\
                 {}\n\
                 {}: {message}\n\
                 at line {}:{} in file `{}`.",
                pre_line.unwrap_or_default().yellow(),
                (start_line + 1).green(),
                current_line.yellow(),
                start_pointy.red(),
                end_pointy.red(),
                post_line.unwrap_or_default().yellow(),
                "[Codegen Error]".red(),
                (start_line + 1).green(),
                (start_col + 1).green(),
                filename.green(),
            );

            eprintln!("{error_message}");
        }
        Err(error) => {
            eprintln!(
                "Error: Could not open file: {}. {}",
                filename.green(),
                error
            );
        }
    }
}
