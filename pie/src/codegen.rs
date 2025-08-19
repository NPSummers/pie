use crate::ast::*;
use crate::piestd::builtins::Registry;
use crate::typecheck::walk_items;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Module as LlvmModule;
use inkwell::types::BasicTypeEnum;
use inkwell::values::{BasicValue, BasicValueEnum, FunctionValue, PointerValue};
use inkwell::AddressSpace;
use std::collections::HashMap;

pub struct CodeGen<'ctx> {
    pub context: &'ctx Context,
    pub module: LlvmModule<'ctx>,
    pub builder: Builder<'ctx>,
    pub functions: HashMap<String, FunctionValue<'ctx>>,
    pub registry: &'ctx Registry<'ctx>,
}

enum GeneratedStatement<'ctx> {
    Void,
    Return(Option<BasicValueEnum<'ctx>>),
}

impl<'ctx> CodeGen<'ctx> {
    pub fn new(context: &'ctx Context, registry: &'ctx Registry<'ctx>, name: &str) -> Self {
        let module = context.create_module(name);
        let builder = context.create_builder();
        let cg = Self {
            context,
            module,
            builder,
            functions: HashMap::new(),
            registry,
        };
        cg.declare_runtime();
        cg
    }

    pub fn declare_runtime(&self) {
        for func in self.registry.functions() {
            let func_type = func.func_type;
            self.module.add_function(func.native_name, func_type, None);
        }
    }

    pub fn compile_program(&mut self, prog: &Program) -> Result<(), String> {
        // Compile all modules and functions
        for item in &prog.items {
            let Item::Module(m) = item else {
                continue;
            };
            walk_items(&[], m, (), &mut |_, prefix, item| {
                let ModuleItem::Function(f) = &item else {
                    return;
                };
                let mpath = prefix.join("__");
                self.compile_function(&mpath, f);
            })
        }

        // Create a wrapper function named "main" that returns i32
        let pie_main_name = format!("{}__{}", "main", "main");
        if let Some(pie_main) = self.functions.get(&pie_main_name) {
            // Create a new function named "main" with the correct signature (returns i32)
            let main_type = self.context.i32_type().fn_type(&[], false);
            let main_fn = self.module.add_function("main", main_type, None);

            let entry = self.context.append_basic_block(main_fn, "entry");
            self.builder.position_at_end(entry);

            // Call the PIE main function
            let _ = self.builder.build_call(*pie_main, &[], "call_main");

            // Return 0
            let ret_val = self.context.i32_type().const_int(0, false);
            let _ = self.builder.build_return(Some(&ret_val));
        } else {
            return Err("PIE main function not found after compilation".into());
        }

        self.module.print_to_stderr();

        // Verify the module
        if let Err(errors) = self.module.verify() {
            return Err(format!("LLVM module verification failed: {}", errors));
        }

        Ok(())
    }

    fn compile_function(&mut self, module_name: &str, func: &Function) {
        let ptr_type = self.context.ptr_type(AddressSpace::from(0u16));
        let param_types: Vec<BasicTypeEnum> = func.params.iter().map(|_| ptr_type.into()).collect();

        // choose return type based on function declaration
        let fn_type = if matches!(func.ret, TypeName::Void) {
            self.context.void_type().fn_type(
                &param_types.iter().map(|t| (*t).into()).collect::<Vec<_>>(),
                false,
            )
        } else {
            ptr_type.fn_type(
                &param_types.iter().map(|t| (*t).into()).collect::<Vec<_>>(),
                false,
            )
        };

        let name = format!("{}__{}", module_name, func.name);
        // compiled function: name
        let fnv = self.module.add_function(&name, fn_type, None);
        self.functions.insert(name.clone(), fnv);

        // build entry and body
        let entry = self.context.append_basic_block(fnv, "entry");
        self.builder.position_at_end(entry);

        let mut locals: HashMap<&str, PointerValue<'ctx>> = HashMap::new();
        for (idx, (_t, pname)) in func.params.iter().enumerate() {
            let alloca = self.builder.build_alloca(ptr_type, pname).expect("alloca");
            let param = fnv.get_nth_param(idx as u32).unwrap();
            let _ = self.builder.build_store(alloca, param);
            locals.insert(pname, alloca);
        }

        let mut has_return = false;

        for stmt in &func.body {
            match self.codegen_stmt(&mut locals, func, fnv, stmt) {
                GeneratedStatement::Void => (),
                GeneratedStatement::Return(_) => {
                    has_return = true;
                    break;
                }
            }
        }

        if !has_return {
            if matches!(func.ret, TypeName::Void) {
                let _ = self.builder.build_return(None);
            } else {
                let null = ptr_type.const_null();
                let null_val = null.as_basic_value_enum();
                let _ = self.builder.build_return(Some(&null_val));
            }
        }
    }

    fn codegen_stmt<'a>(
        &self,
        locals: &mut HashMap<&'a str, PointerValue<'ctx>>,
        func: &Function,
        fnv: FunctionValue<'ctx>,
        stmt: &Statement<'a>,
    ) -> GeneratedStatement<'ctx> {
        match stmt {
            Statement::For {
                iterable,
                var,
                body,
            } => {
                let iterable = self
                    .codegen_expr(iterable, locals)
                    .expect("Cannot iterate over a void expression");
                let iter_new = self.module.get_function("pie_iter_new").unwrap();
                let iterable = self
                    .builder
                    .build_call(iter_new, &[iterable.into()], "iternew")
                    .expect("create call to create iterator");
                let iterable = iterable.try_as_basic_value().left().unwrap();
                // let iterable_alloca = self
                //     .builder
                //     .build_alloca(self.registry.ptr_type(), var)
                //     .unwrap();
                // self.builder.build_store(iterable_alloca, iterable).unwrap();

                let loop_bb = self.context.append_basic_block(fnv, "for_loop");
                self.builder.build_unconditional_branch(loop_bb).unwrap();
                self.builder.position_at_end(loop_bb);

                let iter_next = self.module.get_function("pie_iter_next").unwrap();
                let next = self
                    .builder
                    .build_call(iter_next, &[iterable.into()], "iternext")
                    .unwrap()
                    .try_as_basic_value()
                    .left()
                    .unwrap()
                    .into_pointer_value();
                let var_alloca = self
                    .builder
                    .build_alloca(self.registry.ptr_type(), var)
                    .unwrap();
                self.builder.build_store(var_alloca, next).unwrap();

                let is_null = self.builder.build_is_null(next, "iter_is_null").unwrap();
                let loop_hasnext = self.context.append_basic_block(fnv, "for_loop_hasnext");
                let loop_after = self.context.append_basic_block(fnv, "after_for");
                self.builder
                    .build_conditional_branch(is_null, loop_after, loop_hasnext)
                    .unwrap();

                self.builder.position_at_end(loop_hasnext);

                locals.insert(var, var_alloca);
                for stmt in body {
                    self.codegen_stmt(locals, func, fnv, stmt);
                }
                self.builder.build_unconditional_branch(loop_bb).unwrap();
                locals.remove(var);
                self.builder.position_at_end(loop_after);
                GeneratedStatement::Void
            }
            Statement::Let {
                typ: _typ,
                name,
                expr,
            } => {
                // TODO: Handle expressions that don't evaluate to a value
                if let Some(val) = self.codegen_expr(expr, locals) {
                    let alloca = self
                        .builder
                        .build_alloca(self.registry.ptr_type(), name)
                        .expect("alloca");
                    let _ = self.builder.build_store(alloca, val);
                    locals.insert(name, alloca);
                }
                GeneratedStatement::Void
            }
            Statement::Assignment { target, op, value } => {
                // Load the current value of the target
                let Expression::Ident(name) = target else {
                    panic!("Codegen for assignment to expressions that aren't identifiers is not supported yet")
                };
                let ptr = locals.get(name).cloned().unwrap();

                let current_val = self
                    .builder
                    .build_load(self.registry.ptr_type(), ptr, "current_val")
                    .expect("load");
                let value_val = self.codegen_expr(value, locals).expect("value expression");

                // Handle different assignment operators
                let result = match op {
                    AssignOp::Plus => {
                        let fn_add = self.module.get_function("pie_add").unwrap();
                        let call = self
                            .builder
                            .build_call(
                                fn_add,
                                &[current_val.into(), value_val.into()],
                                "add_result",
                            )
                            .expect("call");
                        call.try_as_basic_value().left().unwrap()
                    }
                    AssignOp::Minus => {
                        let fn_sub = self.module.get_function("pie_sub").unwrap();
                        let call = self
                            .builder
                            .build_call(
                                fn_sub,
                                &[current_val.into(), value_val.into()],
                                "sub_result",
                            )
                            .expect("call");
                        call.try_as_basic_value().left().unwrap()
                    }
                    AssignOp::Star => {
                        let fn_mul = self.module.get_function("pie_mul").unwrap();
                        let call = self
                            .builder
                            .build_call(
                                fn_mul,
                                &[current_val.into(), value_val.into()],
                                "mul_result",
                            )
                            .expect("call");
                        call.try_as_basic_value().left().unwrap()
                    }
                    AssignOp::Slash => {
                        let fn_div = self.module.get_function("pie_div").unwrap();
                        let call = self
                            .builder
                            .build_call(
                                fn_div,
                                &[current_val.into(), value_val.into()],
                                "sub_result",
                            )
                            .expect("call");
                        call.try_as_basic_value().left().unwrap()
                    }
                    AssignOp::Assign => value_val,
                };

                // Store the result back to the target
                let _ = self.builder.build_store(ptr, result);
                GeneratedStatement::Void
            }
            Statement::Expr(e) => {
                let _ = self.codegen_expr(e, locals);
                GeneratedStatement::Void
            }
            Statement::Return(Some(e)) => {
                let Some(v) = self.codegen_expr(e, locals) else {
                    panic!("Attempted to codegen a return with an expression that evaluates to nothing")
                };
                if matches!(func.ret, TypeName::Void) {
                    self.builder.build_return(None).unwrap();
                } else {
                    self.builder.build_return(Some(&v)).unwrap();
                }
                GeneratedStatement::Return(Some(v))
            }
            Statement::Return(None) => {
                let _ = self.builder.build_return(None);
                GeneratedStatement::Return(None)
            }
        }
    }

    fn codegen_expr(
        &self,
        expr: &Expression,
        locals: &HashMap<&str, PointerValue<'ctx>>,
    ) -> Option<BasicValueEnum<'ctx>> {
        let ptr_type = self.context.ptr_type(AddressSpace::from(0u16));
        match expr {
            &Expression::Int(i) => {
                let fn_int_new = self.module.get_function("pie_int_new").unwrap();
                let cv = self.context.i64_type().const_int(i as u64, true);
                let call = self
                    .builder
                    .build_call(fn_int_new, &[cv.into()], "newint")
                    .expect("call");
                Some(call.try_as_basic_value().left().unwrap())
            }
            &Expression::Float(f) => {
                let fn_float_new = self.module.get_function("pie_float_new").unwrap();
                let cv = self.context.f64_type().const_float(f);
                let call = self
                    .builder
                    .build_call(fn_float_new, &[cv.into()], "newfloat")
                    .expect("call");
                Some(call.try_as_basic_value().left().unwrap())
            }
            &Expression::Bool(b) => {
                let fn_bool_new = self.module.get_function("pie_bool_new").unwrap();
                let cv = self.context.i8_type().const_int(b as u64, false);
                let call = self
                    .builder
                    .build_call(fn_bool_new, &[cv.into()], "newbool")
                    .expect("call");
                Some(call.try_as_basic_value().left().unwrap())
            }
            Expression::Str(s) => {
                let gv = self
                    .builder
                    .build_global_string_ptr(s, "gstr")
                    .expect("gstr");
                let ptr = gv.as_pointer_value();
                let fn_str_new = self.module.get_function("pie_string_new").unwrap();
                let len = self.context.i64_type().const_int(s.len() as u64, false);
                let call = self
                    .builder
                    .build_call(fn_str_new, &[ptr.into(), len.into()], "newstr")
                    .expect("call");
                Some(call.try_as_basic_value().left().unwrap())
            }
            Expression::Ident(name) => {
                let Some(ptr) = locals.get(name) else {
                    panic!("Attempted to codegen a load to non-existing ident {name}")
                };
                let loaded = self
                    .builder
                    .build_load(ptr_type, *ptr, &format!("load_{}", name))
                    .expect("load");
                Some(loaded)
            }
            Expression::Binary(lhs, op, rhs) => {
                // evaluate both sides
                let left_val = self.codegen_expr(lhs, locals)?;
                let right_val = self.codegen_expr(rhs, locals)?;

                let fn_name = match op {
                    BinaryOp::Add => "pie_add",
                    BinaryOp::Sub => "pie_sub",
                    BinaryOp::Mul => "pie_mul",
                    BinaryOp::Div => "pie_div",
                };

                let f = self.module.get_function(fn_name).unwrap();

                let call = self
                    .builder
                    .build_call(f, &[left_val.into(), right_val.into()], "binop")
                    .expect("call");
                call.try_as_basic_value().left()
            }
            Expression::Unary(op, expr) => {
                let val = self
                    .codegen_expr(expr, locals)
                    .expect("right hand side of a unary expression to be non-void");
                let fn_name = match op {
                    UnaryOp::Add => "pie_unary_add",
                    UnaryOp::Sub => "pie_unary_sub",
                    UnaryOp::Not => "pie_unary_not",
                };
                let f = self.module.get_function(fn_name).unwrap();

                let call = self
                    .builder
                    .build_call(f, &[val.into()], "binop")
                    .expect("call");
                call.try_as_basic_value().left()
            }
            Expression::Call { callee, args } => {
                match &**callee {
                    Expression::ModuleAccess { components } => {
                        // fallback: module function lookup (module::name -> module__name)
                        let fname = components.join("__");

                        if let Some(native_func) = self.registry.get_by_std_path(components) {
                            let llvm_func =
                                self.module.get_function(native_func.native_name).unwrap();
                            let args: Vec<_> = args
                                .iter()
                                .map(|e| self.codegen_expr(e, locals).unwrap().into())
                                .collect();
                            let call = self
                                .builder
                                .build_call(llvm_func, &args, &fname)
                                .expect("call");
                            return call.try_as_basic_value().left();
                        }

                        if let Some(f) = self.functions.get(&fname) {
                            let mut compiled_args = Vec::new();
                            for a in args {
                                if let Some(av) = self.codegen_expr(a, locals) {
                                    compiled_args.push(av.into());
                                } else {
                                    compiled_args.push(ptr_type.const_null().into());
                                }
                            }
                            let call = self
                                .builder
                                .build_call(*f, &compiled_args, "call")
                                .expect("call");
                            if call.try_as_basic_value().left().is_some() {
                                let ret_val = call.try_as_basic_value().left().unwrap();
                                return Some(ret_val);
                            }
                        }
                        panic!("Attempted to call an unknown function {fname}")
                    }
                    Expression::Ident(fname) => {
                        if let Some(f) = self.functions.get(*fname) {
                            let mut compiled_args = Vec::new();
                            for a in args {
                                if let Some(av) = self.codegen_expr(a, locals) {
                                    compiled_args.push(av.into());
                                } else {
                                    compiled_args.push(ptr_type.const_null().into());
                                }
                            }
                            let call = self
                                .builder
                                .build_call(*f, &compiled_args, "call")
                                .expect("call");
                            if call.try_as_basic_value().left().is_some() {
                                return Some(call.try_as_basic_value().left().unwrap());
                            }
                        }
                        panic!("Attempted to call a function {fname} that could not be found")
                    }
                    expr => panic!("Codegen for {expr:#?} has not been implemented yet"),
                }
            }
            expr => panic!("Codegen for {expr:#?} has not been implemented yet"),
        }
    }
}
