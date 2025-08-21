use crate::ast::*;
use crate::piestd::builtins::Registry;
use crate::typecheck::walk_items;
use inkwell::basic_block::BasicBlock;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Module as LlvmModule;
use inkwell::types::BasicTypeEnum;
use inkwell::values::{BasicValue, BasicValueEnum, FunctionValue, InstructionValue, PointerValue};
use inkwell::AddressSpace;
use std::collections::HashMap;
use std::ops::Deref;

pub struct CodeGen<'ctx> {
    pub context: &'ctx Context,
    pub module: LlvmModule<'ctx>,
    pub builder: Builder<'ctx>,
    pub functions: HashMap<String, FunctionValue<'ctx>>,
    pub registry: &'ctx Registry<'ctx>,
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

    fn build_inc_ref(&self, val: BasicValueEnum<'ctx>) {
        let f = self.module.get_function("pie_inc_ref").expect("inc_ref");
        let ptr = val.into_pointer_value();
        self.builder
            .build_call(f, &[ptr.into()], "inc_ref_call")
            .unwrap();
    }

    fn build_dec_ref(&self, val: BasicValueEnum<'ctx>) {
        let f = self.module.get_function("pie_dec_ref").expect("dec_ref");
        let ptr = val.into_pointer_value();
        self.builder
            .build_call(f, &[ptr.into()], "dec_ref_call")
            .unwrap();
    }

    fn store_owned_ptr(
        &self,
        dest: PointerValue<'ctx>,
        new_val: BasicValueEnum<'ctx>,
        new_val_is_ident: bool,
    ) {
        // Release previous value stored in dest (if any)
        let old = self
            .builder
            .build_load(self.registry.ptr_type(), dest, "old_val")
            .expect("load old");
        self.build_dec_ref(old);
        // If we are copying from an identifier (another owning slot), bump refcount
        if new_val_is_ident {
            self.build_inc_ref(new_val);
        }
        self.builder.build_store(dest, new_val).unwrap();
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
            self.builder
                .build_call(*pie_main, &[], "call_main")
                .unwrap();

            // Return 0
            let ret_val = self.context.i32_type().const_int(0, false);
            self.builder.build_return(Some(&ret_val)).unwrap();
        } else {
            return Err("PIE main function not found after compilation".into());
        }

        #[cfg(debug_assertions)]
        self.module.print_to_stderr();

        // Verify the module
        if let Err(errors) = self.module.verify() {
            return Err(format!(
                "LLVM module verification failed: {}",
                errors.to_string_lossy()
            ));
        }

        Ok(())
    }

    fn get_current_block_terminator(&self) -> Option<InstructionValue<'ctx>> {
        let block = self.builder.get_insert_block()?;
        block.get_terminator()
    }

    fn find_variables<'a>(
        &self,
        locals: &mut HashMap<&'a str, PointerValue<'ctx>>,
        stmts: impl Iterator<Item = &'a Statement<'a>>,
    ) {
        let new_var = |locals: &mut HashMap<&'a str, PointerValue<'ctx>>, name| {
            let ptr = self
                .builder
                .build_alloca(self.registry.ptr_type(), name)
                .unwrap();
            // Initialize to null so releases are safe
            let null = self.registry.ptr_type().const_null().as_basic_value_enum();
            self.builder.build_store(ptr, null).unwrap();
            locals.insert(name, ptr);
        };
        for stmt in stmts {
            match stmt {
                Statement::For { var, body, .. } => {
                    new_var(locals, var);
                    self.find_variables(locals, body.iter());
                }
                Statement::Let { name, .. } => {
                    new_var(locals, name);
                }
                Statement::While { body, .. } => {
                    self.find_variables(locals, body.iter());
                }
                Statement::If {
                    then_body,
                    else_body,
                    ..
                } => {
                    self.find_variables(locals, then_body.iter());
                    if let Some(else_body) = else_body {
                        self.find_variables(locals, else_body.iter());
                    }
                }
                Statement::Assignment { target, .. } => {
                    let Expression::Ident(var) = target else {
                        panic!("Codegen of assignment with non-identifier left hand side is not supported")
                    };
                    new_var(locals, var);
                }
                Statement::Expr(_) | Statement::Return(_) => (),
            }
        }
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

        let retval = (!matches!(func.ret, TypeName::Void)).then(|| {
            self.builder
                .build_alloca(self.registry.ptr_type(), "pie_internal_retval")
                .unwrap()
        });

        let mut locals: HashMap<&str, PointerValue<'ctx>> = HashMap::new();
        for (idx, (_t, pname)) in func.params.iter().enumerate() {
            let alloca = self.builder.build_alloca(ptr_type, pname).expect("alloca");
            // Initialize to null for safety, then store parameter (after inc_ref)
            let null = ptr_type.const_null().as_basic_value_enum();
            self.builder.build_store(alloca, null).unwrap();
            let param = fnv.get_nth_param(idx as u32).unwrap();
            self.build_inc_ref(param);
            self.builder.build_store(alloca, param).unwrap();
            locals.insert(pname, alloca);
        }
        self.find_variables(&mut locals, func.body.iter());

        let body = self.context.append_basic_block(fnv, "body");
        let return_cleanup = self.context.append_basic_block(fnv, "return_cleanup");

        self.builder.build_unconditional_branch(body).unwrap();
        self.builder.position_at_end(body);

        for stmt in &func.body {
            self.codegen_stmt(&mut locals, func, fnv, return_cleanup, retval, stmt);
        }
        let has_term = self
            .builder
            .get_insert_block()
            .and_then(|b| b.get_last_instruction())
            .is_some_and(|i| i.is_terminator());
        if !has_term {
            self.builder
                .build_unconditional_branch(return_cleanup)
                .unwrap();
        }

        let current_block = self.builder.get_insert_block().unwrap();
        // Make return the final block
        return_cleanup.move_after(current_block).unwrap();
        self.builder.position_at_end(return_cleanup);

        // Clean up locals
        for (var, ptr) in locals {
            let rc = self
                .builder
                .build_load(self.registry.ptr_type(), ptr, var)
                .unwrap();
            self.build_dec_ref(rc);
        }

        if let Some(retval) = retval {
            let retval = self
                .builder
                .build_load(self.registry.ptr_type(), retval, "retval")
                .unwrap();
            self.builder.build_return(Some(&retval)).unwrap();
        } else {
            self.builder.build_return(None).unwrap();
        }
    }

    fn codegen_stmt<'a>(
        &self,
        locals: &mut HashMap<&'a str, PointerValue<'ctx>>,
        func: &Function,
        fnv: FunctionValue<'ctx>,
        return_block: BasicBlock<'ctx>,
        retval_ptr: Option<PointerValue<'ctx>>,
        stmt: &Statement<'a>,
    ) {
        match stmt {
            Statement::For {
                iterable,
                var,
                body,
            } => {
                // Special-case: numeric range loops compile to non-allocating integer loop
                'specialize: {
                    let Expression::Call { callee, args } = iterable else {
                        break 'specialize;
                    };
                    let Expression::ModuleAccess { components } = &**callee else {
                        break 'specialize;
                    };
                    if components.as_slice() == ["std", "iter", "range"] && args.len() == 3 {
                        let ptr_t = self.registry.ptr_type();
                        let i64_t = self.context.i64_type();

                        let fn_int_to_i64 = self.module.get_function("pie_int_to_i64").unwrap();
                        let fn_int_new = self.module.get_function("pie_int_new").unwrap();
                        let fn_int_set_in_place =
                            self.module.get_function("pie_int_set_in_place").unwrap();

                        // Compile args and extract i64s; dec_ref temps if needed
                        let mut compiled_args = Vec::with_capacity(3);
                        let mut should_dec = Vec::with_capacity(3);
                        for a in args.iter() {
                            let is_ident = matches!(a, Expression::Ident(_));
                            let av = self.codegen_expr(a, locals).expect("range arg expr");
                            compiled_args.push((av, is_ident));
                            should_dec.push(!is_ident);
                        }

                        let start_i = self
                            .builder
                            .build_call(fn_int_to_i64, &[compiled_args[0].0.into()], "start_i")
                            .expect("call");
                        let start_i = start_i
                            .try_as_basic_value()
                            .left()
                            .unwrap()
                            .into_int_value();
                        let stop_i = self
                            .builder
                            .build_call(fn_int_to_i64, &[compiled_args[1].0.into()], "stop_i")
                            .expect("call");
                        let stop_i = stop_i.try_as_basic_value().left().unwrap().into_int_value();
                        let step_i = self
                            .builder
                            .build_call(fn_int_to_i64, &[compiled_args[2].0.into()], "step_i")
                            .expect("call");
                        let step_i = step_i.try_as_basic_value().left().unwrap().into_int_value();

                        // Dec refs on temporary arg values
                        for (idx, dec) in should_dec.into_iter().enumerate() {
                            if dec {
                                self.build_dec_ref(compiled_args[idx].0);
                            }
                        }

                        let var_alloca = *locals
                            .get(var)
                            .expect("a variable to have been previously allocated with alloca");

                        // Create loop variable GcBox and store into var slot
                        let init_box = self
                            .builder
                            .build_call(fn_int_new, &[start_i.into()], "for_range_var_init")
                            .expect("int new");
                        let init_box = init_box.try_as_basic_value().left().unwrap();
                        self.store_owned_ptr(var_alloca, init_box, false);

                        // Hoist loop index to function entry
                        let i_alloca = {
                            let block = self.builder.get_insert_block().unwrap();
                            let entry = block
                                .get_parent()
                                .unwrap()
                                .get_basic_block_iter()
                                .find(|b| b.get_name() == c"entry")
                                .unwrap();
                            let jumpout = entry.get_last_instruction().unwrap();
                            self.builder.position_before(&jumpout);
                            let ptr = self
                                .builder
                                .build_alloca(i64_t, "loop_index")
                                .expect("alloca loop_index");
                            self.builder.position_at_end(block);
                            ptr
                        };
                        self.builder.build_store(i_alloca, start_i).unwrap();

                        // Build loop blocks
                        let loop_header = self.context.append_basic_block(fnv, "for_range_header");
                        let loop_then = self.context.append_basic_block(fnv, "for_range_then");
                        let loop_after = self.context.append_basic_block(fnv, "for_range_after");
                        self.builder
                            .build_unconditional_branch(loop_header)
                            .unwrap();
                        self.builder.position_at_end(loop_header);

                        // Compute condition: (step>0 && i<stop) || (step<=0 && i>stop)
                        let i_cur = self
                            .builder
                            .build_load(i64_t, i_alloca, "i_cur")
                            .unwrap()
                            .into_int_value();
                        let step_pos = self
                            .builder
                            .build_int_compare(
                                inkwell::IntPredicate::SGT,
                                step_i,
                                i64_t.const_zero(),
                                "step_pos",
                            )
                            .unwrap();
                        let step_neg = self
                            .builder
                            .build_int_compare(
                                inkwell::IntPredicate::SLE,
                                step_i,
                                i64_t.const_zero(),
                                "step_le",
                            )
                            .unwrap();
                        let cmp_lt = self
                            .builder
                            .build_int_compare(
                                inkwell::IntPredicate::SLT,
                                i_cur,
                                stop_i,
                                "i_lt_stop",
                            )
                            .unwrap();
                        let cmp_gt = self
                            .builder
                            .build_int_compare(
                                inkwell::IntPredicate::SGT,
                                i_cur,
                                stop_i,
                                "i_gt_stop",
                            )
                            .unwrap();
                        let cond1 = self.builder.build_and(step_pos, cmp_lt, "cond1").unwrap();
                        let cond2 = self.builder.build_and(step_neg, cmp_gt, "cond2").unwrap();
                        let cond = self.builder.build_or(cond1, cond2, "loop_cond").unwrap();
                        self.builder
                            .build_conditional_branch(cond, loop_then, loop_after)
                            .unwrap();

                        // Then block: set loop var value and body
                        self.builder.position_at_end(loop_then);
                        // load loop var ptr
                        let var_ptr = self
                            .builder
                            .build_load(ptr_t, var_alloca, "load_for_var")
                            .unwrap();
                        self.builder
                            .build_call(
                                fn_int_set_in_place,
                                &[var_ptr.into(), i_cur.into()],
                                "set_loop_var",
                            )
                            .unwrap();

                        for stmt in body {
                            self.codegen_stmt(locals, func, fnv, return_block, retval_ptr, stmt);
                        }
                        // i += step
                        let i_next = self.builder.build_int_add(i_cur, step_i, "i_next").unwrap();
                        self.builder.build_store(i_alloca, i_next).unwrap();
                        self.builder
                            .build_unconditional_branch(loop_header)
                            .unwrap();

                        // After block
                        self.builder.position_at_end(loop_after);
                        // Release the final value held in the loop variable slot
                        self.store_owned_ptr(
                            var_alloca,
                            self.registry.ptr_type().const_null().into(),
                            false,
                        );
                        return;
                    }
                }

                // Fallback: generic iterator-based for
                let iterable_is_ident = matches!(iterable, Expression::Ident(_));
                let iterable_val = self
                    .codegen_expr(iterable, locals)
                    .expect("Cannot iterate over a void expression");
                let iter_new = self.module.get_function("pie_iter_new").unwrap();
                let iterable = self
                    .builder
                    .build_call(iter_new, &[iterable_val.into()], "iternew")
                    .expect("create call to create iterator");
                let iterable = iterable.try_as_basic_value().left().unwrap();
                if !iterable_is_ident {
                    self.build_dec_ref(iterable_val);
                }

                let var_alloca = *locals
                    .get(var)
                    .expect("a variable to have been previously allocated with alloca");

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
                let next_bv = next.as_basic_value_enum();
                self.store_owned_ptr(var_alloca, next_bv, false);

                let is_null = self.builder.build_is_null(next, "iter_is_null").unwrap();
                let loop_hasnext = self.context.append_basic_block(fnv, "for_loop_hasnext");
                let loop_after = self.context.append_basic_block(fnv, "after_for");
                self.builder
                    .build_conditional_branch(is_null, loop_after, loop_hasnext)
                    .unwrap();

                self.builder.position_at_end(loop_hasnext);

                for stmt in body {
                    self.codegen_stmt(locals, func, fnv, return_block, retval_ptr, stmt);
                }
                self.builder.build_unconditional_branch(loop_bb).unwrap();
                self.builder.position_at_end(loop_after);
                let for_var_final = self
                    .builder
                    .build_load(self.registry.ptr_type(), var_alloca, "for_var_final")
                    .expect("load for var at loop end");
                self.build_dec_ref(for_var_final);
                self.build_dec_ref(iterable);
            }
            Statement::While { cond, body } => {
                let while_bb = self.context.append_basic_block(fnv, "while_loop");
                self.builder.build_unconditional_branch(while_bb).unwrap();
                self.builder.position_at_end(while_bb);
                let cond_is_ident = matches!(cond, Expression::Ident(_));
                let cond = self
                    .codegen_expr(cond, locals)
                    .expect("a non-void condition in a while loop");
                let truthy = self.module.get_function("pie_internal_truthy").unwrap();
                let truthy = self
                    .builder
                    .build_call(truthy, &[cond.into()], "while_cond_truthy")
                    .unwrap()
                    .try_as_basic_value()
                    .left()
                    .unwrap()
                    .into_pointer_value();
                // Release the temporary produced by the condition expression if it wasn't an identifier
                if !cond_is_ident {
                    self.build_dec_ref(cond);
                }
                let is_false = self
                    .builder
                    .build_is_null(truthy, "while_is_false")
                    .unwrap();
                let while_then = self.context.append_basic_block(fnv, "while_loop_then");
                let while_after = self.context.append_basic_block(fnv, "while_loop_after");
                self.builder
                    .build_conditional_branch(is_false, while_after, while_then)
                    .unwrap();
                self.builder.position_at_end(while_then);
                for stmt in body {
                    self.codegen_stmt(locals, func, fnv, return_block, retval_ptr, stmt);
                }
                self.builder.build_unconditional_branch(while_bb).unwrap();
                self.builder.position_at_end(while_after);
            }
            Statement::If {
                cond,
                then_body,
                else_body,
            } => {
                let cond_is_ident = matches!(cond, Expression::Ident(_));
                let cond_val = self
                    .codegen_expr(cond, locals)
                    .expect("a non-void condition in an if statement");
                let truthy = self.module.get_function("pie_internal_truthy").unwrap();
                let truthy = self
                    .builder
                    .build_call(truthy, &[cond_val.into()], "if_cond_truthy")
                    .unwrap()
                    .try_as_basic_value()
                    .left()
                    .unwrap()
                    .into_pointer_value();
                if !cond_is_ident {
                    self.build_dec_ref(cond_val);
                }

                let current_block = self.builder.get_insert_block().unwrap();
                let fnv = current_block.get_parent().unwrap();
                let then_bb = self.context.append_basic_block(fnv, "if_then");
                let else_bb = self.context.append_basic_block(fnv, "if_else");
                let cont_bb = self.context.append_basic_block(fnv, "if_cont");
                let is_false = self.builder.build_is_null(truthy, "if_is_false").unwrap();
                self.builder
                    .build_conditional_branch(is_false, else_bb, then_bb)
                    .unwrap();

                // then
                self.builder.position_at_end(then_bb);
                for s in then_body {
                    self.codegen_stmt(locals, func, fnv, return_block, retval_ptr, s);
                }
                if self.get_current_block_terminator().is_none() {
                    self.builder.build_unconditional_branch(cont_bb).unwrap();
                }

                // else
                self.builder.position_at_end(else_bb);
                if let Some(else_body) = else_body {
                    for s in else_body {
                        self.codegen_stmt(locals, func, fnv, return_block, retval_ptr, s);
                    }
                }
                if self.get_current_block_terminator().is_none() {
                    self.builder.build_unconditional_branch(cont_bb).unwrap();
                }

                // cont
                self.builder.position_at_end(cont_bb);
            }
            Statement::Let {
                typ: _typ,
                name,
                expr,
            } => {
                // TODO: Handle expressions that don't evaluate to a value
                if let Some(val) = self.codegen_expr(expr, locals) {
                    let alloca = *locals
                        .get(name)
                        .expect("a variable to have been previously allocated with alloca");
                    let rhs_is_ident = matches!(expr, Expression::Ident(_));
                    self.store_owned_ptr(alloca, val, rhs_is_ident);
                    locals.insert(name, alloca);
                }
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
                let value_is_ident = matches!(value, Expression::Ident(_));
                let value_val = self.codegen_expr(value, locals).expect("value expression");

                // Handle different assignment operators
                match op {
                    AssignOp::Assign => {
                        // Transfer/move ownership into the target slot. If RHS is an identifier,
                        // bump its refcount because two owning slots will now point to it.
                        self.store_owned_ptr(ptr, value_val, value_is_ident);
                        // Do NOT dec_ref the RHS here: it has been stored into the target.
                    }
                    AssignOp::Plus => {
                        let f = self.module.get_function("pie_add_in_place").unwrap();
                        self.builder
                            .build_call(f, &[current_val.into(), value_val.into()], "add_ip")
                            .unwrap();
                        if !value_is_ident {
                            self.build_dec_ref(value_val);
                        }
                    }
                    AssignOp::Minus => {
                        let f = self.module.get_function("pie_sub_in_place").unwrap();
                        self.builder
                            .build_call(f, &[current_val.into(), value_val.into()], "sub_ip")
                            .unwrap();
                        if !value_is_ident {
                            self.build_dec_ref(value_val);
                        }
                    }
                    AssignOp::Star => {
                        let f = self.module.get_function("pie_mul_in_place").unwrap();
                        self.builder
                            .build_call(f, &[current_val.into(), value_val.into()], "mul_ip")
                            .unwrap();
                        if !value_is_ident {
                            self.build_dec_ref(value_val);
                        }
                    }
                    AssignOp::Slash => {
                        let f = self.module.get_function("pie_div_in_place").unwrap();
                        self.builder
                            .build_call(f, &[current_val.into(), value_val.into()], "div_ip")
                            .unwrap();
                        if !value_is_ident {
                            self.build_dec_ref(value_val);
                        }
                    }
                }
            }
            Statement::Expr(e) => {
                if let Some(v) = self.codegen_expr(e, locals) {
                    // Discarding an owned temporary: release it
                    self.build_dec_ref(v);
                }
            }
            Statement::Return(Some(e)) => {
                let Some(v) = self.codegen_expr(e, locals) else {
                    panic!("Attempted to codegen a return with an expression that evaluates to nothing")
                };
                if !matches!(func.ret, TypeName::Void) {
                    // Return transfers ownership to the caller; bump refcount
                    self.build_inc_ref(v);
                    self.builder.build_store(retval_ptr.unwrap(), v).unwrap();
                }
                self.builder
                    .build_unconditional_branch(return_block)
                    .unwrap();
            }
            Statement::Return(None) => {
                self.builder
                    .build_unconditional_branch(return_block)
                    .unwrap();
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
            Expression::MemberAccess { components } => {
                // Lower a.b.c to chained std::map::get(a, "b")["c"]
                let map_get = self.module.get_function("pie_map_get").unwrap();
                let Some(first) = components.first() else {
                    return None;
                };
                let Some(base_ptr) = locals.get(first) else {
                    panic!("unknown identifier in member access: {first}");
                };
                let ptr_type = self.context.ptr_type(AddressSpace::from(0u16));
                let mut current = self
                    .builder
                    .build_load(ptr_type, *base_ptr, "member_base")
                    .expect("load");
                for key in components.iter().skip(1) {
                    let bytes = (*key).as_bytes();
                    let g = self.module.add_global(
                        self.context.i8_type().array_type(bytes.len() as u32),
                        None,
                        "gstr",
                    );
                    g.set_initializer(&self.context.const_string(bytes, false));
                    let ptr = g.as_pointer_value();
                    let fn_str_new = self.module.get_function("pie_string_new_static").unwrap();
                    let len = self.context.i64_type().const_int(bytes.len() as u64, false);
                    let key_box = self
                        .builder
                        .build_call(fn_str_new, &[ptr.into(), len.into()], "memb_key")
                        .expect("call")
                        .try_as_basic_value()
                        .left()
                        .unwrap();
                    let call = self
                        .builder
                        .build_call(map_get, &[current.into(), key_box.into()], "mget_expr")
                        .unwrap();
                    self.build_dec_ref(key_box);
                    let loaded = call.try_as_basic_value().left().unwrap();
                    self.build_dec_ref(current);
                    current = loaded;
                }
                Some(current)
            }
            Expression::StructLiteral { path: _, fields } => {
                // Lower to a map literal at runtime
                let map_new = self.module.get_function("pie_map_new").unwrap();
                let map_set = self.module.get_function("pie_map_set").unwrap();
                let map = self
                    .builder
                    .build_call(map_new, &[], "struct_lit_new")
                    .unwrap()
                    .try_as_basic_value()
                    .left()
                    .unwrap();
                for (key, value) in fields {
                    // materialize key as boxed string
                    let bytes = (*key).as_bytes();
                    let g = self.module.add_global(
                        self.context.i8_type().array_type(bytes.len() as u32),
                        None,
                        "gstr",
                    );
                    g.set_initializer(&self.context.const_string(bytes, false));
                    let ptr = g.as_pointer_value();
                    let fn_str_new = self.module.get_function("pie_string_new_static").unwrap();
                    let len = self.context.i64_type().const_int(bytes.len() as u64, false);
                    let key_box = self
                        .builder
                        .build_call(fn_str_new, &[ptr.into(), len.into()], "struct_key")
                        .expect("call")
                        .try_as_basic_value()
                        .left()
                        .unwrap();

                    let val_is_ident = matches!(value, Expression::Ident(_));
                    let val = self.codegen_expr(value, locals).unwrap();
                    self.builder
                        .build_call(
                            map_set,
                            &[map.into(), key_box.into(), val.into()],
                            "struct_set",
                        )
                        .unwrap();
                    // Release temporary key and value if owned temporaries
                    self.build_dec_ref(key_box);
                    if !val_is_ident {
                        self.build_dec_ref(val);
                    }
                }
                Some(map)
            }
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
            Expression::ListLiteral(values) => {
                let list_new = self.module.get_function("pie_list_new").unwrap();
                let list_push = self.module.get_function("pie_list_push").unwrap();
                let list = self
                    .builder
                    .build_call(list_new, &[], "list_lit_new")
                    .unwrap()
                    .try_as_basic_value()
                    .left()
                    .unwrap();
                for value in values {
                    let val_is_ident = matches!(value, Expression::Ident(_));
                    let val = self.codegen_expr(value, locals).unwrap();
                    self.builder
                        .build_call(list_push, &[list.into(), val.into()], "list_lit_push")
                        .unwrap();
                    if !val_is_ident {
                        self.build_dec_ref(val);
                    }
                }
                Some(list)
            }
            Expression::MapLiteral(values) => {
                let map_new = self.module.get_function("pie_map_new").unwrap();
                let map_set = self.module.get_function("pie_map_set").unwrap();
                let map = self
                    .builder
                    .build_call(map_new, &[], "map_lit_new")
                    .unwrap()
                    .try_as_basic_value()
                    .left()
                    .unwrap();
                for (key, value) in values {
                    let key_is_ident = matches!(value, Expression::Ident(_));
                    let key = self.codegen_expr(key, locals).unwrap();
                    let val_is_ident = matches!(value, Expression::Ident(_));
                    let val = self.codegen_expr(value, locals).unwrap();
                    self.builder
                        .build_call(
                            map_set,
                            &[map.into(), key.into(), val.into()],
                            "map_lit_set",
                        )
                        .unwrap();
                    if !key_is_ident {
                        self.build_dec_ref(key);
                    }
                    if !val_is_ident {
                        self.build_dec_ref(val);
                    }
                }
                Some(map)
            }
            Expression::Str(s) => {
                let bytes = s.as_bytes();
                let global_string = self.module.add_global(
                    self.context.i8_type().array_type(bytes.len() as u32),
                    None,
                    "gstr",
                );
                global_string.set_initializer(&self.context.const_string(bytes, false));
                let ptr = global_string.as_pointer_value();
                let fn_str_new = self.module.get_function("pie_string_new_static").unwrap();
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
                // evaluate LHS first
                let lhs_is_ident = matches!(**lhs, Expression::Ident(_));
                let left_val = self.codegen_expr(lhs, locals)?;

                // Strength-reduce small constant modulus and/or avoid runtime call
                if let (BinaryOp::Rem, &Expression::Int(m @ 1..)) = (op, rhs.deref()) {
                    let i64_t = self.context.i64_type();
                    let fn_int_to_i64 = self.module.get_function("pie_int_to_i64").unwrap();
                    let fn_int_new = self.module.get_function("pie_int_new").unwrap();
                    let i_left = self
                        .builder
                        .build_call(fn_int_to_i64, &[left_val.into()], "rem_lhs_i64")
                        .expect("call");
                    let i_left = i_left.try_as_basic_value().left().unwrap().into_int_value();
                    let m_const = i64_t.const_int(m as u64, true);

                    // If modulus is power of two, use (i & (m-1)) for non-negative i; else srem
                    let result_i = if (m & (m - 1)) == 0 {
                        let is_neg = self
                            .builder
                            .build_int_compare(
                                inkwell::IntPredicate::SLT,
                                i_left,
                                i64_t.const_zero(),
                                "is_neg",
                            )
                            .unwrap();
                        let and_mask = i64_t.const_int((m as u64) - 1, false);
                        let masked = self
                            .builder
                            .build_and(i_left, and_mask, "and_mask")
                            .unwrap();
                        let srem = self
                            .builder
                            .build_int_signed_rem(i_left, m_const, "srem")
                            .unwrap();
                        // select(is_neg, srem, masked)
                        self.builder
                            .build_select(is_neg, srem, masked, "rem_sel")
                            .unwrap()
                            .into_int_value()
                    } else {
                        self.builder
                            .build_int_signed_rem(i_left, m_const, "srem")
                            .unwrap()
                    };

                    let boxed = self
                        .builder
                        .build_call(fn_int_new, &[result_i.into()], "rem_box")
                        .expect("call");
                    if !lhs_is_ident {
                        self.build_dec_ref(left_val);
                    }
                    return boxed.try_as_basic_value().left();
                }

                // Generic binary op path
                let rhs_is_ident = matches!(**rhs, Expression::Ident(_));
                let right_val = self.codegen_expr(rhs, locals)?;

                let fn_name = match op {
                    BinaryOp::Add => "pie_add",
                    BinaryOp::Sub => "pie_sub",
                    BinaryOp::Mul => "pie_mul",
                    BinaryOp::Div => "pie_div",
                    BinaryOp::Eq => "pie_eq",
                    BinaryOp::Ne => "pie_ne",
                    BinaryOp::Lt => "pie_lt",
                    BinaryOp::Gt => "pie_gt",
                    BinaryOp::LtEq => "pie_lteq",
                    BinaryOp::GtEq => "pie_gteq",
                    BinaryOp::Rem => "pie_rem",
                    BinaryOp::And => "pie_and",
                    BinaryOp::Or => "pie_or",
                    BinaryOp::BitAnd => "pie_bitand",
                    BinaryOp::BitOr => "pie_bitor",
                    BinaryOp::BitXor => "pie_bitxor",
                };

                let f = self.module.get_function(fn_name).unwrap();

                let call = self
                    .builder
                    .build_call(f, &[left_val.into(), right_val.into()], "binop")
                    .expect("call");
                // Release temporary operands if they are not identifiers
                if !lhs_is_ident {
                    self.build_dec_ref(left_val);
                }
                if !rhs_is_ident {
                    self.build_dec_ref(right_val);
                }
                call.try_as_basic_value().left()
            }
            Expression::Unary(op, expr) => {
                let expr_is_ident = matches!(**expr, Expression::Ident(_));
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
                if !expr_is_ident {
                    self.build_dec_ref(val);
                }
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
                            let mut compiled_args = Vec::with_capacity(args.len());
                            let mut should_dec = Vec::with_capacity(args.len());
                            for a in args {
                                let is_ident = matches!(a, Expression::Ident(_));
                                if let Some(av) = self.codegen_expr(a, locals) {
                                    should_dec.push((!is_ident, Some(av)));
                                    compiled_args.push(av.into());
                                } else {
                                    should_dec.push((false, None));
                                    compiled_args
                                        .push(self.registry.ptr_type().const_null().into());
                                }
                            }
                            let call = self
                                .builder
                                .build_call(llvm_func, &compiled_args, &fname)
                                .expect("call");
                            // Release temporary args
                            for (dec, val) in should_dec {
                                if dec {
                                    if let Some(v) = val {
                                        self.build_dec_ref(v);
                                    }
                                }
                            }
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
                        let Some(f) = self.functions.get(*fname) else {
                            panic!("Attempted to call a function {fname} that could not be found")
                        };
                        let mut compiled_args = Vec::with_capacity(args.len());
                        let mut should_dec = Vec::with_capacity(args.len());
                        for a in args {
                            let is_ident = matches!(a, Expression::Ident(_));
                            if let Some(av) = self.codegen_expr(a, locals) {
                                should_dec.push((!is_ident, Some(av)));
                                compiled_args.push(av.into());
                            } else {
                                should_dec.push((false, None));
                                compiled_args.push(ptr_type.const_null().into());
                            }
                        }
                        let call = self
                            .builder
                            .build_call(*f, &compiled_args, "call")
                            .expect("call");
                        for (dec, val) in should_dec {
                            if dec {
                                if let Some(v) = val {
                                    self.build_dec_ref(v);
                                }
                            }
                        }
                        Some(call.try_as_basic_value().left().unwrap())
                    }
                    Expression::MemberAccess { components } => {
                        // Lower member access chain to successive std::map::get calls
                        let map_get = self.module.get_function("pie_map_get").unwrap();
                        // First element must be a local identifier
                        let Some(first) = components.first() else {
                            panic!("empty member access")
                        };
                        let Some(ptr) = locals.get(first) else {
                            panic!("unknown identifier in member access: {first}")
                        };
                        let ptr_type = self.context.ptr_type(AddressSpace::from(0u16));
                        let mut current = self
                            .builder
                            .build_load(ptr_type, *ptr, "load_member_base")
                            .expect("load");
                        for key in components.iter().skip(1) {
                            // materialize key string
                            let bytes = (*key).as_bytes();
                            let g = self.module.add_global(
                                self.context.i8_type().array_type(bytes.len() as u32),
                                None,
                                "gstr",
                            );
                            g.set_initializer(&self.context.const_string(bytes, false));
                            let ptr = g.as_pointer_value();
                            let fn_str_new =
                                self.module.get_function("pie_string_new_static").unwrap();
                            let len = self.context.i64_type().const_int(bytes.len() as u64, false);
                            let key_box = self
                                .builder
                                .build_call(fn_str_new, &[ptr.into(), len.into()], "memb_key")
                                .expect("call")
                                .try_as_basic_value()
                                .left()
                                .unwrap();

                            let call = self
                                .builder
                                .build_call(map_get, &[current.into(), key_box.into()], "mget")
                                .unwrap();
                            self.build_dec_ref(key_box);
                            let loaded = call.try_as_basic_value().left().unwrap();
                            // Release previous current
                            self.build_dec_ref(current);
                            current = loaded;
                        }
                        // A call on a value: not supported; return the final value for now
                        return Some(current);
                    }
                    expr => panic!("Codegen for {expr:#?} has not been implemented yet"),
                }
            }
            expr => panic!("Codegen for {expr:#?} has not been implemented yet"),
        }
    }
}
