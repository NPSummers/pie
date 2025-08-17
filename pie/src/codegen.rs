use crate::ast::*;
use crate::piestd::StdLib;
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
}

impl<'ctx> CodeGen<'ctx> {
    pub fn new(context: &'ctx Context, name: &str) -> Self {
        let module = context.create_module(name);
        let builder = context.create_builder();
        Self {
            context,
            module,
            builder,
            functions: HashMap::new(),
        }
    }

    pub fn declare_runtime(&self) {
        let i64_type = self.context.i64_type();
        let i8ptr_type = self.context.i8_type().ptr_type(AddressSpace::from(0u16));
        let voidt = self.context.void_type();
        // Declare runtime functions (no bodies) so the real runtime implementations
        // are linked/used at execution time.
        let fn_new_int = i8ptr_type.fn_type(&[i64_type.into()], false);
        let _ = self.module.add_function("pie_new_int", fn_new_int, None);

        let fn_new_str = i8ptr_type.fn_type(&[i8ptr_type.into(), i64_type.into()], false);
        let _ = self.module.add_function("pie_new_string", fn_new_str, None);

        let fn_inc = voidt.fn_type(&[i8ptr_type.into()], false);
        let _ = self.module.add_function("pie_inc_ref", fn_inc, None);

        let fn_dec = voidt.fn_type(&[i8ptr_type.into()], false);
        let _ = self.module.add_function("pie_dec_ref", fn_dec, None);

        let fn_list_new = i8ptr_type.fn_type(&[], false);
        let _ = self.module.add_function("pie_list_new", fn_list_new, None);

        let fn_list_push = voidt.fn_type(&[i8ptr_type.into(), i8ptr_type.into()], false);
        let _ = self
            .module
            .add_function("pie_list_push", fn_list_push, None);

        let fn_map_new = i8ptr_type.fn_type(&[], false);
        let _ = self.module.add_function("pie_map_new", fn_map_new, None);

        let fn_map_set = voidt.fn_type(
            &[i8ptr_type.into(), i8ptr_type.into(), i8ptr_type.into()],
            false,
        );
        let _ = self.module.add_function("pie_map_set", fn_map_set, None);

        let fn_map_get = i8ptr_type.fn_type(&[i8ptr_type.into(), i8ptr_type.into()], false);
        let _ = self.module.add_function("pie_map_get", fn_map_get, None);

        let fn_to_string = i8ptr_type.fn_type(&[i8ptr_type.into()], false);
        let _ = self
            .module
            .add_function("pie_to_string", fn_to_string, None);

        let fn_http_get = i8ptr_type.fn_type(&[i8ptr_type.into(), i8ptr_type.into()], false);
        let _ = self.module.add_function("pie_http_get", fn_http_get, None);

        let fn_print = voidt.fn_type(&[i8ptr_type.into()], false);
        let _ = self.module.add_function("pie_print", fn_print, None);

        let fn_str_concat = i8ptr_type.fn_type(&[i8ptr_type.into(), i8ptr_type.into()], false);
        let _ = self
            .module
            .add_function("pie_string_concat", fn_str_concat, None);

        // Arithmetic functions
        let fn_add = i8ptr_type.fn_type(&[i8ptr_type.into(), i8ptr_type.into()], false);
        let _ = self.module.add_function("pie_add", fn_add, None);

        let fn_sub = i8ptr_type.fn_type(&[i8ptr_type.into(), i8ptr_type.into()], false);
        let _ = self.module.add_function("pie_sub", fn_sub, None);

        let fn_mul = i8ptr_type.fn_type(&[i8ptr_type.into(), i8ptr_type.into()], false);
        let _ = self.module.add_function("pie_mul", fn_mul, None);

        let fn_div = i8ptr_type.fn_type(&[i8ptr_type.into(), i8ptr_type.into()], false);
        let _ = self.module.add_function("pie_div", fn_div, None);
    }

    pub fn compile_program(&mut self, prog: &Program, _stdlib: &StdLib) -> Result<(), String> {
        // declare runtime
        self.declare_runtime();

        // Compile all modules and functions
        for item in &prog.items {
            if let Item::Module(m) = item {
                for mi in &m.items {
                    if let ModuleItem::Function(f) = mi {
                        self.compile_function(&m.name, f);
                    }
                }
            }
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

        // Verify the module
        if let Err(errors) = self.module.verify() {
            return Err(format!("LLVM module verification failed: {}", errors));
        }

        Ok(())
    }

    fn compile_function(&mut self, module_name: &str, func: &Function) {
        let i8ptr_type = self.context.i8_type().ptr_type(AddressSpace::from(0u16));
        let param_types: Vec<BasicTypeEnum> =
            func.params.iter().map(|_| i8ptr_type.into()).collect();

        // choose return type based on function declaration
        let fn_type = if matches!(func.ret, TypeName::Void) {
            self.context.void_type().fn_type(
                &param_types.iter().map(|t| (*t).into()).collect::<Vec<_>>(),
                false,
            )
        } else {
            i8ptr_type.fn_type(
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

        let mut locals: HashMap<String, PointerValue<'ctx>> = HashMap::new();
        for (idx, (_t, pname)) in func.params.iter().enumerate() {
            let alloca = self
                .builder
                .build_alloca(i8ptr_type, pname)
                .expect("alloca");
            let param = fnv.get_nth_param(idx as u32).unwrap();
            let _ = self.builder.build_store(alloca, param);
            locals.insert(pname.clone(), alloca);
        }

        let mut has_return = false;

        for stmt in &func.body {
            match stmt {
                Statement::Let {
                    typ: _typ,
                    name,
                    expr,
                } => {
                    if let Some(val) = self.codegen_expr(expr, &locals) {
                        let alloca = self.builder.build_alloca(i8ptr_type, name).expect("alloca");
                        let _ = self.builder.build_store(alloca, val);
                        locals.insert(name.clone(), alloca);
                    }
                }
                Statement::Assignment { target, op, value } => {
                    // Load the current value of the target
                    let target_ptr = if let Expression::Ident(name) = target {
                        locals.get(name).cloned()
                    } else {
                        None
                    };

                    if let Some(ptr) = target_ptr {
                        let current_val = self
                            .builder
                            .build_load(i8ptr_type, ptr, "current_val")
                            .expect("load");
                        let value_val =
                            self.codegen_expr(value, &locals).expect("value expression");

                        // Handle different assignment operators
                        let result = match op.as_str() {
                            "+=" => {
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
                            "-=" => {
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
                            "*=" => {
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
                            "/=" => {
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
                            _ => value_val, // For unknown operators, just store the value
                        };

                        // Store the result back to the target
                        let _ = self.builder.build_store(ptr, result);
                    }
                }
                Statement::Expr(e) => {
                    let _ = self.codegen_expr(e, &locals);
                }
                Statement::Return(Some(e)) => {
                    if let Some(v) = self.codegen_expr(e, &locals) {
                        if matches!(func.ret, TypeName::Void) {
                            let _ = self.builder.build_return(None);
                        } else {
                            let _ = self.builder.build_return(Some(&v));
                        }
                        has_return = true;
                        break;
                    }
                }
                Statement::Return(None) => {
                    let _ = self.builder.build_return(None);
                    has_return = true;
                    break;
                }
            }
        }

        if !has_return {
            if matches!(func.ret, TypeName::Void) {
                let _ = self.builder.build_return(None);
            } else {
                let null = i8ptr_type.const_null();
                let null_val = null.as_basic_value_enum();
                let _ = self.builder.build_return(Some(&null_val));
            }
        }
    }

    fn codegen_expr(
        &self,
        expr: &Expression,
        locals: &HashMap<String, PointerValue<'ctx>>,
    ) -> Option<BasicValueEnum<'ctx>> {
        let i8ptr_type = self.context.i8_type().ptr_type(AddressSpace::from(0u16));
        match expr {
            Expression::Int(i) => {
                let fn_new_int = self.module.get_function("pie_new_int").unwrap();
                let cv = self.context.i64_type().const_int(*i as u64, true);
                let call = self
                    .builder
                    .build_call(fn_new_int, &[cv.into()], "newint")
                    .expect("call");
                Some(call.try_as_basic_value().left().unwrap())
            }
            Expression::Str(s) => {
                let gv = self
                    .builder
                    .build_global_string_ptr(s, "gstr")
                    .expect("gstr");
                let ptr = gv.as_pointer_value();
                let fn_new_str = self.module.get_function("pie_new_string").unwrap();
                let len = self.context.i64_type().const_int(s.len() as u64, false);
                let call = self
                    .builder
                    .build_call(fn_new_str, &[ptr.into(), len.into()], "newstr")
                    .expect("call");
                Some(call.try_as_basic_value().left().unwrap())
            }
            Expression::Ident(name) => {
                if let Some(ptr) = locals.get(name) {
                    let loaded = self
                        .builder
                        .build_load(i8ptr_type, *ptr, &format!("load_{}", name))
                        .expect("load");
                    Some(loaded)
                } else {
                    None
                }
            }
            Expression::Binary(lhs, op, rhs) => {
                // evaluate both sides
                let left_val = self.codegen_expr(lhs, locals)?;
                let right_val = self.codegen_expr(rhs, locals)?;

                let fn_name = match op {
                    '+' => "pie_add",
                    '-' => "pie_sub",
                    '*' => "pie_mul",
                    '/' => "pie_div",
                    _ => return None,
                };

                if let Some(f) = self.module.get_function(fn_name) {
                    let call = self
                        .builder
                        .build_call(f, &[left_val.into(), right_val.into()], "binop")
                        .expect("call");
                    if call.try_as_basic_value().left().is_some() {
                        return Some(call.try_as_basic_value().left().unwrap());
                    }
                }
                None
            }
            Expression::Call { callee, args } => {
                match &**callee {
                    Expression::ModuleAccess { module, name } => {
                        // support std namespace with chained names like "map::new" etc.
                        if module == "std" {
                            // handle simple std functions first
                            if name == "print" {
                                if args.len() == 1 {
                                    if let Some(argv) = self.codegen_expr(&args[0], locals) {
                                        let fn_print =
                                            self.module.get_function("pie_print").unwrap();
                                        let _ = self
                                            .builder
                                            .build_call(fn_print, &[argv.into()], "print")
                                            .expect("call");
                                    }
                                }
                                return None;
                            }
                            if name == "to_string" {
                                if args.len() == 1 {
                                    if let Some(argv) = self.codegen_expr(&args[0], locals) {
                                        let fn_to_string =
                                            self.module.get_function("pie_to_string").unwrap();
                                        let call = self
                                            .builder
                                            .build_call(fn_to_string, &[argv.into()], "to_str")
                                            .expect("call");
                                        return Some(call.try_as_basic_value().left().unwrap());
                                    }
                                }
                                return None;
                            }

                            let parts: Vec<&str> = name.split("::").collect();
                            if parts.len() == 2 && parts[0] == "map" && parts[1] == "new" {
                                let fn_map_new = self.module.get_function("pie_map_new").unwrap();
                                let call = self
                                    .builder
                                    .build_call(fn_map_new, &[], "map_new")
                                    .expect("call");
                                return Some(call.try_as_basic_value().left().unwrap());
                            }
                            if parts.len() == 2 && parts[0] == "list" && parts[1] == "new" {
                                let fn_list_new = self.module.get_function("pie_list_new").unwrap();
                                let call = self
                                    .builder
                                    .build_call(fn_list_new, &[], "list_new")
                                    .expect("call");
                                return Some(call.try_as_basic_value().left().unwrap());
                            }
                            if parts.len() == 2 && parts[0] == "http" && parts[1] == "get" {
                                if args.len() == 1 {
                                    if let Some(argv) = self.codegen_expr(&args[0], locals) {
                                        let fn_http =
                                            self.module.get_function("pie_http_get").unwrap();
                                        let null = i8ptr_type.const_null();
                                        let call = self
                                            .builder
                                            .build_call(
                                                fn_http,
                                                &[argv.into(), null.into()],
                                                "http_get",
                                            )
                                            .expect("call");
                                        return Some(call.try_as_basic_value().left().unwrap());
                                    }
                                } else if args.len() == 2 {
                                    if let Some(argv) = self.codegen_expr(&args[0], locals) {
                                        if let Some(hv) = self.codegen_expr(&args[1], locals) {
                                            let fn_http =
                                                self.module.get_function("pie_http_get").unwrap();
                                            let call = self
                                                .builder
                                                .build_call(
                                                    fn_http,
                                                    &[argv.into(), hv.into()],
                                                    "http_get",
                                                )
                                                .expect("call");
                                            return Some(call.try_as_basic_value().left().unwrap());
                                        }
                                    }
                                }
                                return None;
                            }
                        }

                        // If `module` is a local variable, treat as method call: `var::method(...)`
                        if locals.contains_key(module) {
                            let var_ptr = locals.get(module).unwrap();
                            let loaded = self
                                .builder
                                .build_load(i8ptr_type, *var_ptr, &format!("load_{}", module))
                                .expect("load");
                            // map methods
                            if name == "set" {
                                // expects (key, val) -> call pie_map_set(map, key, val)
                                let mut compiled_args = Vec::new();
                                compiled_args.push(loaded.into());
                                if !args.is_empty() {
                                    if let Some(a0) = self.codegen_expr(&args[0], locals) {
                                        compiled_args.push(a0.into());
                                    } else {
                                        compiled_args.push(i8ptr_type.const_null().into());
                                    }
                                } else {
                                    compiled_args.push(i8ptr_type.const_null().into());
                                }
                                if args.len() >= 2 {
                                    if let Some(a1) = self.codegen_expr(&args[1], locals) {
                                        compiled_args.push(a1.into());
                                    } else {
                                        compiled_args.push(i8ptr_type.const_null().into());
                                    }
                                } else {
                                    compiled_args.push(i8ptr_type.const_null().into());
                                }
                                let fn_map_set = self.module.get_function("pie_map_set").unwrap();
                                let _ = self
                                    .builder
                                    .build_call(fn_map_set, &compiled_args, "map_set")
                                    .expect("call");
                                return None;
                            }
                            if name == "get" {
                                let mut compiled_args = Vec::new();
                                compiled_args.push(loaded.into());
                                if !args.is_empty() {
                                    if let Some(a0) = self.codegen_expr(&args[0], locals) {
                                        compiled_args.push(a0.into());
                                    } else {
                                        compiled_args.push(i8ptr_type.const_null().into());
                                    }
                                } else {
                                    compiled_args.push(i8ptr_type.const_null().into());
                                }
                                let fn_map_get = self.module.get_function("pie_map_get").unwrap();
                                let call = self
                                    .builder
                                    .build_call(fn_map_get, &compiled_args, "map_get")
                                    .expect("call");
                                if call.try_as_basic_value().left().is_some() {
                                    return Some(call.try_as_basic_value().left().unwrap());
                                }
                                return None;
                            }
                            // list methods
                            if name == "push" {
                                let mut compiled_args = Vec::new();
                                compiled_args.push(loaded.into());
                                if !args.is_empty() {
                                    if let Some(a0) = self.codegen_expr(&args[0], locals) {
                                        compiled_args.push(a0.into());
                                    } else {
                                        compiled_args.push(i8ptr_type.const_null().into());
                                    }
                                } else {
                                    compiled_args.push(i8ptr_type.const_null().into());
                                }
                                let fn_list_push =
                                    self.module.get_function("pie_list_push").unwrap();
                                let _ = self
                                    .builder
                                    .build_call(fn_list_push, &compiled_args, "list_push")
                                    .expect("call");
                                return None;
                            }
                        }

                        // fallback: module function lookup (module::name -> module__name)
                        let fname = format!("{}__{}", module, name.replace("::", "__"));
                        if let Some(f) = self.functions.get(&fname) {
                            let mut compiled_args = Vec::new();
                            for a in args {
                                if let Some(av) = self.codegen_expr(a, locals) {
                                    compiled_args.push(av.into());
                                } else {
                                    compiled_args.push(i8ptr_type.const_null().into());
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
                        None
                    }
                    Expression::Ident(n) => {
                        let fname = n.clone();
                        if let Some(f) = self.functions.get(&fname) {
                            let mut compiled_args = Vec::new();
                            for a in args {
                                if let Some(av) = self.codegen_expr(a, locals) {
                                    compiled_args.push(av.into());
                                } else {
                                    compiled_args.push(i8ptr_type.const_null().into());
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
                        None
                    }
                    _ => None,
                }
            }
            _ => None,
        }
    }
}
