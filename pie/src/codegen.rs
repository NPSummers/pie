use crate::ast::*;
use crate::std::StdLib;
use inkwell::context::Context;
use inkwell::module::Module as LlvmModule;
use inkwell::builder::Builder;
use inkwell::values::{BasicValueEnum, FunctionValue, PointerValue, BasicValue};
use inkwell::types::{BasicTypeEnum};
use inkwell::AddressSpace;
use std::collections::HashMap;

pub struct CodeGen<'ctx> {
    pub context: &'ctx Context,
    pub module: LlvmModule<'ctx>,
    pub builder: Builder<'ctx>,
    // symbol table: function name -> FunctionValue
    pub functions: HashMap<String, FunctionValue<'ctx>>,
}

impl<'ctx> CodeGen<'ctx> {
    pub fn new(context: &'ctx Context, name: &str) -> Self {
        let module = context.create_module(name);
        let builder = context.create_builder();
        Self { context, module, builder, functions: HashMap::new() }
    }

    pub fn declare_runtime(&self) {
        let i64_type = self.context.i64_type();
        let i8ptr_type = self.context.i8_type().ptr_type(AddressSpace::from(0u16));
        let voidt = self.context.void_type();

        // pie_new_int(i64) -> i8*
        let fn_new_int = i8ptr_type.fn_type(&[i64_type.into()], false);
        let fn_new_int_val = self.module.add_function("pie_new_int", fn_new_int, None);
        let entry = self.context.append_basic_block(fn_new_int_val, "entry");
        self.builder.position_at_end(entry);
        // Get the integer parameter and cast it to IntValue
        let int_param = fn_new_int_val.get_nth_param(0).unwrap();
        let int_val = int_param.into_int_value();
        // Create a proper integer value that can be used
        let cast = self.builder.build_int_to_ptr(int_val, i8ptr_type, "int_to_ptr").expect("cast");
        self.builder.build_return(Some(&cast));

        // pie_new_string(i8*) -> i8*
        let fn_new_str = i8ptr_type.fn_type(&[i8ptr_type.into()], false);
        let fn_new_str_val = self.module.add_function("pie_new_string", fn_new_str, None);
        let entry = self.context.append_basic_block(fn_new_str_val, "entry");
        self.builder.position_at_end(entry);
        // Just return the input string pointer
        let str_param = fn_new_str_val.get_nth_param(0).unwrap();
        self.builder.build_return(Some(&str_param));

        // pie_inc_ref(i8*) -> void
        let fn_inc = voidt.fn_type(&[i8ptr_type.into()], false);
        let fn_inc_val = self.module.add_function("pie_inc_ref", fn_inc, None);
        let entry = self.context.append_basic_block(fn_inc_val, "entry");
        self.builder.position_at_end(entry);
        self.builder.build_return(None);

        let fn_dec = voidt.fn_type(&[i8ptr_type.into()], false);
        let fn_dec_val = self.module.add_function("pie_dec_ref", fn_dec, None);
        let entry = self.context.append_basic_block(fn_dec_val, "entry");
        self.builder.position_at_end(entry);
        self.builder.build_return(None);

        let fn_list_new = i8ptr_type.fn_type(&[], false);
        let fn_list_new_val = self.module.add_function("pie_list_new", fn_list_new, None);
        let entry = self.context.append_basic_block(fn_list_new_val, "entry");
        self.builder.position_at_end(entry);
        let null_ptr = i8ptr_type.const_null();
        self.builder.build_return(Some(&null_ptr));

        let fn_list_push = voidt.fn_type(&[i8ptr_type.into(), i8ptr_type.into()], false);
        let fn_list_push_val = self.module.add_function("pie_list_push", fn_list_push, None);
        let entry = self.context.append_basic_block(fn_list_push_val, "entry");
        self.builder.position_at_end(entry);
        self.builder.build_return(None);

        let fn_map_new = i8ptr_type.fn_type(&[], false);
        let fn_map_new_val = self.module.add_function("pie_map_new", fn_map_new, None);
        let entry = self.context.append_basic_block(fn_map_new_val, "entry");
        self.builder.position_at_end(entry);
        let null_ptr = i8ptr_type.const_null();
        self.builder.build_return(Some(&null_ptr));

        let fn_map_set = voidt.fn_type(&[i8ptr_type.into(), i8ptr_type.into(), i8ptr_type.into()], false);
        let fn_map_set_val = self.module.add_function("pie_map_set", fn_map_set, None);
        let entry = self.context.append_basic_block(fn_map_set_val, "entry");
        self.builder.position_at_end(entry);
        self.builder.build_return(None);

        let fn_map_get = i8ptr_type.fn_type(&[i8ptr_type.into(), i8ptr_type.into()], false);
        let fn_map_get_val = self.module.add_function("pie_map_get", fn_map_get, None);
        let entry = self.context.append_basic_block(fn_map_get_val, "entry");
        self.builder.position_at_end(entry);
        let null_ptr = i8ptr_type.const_null();
        self.builder.build_return(Some(&null_ptr));

        let fn_to_string = i8ptr_type.fn_type(&[i8ptr_type.into()], false);
        let fn_to_string_val = self.module.add_function("pie_to_string", fn_to_string, None);
        let entry = self.context.append_basic_block(fn_to_string_val, "entry");
        self.builder.position_at_end(entry);
        let null_ptr = i8ptr_type.const_null();
        self.builder.build_return(Some(&null_ptr));

        let fn_print = voidt.fn_type(&[i8ptr_type.into()], false);
        let fn_print_val = self.module.add_function("pie_print", fn_print, None);
        let entry = self.context.append_basic_block(fn_print_val, "entry");
        self.builder.position_at_end(entry);
        // Get the parameter (the value to print) and cast it to PointerValue
        let param = fn_print_val.get_nth_param(0).unwrap();
        let ptr_val = param.into_pointer_value();
        // Convert the pointer back to an integer for printing
        let int_val = self.builder.build_ptr_to_int(ptr_val, i64_type, "ptr_to_int").expect("ptr_to_int");
        // Create format string for integer
        let msg = self.builder.build_global_string_ptr("PIE: Value = %ld\n", "print_msg").expect("global string");
        let msg_ptr = msg.as_pointer_value();
        // Call printf to actually print the value
        let printf_type = i8ptr_type.fn_type(&[i8ptr_type.into(), i64_type.into()], true);
        let printf_fn = self.module.add_function("printf", printf_type, None);
        let _ = self.builder.build_call(printf_fn, &[msg_ptr.into(), int_val.into()], "printf_call");
        self.builder.build_return(None);

        let fn_str_concat = i8ptr_type.fn_type(&[i8ptr_type.into(), i8ptr_type.into()], false);
        let fn_str_concat_val = self.module.add_function("pie_string_concat", fn_str_concat, None);
        let entry = self.context.append_basic_block(fn_str_concat_val, "entry");
        self.builder.position_at_end(entry);
        let null_ptr = i8ptr_type.const_null();
        self.builder.build_return(Some(&null_ptr));
        
        // Arithmetic functions
        let fn_add = i8ptr_type.fn_type(&[i8ptr_type.into(), i8ptr_type.into()], false);
        let fn_add_val = self.module.add_function("pie_add", fn_add, None);
        let entry = self.context.append_basic_block(fn_add_val, "entry");
        self.builder.position_at_end(entry);
        // Get both parameters and convert them to integers
        let a_param = fn_add_val.get_nth_param(0).unwrap();
        let b_param = fn_add_val.get_nth_param(1).unwrap();
        let a_ptr = a_param.into_pointer_value();
        let b_ptr = b_param.into_pointer_value();
        let a_int = self.builder.build_ptr_to_int(a_ptr, i64_type, "a_int").expect("ptr_to_int");
        let b_int = self.builder.build_ptr_to_int(b_ptr, i64_type, "b_int").expect("ptr_to_int");
        // Add them
        let sum = self.builder.build_int_add(a_int, b_int, "sum").expect("add");
        // Convert back to pointer
        let result = self.builder.build_int_to_ptr(sum, i8ptr_type, "sum_ptr").expect("int_to_ptr");
        self.builder.build_return(Some(&result));

        let fn_sub = i8ptr_type.fn_type(&[i8ptr_type.into(), i8ptr_type.into()], false);
        let fn_sub_val = self.module.add_function("pie_sub", fn_sub, None);
        let entry = self.context.append_basic_block(fn_sub_val, "entry");
        self.builder.position_at_end(entry);
        let a_param = fn_sub_val.get_nth_param(0).unwrap();
        let b_param = fn_sub_val.get_nth_param(1).unwrap();
        let a_ptr = a_param.into_pointer_value();
        let b_ptr = b_param.into_pointer_value();
        let a_int = self.builder.build_ptr_to_int(a_ptr, i64_type, "a_int").expect("ptr_to_int");
        let b_int = self.builder.build_ptr_to_int(b_ptr, i64_type, "b_int").expect("ptr_to_int");
        let diff = self.builder.build_int_sub(a_int, b_int, "diff").expect("sub");
        let result = self.builder.build_int_to_ptr(diff, i8ptr_type, "diff_ptr").expect("int_to_ptr");
        self.builder.build_return(Some(&result));

        let fn_mul = i8ptr_type.fn_type(&[i8ptr_type.into(), i8ptr_type.into()], false);
        let fn_mul_val = self.module.add_function("pie_mul", fn_mul, None);
        let entry = self.context.append_basic_block(fn_mul_val, "entry");
        self.builder.position_at_end(entry);
        let a_param = fn_mul_val.get_nth_param(0).unwrap();
        let b_param = fn_mul_val.get_nth_param(1).unwrap();
        let a_ptr = a_param.into_pointer_value();
        let b_ptr = b_param.into_pointer_value();
        let a_int = self.builder.build_ptr_to_int(a_ptr, i64_type, "a_int").expect("ptr_to_int");
        let b_int = self.builder.build_ptr_to_int(b_ptr, i64_type, "b_int").expect("ptr_to_int");
        let product = self.builder.build_int_mul(a_int, b_int, "product").expect("mul");
        let result = self.builder.build_int_to_ptr(product, i8ptr_type, "product_ptr").expect("int_to_ptr");
        self.builder.build_return(Some(&result));

        let fn_div = i8ptr_type.fn_type(&[i8ptr_type.into(), i8ptr_type.into()], false);
        let fn_div_val = self.module.add_function("pie_div", fn_div, None);
        let entry = self.context.append_basic_block(fn_div_val, "entry");
        self.builder.position_at_end(entry);
        let a_param = fn_div_val.get_nth_param(0).unwrap();
        let b_param = fn_div_val.get_nth_param(1).unwrap();
        let a_ptr = a_param.into_pointer_value();
        let b_ptr = b_param.into_pointer_value();
        let a_int = self.builder.build_ptr_to_int(a_ptr, i64_type, "a_int").expect("ptr_to_int");
        let b_int = self.builder.build_ptr_to_int(b_ptr, i64_type, "b_int").expect("ptr_to_int");
        let quotient = self.builder.build_int_signed_div(a_int, b_int, "quotient").expect("div");
        let result = self.builder.build_int_to_ptr(quotient, i8ptr_type, "quotient_ptr").expect("int_to_ptr");
        self.builder.build_return(Some(&result));
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
        let param_types: Vec<BasicTypeEnum> = func.params.iter().map(|_| i8ptr_type.into()).collect();

        // choose return type based on function declaration
        let fn_type = if matches!(func.ret, TypeName::Void) {
            self.context.void_type().fn_type(&param_types.iter().map(|t| t.clone().into()).collect::<Vec<_>>(), false)
        } else {
            i8ptr_type.fn_type(&param_types.iter().map(|t| t.clone().into()).collect::<Vec<_>>(), false)
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
            let alloca = self.builder.build_alloca(i8ptr_type, &pname).expect("alloca");
            let param = fnv.get_nth_param(idx as u32).unwrap();
            let _ = self.builder.build_store(alloca, param);
            locals.insert(pname.clone(), alloca);
        }

        let mut has_return = false;

        for stmt in &func.body {
            match stmt {
                Statement::Let { typ: _typ, name, expr } => {
                    if let Some(val) = self.codegen_expr(expr, &locals) {
                        let alloca = self.builder.build_alloca(i8ptr_type, &name).expect("alloca");
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
                        let current_val = self.builder.build_load(i8ptr_type, ptr, "current_val").expect("load");
                        let value_val = self.codegen_expr(value, &locals).expect("value expression");
                        
                        // Handle different assignment operators
                        let result = match op.as_str() {
                            "+=" => {
                                let fn_add = self.module.get_function("pie_add").unwrap();
                                let call = self.builder.build_call(fn_add, &[current_val.into(), value_val.into()], "add_result").expect("call");
                                call.try_as_basic_value().left().unwrap()
                            }
                            "-=" => {
                                let fn_sub = self.module.get_function("pie_sub").unwrap();
                                let call = self.builder.build_call(fn_sub, &[current_val.into(), value_val.into()], "sub_result").expect("call");
                                call.try_as_basic_value().left().unwrap()
                            }
                            "*=" => {
                                let fn_mul = self.module.get_function("pie_mul").unwrap();
                                let call = self.builder.build_call(fn_mul, &[current_val.into(), value_val.into()], "mul_result").expect("call");
                                call.try_as_basic_value().left().unwrap()
                            }
                            "/=" => {
                                let fn_div = self.module.get_function("pie_div").unwrap();
                                let call = self.builder.build_call(fn_div, &[current_val.into(), value_val.into()], "sub_result").expect("call");
                                call.try_as_basic_value().left().unwrap()
                            }
                            _ => value_val, // For unknown operators, just store the value
                        };
                        
                        // Store the result back to the target
                        let _ = self.builder.build_store(ptr, result);
                    }
                }
                Statement::Expr(e) => { let _ = self.codegen_expr(e, &locals); }
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
                Statement::Return(None) => { let _ = self.builder.build_return(None); has_return = true; break; }
                _ => {}
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

    fn codegen_expr(&self, expr: &Expression, locals: &HashMap<String, PointerValue<'ctx>>) -> Option<BasicValueEnum<'ctx>> {
        let i8ptr_type = self.context.i8_type().ptr_type(AddressSpace::from(0u16));
        match expr {
            Expression::Int(i) => {
                let fn_new_int = self.module.get_function("pie_new_int").unwrap();
                let cv = self.context.i64_type().const_int(*i as u64, true);
                let call = self.builder.build_call(fn_new_int, &[cv.into()], "newint").expect("call");
                Some(call.try_as_basic_value().left().unwrap())
            }
            Expression::Str(s) => {
                let gv = self.builder.build_global_string_ptr(&s, "gstr").expect("gstr");
                let ptr = gv.as_pointer_value();
                let fn_new_str = self.module.get_function("pie_new_string").unwrap();
                let call = self.builder.build_call(fn_new_str, &[ptr.into()], "newstr").expect("call");
                Some(call.try_as_basic_value().left().unwrap())
            }
            Expression::Ident(name) => {
                if let Some(ptr) = locals.get(name) {
                    let loaded = self.builder.build_load(i8ptr_type, *ptr, &format!("load_{}", name)).expect("load");
                    Some(loaded)
                } else { None }
            }
            Expression::Call { callee, args } => {
                match &**callee {
                    Expression::ModuleAccess { module, name } => {
                        if module == "std" {
                            if name == "print" {
                                // std::print call
                                if args.len() == 1 {
                                    if let Some(argv) = self.codegen_expr(&args[0], locals) {
                                        // got argument for print
                                        let fn_print = self.module.get_function("pie_print").unwrap();
                                        let _ = self.builder.build_call(fn_print, &[argv.into()], "print").expect("call");
                                        // print call generated
                                    } else {
                                        // no argument value for print
                                    }
                                }
                                return None;
                            }
                            if name == "to_string" {
                                if args.len() == 1 {
                                    if let Some(argv) = self.codegen_expr(&args[0], locals) {
                                        let fn_to_string = self.module.get_function("pie_to_string").unwrap();
                                        let call = self.builder.build_call(fn_to_string, &[argv.into()], "to_str").expect("call");
                                        return Some(call.try_as_basic_value().left().unwrap());
                                    }
                                }
                                return None;
                            }
                        }
                        let fname = format!("{}__{}", module, name);
                        // looking for function: fname
                        if let Some(f) = self.functions.get(&fname) {
                            // found function: fname
                            let mut compiled_args = Vec::new();
                            for a in args {
                                if let Some(av) = self.codegen_expr(a, locals) { compiled_args.push(av.into()); }
                                else { compiled_args.push(i8ptr_type.const_null().into()); }
                            }
                            let call = self.builder.build_call(*f, &compiled_args, "call").expect("call");
                            // module function call generated
                            if call.try_as_basic_value().left().is_some() { 
                                let ret_val = call.try_as_basic_value().left().unwrap();
                                // module function returned value
                                return Some(ret_val); 
                            } else {
                                // module function returned no value
                            }
                        } else {
                            // function not found: fname
                            // available functions listed in self.functions
                        }
                        None
                    }
                    Expression::Ident(n) => {
                        let fname = n.clone(); if let Some(f) = self.functions.get(&fname) {
                            let mut compiled_args = Vec::new();
                            for a in args { if let Some(av) = self.codegen_expr(a, locals) { compiled_args.push(av.into()); } else { compiled_args.push(i8ptr_type.const_null().into()); } }
                            let call = self.builder.build_call(*f, &compiled_args, "call").expect("call");
                            if call.try_as_basic_value().left().is_some() { return Some(call.try_as_basic_value().left().unwrap()); }
                        }
                        None
                    }
                    _ => None
                }
            }
            _ => None
        }
    }

    fn find_main_function<'a>(&self, prog: &'a Program) -> Option<&'a Function> {
        for item in &prog.items {
            if let Item::Module(m) = item {
                for mi in &m.items {
                    if let ModuleItem::Function(f) = mi {
                        if f.name == "main" && f.public {
                            return Some(f);
                        }
                    }
                }
            }
        }
        None
    }
}


