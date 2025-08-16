use inkwell::context::Context;
use inkwell::values::FunctionValue;
use std::collections::HashMap;

pub struct StdLib<'ctx> {
    pub functions: HashMap<String, FunctionValue<'ctx>>,
}

impl<'ctx> StdLib<'ctx> {
    pub fn new(context: &'ctx Context) -> Self {
        let mut stdlib = StdLib {
            functions: HashMap::new(),
        };
        
        // Create a temporary module for std functions
        let module = context.create_module("std");
        
        // Add print function
        let i8_ptr_type = context.i8_type().ptr_type(inkwell::AddressSpace::from(0u16));
        let print_type = i8_ptr_type.fn_type(&[], false);
        let print_fn = module.add_function("printf", print_type, None);
        stdlib.functions.insert("print".to_string(), print_fn);
        
        // Add to_string function
        let i64_type = context.i64_type();
        let to_string_type = i64_type.fn_type(&[], false);
        let to_string_fn = module.add_function("to_string", to_string_type, None);
        stdlib.functions.insert("to_string".to_string(), to_string_fn);
        
        // Add arithmetic functions
        let add_type = i8_ptr_type.fn_type(&[i8_ptr_type.into(), i8_ptr_type.into()], false);
        let add_fn = module.add_function("pie_add", add_type, None);
        stdlib.functions.insert("pie_add".to_string(), add_fn);
        
        let sub_type = i8_ptr_type.fn_type(&[i8_ptr_type.into(), i8_ptr_type.into()], false);
        let sub_fn = module.add_function("pie_sub", sub_type, None);
        stdlib.functions.insert("pie_sub".to_string(), sub_fn);
        
        let mul_type = i8_ptr_type.fn_type(&[i8_ptr_type.into(), i8_ptr_type.into()], false);
        let mul_fn = module.add_function("pie_mul", mul_type, None);
        stdlib.functions.insert("pie_mul".to_string(), mul_fn);
        
        let div_type = i8_ptr_type.fn_type(&[i8_ptr_type.into(), i8_ptr_type.into()], false);
        let div_fn = module.add_function("pie_div", div_type, None);
        stdlib.functions.insert("pie_div".to_string(), div_fn);
        
        stdlib
    }
    
    pub fn get_function(&self, name: &str) -> Option<&FunctionValue<'ctx>> {
        self.functions.get(name)
    }
}
