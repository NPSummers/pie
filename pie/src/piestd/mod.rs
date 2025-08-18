use inkwell::context::Context;
use inkwell::values::FunctionValue;
use std::collections::HashMap;

use crate::piestd::builtins::{Registry, StdFunction};

pub mod builtins;
pub mod llvmtypes;

pub struct StdLib<'ctx> {
    pub functions: HashMap<String, FunctionValue<'ctx>>,
    pub registry: Registry<'ctx>,
}

impl<'ctx> StdLib<'ctx> {
    pub fn new(context: &'ctx Context) -> Self {
        let mut registry = Registry::new(context);
        registry.register_builtins();

        let mut stdlib = StdLib {
            functions: HashMap::new(),
            registry,
        };

        // Create a temporary module for std functions
        let module = context.create_module("std");

        for func in stdlib.registry.functions() {
            let fn_type = func.func_type;
            let function = module.add_function(func.native_name, fn_type, None);
            stdlib
                .functions
                .insert(func.native_name.to_string(), function);
        }

        stdlib
    }
}
