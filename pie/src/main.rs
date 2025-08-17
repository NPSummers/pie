#![allow(dead_code)]
mod ast;
mod codegen;
mod parser;
mod piestd;
mod runtime;
mod typecheck;

use inkwell::context::Context;
use inkwell::execution_engine::JitFunction;
use inkwell::OptimizationLevel;
use std::env;
use std::fs;

fn main() -> anyhow::Result<()> {
    // Get file path from command line arguments
    let mut args = env::args().skip(1);
    let [Some(file_path), None] = core::array::from_fn(|_| args.next()) else {
        eprintln!("Usage: {} <file.pie>", env::args().next().unwrap());
        std::process::exit(1);
    };
    let src = fs::read_to_string(&file_path)
        .map_err(|e| anyhow::anyhow!("failed to read file {file_path}: {}", e))?;

    let prog = parser::parse(&src).map_err(|e| anyhow::anyhow!("parse error: {}", e))?;

    println!("{prog:#?}");

    typecheck::typecheck(&prog).map_err(|e| anyhow::anyhow!("type error: {}", e))?;

    // Codegen + PIE
    let context = Context::create();
    let mut cg = codegen::CodeGen::new(&context, "pie_module");

    // Initialize standard library
    let stdlib = piestd::StdLib::new(&context);

    cg.compile_program(&prog, &stdlib)
        .map_err(|e| anyhow::anyhow!("compilation error: {}", e))?;

    cg.module.print_to_stderr();

    // todo!();

    // JIT compile and execute
    let ee = cg
        .module
        .create_jit_execution_engine(OptimizationLevel::None)
        .map_err(|e| anyhow::anyhow!("failed to create execution engine: {}", e))?;
    // Map declared runtime function symbols in the JIT module to the actual
    // runtime implementations in this process so calls (e.g., `pie_print`)
    // resolve to the real functions instead of hanging or being undefined.
    let mappings: &[(&str, usize)] = &[
        ("pie_new_int", runtime::pie_new_int as usize),
        ("pie_new_string", runtime::pie_new_string as usize),
        ("pie_inc_ref", runtime::pie_inc_ref as usize),
        ("pie_dec_ref", runtime::pie_dec_ref as usize),
        ("pie_list_new", runtime::pie_list_new as usize),
        ("pie_list_push", runtime::pie_list_push as usize),
        ("pie_map_new", runtime::pie_map_new as usize),
        ("pie_map_set", runtime::pie_map_set as usize),
        ("pie_map_get", runtime::pie_map_get as usize),
        ("pie_to_string", runtime::pie_to_string as usize),
        ("pie_print", runtime::pie_print as usize),
        ("pie_string_concat", runtime::pie_string_concat as usize),
        ("pie_add", runtime::pie_add as usize),
        ("pie_sub", runtime::pie_sub as usize),
        ("pie_mul", runtime::pie_mul as usize),
        ("pie_div", runtime::pie_div as usize),
        ("pie_http_get", runtime::pie_http_get as usize),
    ];

    for (name, addr) in mappings {
        if let Some(fval) = cg.module.get_function(name) {
            ee.add_global_mapping(&fval, *addr);
        }
    }

    // Get the main function
    let main_fn: JitFunction<unsafe extern "C" fn() -> i32> = unsafe {
        ee.get_function("main")
            .map_err(|e| anyhow::anyhow!("failed to get main function: {}", e))?
    };

    // Execute the program
    println!("Executing program...");
    let result = unsafe { main_fn.call() };
    println!("Program finished with exit code: {}", result);

    Ok(())
}
