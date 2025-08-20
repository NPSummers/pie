#![allow(dead_code)]
mod ast;
mod codegen;
mod constfold;
mod diagnostics;
mod lexer;
mod parser;
mod piestd;
mod runtime;
mod typecheck;

use diagnostics::print_diagnostic;
use inkwell::context::Context;
use inkwell::execution_engine::JitFunction;
use inkwell::OptimizationLevel;
use parser::Parser;
use std::env;
use std::fs;
use std::time::Instant;

fn main() -> anyhow::Result<()> {
    // Get file path from command line arguments
    let mut args = env::args().skip(1);
    let [Some(file_path), None] = core::array::from_fn(|_| args.next()) else {
        eprintln!("Usage: {} <file.pie>", env::args().next().unwrap());
        std::process::exit(1);
    };
    let src = fs::read_to_string(&file_path)
        .map_err(|e| anyhow::anyhow!("failed to read file {file_path}: {}", e))?;

    let parser = match Parser::new(&src) {
        Ok(p) => p,
        Err(d) => {
            print_diagnostic(&file_path, &src, &d);
            std::process::exit(1);
        }
    };
    let mut prog = match parser.parse() {
        Ok(p) => p,
        Err(d) => {
            print_diagnostic(&file_path, &src, &d);
            std::process::exit(1);
        }
    };
    constfold::constfold_program(&mut prog);

    // println!("{prog:#?}");

    // Codegen + PIE
    let context = Context::create();
    // Initialize standard library
    let stdlib = piestd::StdLib::new(&context);

    let mut cg = codegen::CodeGen::new(&context, &stdlib.registry, "pie_module");

    typecheck::typecheck(&stdlib.registry, &prog)
        .map_err(|e| anyhow::anyhow!("type error: {}", e))?;

    cg.compile_program(&prog)
        .map_err(|e| anyhow::anyhow!("compilation error: {}", e))?;

    // cg.module.print_to_stderr();

    // JIT compile and execute
    let ee = cg
        .module
        .create_jit_execution_engine(OptimizationLevel::None)
        .map_err(|e| anyhow::anyhow!("failed to create execution engine: {}", e))?;
    // Map declared runtime function symbols in the JIT module to the actual
    // runtime implementations in this process so calls (e.g., `pie_print`)
    // resolve to the real functions instead of hanging or being undefined.
    for func in stdlib.registry.functions() {
        if let Some(fval) = cg.module.get_function(func.native_name) {
            ee.add_global_mapping(&fval, func.addr);
        }
    }

    // Get the main function
    let main_fn: JitFunction<unsafe extern "C" fn() -> i32> = unsafe {
        ee.get_function("main")
            .map_err(|e| anyhow::anyhow!("failed to get main function: {}", e))?
    };

    // Execute the program
    eprintln!("Executing program...");
    let start = Instant::now();
    let result = unsafe { main_fn.call() };
    let took = start.elapsed();
    eprintln!("Program finished with exit code: {}", result);
    eprintln!("Runtime: {took:?}");

    Ok(())
}
