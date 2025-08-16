mod runtime;
mod ast;
mod parser;
mod typecheck;
mod codegen;
mod std;

use inkwell::context::Context;
use inkwell::OptimizationLevel;
use inkwell::execution_engine::JitFunction;
use ::std::env;
use ::std::fs;

fn main() -> anyhow::Result<()> {
    // Get file path from command line arguments
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <file.pie>", args[0]);
        ::std::process::exit(1);
    }
    
    let file_path = &args[1];
    let src = fs::read_to_string(file_path).map_err(|e| anyhow::anyhow!("failed to read file {}: {}", file_path, e))?;
    
    let prog = parser::parse(&src).map_err(|e| anyhow::anyhow!("parse error: {}", e))?;
    
    typecheck::typecheck(&prog).map_err(|e| anyhow::anyhow!("type error: {}", e))?;

    // Codegen + PIE
    let context = Context::create();
    let mut cg = codegen::CodeGen::new(&context, "pie_module");
    
    // Initialize standard library
    let stdlib = std::StdLib::new(&context);
    
    cg.compile_program(&prog, &stdlib).map_err(|e| anyhow::anyhow!("compilation error: {}", e))?;

    // JIT compile and execute
    let ee = cg.module.create_jit_execution_engine(OptimizationLevel::None)
        .map_err(|e| anyhow::anyhow!("failed to create execution engine: {}", e))?;
    
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


