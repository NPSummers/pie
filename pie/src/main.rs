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

use crate::lexer::Token;
use diagnostics::print_diagnostic;
use inkwell::context::Context;
use inkwell::execution_engine::JitFunction;
use inkwell::OptimizationLevel;
use logos::Logos;
use parser::Parser;
use std::env;
use std::fs;
use std::path::{Path, PathBuf};
use std::time::Instant;

fn expand_uses(
    source: &str,
    base_dir: &Path,
    visited: &mut std::collections::HashSet<PathBuf>,
) -> anyhow::Result<String> {
    let mut out = String::with_capacity(source.len());
    let mut last_end = 0usize;
    let mut lexer = Token::lexer(source).spanned().peekable();
    while let Some((tok, span)) = lexer.next() {
        if let Ok(Token::Use) = tok {
            // Flush text before 'use'
            out.push_str(&source[last_end..span.start]);

            // Collect path components until semicolon
            let mut components: Vec<&str> = Vec::new();
            // First component must be Ident
            let Some((Ok(Token::Ident(root)), _)) = lexer.next() else {
                // Malformed; just skip
                last_end = span.end;
                continue;
            };
            components.push(root);
            // zero or more '/ ident'
            loop {
                match lexer.peek() {
                    Some((Ok(Token::Slash), _)) => {
                        let _ = lexer.next();
                        if let Some((Ok(Token::Ident(seg)), _)) = lexer.next() {
                            components.push(seg);
                        } else {
                            break;
                        }
                    }
                    _ => break,
                }
            }
            // Expect semicolon; if not found, skip gracefully
            let mut end_pos = span.end;
            if let Some((tok2, s)) = lexer.next() {
                match tok2 {
                    Ok(Token::Semicolon) => {
                        end_pos = s.end;
                    }
                    _ => {
                        end_pos = s.end;
                    }
                }
            }

            // If this is built-in std, drop the use
            if components.len() >= 2 && components[0] == "pie" && components[1] == "std" {
                last_end = end_pos;
                continue;
            }

            // Resolve file path relative to base_dir
            let mut path = PathBuf::from(base_dir);
            for c in &components {
                path.push(c);
            }
            path.set_extension("pie");

            if path.exists() {
                // Avoid repeated includes
                let canonical = path.canonicalize().unwrap_or(path.clone());
                if !visited.insert(canonical.clone()) {
                    // already included; drop the use
                    last_end = end_pos;
                    continue;
                }
                let included_src = fs::read_to_string(&path).map_err(|e| {
                    anyhow::anyhow!("failed to read module {}: {}", path.display(), e)
                })?;
                let included_base = canonical.parent().unwrap_or(base_dir);
                let expanded_included = expand_uses(&included_src, included_base, visited)?;
                out.push_str(&expanded_included);
            } else {
                eprintln!(
                    "warning: could not resolve module '{}'; skipping",
                    components.join("/")
                );
            }

            last_end = end_pos;
            continue;
        }
    }
    // Append remaining tail
    out.push_str(&source[last_end..]);
    Ok(out)
}

fn main() -> anyhow::Result<()> {
    // Get file path from command line arguments
    let mut file_path = None;
    let mut opt_level = OptimizationLevel::None;
    for arg in env::args().skip(1) {
        if let Some(rest) = arg.strip_prefix("-O") {
            if rest.is_empty() {
                opt_level = OptimizationLevel::Default;
                continue;
            }
            let level_int: usize = rest
                .parse()
                .expect("Expected an integer optimization level after -O");
            let level = [
                OptimizationLevel::None,
                OptimizationLevel::Less,
                OptimizationLevel::Default,
                OptimizationLevel::Aggressive,
            ]
            .get(level_int)
            .expect("Optimization levels above 3 are not valid");
            opt_level = *level;
            continue;
        }
        file_path = file_path.or(Some(arg));
    }
    let Some(file_path) = file_path else {
        eprintln!("Usage: {} <file.pie>", env::args().next().unwrap());
        std::process::exit(1);
    };
    let src = fs::read_to_string(&file_path)
        .map_err(|e| anyhow::anyhow!("failed to read file {file_path}: {}", e))?;

    // Preprocess: expand `use` statements by inlining referenced .pie files
    let base_dir = std::path::Path::new(&file_path)
        .parent()
        .map(|p| p.to_path_buf())
        .unwrap_or_else(|| std::env::current_dir().unwrap());
    let mut visited = std::collections::HashSet::new();
    let src = expand_uses(&src, &base_dir, &mut visited)?;

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
    #[cfg(debug_assertions)]
    eprintln!("Program before constfolding: {prog:#?}");
    constfold::constfold_program(&mut prog);
    #[cfg(debug_assertions)]
    eprintln!("Program after constfolding: {prog:#?}");

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
        .create_jit_execution_engine(opt_level)
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
    #[cfg(debug_assertions)]
    {
        let refs = runtime::REFS.load(std::sync::atomic::Ordering::Acquire);
        eprintln!("Living Rc references at the end of program: {refs}");
    }

    Ok(())
}
