use crate::ast::*;
use std::collections::HashMap;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    Int,
    String,
    List(Box<Type>),
    Map,
    Void,
    Any,
}

fn to_type(ty: &TypeName) -> Type {
    match ty {
        TypeName::Int => Type::Int,
        TypeName::String => Type::String,
        TypeName::List(inner) => Type::List(Box::new(to_type(inner))),
        TypeName::Map => Type::Map,
        TypeName::Void => Type::Void,
        TypeName::Custom(_) => Type::Any,
    }
}

pub fn typecheck(prog: &Program) -> Result<(), String> {
    // collect module function signatures
    let mut module_fns: HashMap<String, HashMap<String, (Vec<Type>, Type)>> = HashMap::new();

    for item in &prog.items {
        if let Item::Module(m) = item {
            let mut sigs = HashMap::new();
            for mi in &m.items {
                if let ModuleItem::Function(f) = mi {
                    let params = f.params.iter().map(|(t, _)| to_type(t)).collect();
                    let ret = to_type(&f.ret);
                    sigs.insert(f.name.clone(), (params, ret));
                }
            }
            module_fns.insert(m.name.clone(), sigs);
        }
    }

    // check function bodies
    for item in &prog.items {
        if let Item::Module(m) = item {
            let sigs = module_fns.get(&m.name).unwrap();
            for mi in &m.items {
                if let ModuleItem::Function(f) = mi {
                    // build local env: params
                    let mut env: HashMap<String, Type> = HashMap::new();
                    for (t, name) in &f.params {
                        env.insert(name.clone(), to_type(t));
                    }
                    // check statements
                    for stmt in &f.body {
                        match stmt {
                            Statement::Let { typ, name, expr } => {
                                let et = infer_expr_type(expr, &env, &module_fns)?;
                                let declared = to_type(typ);
                                if declared != Type::Any && declared != et {
                                    return Err(format!("type mismatch for let {}: declared {:?} but expr is {:?}", name, declared, et));
                                }
                                env.insert(name.clone(), declared);
                            }
                            Statement::Expr(e) => { let _ = infer_expr_type(e, &env, &module_fns)?; }
                            Statement::Return(Some(e)) => { let et = infer_expr_type(e, &env, &module_fns)?; let ret = to_type(&f.ret); if ret != Type::Any && ret != et { return Err(format!("return type mismatch in {}: expected {:?}, got {:?}", f.name, ret, et)); } }
                            Statement::Return(None) => { if to_type(&f.ret) != Type::Void { return Err(format!("missing return in function expected {:?}", to_type(&f.ret))); } }
                            _ => {}
                        }
                    }
                }
            }
        }
    }

    Ok(())
}

fn infer_expr_type(expr: &Expression, env: &HashMap<String, Type>, modules: &HashMap<String, HashMap<String, (Vec<Type>, Type)>>) -> Result<Type, String> {
    match expr {
        Expression::Int(_) => Ok(Type::Int),
        Expression::Str(_) => Ok(Type::String),
        Expression::Ident(name) => Ok(env.get(name).cloned().unwrap_or(Type::Any)),
        Expression::Binary(lhs, op, rhs) => {
            let lt = infer_expr_type(lhs, env, modules)?;
            let rt = infer_expr_type(rhs, env, modules)?;
            if *op == '+' && (lt==Type::String || rt==Type::String) { return Ok(Type::String); }
            if lt==Type::Int && rt==Type::Int { return Ok(Type::Int); }
            Ok(Type::Any)
        }
        Expression::Call { callee, args } => {
            match &**callee {
                Expression::Ident(n) => {
                    // try find in current module - unknown so Any
                    Ok(Type::Any)
                }
                Expression::ModuleAccess { module, name } => {
                    if let Some(ms) = modules.get(module) {
                        if let Some((params, ret)) = ms.get(name) { return Ok(ret.clone()); }
                    }
                    // handle std functions
                    if module == "std" {
                        if name == "print" { return Ok(Type::Void); }
                        if name == "to_string" { return Ok(Type::String); }
                    }
                    Ok(Type::Any)
                }
                _ => Ok(Type::Any),
            }
        }
        Expression::ModuleAccess { .. } => Ok(Type::Any),
        Expression::ListLiteral(_) => Ok(Type::List(Box::new(Type::Any))),
        Expression::MapLiteral(_) => Ok(Type::Map),
        Expression::PatternMap(_) => Ok(Type::Map),
    }
}


