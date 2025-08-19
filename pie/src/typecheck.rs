use crate::{ast::*, piestd::builtins::Registry};
use std::collections::HashMap;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    Int,
    String,
    List,
    Map,
    Void,
    Float,
    Bool,
    Any,
    Function(Vec<Type>, Box<Type>),
}

pub enum ModuleNode<'s> {
    Module(HashMap<&'s str, ModuleNode<'s>>),
    Type(Type),
}

struct ModuleTree<'s>(HashMap<&'s str, ModuleNode<'s>>);

impl<'s> ModuleTree<'s> {
    pub fn new() -> Self {
        Self(HashMap::new())
    }
    pub fn get<'a>(&self, path: impl IntoIterator<Item = &'a str>) -> Option<Type> {
        let mut path = path.into_iter();
        let root = path.next()?;
        let mut node = self.0.get(root)?;
        for level in path {
            let ModuleNode::Module(m) = node else {
                return None;
            };
            node = m.get(level)?;
        }
        let ModuleNode::Type(t) = node else {
            return None;
        };
        Some(t.clone())
    }
    pub fn insert(&mut self, path: impl IntoIterator<Item = &'s str>, typ: Type) {
        let mut path = path.into_iter();
        let root = path
            .next()
            .expect("Attempted to insert a type into a ModuleTree without a path");
        let mut node = self.0.entry(root);
        for component in path {
            let level = node.or_insert_with(|| ModuleNode::Module(HashMap::new()));
            let ModuleNode::Module(m) = level else {
                panic!("Attempted to insert a type under a Type in a ModuleTree")
            };
            node = m.entry(component);
        }
        let std::collections::hash_map::Entry::Vacant(v) = node else {
            panic!("Attempted to overwrite a Type in a ModuleTree with a new one")
        };
        v.insert(ModuleNode::Type(typ));
    }
}

impl From<&TypeName<'_>> for Type {
    fn from(value: &TypeName<'_>) -> Self {
        match value {
            TypeName::Int => Type::Int,
            TypeName::String => Type::String,
            TypeName::List => Type::List,
            TypeName::Map => Type::Map,
            TypeName::Void => Type::Void,
            TypeName::Custom(_) => Type::Any,
            TypeName::Float => Type::Float,
            TypeName::Bool => Type::Bool,
        }
    }
}

pub fn walk_items<'s, S>(
    prefix: &[&'s str],
    module: &Module<'s>,
    mut state: S,
    cb: &mut impl FnMut(S, &[&'s str], &ModuleItem<'s>) -> S,
) -> S {
    let mut path = prefix.to_vec();
    path.push(module.name);
    for item in &module.items {
        state = cb(state, &path, item);
        if let ModuleItem::Module(module) = item {
            state = walk_items(&path, module, state, cb);
        };
    }
    state
}

fn check_fn(modules: &ModuleTree<'_>, path: &[&str], func: &Function<'_>) -> Result<(), String> {
    // TODO: typecheck
    Ok(())
}

fn check_let(
    modules: &ModuleTree<'_>,
    path: &[&str],
    name: &str,
    typ: Type,
    expr: &Expression<'_>,
) -> Result<(), String> {
    // TODO: typecheck
    Ok(())
}

pub fn typecheck(registry: &Registry<'_>, prog: &Program<'_>) -> Result<(), String> {
    // collect module function and variable signatures
    let mut modules: ModuleTree = ModuleTree::new();
    for func in registry.functions() {
        let Some(std) = &func.std else {
            continue;
        };
        modules.insert(
            std.path.to_vec(),
            Type::Function(std.params.to_vec(), Box::new(std.return_type.clone())),
        );
    }

    for item in &prog.items {
        let Item::Module(module) = item else {
            continue;
        };
        modules = walk_items(&[], module, modules, &mut |mut modules, path, item| {
            match item {
                ModuleItem::Let { typ, name, .. } => {
                    let path = path.iter().copied().chain([*name]);
                    modules.insert(path, typ.into());
                }
                ModuleItem::Function(func) => {
                    let path = path.iter().copied().chain([func.name]);
                    let ret = (&func.ret).into();
                    let params = func.params.iter().map(|(t, _)| t.into()).collect();
                    modules.insert(path, Type::Function(params, Box::new(ret)));
                }
                ModuleItem::Module(_) => (),
            }
            modules
        })
    }

    // check function bodies and vars
    for item in &prog.items {
        let Item::Module(module) = item else {
            continue;
        };
        let err = walk_items(
            &[],
            module,
            None,
            &mut |mut errors: Option<String>, path, item| {
                let check = match item {
                    ModuleItem::Let { typ, expr, name } => {
                        check_let(&modules, path, name, typ.into(), expr)
                    }
                    ModuleItem::Function(func) => check_fn(&modules, path, func),
                    _ => Ok(()),
                };
                if let Err(err) = check {
                    let mut text = errors
                        .map(|mut s| {
                            s.push('\n');
                            s
                        })
                        .unwrap_or_default();
                    text.push_str(&err);
                    errors = Some(text);
                }
                errors
            },
        );
        if let Some(err) = err {
            return Err(err);
        }
        // let _sigs = module_fns.get(&m.name).unwrap();
        // for mi in &m.items {
        //     if let ModuleItem::Function(f) = mi {
        //         // build local env: params
        //         let mut env: HashMap<&str, Type> = HashMap::new();
        //         for (t, name) in &f.params {
        //             env.insert(name, t.into());
        //         }
        //         // check statements
        //         for stmt in &f.body {
        //             match stmt {
        //                 Statement::Let { typ, name, expr } => {
        //                     let et = infer_expr_type(expr, &env, &module_fns)?;
        //                     let declared = typ.into();
        //                     if declared != Type::Any && declared != et {
        //                         return Err(format!(
        //                             "type mismatch for let {}: declared {:?} but expr is {:?}",
        //                             name, declared, et
        //                         ));
        //                     }
        //                     env.insert(name, declared);
        //                 }
        //                 Statement::Expr(e) => {
        //                     let _ = infer_expr_type(e, &env, &module_fns)?;
        //                 }
        //                 Statement::Return(Some(e)) => {
        //                     let et = infer_expr_type(e, &env, &module_fns)?;
        //                     let ret: Type = (&f.ret).into();
        //                     if ret != Type::Any && ret != et {
        //                         return Err(format!(
        //                             "return type mismatch in {}: expected {:?}, got {:?}",
        //                             f.name, ret, et
        //                         ));
        //                     }
        //                 }
        //                 Statement::Return(None) => {
        //                     let ret: Type = (&f.ret).into();
        //                     if ret != Type::Void {
        //                         return Err(format!(
        //                             "missing return in function expected {ret:?}",
        //                         ));
        //                     }
        //                 }
        //                 _ => {}
        //             }
        //         }
        //     }
        // }
    }

    Ok(())
}

fn infer_expr_type<'s>(
    expr: &Expression<'s>,
    env: &HashMap<&'s str, Type>,
    module_fns: &HashMap<Vec<&'s str>, (Vec<Type>, Type)>,
) -> Result<Type, String> {
    match expr {
        Expression::Int(_) => Ok(Type::Int),
        Expression::Str(_) => Ok(Type::String),
        Expression::Ident(name) => Ok(env.get(name).cloned().unwrap_or(Type::Any)),
        Expression::Binary(lhs, op, rhs) => {
            let lt = infer_expr_type(lhs, env, module_fns)?;
            let rt = infer_expr_type(rhs, env, module_fns)?;
            if *op == BinaryOp::Add && (lt == Type::String || rt == Type::String) {
                return Ok(Type::String);
            }
            if lt == Type::Int && rt == Type::Int {
                return Ok(Type::Int);
            }
            Ok(Type::Any)
        }
        Expression::Call { callee, .. } => {
            match &**callee {
                Expression::Ident(_n) => {
                    // try find in current module - unknown so Any
                    Ok(Type::Any)
                }
                Expression::ModuleAccess { components } => {
                    if let Some((_params, ret)) = module_fns.get(components) {
                        return Ok(ret.clone());
                    }
                    Ok(Type::Any)
                }
                _ => Ok(Type::Any),
            }
        }
        Expression::ModuleAccess { .. } => Ok(Type::Any),
        Expression::ListLiteral(_) => Ok(Type::List),
        Expression::MapLiteral(_) => Ok(Type::Map),
        Expression::MemberAccess { .. } => Ok(Type::Any),
        // TODO: Account for unary ops affecting the type
        Expression::Unary(_op, expr) => infer_expr_type(expr, env, module_fns),
        Expression::Float(_) => Ok(Type::Float),
        Expression::Bool(_) => Ok(Type::Bool),
    }
}
