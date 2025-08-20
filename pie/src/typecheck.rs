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

fn check_fn<'s>(
    modules: &ModuleTree<'s>,
    path: &[&'s str],
    func: &Function<'s>,
) -> Result<(), String> {
    let mut locals: HashMap<&'s str, Type> = HashMap::new();
    for (t, name) in &func.params {
        locals.insert(*name, t.into());
    }

    let mut saw_return = false;
    for stmt in &func.body {
        if matches!(stmt, Statement::Return(_)) {
            saw_return = true;
        }
        check_stmt(modules, path, &mut locals, &func.ret, stmt)?;
    }
    if func.ret != TypeName::Void && !saw_return {
        return Err(format!(
            "missing return in function expected {:?}",
            Type::from(&func.ret)
        ));
    }
    Ok(())
}

fn check_let<'s>(
    modules: &ModuleTree<'s>,
    path: &[&'s str],
    name: &str,
    typ: Type,
    expr: &Expression<'s>,
) -> Result<(), String> {
    let locals: HashMap<&'s str, Type> = HashMap::new();
    let et = infer_expr_type(expr, &locals, modules, path)?;
    if typ != Type::Any && et != Type::Any && typ != et {
        return Err(format!(
            "type mismatch for let {}: declared {:?} but expr is {:?}",
            name, typ, et
        ));
    }
    Ok(())
}

fn check_stmt<'s>(
    modules: &ModuleTree<'s>,
    path: &[&'s str],
    locals: &mut HashMap<&'s str, Type>,
    func_ret: &TypeName<'s>,
    stmt: &Statement<'s>,
) -> Result<(), String> {
    match stmt {
        Statement::Let { typ, name, expr } => {
            let declared: Type = typ.into();
            let et = infer_expr_type(expr, locals, modules, path)?;
            if declared != Type::Any && et != Type::Any && declared != et {
                return Err(format!(
                    "type mismatch for let {}: declared {:?} but expr is {:?}",
                    name, declared, et
                ));
            }
            locals.insert(*name, declared);
            Ok(())
        }
        Statement::If {
            cond,
            then_body,
            else_body,
        } => {
            let ct = infer_expr_type(cond, locals, modules, path)?;
            if let Type::Function(_, _) | Type::Void = ct {
                return Err(format!("invalid type for if condition: {ct:?}"));
            }
            for s in then_body {
                check_stmt(modules, path, locals, func_ret, s)?;
            }
            if let Some(else_body) = else_body {
                for s in else_body {
                    check_stmt(modules, path, locals, func_ret, s)?;
                }
            }
            Ok(())
        }
        Statement::While { cond, body } => {
            let cond = infer_expr_type(cond, locals, modules, path)?;
            if let Type::Function(_, _) | Type::Void = cond {
                return Err(format!("invalid type for while loop condition: {cond:?}"));
            };
            for stmt in body {
                check_stmt(modules, path, locals, func_ret, stmt)?;
            }
            Ok(())
        }
        Statement::For {
            iterable,
            var,
            body,
        } => {
            let iterable = infer_expr_type(iterable, locals, modules, path)?;
            let item_type = match iterable {
                Type::Void | Type::Int | Type::Float | Type::Bool | Type::Function(_, _) => {
                    return Err(format!("Attempted to iterate over {iterable:?}"))
                }
                Type::Map => Type::Map,
                Type::List => Type::Any,
                Type::String => Type::String,
                Type::Any => Type::Any,
            };

            let old = locals.insert(var, item_type);

            for stmt in body {
                check_stmt(modules, path, locals, func_ret, stmt)?;
            }
            locals.remove(var);
            if let Some(old) = old {
                locals.insert(var, old);
            }
            Ok(())
        }
        Statement::Assignment { target, op, value } => {
            let (var_name, var_type) = match target {
                Expression::Ident(name) => {
                    let Some(vt) = locals.get(name).cloned() else {
                        return Err(format!("assignment to undefined variable '{name}'"));
                    };
                    (*name, vt)
                }
                _ => return Ok(()),
            };
            let vt = var_type;
            let val_t = infer_expr_type(value, locals, modules, path)?;
            let result_t = match op {
                AssignOp::Assign => val_t.clone(),
                AssignOp::Plus => binary_result_type(BinaryOp::Add, &vt, &val_t),
                AssignOp::Minus => binary_result_type(BinaryOp::Sub, &vt, &val_t),
                AssignOp::Star => binary_result_type(BinaryOp::Mul, &vt, &val_t),
                AssignOp::Slash => binary_result_type(BinaryOp::Div, &vt, &val_t),
            };
            if vt != Type::Any && result_t != Type::Any && vt != result_t {
                return Err(format!(
                    "type mismatch for assignment to {}: {:?} with value {:?} yields {:?}",
                    var_name, vt, val_t, result_t
                ));
            }
            Ok(())
        }
        Statement::Expr(e) => {
            let _ = infer_expr_type(e, locals, modules, path)?;
            Ok(())
        }
        Statement::Return(Some(e)) => {
            let et = infer_expr_type(e, locals, modules, path)?;
            let rt: Type = func_ret.into();
            if rt != Type::Any && et != Type::Any && rt != et {
                return Err(format!(
                    "return type mismatch: expected {:?}, got {:?}",
                    rt, et
                ));
            }
            Ok(())
        }
        Statement::Return(None) => {
            if *func_ret != TypeName::Void {
                return Err(format!(
                    "missing return in function expected {:?}",
                    Type::from(func_ret)
                ));
            }
            Ok(())
        }
    }
}

fn binary_result_type(op: BinaryOp, lhs: &Type, rhs: &Type) -> Type {
    use BinaryOp::*;
    match op {
        BitAnd | BitXor | BitOr => Type::Int,
        Add => {
            if *lhs == Type::String || *rhs == Type::String {
                return Type::String;
            }
            if *lhs == Type::Float || *rhs == Type::Float {
                return Type::Float;
            }
            if *lhs == Type::Int && *rhs == Type::Int {
                return Type::Int;
            }
            Type::Any
        }
        Sub | Mul | Div | Rem => {
            if *lhs == Type::Float || *rhs == Type::Float {
                return Type::Float;
            }
            if *lhs == Type::Int && *rhs == Type::Int {
                return Type::Int;
            }
            Type::Any
        }
        Lt | Gt | Eq | Ne | LtEq | GtEq | And | Or => Type::Bool,
    }
}

fn infer_expr_type<'s>(
    expr: &Expression<'s>,
    locals: &HashMap<&'s str, Type>,
    modules: &ModuleTree<'s>,
    path: &[&'s str],
) -> Result<Type, String> {
    Ok(match expr {
        Expression::Int(_) => Type::Int,
        Expression::Float(_) => Type::Float,
        Expression::Bool(_) => Type::Bool,
        Expression::Str(_) => Type::String,
        Expression::Ident(name) => {
            if let Some(t) = locals.get(name) {
                t.clone()
            } else if let Some(t) = modules.get(path.iter().copied().chain([*name])) {
                match t {
                    Type::Function(_, _) => return Err(format!("undefined variable '{name}'")),
                    other => other,
                }
            } else {
                return Err(format!("undefined variable '{name}'"));
            }
        }
        Expression::ModuleAccess { components } => {
            if let Some(t) = modules.get(components.iter().copied()) {
                t
            } else {
                let qualified = components.join("::");
                return Err(format!("unresolved name '{qualified}'"));
            }
        }
        Expression::MemberAccess { .. } => Type::Any,
        Expression::Binary(lhs, op, rhs) => {
            let lt = infer_expr_type(lhs, locals, modules, path)?;
            let rt = infer_expr_type(rhs, locals, modules, path)?;
            binary_result_type(op.clone(), &lt, &rt)
        }
        Expression::Unary(op, e) => {
            let et = infer_expr_type(e, locals, modules, path)?;
            match op {
                UnaryOp::Not => Type::Bool,
                UnaryOp::Add | UnaryOp::Sub => {
                    if et == Type::Float {
                        Type::Float
                    } else if et == Type::Int {
                        Type::Int
                    } else {
                        Type::Any
                    }
                }
            }
        }
        Expression::Call { callee, args } => {
            let (params, ret) = match &**callee {
                Expression::Ident(n) => {
                    if let Some(Type::Function(p, r)) =
                        modules.get(path.iter().copied().chain([*n]))
                    {
                        (p, r)
                    } else {
                        return Err(format!("unknown function '{n}'"));
                    }
                }
                Expression::ModuleAccess { components } => {
                    if let Some(Type::Function(p, r)) = modules.get(components.iter().copied()) {
                        (p, r)
                    } else {
                        let q = components.join("::");
                        return Err(format!("unknown function '{q}'"));
                    }
                }
                _ => return Ok(Type::Any),
            };
            if params.len() != args.len() {
                return Err(format!(
                    "argument count mismatch: expected {}, got {}",
                    params.len(),
                    args.len()
                ));
            }
            for (i, (pt, a)) in params.iter().zip(args.iter()).enumerate() {
                let at = infer_expr_type(a, locals, modules, path)?;
                if *pt != Type::Any && at != Type::Any && *pt != at {
                    return Err(format!(
                        "argument {} type mismatch: expected {:?}, got {:?}",
                        i + 1,
                        pt,
                        at
                    ));
                }
            }
            (*ret).clone()
        }
        Expression::ListLiteral(_) => Type::List,
        Expression::MapLiteral(_) => Type::Map,
    })
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
    }

    Ok(())
}
