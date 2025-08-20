use std::borrow::Cow;

use crate::ast::{BinaryOp, Expression, Item, Module, ModuleItem, Program, Statement, UnaryOp};

pub fn constfold_program(prog: &mut Program<'_>) {
    for item in &mut prog.items {
        let Item::Module(m) = item else {
            continue;
        };
        walk_items_mut(&[], m, (), &mut |_, _, item| {
            let ModuleItem::Function(f) = item else {
                return;
            };
            for stmt in &mut f.body {
                constfold_stmt(stmt);
            }
        })
    }
}

fn constfold_stmt<'s>(stmt: &mut Statement<'s>) {
    use Statement::*;
    match stmt {
        Expr(expr) => {
            constfold_expr(expr);
        }
        Let { expr, .. } => {
            constfold_expr(expr);
        }
        Assignment { target, value, .. } => {
            constfold_expr(target);
            constfold_expr(value);
        }
        If {
            cond,
            then_body,
            else_body,
        } => {
            constfold_expr(cond);
            for stmt in then_body {
                constfold_stmt(stmt);
            }
            for stmt in else_body.iter_mut().flatten() {
                constfold_stmt(stmt);
            }
        }
        Statement::While { cond, body } => {
            constfold_expr(cond);
            for stmt in body {
                constfold_stmt(stmt);
            }
        }
        Statement::For { iterable, body, .. } => {
            constfold_expr(iterable);
            for stmt in body {
                constfold_stmt(stmt);
            }
        }
        Return(Some(out)) => {
            constfold_expr(out);
        }
        Return(None) => (),
    }
}

// TODO: Allow const-folding maps
#[derive(Debug, PartialEq, Clone)]
enum ConstVal<'s> {
    Int(i64),
    Float(f64),
    Bool(bool),
    List(Vec<ConstVal<'s>>),
    Str(Cow<'s, str>),
}

impl PartialOrd for ConstVal<'_> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        use ConstVal::*;
        match (self, other) {
            (Int(a), Int(b)) => a.partial_cmp(b),
            (Float(a), Float(b)) => a.partial_cmp(b),
            (Bool(a), Bool(b)) => a.partial_cmp(b),
            (Str(a), Str(b)) => a.partial_cmp(b),
            _ => None,
        }
    }
}

impl<'s> ConstVal<'s> {
    pub fn coerce_int(&self) -> Option<i64> {
        use ConstVal::*;
        Some(match *self {
            Int(i) => i,
            Bool(b) => b.into(),
            _ => return None,
        })
    }
    pub fn coerce_float(&self) -> Option<f64> {
        use ConstVal::*;
        if let Some(i) = self.coerce_int() {
            return Some(i as f64);
        };
        Some(match self {
            &Float(f) => f,
            _ => return None,
        })
    }
    pub fn truthy(&self) -> bool {
        use ConstVal::*;
        match self {
            &Bool(b) => b,
            &Int(i) => i != 0,
            &Float(f) => f.is_finite() && f != 0.0,
            List(l) => !l.is_empty(),
            Str(s) => !s.is_empty(),
        }
    }
    pub fn coerce_string(&self) -> Option<Cow<'s, str>> {
        use ConstVal::*;
        Some(match self {
            Int(i) => i.to_string().into(),
            Float(f) => f.to_string().into(),
            Bool(b) => b.to_string().into(),
            Str(s) => s.clone(),
            _ => return None,
        })
    }
    pub fn as_expr(&self) -> Expression<'s> {
        match self {
            &ConstVal::Int(i) => Expression::Int(i),
            &ConstVal::Float(f) => Expression::Float(f),
            &ConstVal::Bool(b) => Expression::Bool(b),
            ConstVal::Str(s) => Expression::Str(s.clone()),
            ConstVal::List(l) => {
                let l = l.into_iter().map(ConstVal::as_expr).collect();
                Expression::ListLiteral(l)
            }
        }
    }
}

fn constfold_expr<'s>(outer: &mut Expression<'s>) -> Option<ConstVal<'s>> {
    use Expression::*;
    match outer {
        Ident(_) => (),
        &mut Int(i) => return Some(ConstVal::Int(i)),
        &mut Float(i) => return Some(ConstVal::Float(i)),
        &mut Bool(b) => return Some(ConstVal::Bool(b)),
        Str(s) => return Some(ConstVal::Str(s.clone())),
        ListLiteral(values) => {
            let mut values = values.iter_mut();
            'constlist: {
                let mut constlist = Vec::new();
                for value in &mut values {
                    let Some(constval) = constfold_expr(value) else {
                        break 'constlist;
                    };
                    constlist.push(constval);
                }
                return Some(ConstVal::List(constlist));
            }
            for value in values {
                _ = constfold_expr(value);
            }
        }
        MapLiteral(m) => {
            for (k, v) in m {
                constfold_expr(k);
                constfold_expr(v);
            }
        }
        // TODO: Allow const-folding certain function calls?
        Call { callee, args } => {
            constfold_expr(callee);
            for arg in args {
                constfold_expr(arg);
            }
        }
        Binary(lhs, op, rhs) => {
            let lhs = constfold_expr(lhs)?;
            let rhs = constfold_expr(rhs)?;
            let sides = [&lhs, &rhs];
            let constant = 'cf: {
                macro_rules! basic_bop {
                    ($op:ident::$f:ident) => {{
                        if let [Some(lhs), Some(rhs)] = sides.map(ConstVal::coerce_int) {
                            break 'cf Some(ConstVal::Int(::core::ops::$op::$f(lhs, rhs)));
                        };
                        if let [Some(lhs), Some(rhs)] = sides.map(ConstVal::coerce_float) {
                            break 'cf Some(ConstVal::Float(::core::ops::$op::$f(lhs, rhs)));
                        };
                        None::<ConstVal>
                    }};
                }
                match op {
                    BinaryOp::And => break 'cf Some(ConstVal::Bool(lhs.truthy() && rhs.truthy())),
                    BinaryOp::Or => break 'cf Some(ConstVal::Bool(lhs.truthy() || rhs.truthy())),
                    BinaryOp::Add => {
                        basic_bop!(Add::add);
                        if let [Some(lhs), Some(rhs)] = sides.map(ConstVal::coerce_string) {
                            break 'cf Some(ConstVal::Str(::core::ops::Add::add(lhs, rhs)));
                        };
                        None
                    }
                    BinaryOp::Sub => basic_bop!(Sub::sub),
                    BinaryOp::Mul => {
                        match &lhs {
                            ConstVal::List(list) => {
                                if let Some(factor) = rhs.coerce_int() {
                                    let factor: usize = factor.try_into().unwrap();
                                    let mut out = Vec::with_capacity(list.len() * factor);
                                    for _ in 0..factor {
                                        out.extend(list.iter().cloned());
                                    }
                                    break 'cf Some(ConstVal::List(out));
                                }
                            }
                            ConstVal::Str(s) => {
                                if let Some(factor) = rhs.coerce_int() {
                                    let factor: usize = factor.try_into().unwrap();
                                    break 'cf Some(ConstVal::Str(s.repeat(factor).into()));
                                }
                            }
                            _ => (),
                        };
                        basic_bop!(Mul::mul)
                    }
                    BinaryOp::Div => basic_bop!(Div::div),
                    BinaryOp::Eq => Some(ConstVal::Bool(lhs == rhs)),
                    BinaryOp::Ne => Some(ConstVal::Bool(lhs != rhs)),
                    BinaryOp::Lt => Some(ConstVal::Bool(lhs < rhs)),
                    BinaryOp::Gt => Some(ConstVal::Bool(lhs > rhs)),
                    BinaryOp::LtEq => Some(ConstVal::Bool(lhs <= rhs)),
                    BinaryOp::GtEq => Some(ConstVal::Bool(lhs >= rhs)),
                    BinaryOp::Rem => basic_bop!(Rem::rem),
                }
            };
            let constant = constant?;
            *outer = constant.as_expr();
            return Some(constant);
        }
        Unary(op, expr) => {
            let val = constfold_expr(expr)?;
            let constant = 'cf: {
                match op {
                    UnaryOp::Add => {
                        if let Some(i) = val.coerce_int() {
                            break 'cf Some(ConstVal::Int(i));
                        }
                        if let Some(v) = val.coerce_float() {
                            break 'cf Some(ConstVal::Float(v));
                        }
                        None
                    }
                    UnaryOp::Sub => {
                        if let Some(i) = val.coerce_int() {
                            break 'cf Some(ConstVal::Int(-i));
                        }
                        if let Some(v) = val.coerce_float() {
                            break 'cf Some(ConstVal::Float(-v));
                        }
                        None
                    }
                    UnaryOp::Not => Some(ConstVal::Bool(!val.truthy())),
                }
            };
            let constant = constant?;
            *outer = constant.as_expr();
            return Some(constant);
        }
        ModuleAccess { .. } | MemberAccess { .. } => (),
    }
    None
}

pub fn walk_items_mut<'s, S>(
    prefix: &[&'s str],
    module: &mut Module<'s>,
    mut state: S,
    cb: &mut impl FnMut(S, &[&'s str], &mut ModuleItem<'s>) -> S,
) -> S {
    let mut path = prefix.to_vec();
    path.push(module.name);
    for item in &mut module.items {
        state = cb(state, &path, item);
        if let ModuleItem::Module(module) = item {
            state = walk_items_mut(&path, module, state, cb);
        };
    }
    state
}
