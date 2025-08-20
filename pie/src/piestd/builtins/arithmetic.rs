use std::{borrow::Cow, ops::Neg};

use crate::{
    piestd::builtins::pie_native_fn,
    runtime::{GcBox, GcRef, Value},
};

pie_native_fn!(pie_int_new(v: i64) -> GcBox {
    v.into()
});

pie_native_fn!(pie_float_new(v: f64) -> GcBox {
    v.into()
});

pie_native_fn!(pie_bool_new(v: bool) -> GcBox {
    v.into()
});

pub fn bool_to_box(b: bool) -> GcBox {
    // Use statically allocated true/false
    if b {
        GcBox::new_true()
    } else {
        GcBox::new_false()
    }
}

// Returns null if the provided value is not truthy, a valid GcRef(the one provided to it) otherwise
pie_native_fn!(pie_internal_truthy(a: GcRef) -> Option<GcBox> {
    use Value::*;
    let val = a.try_value()?;
    match &*val {
        &Bool(b) => b,
        &Int(i) => i != 0,
        &Float(f) => f.is_finite() && f != 0.0,
        List(l) => !l.is_empty(),
        Map(m) => !m.is_empty(),
        Str(s) =>  !s.is_empty(),
        Iterator(_) => false,
    }.then(GcBox::new_true)
});

// NOTE: Equality functions return:
// Value::Bool(true) if true
// None/null if false
// To turn this into a valid GcBox true/false, use pie_internal_truthy
pie_native_fn!(pie_eq(a: GcRef, b: GcRef) -> GcBox {
    bool_to_box(a.try_value().as_deref() == b.try_value().as_deref())
});

pie_native_fn!(pie_ne(a: GcRef, b: GcRef) -> GcBox {
    bool_to_box(a.try_value().as_deref() != b.try_value().as_deref())
});

pie_native_fn!(pie_lt(a: GcRef, b: GcRef) -> GcBox {
    bool_to_box(a.try_value().as_deref() < b.try_value().as_deref())
});

pie_native_fn!(pie_gt(a: GcRef, b: GcRef) -> GcBox {
    bool_to_box(a.try_value().as_deref() > b.try_value().as_deref())
});

pie_native_fn!(pie_gteq(a: GcRef, b: GcRef) -> GcBox {
    bool_to_box(a.try_value().as_deref() >= b.try_value().as_deref())
});

pie_native_fn!(pie_lteq(a: GcRef, b: GcRef) -> GcBox {
    bool_to_box(a.try_value().as_deref() <= b.try_value().as_deref())
});

pie_native_fn!(pie_add(a: GcRef, b: GcRef) -> Option<GcBox> {
    use Value::*;
    Some(match (&*a.value(), &*b.value()) {
        (Int(a), Int(b)) => (a + b).into(),
        (Str(a), b) => format!("{a}{b}").into(),
        (a, Str(b)) => format!("{a}{b}").into(),
        (Float(a), Float(b)) => (a + b).into(),
        (Float(a), &Int(b)) => (a + b as f64).into(),
        (&Int(a), Float(b)) => (a as f64 + b).into(),
        _ => return None
    })
});

pie_native_fn!(pie_sub(a: GcRef, b: GcRef) -> Option<GcBox> {
    use Value::*;
    Some(match (&*a.value(), &*b.value()) {
        (Int(a), Int(b)) => (a - b).into(),
        (Float(a), Float(b)) => (a - b).into(),
        (&Int(a), Float(b)) => (a as f64 - b).into(),
        (Float(a), &Int(b)) => (a - b as f64).into(),
        _ => return None,
    })
});

pie_native_fn!(pie_mul(a: GcRef, b: GcRef) -> Option<GcBox> {
    use Value::*;
    Some(match (&*a.value(), &*b.value()) {
        (Int(a), Int(b)) => (a * b).into(),
        (Float(a), Float(b)) => (a * b).into(),
        (&Int(a), Float(b)) => (a as f64 * b).into(),
        (Float(a), &Int(b)) => (a * b as f64).into(),
        (Str(str), &Int(factor)) => {
            str.repeat(factor.try_into().ok()?).into()
        }
        (List(list), &Int(factor)) => {
            let factor: usize = factor.try_into().unwrap();
            let mut out = Vec::with_capacity(list.len() * factor);
            for _ in 0..factor {
                out.extend(list.iter().cloned());
            }
            out.into()
        }
        _ => return None,
    })
});

pie_native_fn!(pie_div(a: GcRef, b: GcRef) -> Option<GcBox> {
    use Value::*;
    Some(match (&*a.value(), &*b.value()) {
        (_, Int(0)) => return None,
        (Int(a), Int(b)) => (a / b).into(),
        (Float(a), Float(b)) => (a / b).into(),
        (&Int(a), Float(b)) => (a as f64 / b).into(),
        (Float(a), &Int(b)) => (a / b as f64).into(),
        _ => return None,
    })
});

pie_native_fn!(pie_rem(a: GcRef, b: GcRef) -> Option<GcBox> {
    use Value::*;
    Some(match (&*a.value(), &*b.value()) {
        (_, Int(0)) => return None,
        (Int(a), Int(b)) => (a % b).into(),
        (Float(a), Float(b)) => (a % b).into(),
        (&Int(a), Float(b)) => (a as f64 % b).into(),
        (Float(a), &Int(b)) => (a % b as f64).into(),
        _ => return None,
    })
});

pie_native_fn!(pie_unary_add(val: GcRef) -> Option<GcBox> {
    use Value::*;
    Some(match &*val.value() {
        &Int(i) => i.into(),
        &Float(f) => f.into(),
        Str(s) => return s.parse::<i64>().ok().map(GcBox::from),
        &Bool(b) => (b as i64).into(),
        _ => return None,
    })
});

pie_native_fn!(pie_unary_sub(val: GcRef) -> Option<GcBox> {
    use Value::*;
    Some(match &*val.value() {
        &Int(i) => i.neg().into(),
        &Float(f) => f.neg().into(),
        Str(s) => return s.parse::<i64>().ok().map(Neg::neg).map(GcBox::from),
        &Bool(b) => (b as i64).neg().into(),
        _ => return None,
    })
});

pie_native_fn!(pie_unary_not(val: GcRef) -> Option<GcBox> {
    use Value::*;
    Some(match &*val.value() {
        &Int(i) => (i == 0).into(),
        &Float(f) => (!f.is_finite()).into(),
        Str(s) => s.is_empty().into(),
        &Bool(b) => (!b).into(),
        List(l) => l.is_empty().into(),
        Map(m) => m.is_empty().into(),
        Iterator(_) => return None,
    })
});

// In-place numeric ops: mutate destination Value to hold the result to avoid allocating a new GcBox
pie_native_fn!(pie_add_in_place(dst: GcRef, src: GcRef) pie "std::num::add_in_place"[Any, Any] {
    use Value::*;
    let mut d = dst.value_mut();
    let sref = src.value();
    match (&mut *d, &*sref) {
        (Int(a), &Int(b)) => {
            *a += b;
        }
        (Float(a), &Float(b)) => {*a += b},
        (Float(a), &Int(b)) => {*a += b as f64}
        (lhs @ &mut Int(a), Float(b)) => { *lhs = Float(a as f64 + b) },
        (Str(a), other) => {
            use std::fmt::Write;
            write!(Cow::to_mut(a), "{other}").unwrap();
        },
        (other, Str(b)) => {*other = Str(format!("{other}{b}").into())},
        _ => return,
    };
});

pie_native_fn!(pie_sub_in_place(dst: GcRef, src: GcRef) pie "std::num::sub_in_place"[Any, Any] {
    use Value::*;
    let mut d = dst.value_mut();
    let sref = src.value();
    match (&mut *d, &*sref) {
        (Int(a), Int(b)) => {*a -= b},
        (Float(a), Float(b)) => {*a -= b},
        (Float(a), Int(b)) => {*a -= *b as f64},
        (lhs @ &mut Int(a), Float(b)) => { *lhs = Float(a as f64 - b) },
        _ => return,
    }
});

pie_native_fn!(pie_mul_in_place(dst: GcRef, src: GcRef) pie "std::num::mul_in_place"[Any, Any] {
    use Value::*;
    let mut d = dst.value_mut();
    let sref = src.value();
    *d = match (&*d, &*sref) {
        (Int(a), Int(b)) => Int(a * b),
        (Float(a), Float(b)) => Float(a * b),
        (Float(a), Int(b)) => Float(a * *b as f64),
        (Int(a), Float(b)) => Float(*a as f64 * b),
        _ => return,
    }
});

pie_native_fn!(pie_div_in_place(dst: GcRef, src: GcRef) pie "std::num::div_in_place"[Any, Any] {
    use Value::*;
    let mut d = dst.value_mut();
    let sref = src.value();
    *d = match (&*d, &*sref) {
        (_, Int(0)) => return,
        (Int(a), Int(b)) => Int(a / b),
        (Float(a), Float(b)) => Float(a / b),
        (Float(a), Int(b)) => Float(a / *b as f64),
        (Int(a), Float(b)) => Float(*a as f64 / b),
        _ => return,
    }
});

// Helpers for numeric for-loops: extract and set ints without allocating
pie_native_fn!(pie_int_to_i64(val: GcRef) -> i64 {
    let Some(&Value::Int(i)) = val.try_value().as_deref() else { return 0 };
    i
});

pie_native_fn!(pie_int_set_in_place(dst: GcRef, v: i64) {
    let mut val = dst.try_value_mut();
    let Some(Value::Int(i)) = val.as_deref_mut() else { return };
    *i = v;
});
