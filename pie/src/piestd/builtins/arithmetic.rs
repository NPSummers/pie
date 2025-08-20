use std::ops::Neg;

use crate::{
    piestd::builtins::{pie_native_fn, Registry},
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

// Returns null if the provided value is not truthy, a valid GcRef(the one provided to it) otherwise
pie_native_fn!(pie_internal_truthy(a: GcRef) -> GcRef {
    use Value::*;
    if a.is_null() {
        return GcRef::new_null();
    }
    let val = a.value();
    let bool_to_null = |b| if b {a.clone()} else {GcRef::new_null()};
    bool_to_null(match &*val {
        &Bool(b) => b,
        &Int(i) => i != 0,
        &Float(f) => f.is_finite() && f != 0.0,
        List(l) => !l.is_empty(),
        Map(m) => !m.is_empty(),
        Str(s) =>  !s.is_empty(),
        Iterator(_) => false,
    })
});

pie_native_fn!(pie_eq(a: GcRef, b: GcRef) -> GcBox {
    (*a.value() == *b.value()).into()
});

pie_native_fn!(pie_ne(a: GcRef, b: GcRef) -> GcBox {
    (*a.value() != *b.value()).into()
});

pie_native_fn!(pie_lt(a: GcRef, b: GcRef) -> GcBox {
    (*a.value() < *b.value()).into()
});

pie_native_fn!(pie_gt(a: GcRef, b: GcRef) -> GcBox {
    (*a.value() > *b.value()).into()
});

pie_native_fn!(pie_gteq(a: GcRef, b: GcRef) -> GcBox {
    (*a.value() >= *b.value()).into()
});

pie_native_fn!(pie_lteq(a: GcRef, b: GcRef) -> GcBox {
    (*a.value() <= *b.value()).into()
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
