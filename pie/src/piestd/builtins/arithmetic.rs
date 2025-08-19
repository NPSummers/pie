use crate::{
    piestd::builtins::{pie_native_fn, Registry},
    runtime::{GcBox, GcRef, Value},
};

pub fn register(reg: &mut Registry) {
    pie_int_new_register(reg);
    pie_float_new_register(reg);
    pie_bool_new_register(reg);

    pie_add_register(reg);
    pie_sub_register(reg);
    pie_mul_register(reg);
    pie_div_register(reg);
}

pie_native_fn!(pie_int_new(v: i64) -> GcBox {
    v.into()
});

pie_native_fn!(pie_float_new(v: f64) -> GcBox {
    v.into()
});

pie_native_fn!(pie_bool_new(v: bool) -> GcBox {
    v.into()
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
