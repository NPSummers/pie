use crate::{
    piestd::builtins::{pie_native_fn, Registry},
    runtime::{GcBox, GcRef, Value},
};

pub fn register(reg: &mut Registry) {
    pie_add_register(reg);
    pie_sub_register(reg);
    pie_mul_register(reg);
    pie_div_register(reg);
}

pie_native_fn!(pie_add(a: GcRef, b: GcRef) -> Option<GcBox> {
    use Value::*;
    Some(match (&*a.0.borrow(), &*b.0.borrow()) {
        (Int(a), Int(b)) => (a + b).into(),
        (Str(a), Str(b)) => format!("{a}{b}").into(),
        _ => return None,
    })
});

pie_native_fn!(pie_sub(a: GcRef, b: GcRef) -> Option<GcBox> {
    use Value::*;
    Some(match (&*a.0.borrow(), &*b.0.borrow()) {
        (Int(a), Int(b)) => (a - b).into(),
        _ => return None,
    })
});

pie_native_fn!(pie_mul(a: GcRef, b: GcRef) -> Option<GcBox> {
    use Value::*;
    Some(match (&*a.0.borrow(), &*b.0.borrow()) {
        (Int(a), Int(b)) => (a * b).into(),
        _ => return None,
    })
});

pie_native_fn!(pie_div(a: GcRef, b: GcRef) -> Option<GcBox> {
    use Value::*;
    Some(match (&*a.0.borrow(), &*b.0.borrow()) {
        (_, Int(0)) => return None,
        (Int(a), Int(b)) => (a / b).into(),
        _ => return None,
    })
});
