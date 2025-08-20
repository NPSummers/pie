use rand::Rng;

use crate::{
    piestd::builtins::pie_native_fn,
    runtime::{GcBox, GcRef, Value},
};

pie_native_fn!(pie_rand_int() pie "std::rand::int"[] => Int -> GcBox {
    rand::rng().random::<i64>().into()
});

pie_native_fn!(pie_rand_int_range(min: GcRef, max: GcRef) pie "std::rand::int_range"[Int, Int] => Int -> Option<GcBox> {
    let &Value::Int(min) = &*min.value() else {
        return None
    };
    let &Value::Int(max) = &*max.value() else {
        return None
    };
    Some(rand::rng().random_range(min..=max).into())
});

pie_native_fn!(pie_rand_float() pie "std::rand::float"[] => Float -> GcBox {
    rand::rng().random::<f64>().into()
});

pie_native_fn!(pie_rand_float_range(min: GcRef, max: GcRef) pie "std::rand::float_range"[Float, Float] => Float -> Option<GcBox> {
    let &Value::Float(min) = &*min.value() else {
        return None
    };
    let &Value::Float(max) = &*max.value() else {
        return None
    };
    Some(rand::rng().random_range(min..=max).into())
});

pie_native_fn!(pie_rand_ratio(num: GcRef, denom: GcRef) pie "std::rand::ratio"[Int, Int] => Bool -> Option<GcBox> {
    let &Value::Int(num) = &*num.value() else {
        return None
    };
    let &Value::Int(denom) = &*denom.value() else {
        return None
    };
    Some(rand::rng().random_ratio(num.try_into().unwrap(), denom.try_into().unwrap()).into())
});
