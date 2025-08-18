use crate::{
    piestd::builtins::{pie_native_fn, Registry},
    runtime::GcBox,
};

pub fn register(reg: &mut Registry) {
    pie_int_new_register(reg);
}

pie_native_fn!(pie_int_new(v: i64) -> GcBox {
    v.into()
});
