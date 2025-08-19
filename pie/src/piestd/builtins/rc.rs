use std::rc::Rc;

use crate::{
    piestd::builtins::{pie_native_fn, Registry},
    runtime::{GcBox, GcRef},
};

pub fn register(reg: &mut Registry) {
    pie_is_null_register(reg);

    pie_inc_ref_register(reg);
    pie_dec_ref_register(reg);
}

pie_native_fn!(pie_is_null(p: GcRef) pie "std::is_null"[Any] => Bool -> GcBox {
    GcBox::from(p.is_null())
});

pie_native_fn!(pie_inc_ref(p: GcRef) {
    unsafe {Rc::increment_strong_count(p.as_ptr())};
});

pie_native_fn!(unsafe pie_dec_ref(p: GcRef) {
    unsafe {Rc::decrement_strong_count(p.as_ptr());};
});
