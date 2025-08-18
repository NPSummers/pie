use crate::{piestd::builtins::pie_native_fn, runtime::GcRef};

pie_native_fn!(pie_inc_ref(p: GcRef) {
    // This avoids running Drop for p, which leaves the refcount with 1 extra reference
    core::mem::forget(p.new_box());
});

pie_native_fn!(unsafe pie_dec_ref(p: GcRef) {
    unsafe {_ = p.new_box_noincrement() };
});
