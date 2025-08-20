use crate::{
    piestd::builtins::{pie_native_fn, Registry},
    runtime::{GcBox, GcRef},
};

// Calling convention:
// The callee is the one to increment the rc count of any arguments passed into a function
//
// The refcount of the return value of a function is incremented before it returns, not by the
// caller.
//
// a = expr increments the refcount only if expr is an identifier.
// This also decrements the old value of a.

pie_native_fn!(pie_is_null(p: GcRef) pie "std::is_null"[Any] => Bool -> GcBox {
    GcBox::from(p.is_null())
});

pie_native_fn!(pie_inc_ref(p: GcRef) {
    p.inc_ref();
});

pie_native_fn!(unsafe pie_dec_ref(p: GcRef) {
    p.dec_ref();
});
