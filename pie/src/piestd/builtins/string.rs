use crate::{
    piestd::builtins::pie_native_fn,
    runtime::{GcBox, GcRef},
};

pie_native_fn! {pie_string_new(ptr: *const u8, len: u64) -> GcBox {
    unsafe {
        let bytes = core::slice::from_raw_parts(ptr, len as usize);
        // SAFETY: The frontend must ensure the strings are UTF-8
        let string = str::from_utf8_unchecked(bytes).to_owned();
        string.into()
    }
} }

pie_native_fn!(pie_to_string(val: GcRef) pie "std::to_string"[Any] => String -> GcBox {
    let s = val.value().to_string();
    s.into()
});
