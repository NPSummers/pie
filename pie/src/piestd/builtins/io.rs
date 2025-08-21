use std::io::stdin;

use crate::{
    piestd::builtins::pie_native_fn,
    runtime::{GcBox, GcRef, Value},
};

pie_native_fn!(pie_print(val: GcRef) pie "std::print"[Any] {
    println!("{}", val.value())
});

pie_native_fn!(pie_print_err(val: GcRef) pie "std::print_err"[Any] {
    eprintln!("{}", val.value())
});

pie_native_fn!(pie_get_args() pie "std::args"[] => List -> GcBox {
    let args = std::env::args().map(|arg| arg.into()).collect();
    GcBox::new(Value::List(args))
});

pie_native_fn!(pie_input_get_int() pie "std::input::get_int"[] => Int -> Option<GcBox> {
    let mut buf = String::new();
    stdin().read_line(&mut buf).ok()?;
    Some(GcBox::new(Value::Int(buf.trim().parse().ok()?)))
});

// Filesystem helpers
pie_native_fn!(pie_fs_read(path: GcRef) pie "std::fs::read"[String] => String -> Option<GcBox> {
    let Value::Str(p) = &*path.try_value()? else { return None; };
    match std::fs::read_to_string(p.as_ref()) {
        Ok(s) => Some(s.into()),
        Err(_) => None,
    }
});

pie_native_fn!(pie_fs_write(path: GcRef, contents: GcRef) pie "std::fs::write"[String, String] => Bool -> GcBox {
    let Value::Str(p) = &*path.value() else {
        return false.into();
    };
    let Value::Str(c) = &*contents.value() else {
        return false.into();
    };
    match std::fs::write(p.as_ref(), c.as_bytes()) {
        Ok(_) => true.into(),
        Err(_) => false.into(),
    }
});
