use crate::{
    piestd::builtins::{pie_native_fn, Registry},
    runtime::{GcBox, GcRef, Value},
};

pub fn register(reg: &mut Registry) {
    pie_print_register(reg);
    pie_http_get_register(reg);
}

pie_native_fn!(pie_print(val: GcRef) pie "std::print"[Any] {
    println!("{}", val.0.borrow())
});

pie_native_fn!(pie_http_get(url_val: GcRef, headers: GcRef) pie "std::http_get"[String, Map] => String -> GcBox {
    let Value::Str(url) = &*url_val.0.borrow() else {
        return "http error: expected url to be a string".into();
    };
    let Value::Map(headers) = &*headers.0.borrow() else {
        return "http error: expected headers to be a map".into();
    };

    // build request and apply headers if provided
    let mut req = ureq::get(url);

    for (k, v) in headers {
        req = req.header(k, &v.as_ref().0.borrow().to_string());
    }

    match req.call() {
        Ok(resp) => {
            let status = resp.status();
            let mut body = resp.into_body().read_to_string().unwrap_or_default();
            if body.is_empty() {
                body = format!("(status {})", status);
            }
            body.into()
        }
        Err(e) => {
            let msg = format!("http error: {}", e);
            msg.into()
        }
    }
});
