use crate::{
    piestd::builtins::{pie_native_fn, Registry},
    runtime::{GcBox, GcRef, Value},
};

pie_native_fn!(pie_print(val: GcRef) pie "std::print"[Any] {
    println!("{}", val.value())
});

pie_native_fn!(pie_print_err(val: GcRef) pie "std::print_err"[Any] {
    eprintln!("{}", val.value())
});

pie_native_fn!(pie_http_get(url_val: GcRef, headers: GcRef) pie "std::http_get"[String, Map] => String -> GcBox {
    let Value::Str(url) = &*url_val.value() else {
        return "http error: expected url to be a string".into();
    };
    let Value::Map(headers) = &*headers.value() else {
        return "http error: expected headers to be a map".into();
    };

    // build request and apply headers if provided
    let mut req = ureq::get(url);

    for (k, v) in headers {
        req = req.header(k, &v.as_ref().value().to_string());
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
