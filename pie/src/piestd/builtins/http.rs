use crate::{
    piestd::builtins::pie_native_fn,
    runtime::{GcBox, GcRef, Value},
};

pie_native_fn!(pie_http_method_get() pie "std::http::get"[] => String -> GcBox { "GET".into() });
pie_native_fn!(pie_http_method_post() pie "std::http::post"[] => String -> GcBox { "POST".into() });
pie_native_fn!(pie_http_method_put() pie "std::http::put"[] => String -> GcBox { "PUT".into() });
pie_native_fn!(pie_http_method_delete() pie "std::http::delete"[] => String -> GcBox { "DELETE".into() });

pie_native_fn!(pie_http_request(method_val: GcRef, url_val: GcRef, headers: GcRef, body_val: GcRef) pie "std::http::request"[String, String, Map, String] => String -> GcBox {
    let Value::Str(method) = &*method_val.value() else {
        return "http error: expected method to be a string".into();
    };
    let Value::Str(url) = &*url_val.value() else {
        return "http error: expected url to be a string".into();
    };
    let Value::Map(headers) = &*headers.value() else {
        return "http error: expected headers to be a map".into();
    };
    let Value::Str(body) = &*body_val.value() else {
        return "http error: expected body to be a string".into();
    };

    let method_upper = method.to_ascii_uppercase();
    let response_result = match method_upper.as_str() {
        "GET" => {
            let mut req = ureq::get(url.as_ref());
            for (k, v) in headers.iter() {
                req = req.header(k.as_ref(), &v.as_ref().value().to_string());
            }
            req.call()
        }
        "POST" => {
            let mut req = ureq::post(url.as_ref());
            for (k, v) in headers.iter() {
                req = req.header(k.as_ref(), &v.as_ref().value().to_string());
            }
            if body.is_empty() {
                req.send_empty()
            } else {
                req.send(&**body)
            }
        }
        "PUT" => {
            let mut req = ureq::put(url.as_ref());
            for (k, v) in headers.iter() {
                req = req.header(k.as_ref(), &v.as_ref().value().to_string());
            }
            if body.is_empty() {
                req.send_empty()
            } else {
                req.send(&**body)
            }
        }
        "DELETE" => {
            let mut req = ureq::delete(url.as_ref());
            for (k, v) in headers.iter() {
                req = req.header(k.as_ref(), &v.as_ref().value().to_string());
            }
            req.call()
        }
        _ => Err(ureq::Error::ConnectionFailed),
    };

    match response_result {
        Ok(resp) => {
            let status = resp.status();
            let mut rb = resp.into_body();
            let mut body = rb.read_to_string().unwrap_or_default();
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
