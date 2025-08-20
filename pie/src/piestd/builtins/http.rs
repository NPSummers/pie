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

pie_native_fn!(pie_http_serve(port_val: GcRef, body_val: GcRef) pie "std::http::serve"[Int, String] {
    let &Value::Int(port_i64) = &*port_val.value() else {
        eprintln!("http server error: expected port to be an int");
        return;
    };
    let Value::Str(body) = &*body_val.value() else {
        eprintln!("http server error: expected body to be a string");
        return;
    };

    // Clamp port to valid u16 range
    let port: u16 = if port_i64 < 0 { 0 } else { (port_i64 as u64).min(u16::MAX as u64) as u16 };

    let addr = ("0.0.0.0", port);
    let server = match tiny_http::Server::http(addr) {
        Ok(s) => s,
        Err(e) => {
            eprintln!("http server error: {e}");
            return;
        }
    };

    let body_string: String = body.to_string();
    let content_type = tiny_http::Header::from_bytes(
        &b"Content-Type"[..],
        &b"text/plain; charset=utf-8"[..],
    )
    .ok();

    // Blocking serve loop
    for request in server.incoming_requests() {
        let mut response = tiny_http::Response::from_string(body_string.clone())
            .with_status_code(tiny_http::StatusCode(200));
        if let Some(ct) = content_type.clone() {
            response = response.with_header(ct);
        }
        let _ = request.respond(response);
    }
});

pie_native_fn!(pie_http_serve_routes(port_val: GcRef, routes_val: GcRef) pie "std::http::serve_routes"[Int, Map] {
    use std::borrow::Cow;

    let &Value::Int(port_i64) = &*port_val.value() else {
        eprintln!("http server error: expected port to be an int");
        return;
    };
    let Value::Map(routes) = &*routes_val.value() else {
        eprintln!("http server error: expected routes to be a map");
        return;
    };

    let port: u16 = if port_i64 < 0 { 0 } else { (port_i64 as u64).min(u16::MAX as u64) as u16 };
    let addr = ("0.0.0.0", port);
    let server = match tiny_http::Server::http(addr) {
        Ok(s) => s,
        Err(e) => {
            eprintln!("http server error: {e}");
            return;
        }
    };

    fn pick_route<'a>(
        routes: &'a std::collections::HashMap<Cow<'static, str>, GcBox>,
        method: &str,
        path: &str,
    ) -> Option<&'a GcBox> {
        let method_path = format!("{method} {path}");
        if let Some(v) = routes.get(method_path.as_str()) {
            return Some(v);
        }
        if let Some(v) = routes.get(path) {
            return Some(v);
        }
        // Try trimming trailing slash fallback
        if path.ends_with('/') && path.len() > 1 {
            let trimmed = &path[..path.len() - 1];
            let method_trimmed = format!("{method} {trimmed}");
            if let Some(v) = routes.get(method_trimmed.as_str()) {
                return Some(v);
            }
            if let Some(v) = routes.get(trimmed) {
                return Some(v);
            }
        }
        None
    }

    for request in server.incoming_requests() {
        let url = request.url();
        let path = url.split('?').next().unwrap_or(url);
        let method = request.method().as_str();

        let mut status = 200u16;
        let mut body_string: String = String::new();
        let mut headers: Vec<tiny_http::Header> = Vec::new();

        if let Some(route_val) = pick_route(routes, method, path) {
            match &*route_val.as_ref().value() {
                Value::Str(s) => {
                    body_string = s.to_string();
                }
                Value::Map(m) => {
                    // Optional: status
                    if let Some(v) = m.get(&Cow::Borrowed("status")) {
                        if let Value::Int(i) = &*v.as_ref().value() {
                            status = (*i as i64).clamp(100, 599) as u16;
                        }
                    }
                    // body
                    if let Some(v) = m.get(&Cow::Borrowed("body")) {
                        if let Value::Str(s) = &*v.as_ref().value() {
                            body_string = s.to_string();
                        }
                    }
                    // headers map: arbitrary headers
                    if let Some(v) = m.get(&Cow::Borrowed("headers")) {
                        if let Value::Map(hm) = &*v.as_ref().value() {
                            for (hk, hv) in hm.iter() {
                                let hvs = hv.as_ref().value().to_string();
                                if let Some(h) = tiny_http::Header::from_bytes(hk.as_bytes(), hvs.as_bytes()).ok() {
                                    headers.push(h);
                                }
                            }
                        }
                    }
                }
                other => {
                    body_string = format!("invalid route value: {other}");
                    status = 500;
                }
            }
        } else {
            body_string = format!("No route for {method} {path}\n");
            status = 404;
        }

        // Default Content-Type if none provided
        if !headers.iter().any(|h| h.field.equiv("Content-Type")) {
            if let Some(ct) = tiny_http::Header::from_bytes(&b"Content-Type"[..], &b"text/plain; charset=utf-8"[..]).ok() {
                headers.push(ct);
            }
        }

        let mut response = tiny_http::Response::from_string(body_string).with_status_code(tiny_http::StatusCode(status));
        for h in headers.into_iter() {
            response = response.with_header(h);
        }
        let _ = request.respond(response);
    }
});
