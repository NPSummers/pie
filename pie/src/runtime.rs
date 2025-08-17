use std::collections::HashMap;
use std::ffi::{CStr, CString};
use std::os::raw::c_char;
use std::ptr;
use std::sync::atomic::{AtomicUsize, Ordering};
use ureq;

#[derive(Debug)]
pub enum ValueKind {
    Int(i64),
    Str(String),
    List(Vec<*mut GcBox>),
    Map(HashMap<String, *mut GcBox>),
}

#[derive(Debug)]
pub struct GcBox {
    refcount: AtomicUsize,
    kind: ValueKind,
}

impl GcBox {
    fn new(kind: ValueKind) -> *mut GcBox {
        let b = Box::new(GcBox {
            refcount: AtomicUsize::new(1),
            kind,
        });
        Box::into_raw(b)
    }
}

#[no_mangle]
pub extern "C" fn pie_new_int(v: i64) -> *mut GcBox {
    GcBox::new(ValueKind::Int(v))
}

#[no_mangle]
pub extern "C" fn pie_new_string(s: *const c_char) -> *mut GcBox {
    if s.is_null() {
        return ptr::null_mut();
    }
    let cs = unsafe { CStr::from_ptr(s) };
    let st = cs.to_string_lossy().into_owned();
    GcBox::new(ValueKind::Str(st))
}

#[no_mangle]
pub extern "C" fn pie_inc_ref(p: *mut GcBox) {
    if p.is_null() {
        return;
    }
    unsafe {
        (*p).refcount.fetch_add(1, Ordering::SeqCst);
    }
}

#[no_mangle]
pub extern "C" fn pie_dec_ref(p: *mut GcBox) {
    if p.is_null() {
        return;
    }
    let prev = unsafe { (*p).refcount.fetch_sub(1, Ordering::SeqCst) };
    if prev == 1 {
        // free contents recursively
        unsafe {
            match &mut (*p).kind {
                ValueKind::List(v) => {
                    for &elem in v.iter() {
                        pie_dec_ref(elem);
                    }
                }
                ValueKind::Map(m) => {
                    for (_k, &val) in m.iter() {
                        pie_dec_ref(val);
                    }
                }
                ValueKind::Str(_) | ValueKind::Int(_) => {}
            }
            // drop box
            Box::from_raw(p);
        }
    }
}

#[no_mangle]
pub extern "C" fn pie_list_new() -> *mut GcBox {
    GcBox::new(ValueKind::List(Vec::new()))
}

#[no_mangle]
pub extern "C" fn pie_list_len(list: *mut GcBox) -> i64 {
    if list.is_null() {
        return 0;
    }
    unsafe {
        match &(*list).kind {
            ValueKind::List(v) => v.len() as i64,
            _ => 0,
        }
    }
}

#[no_mangle]
pub extern "C" fn pie_list_get(list: *mut GcBox, idx: i64) -> *mut GcBox {
    if list.is_null() {
        return ptr::null_mut();
    }
    unsafe {
        match &(*list).kind {
            ValueKind::List(v) => {
                let i = idx as usize;
                if i >= v.len() {
                    return ptr::null_mut();
                }
                let val = v[i];
                pie_inc_ref(val);
                val
            }
            _ => ptr::null_mut(),
        }
    }
}

#[no_mangle]
pub extern "C" fn pie_list_push(list: *mut GcBox, val: *mut GcBox) {
    if list.is_null() || val.is_null() {
        return;
    }
    unsafe {
        match &mut (*list).kind {
            ValueKind::List(v) => {
                pie_inc_ref(val);
                v.push(val);
            }
            _ => {}
        }
    }
}

#[no_mangle]
pub extern "C" fn pie_map_new() -> *mut GcBox {
    GcBox::new(ValueKind::Map(HashMap::new()))
}

#[no_mangle]
pub extern "C" fn pie_map_set(map: *mut GcBox, key: *mut GcBox, val: *mut GcBox) {
    if map.is_null() || key.is_null() {
        return;
    }
    // extract key string from a PIE GcBox (string or int)
    let k = unsafe {
        match &(*key).kind {
            ValueKind::Str(s) => s.clone(),
            ValueKind::Int(i) => format!("{}", i),
            _ => return,
        }
    };
    unsafe {
        match &mut (*map).kind {
            ValueKind::Map(m) => {
                if let Some(old) = m.insert(k, val) {
                    pie_dec_ref(old);
                }
                pie_inc_ref(val);
            }
            _ => {}
        }
    }
}

#[no_mangle]
pub extern "C" fn pie_map_get(map: *mut GcBox, key: *mut GcBox) -> *mut GcBox {
    if map.is_null() || key.is_null() {
        return ptr::null_mut();
    }
    let k = unsafe {
        match &(*key).kind {
            ValueKind::Str(s) => s.clone(),
            ValueKind::Int(i) => format!("{}", i),
            _ => return ptr::null_mut(),
        }
    };
    unsafe {
        match &(*map).kind {
            ValueKind::Map(m) => {
                if let Some(&v) = m.get(&k) {
                    pie_inc_ref(v);
                    return v;
                }
                ptr::null_mut()
            }
            _ => ptr::null_mut(),
        }
    }
}

#[no_mangle]
pub extern "C" fn pie_to_string(val: *mut GcBox) -> *mut GcBox {
    if val.is_null() {
        return ptr::null_mut();
    }
    // Helper to format a value (recursively) into a Rust String
    fn format_val(vptr: *mut GcBox) -> String {
        if vptr.is_null() {
            return "null".to_string();
        }
        unsafe {
            match &(*vptr).kind {
                ValueKind::Int(i) => format!("{}", i),
                ValueKind::Str(s) => s.clone(),
                ValueKind::List(vec) => {
                    let mut parts: Vec<String> = Vec::new();
                    for &elem in vec.iter() {
                        parts.push(format_val(elem));
                    }
                    format!("[{}]", parts.join(", "))
                }
                ValueKind::Map(map) => {
                    let mut parts: Vec<String> = Vec::new();
                    for (k, &vptr) in map.iter() {
                        parts.push(format!("{}: {}", k, format_val(vptr)));
                    }
                    format!("{{{}}}", parts.join(", "))
                }
            }
        }
    }

    let s = format_val(val);
    let c = CString::new(s).unwrap();
    pie_new_string(c.as_ptr())
}

#[no_mangle]
pub extern "C" fn pie_print(val: *mut GcBox) {
    if val.is_null() {
        return;
    }
    unsafe {
        match &(*val).kind {
            ValueKind::Str(s) => println!("{}", s),
            ValueKind::Int(i) => println!("{}", i),
            _ => println!("<value>"),
        }
    }
}

#[no_mangle]
pub extern "C" fn pie_string_concat(a: *mut GcBox, b: *mut GcBox) -> *mut GcBox {
    let sa = if a.is_null() {
        String::new()
    } else {
        unsafe {
            match &(*a).kind {
                ValueKind::Str(s) => s.clone(),
                ValueKind::Int(i) => format!("{}", i),
                _ => "<value>".to_string(),
            }
        }
    };
    let sb = if b.is_null() {
        String::new()
    } else {
        unsafe {
            match &(*b).kind {
                ValueKind::Str(s) => s.clone(),
                ValueKind::Int(i) => format!("{}", i),
                _ => "<value>".to_string(),
            }
        }
    };
    let c = CString::new(format!("{}{}", sa, sb)).unwrap();
    pie_new_string(c.as_ptr())
}

#[no_mangle]
pub extern "C" fn pie_http_get(url_val: *mut GcBox, headers_map: *mut GcBox) -> *mut GcBox {
    if url_val.is_null() {
        let c = CString::new("http error: null url").unwrap();
        return pie_new_string(c.as_ptr());
    }

    let u = unsafe {
        match &(*url_val).kind {
            ValueKind::Str(s) => s.clone(),
            ValueKind::Int(i) => format!("{}", i),
            _ => {
                return pie_new_string(
                    CString::new("http error: expected string url")
                        .unwrap()
                        .as_ptr(),
                )
            }
        }
    };

    // build request and apply headers if provided
    let mut req = ureq::get(&u);
    if !headers_map.is_null() {
        unsafe {
            if let ValueKind::Map(m) = &(*headers_map).kind {
                for (k, &vptr) in m.iter() {
                    if vptr.is_null() {
                        continue;
                    }
                    match &(*vptr).kind {
                        ValueKind::Str(sv) => {
                            req = req.set(k, sv);
                        }
                        ValueKind::Int(iv) => {
                            req = req.set(k, &format!("{}", iv));
                        }
                        _ => {}
                    }
                }
            }
        }
    }

    match req.call() {
        Ok(resp) => {
            let status = resp.status();
            let mut body = resp.into_string().unwrap_or_default();
            if body.is_empty() {
                body = format!("(status {})", status);
            }
            let c = CString::new(body).unwrap();
            pie_new_string(c.as_ptr())
        }
        Err(e) => {
            let msg = format!("http error: {}", e);
            let c = CString::new(msg).unwrap();
            pie_new_string(c.as_ptr())
        }
    }
}

#[no_mangle]
pub extern "C" fn pie_add(a: *mut GcBox, b: *mut GcBox) -> *mut GcBox {
    if a.is_null() || b.is_null() {
        return ptr::null_mut();
    }
    unsafe {
        match (&(*a).kind, &(*b).kind) {
            (ValueKind::Int(ia), ValueKind::Int(ib)) => GcBox::new(ValueKind::Int(ia + ib)),
            (ValueKind::Str(sa), ValueKind::Str(sb)) => {
                let result = format!("{}{}", sa, sb);
                let c = CString::new(result).unwrap();
                pie_new_string(c.as_ptr())
            }
            _ => ptr::null_mut(),
        }
    }
}

#[no_mangle]
pub extern "C" fn pie_sub(a: *mut GcBox, b: *mut GcBox) -> *mut GcBox {
    if a.is_null() || b.is_null() {
        return ptr::null_mut();
    }
    unsafe {
        match (&(*a).kind, &(*b).kind) {
            (ValueKind::Int(ia), ValueKind::Int(ib)) => GcBox::new(ValueKind::Int(ia - ib)),
            _ => ptr::null_mut(),
        }
    }
}

#[no_mangle]
pub extern "C" fn pie_mul(a: *mut GcBox, b: *mut GcBox) -> *mut GcBox {
    if a.is_null() || b.is_null() {
        return ptr::null_mut();
    }
    unsafe {
        match (&(*a).kind, &(*b).kind) {
            (ValueKind::Int(ia), ValueKind::Int(ib)) => GcBox::new(ValueKind::Int(ia * ib)),
            _ => ptr::null_mut(),
        }
    }
}

#[no_mangle]
pub extern "C" fn pie_div(a: *mut GcBox, b: *mut GcBox) -> *mut GcBox {
    if a.is_null() || b.is_null() {
        return ptr::null_mut();
    }
    unsafe {
        match (&(*a).kind, &(*b).kind) {
            (ValueKind::Int(ia), ValueKind::Int(ib)) => {
                if *ib == 0 {
                    return ptr::null_mut();
                }
                GcBox::new(ValueKind::Int(ia / ib))
            }
            _ => ptr::null_mut(),
        }
    }
}
