#![allow(improper_ctypes_definitions)]
use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt::{Debug, Display};
use std::ptr::NonNull;
use std::rc::Rc;

#[derive(Debug)]
pub enum Value {
    Int(i64),
    Str(String),
    List(Vec<GcBox>),
    Map(HashMap<String, GcBox>),
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use Value::*;
        match self {
            Int(v) => write!(f, "{v}"),
            Str(s) => write!(f, "{s}"),
            List(l) => {
                write!(f, "[")?;
                let mut values = l.iter();
                if let Some(v) = values.next() {
                    v.fmt(f)?;
                }
                for v in values {
                    write!(f, ", {}", v.as_ref().0.borrow())?;
                }
                write!(f, "]")
            }
            Map(m) => {
                write!(f, "{{")?;
                let mut values = m.iter();
                if let Some((k, v)) = values.next() {
                    write!(f, "{k}: {v}", v = v.as_ref().0.borrow())?;
                }
                for (k, v) in m {
                    write!(f, ", {k}: {v}", v = v.as_ref().0.borrow())?;
                }
                write!(f, "}}")
            }
        }
    }
}

#[derive(Debug, Clone)]
#[repr(transparent)]
pub struct GcBox(NonNull<RefCell<Value>>);

impl Drop for GcBox {
    fn drop(&mut self) {
        eprintln!("dropping {self:?}");
        unsafe { Rc::from_raw(self.0.as_ptr()) };
    }
}

const _: () = assert!(core::mem::size_of::<GcBox>() == core::mem::size_of::<usize>());
const _: () = assert!(core::mem::size_of::<Option<GcBox>>() == core::mem::size_of::<usize>());
const _: () = assert!(core::mem::size_of::<GcRef>() == core::mem::size_of::<usize>());

impl GcBox {
    pub fn as_ref<'s>(&'s self) -> GcRef<'s> {
        GcRef(unsafe { self.0.as_ref() })
    }
}

impl From<String> for GcBox {
    fn from(value: String) -> Self {
        GcBox::new(Value::Str(value))
    }
}

impl From<&str> for GcBox {
    fn from(value: &str) -> Self {
        GcBox::new(Value::Str(value.to_owned()))
    }
}

impl From<i64> for GcBox {
    fn from(value: i64) -> Self {
        GcBox::new(Value::Int(value))
    }
}

#[derive(Debug, Clone)]
#[repr(transparent)]
// SAFETY: A GcRef may only be created via the as_ref method on GcBox
pub struct GcRef<'a>(&'a RefCell<Value>);

impl GcRef<'_> {
    pub unsafe fn new_box_noincrement(&self) -> Rc<RefCell<Value>> {
        // This creates a new Rc without incrementing the refcount
        unsafe { Rc::from_raw(core::ptr::from_ref(self.0)) }
    }

    pub fn new_box(&self) -> GcBox {
        let rc = unsafe { self.new_box_noincrement() };

        GcBox(unsafe { NonNull::new_unchecked(Rc::into_raw(rc).cast_mut()) })
    }
}

impl GcBox {
    fn new(val: Value) -> GcBox {
        let ptr = Rc::into_raw(Rc::new(RefCell::new(val)));
        unsafe { GcBox(NonNull::new_unchecked(ptr.cast_mut())) }
    }
}

#[no_mangle]
pub extern "C" fn pie_new_int(v: i64) -> GcBox {
    v.into()
}

#[no_mangle]
pub extern "C" fn pie_new_string(ptr: *const u8, len: u64) -> GcBox {
    unsafe {
        let bytes = core::slice::from_raw_parts(ptr, len as usize);
        // SAFETY: The frontend must ensure the strings are UTF-8
        let string = str::from_utf8_unchecked(bytes).to_owned();
        string.into()
    }
}

#[no_mangle]
pub extern "C" fn pie_inc_ref(p: GcRef) {
    // This avoids running Drop for p, which leaves the refcount with 1 extra reference
    core::mem::forget(p.new_box());
}

#[no_mangle]
pub unsafe extern "C" fn pie_dec_ref(p: GcRef) {
    _ = p.new_box_noincrement();
}

#[no_mangle]
pub extern "C" fn pie_list_new() -> GcBox {
    GcBox::new(Value::List(Vec::new()))
}

#[no_mangle]
pub extern "C" fn pie_list_len(list: GcRef) -> i64 {
    let Value::List(v) = &*list.0.borrow() else {
        return 0;
    };
    v.len() as i64
}

#[no_mangle]
pub extern "C" fn pie_list_get(list: GcRef, idx: i64) -> Option<GcBox> {
    let Value::List(v) = &*list.0.borrow() else {
        return None;
    };
    if idx.is_negative() {
        return None;
    }
    Some(v[idx as usize].clone())
}

#[no_mangle]
pub extern "C" fn pie_list_push(list: GcRef, val: GcRef) {
    let Value::List(v) = &mut *list.0.borrow_mut() else {
        return;
    };
    v.push(val.new_box());
}

#[no_mangle]
pub extern "C" fn pie_map_new() -> GcBox {
    GcBox::new(Value::Map(HashMap::new()))
}

#[no_mangle]
pub extern "C" fn pie_map_set(map: GcRef, key: GcRef, val: GcRef) {
    // extract key string from a PIE GcBox (string or int)
    let k = key.0.borrow().to_string();
    if let Value::Map(m) = &mut *map.0.borrow_mut() {
        m.insert(k, val.new_box());
    }
}

#[no_mangle]
pub extern "C" fn pie_map_get(map: GcRef, key: GcRef) -> Option<GcBox> {
    let k = key.0.borrow().to_string();
    if let Value::Map(m) = &*map.0.borrow() {
        return m.get(&k).cloned();
    }
    None
}

#[no_mangle]
pub extern "C" fn pie_to_string(val: GcRef) -> GcBox {
    let s = val.0.borrow().to_string();
    s.into()
}

#[no_mangle]
pub extern "C" fn pie_print(val: GcRef) {
    println!("{}", val.0.borrow())
}

#[no_mangle]
pub extern "C" fn pie_string_concat(a: GcRef, b: GcRef) -> GcBox {
    let s = format!("{}{}", a.0.borrow(), b.0.borrow());
    s.into()
}

#[no_mangle]
pub extern "C" fn pie_http_get(url_val: GcRef, headers: GcRef) -> GcBox {
    let Value::Str(url) = &*url_val.0.borrow() else {
        return "http error: expected url to be a string".into();
    };
    let Value::Map(headers) = &*headers.0.borrow() else {
        return "http error: expected headers to be a map".into();
    };

    // build request and apply headers if provided
    let mut req = ureq::get(url);

    for (k, v) in headers {
        req = req.set(k, &v.as_ref().0.borrow().to_string());
    }

    match req.call() {
        Ok(resp) => {
            let status = resp.status();
            let mut body = resp.into_string().unwrap_or_default();
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
}

#[no_mangle]
pub extern "C" fn pie_add(a: GcRef, b: GcRef) -> Option<GcBox> {
    use Value::*;
    Some(match (&*a.0.borrow(), &*b.0.borrow()) {
        (Int(a), Int(b)) => (a + b).into(),
        (Str(a), Str(b)) => format!("{a}{b}").into(),
        _ => return None,
    })
}

#[no_mangle]
pub extern "C" fn pie_sub(a: GcRef, b: GcRef) -> Option<GcBox> {
    use Value::*;
    Some(match (&*a.0.borrow(), &*b.0.borrow()) {
        (Int(a), Int(b)) => (a - b).into(),
        _ => return None,
    })
}

#[no_mangle]
pub extern "C" fn pie_mul(a: GcRef, b: GcRef) -> Option<GcBox> {
    use Value::*;
    Some(match (&*a.0.borrow(), &*b.0.borrow()) {
        (Int(a), Int(b)) => (a * b).into(),
        _ => return None,
    })
}

#[no_mangle]
pub extern "C" fn pie_div(a: GcRef, b: GcRef) -> Option<GcBox> {
    use Value::*;
    Some(match (&*a.0.borrow(), &*b.0.borrow()) {
        (_, Int(0)) => return None,
        (Int(a), Int(b)) => (a / b).into(),
        _ => return None,
    })
}
