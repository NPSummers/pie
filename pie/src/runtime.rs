#![allow(improper_ctypes_definitions)]
use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt::{Debug, Display};
use std::ptr::NonNull;
use std::rc::Rc;

#[derive(Debug)]
pub enum Value {
    Int(i64),
    Float(f64),
    Bool(bool),
    Str(String),
    List(Vec<GcBox>),
    Map(HashMap<String, GcBox>),
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use Value::*;
        match self {
            Int(v) => write!(f, "{v}"),
            Float(v) => write!(f, "{v}"),
            Bool(v) => write!(f, "{v}"),
            Str(s) => write!(f, "{s}"),
            List(l) => {
                write!(f, "[")?;
                let mut values = l.iter();
                if let Some(v) = values.next() {
                    v.fmt(f)?;
                }
                for v in values {
                    write!(f, ", {}", v.as_ref().value())?;
                }
                write!(f, "]")
            }
            Map(m) => {
                write!(f, "{{")?;
                let mut values = m.iter();
                if let Some((k, v)) = values.next() {
                    write!(f, "{k}: {v}", v = v.as_ref().value())?;
                }
                for (k, v) in m {
                    write!(f, ", {k}: {v}", v = v.as_ref().value())?;
                }
                write!(f, "}}")
            }
        }
    }
}

#[derive(Debug, Clone)]
#[repr(transparent)]
/// A non-none reference counted value
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
        GcRef(Some(unsafe { self.0.as_ref() }))
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

impl From<f64> for GcBox {
    fn from(value: f64) -> Self {
        GcBox::new(Value::Float(value))
    }
}

impl From<bool> for GcBox {
    fn from(value: bool) -> Self {
        GcBox::new(Value::Bool(value))
    }
}

#[derive(Debug, Clone)]
#[repr(transparent)]
// SAFETY: A GcRef may only be created via the as_ref method on GcBox
/// A possibly None gc-tracked value. Bit-equivalent to Option<GcBox>
pub struct GcRef<'a>(pub Option<&'a RefCell<Value>>);

impl GcRef<'_> {
    pub unsafe fn new_box_noincrement(&self) -> Rc<RefCell<Value>> {
        let Some(rc) = self.0 else {
            panic!("Attempted to construct a GcBox from a None GcRef")
        };
        // This creates a new Rc without incrementing the refcount
        unsafe { Rc::from_raw(core::ptr::from_ref(rc)) }
    }

    pub fn new_box(&self) -> GcBox {
        let rc = unsafe { self.new_box_noincrement() };

        GcBox(unsafe { NonNull::new_unchecked(Rc::into_raw(rc).cast_mut()) })
    }

    pub fn value(&self) -> std::cell::Ref<'_, Value> {
        self.0
            .expect("Attempted to access the value of a null GcRef")
            .borrow()
    }

    pub fn value_mut(&self) -> std::cell::RefMut<'_, Value> {
        self.0
            .expect("Attempted to access the value of a null GcRef")
            .borrow_mut()
    }
}

impl GcBox {
    pub fn new(val: Value) -> GcBox {
        let ptr = Rc::into_raw(Rc::new(RefCell::new(val)));
        unsafe { GcBox(NonNull::new_unchecked(ptr.cast_mut())) }
    }
}
