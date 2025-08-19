#![allow(improper_ctypes_definitions)]
use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt::{Debug, Display};
use std::mem::ManuallyDrop;
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
    Iterator(Box<dyn DebugIter>),
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        use Value::*;
        match (self, other) {
            (Int(a), Int(b)) => a == b,
            (Float(a), Float(b)) => a == b,
            (Bool(a), Bool(b)) => a == b,
            (Str(a), Str(b)) => a == b,
            (List(a), List(b)) if a.len() == b.len() => a.iter().eq(b.iter()),
            (Map(a), Map(b)) if a.len() == b.len() => {
                for (k, v) in a {
                    if b.get(k) != Some(v) {
                        return false;
                    }
                }
                true
            }
            _ => false,
        }
    }
}

impl PartialOrd for Value {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        use Value::*;
        match (self, other) {
            (Int(a), Int(b)) => a.partial_cmp(b),
            (Float(a), Float(b)) => a.partial_cmp(b),
            (Bool(a), Bool(b)) => a.partial_cmp(b),
            (Str(a), Str(b)) => a.partial_cmp(b),
            (List(a), List(b)) => a.len().partial_cmp(&b.len()),
            (Map(a), Map(b)) => a.len().partial_cmp(&b.len()),
            _ => None,
        }
    }
}

impl Clone for Value {
    fn clone(&self) -> Self {
        use Value::*;
        match self {
            &Int(v) => Int(v),
            &Float(v) => Float(v),
            &Bool(v) => Bool(v),
            Str(v) => Str(v.clone()),
            List(v) => List(v.clone()),
            Map(v) => Map(v.clone()),
            Iterator(iter) => Iterator(iter.clone_as_debug_iter()),
        }
    }
}

pub trait DebugIter: Iterator<Item = GcBox> + Debug {
    fn clone_as_debug_iter(&self) -> Box<dyn DebugIter>;
}
impl<T: Iterator<Item = GcBox> + Debug + Clone + 'static> DebugIter for T {
    fn clone_as_debug_iter(&self) -> Box<dyn DebugIter> {
        Box::new(self.clone())
    }
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
            Iterator(iter) => write!(f, "iterator {iter:?}"),
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

impl PartialEq for GcBox {
    fn eq(&self, other: &Self) -> bool {
        *self.as_ref().value() == *other.as_ref().value()
    }
}

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

impl From<Vec<GcBox>> for GcBox {
    fn from(value: Vec<GcBox>) -> Self {
        GcBox::new(Value::List(value))
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
    pub fn new_null() -> Self {
        Self(None)
    }

    pub fn is_null(&self) -> bool {
        self.0.is_none()
    }

    pub unsafe fn new_box_noincrement(&self) -> Rc<RefCell<Value>> {
        let Some(rc) = self.0 else {
            panic!("Attempted to construct a GcBox from a None GcRef")
        };
        // This creates a new Rc without incrementing the refcount
        unsafe { Rc::from_raw(core::ptr::from_ref(rc)) }
    }

    pub fn new_rc(&self) -> Rc<RefCell<Value>> {
        unsafe {
            let rc = self.new_box_noincrement();
            Rc::increment_strong_count(rc.as_ptr());
            rc
        }
    }

    pub fn as_ptr(&self) -> *const RefCell<Value> {
        core::ptr::from_ref(self.0.unwrap())
    }

    pub fn new_box(&self) -> GcBox {
        let rc = self.new_rc();
        let rc = ManuallyDrop::new(rc);
        let ptr = Rc::as_ptr(&rc);
        GcBox(unsafe { NonNull::new_unchecked(ptr.cast_mut()) })
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
