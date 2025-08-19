use std::{
    cell::{Ref, RefCell},
    collections::HashMap,
    rc::Rc,
};

use crate::{
    piestd::builtins::{pie_native_fn, Registry},
    runtime::{DebugIter, GcBox, GcRef, Value},
};

pie_native_fn!(pie_iter_new(iterable: GcRef) pie "std::iter::new"[Any] => Any -> Option<GcBox> {
    let val = iterable.new_rc();
    use Value::*;
    let iter: Box<dyn DebugIter> = match &*val.borrow() {
        Iterator(b) => b.clone_as_debug_iter(),
        Str(_) => Box::new(StringIter::new(val.clone()).unwrap()),
        Map(_) => Box::new(MapIter::new(val.clone()).unwrap()),
        List(_) => Box::new(ListIter::new(val.clone()).unwrap()),
        // TODO: Should these types be iterable?
        Bool(_) | Float(_) | Int(_) => return None,
    };
    Some(GcBox::new(Value::Iterator(iter)))
});

pie_native_fn!(pie_iter_next(iter: GcRef) pie "std::iter::next"[Any] => Any -> Option<GcBox> {
    let mut val = iter.value_mut();
    let Value::Iterator(iter) = &mut *val else {
        return None;
    };
    iter.next()
});

pie_native_fn!(pie_iter_range(start: GcRef, stop: GcRef, step: GcRef) pie "std::iter::range"[Int, Int, Int] => Any -> Option<GcBox> {
    let &Value::Int(start) = &*start.value() else {
        return None
    };
    let &Value::Int(stop) = &*stop.value() else {
        return None
    };
    let &Value::Int(step) = &*step.value() else {
        return None
    };
    let iter = (start..stop).step_by(step.try_into().unwrap()).map(GcBox::from);
    Some(GcBox::new(Value::Iterator(Box::new(iter))))
});

#[derive(Debug)]
// A wrapper that allows holding a reference to variant of a Value in a GcBox
struct BorrowWrapper<Inner: 'static> {
    rc: Rc<RefCell<Value>>,
    map_fn: for<'a> fn(&'a Value) -> &'a Inner,
    // This borrow ensures that the Value cannot be mutably borrowed while the reference to it exists
    // The 'static lifetime is valid here as it points to the Rc above
    borrow: Ref<'static, Inner>,
}

impl<Inner> Clone for BorrowWrapper<Inner> {
    fn clone(&self) -> Self {
        let rc = self.rc.clone();
        BorrowWrapper::new(rc, self.map_fn)
    }
}

impl<Inner> BorrowWrapper<Inner> {
    pub fn new(rc: Rc<RefCell<Value>>, map_fn: fn(&Value) -> &Inner) -> Self {
        let borrow = rc.borrow();
        let borrow: Ref<'_, Inner> = Ref::map(borrow, map_fn);
        // SAFETY: Erase the lifetime of borrow
        // This is safe as it points to the RefCell in Rc, which means it cannot move while the Rc
        // exists
        let borrow: Ref<'static, Inner> = unsafe { core::mem::transmute(borrow) };
        BorrowWrapper { rc, borrow, map_fn }
    }
    pub fn get_ref(&self) -> &Inner {
        let r: &Inner = &self.borrow;
        // SAFETY: Erase the lifetime of borrow
        // This is safe as it points to the RefCell in Rc, which means it cannot move while the Rc
        // exists
        unsafe { std::mem::transmute(r) }
    }
    /// # Safety:
    ///
    /// Tracking that the lifetime of this borrow is not held beyond the lifetime of the
    /// underlying value is left up to the caller
    pub unsafe fn get_static_ref(&self) -> &'static Inner {
        let r: &Inner = &self.borrow;
        // SAFETY: Erase the lifetime of borrow
        // Tracking that the lifetime of this borrow is not held beyond the lifetime of the
        // underlying value is up to the caller
        unsafe { std::mem::transmute(r) }
    }
}

#[derive(Debug, Clone)]
struct MapIter {
    borrow: BorrowWrapper<HashMap<String, GcBox>>,
    iter: std::collections::hash_map::Iter<'static, String, GcBox>,
}

impl MapIter {
    pub fn new(rc: Rc<RefCell<Value>>) -> Option<Self> {
        fn get_map(v: &Value) -> &HashMap<String, GcBox> {
            let Value::Map(m) = v else { unreachable!() };
            m
        }
        // Ensure the Value is a Map
        let val = rc.borrow();
        let Value::Map(_) = &*val else {
            return None;
        };
        let borrow =
            BorrowWrapper::new(rc.clone(), get_map as fn(&Value) -> &HashMap<String, GcBox>);
        // SAFETY: This reference will only exist while the Ref/Rc it references is valid, by being
        // stored in the same struct
        let r = unsafe { borrow.get_static_ref() };
        let iter = r.iter();
        Some(MapIter { borrow, iter })
    }
}

impl Iterator for MapIter {
    type Item = GcBox;

    fn next(&mut self) -> Option<Self::Item> {
        let (k, v) = self.iter.next()?;
        let map = HashMap::from_iter([(k.clone(), v.clone())]);
        Some(GcBox::new(Value::Map(map)))
    }
}

#[derive(Debug, Clone)]
struct ListIter {
    borrow: BorrowWrapper<Vec<GcBox>>,
    iter: std::slice::Iter<'static, GcBox>,
}

impl ListIter {
    pub fn new(rc: Rc<RefCell<Value>>) -> Option<Self> {
        fn get_list(v: &Value) -> &Vec<GcBox> {
            let Value::List(l) = v else { unreachable!() };
            l
        }
        // Ensure the Value is a List
        let val = rc.borrow();
        let Value::List(_) = &*val else {
            return None;
        };
        let borrow = BorrowWrapper::new(rc.clone(), get_list as fn(&Value) -> &Vec<GcBox>);
        // SAFETY: This reference will only exist while the Ref/Rc it references is valid, by being
        // stored in the same struct
        let r = unsafe { borrow.get_static_ref() };
        let iter = r.iter();
        Some(ListIter { borrow, iter })
    }
}

impl Iterator for ListIter {
    type Item = GcBox;

    fn next(&mut self) -> Option<Self::Item> {
        self.iter.next().cloned()
    }
}

#[derive(Debug, Clone)]
struct StringIter {
    borrow: BorrowWrapper<String>,
    iter: std::str::Chars<'static>,
}

impl StringIter {
    pub fn new(rc: Rc<RefCell<Value>>) -> Option<Self> {
        fn get_string(v: &Value) -> &String {
            let Value::Str(v) = &v else { unreachable!() };
            v
        }
        // Ensure the Value is a List
        let val = rc.borrow();
        let Value::Str(_) = &*val else {
            return None;
        };
        let borrow = BorrowWrapper::new(rc.clone(), get_string as fn(&Value) -> &String);
        // SAFETY: This reference will only exist while the Ref/Rc it references is valid, by being
        // stored in the same struct
        let r = unsafe { borrow.get_static_ref() };
        let iter = r.chars();
        Some(StringIter { borrow, iter })
    }
}

impl Iterator for StringIter {
    type Item = GcBox;

    fn next(&mut self) -> Option<Self::Item> {
        self.iter.next().map(String::from).map(GcBox::from)
    }
}
