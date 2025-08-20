use crate::{
    piestd::builtins::{pie_native_fn, Registry},
    runtime::{GcBox, GcRef, Value},
};

pie_native_fn!(pie_list_new() pie "std::list::new"[] => List -> GcBox {
    GcBox::new(Value::List(Vec::new()))
});

pie_native_fn!(pie_list_len(list: GcRef) pie "std::list::len"[List] => Int -> i64 {
    let Value::List(v) = &*list.value() else {
        return 0;
    };
    v.len() as i64
});

pie_native_fn!(pie_list_get(list: GcRef, idx: GcRef) pie "std::list::get"[List, Int] => Any -> Option<GcBox> {
    let Value::List(v) = &*list.value() else {
        return None;
    };
    let &Value::Int(idx) = &*idx.value() else {
        return None;
    };
    if idx.is_negative() {
        return None;
    }
    Some(v[idx as usize].clone())
});

pie_native_fn!(pie_list_set(list: GcRef, idx: GcRef, val: GcRef) pie "std::list::set"[List, Int, Any] {
    let Value::List(list) = &mut *list.value_mut() else {
        unreachable!()
    };
    let &Value::Int(idx) = &*idx.value() else {
        unreachable!()
    };
    let val = val.value();
    debug_assert!(!idx.is_negative());
    list[idx as usize] = GcBox::new(val.clone());
});

pie_native_fn!(pie_list_push(list: GcRef, val: GcRef) pie "std::list::push"[List, Any] {
    let Value::List(v) = &mut *list.value_mut() else {
        return;
    };
    v.push(val.new_box());
});

pie_native_fn!(pie_list_pop(list: GcRef) pie "std::list::pop"[List] => Any -> Option<GcBox> {
    let Value::List(v) = &mut *list.value_mut() else {
        return None;
    };
    v.pop()
});

pie_native_fn!(pie_list_remove(list: GcRef, idx: GcRef) pie "std::list::remove"[List, Int] => Any -> Option<GcBox> {
    let Value::List(v) = &mut *list.value_mut() else {
        return None;
    };
    let Value::Int(idx) = *idx.value() else {
        return None;
    };
    if !(0..v.len() as i64).contains(&idx) {
        return None
    }
    Some(v.remove(idx as usize))
});
