use std::collections::HashMap;

use crate::{
    piestd::builtins::{pie_native_fn, Registry},
    runtime::{GcBox, GcRef, Value},
};

pub fn register(reg: &mut Registry) {
    pie_map_new_register(reg);
    pie_map_set_register(reg);
    pie_map_get_register(reg);
}

pie_native_fn!(pie_map_new() pie "std::map::new"[] => Map -> GcBox {
    GcBox::new(Value::Map(HashMap::new()))
});

pie_native_fn!(pie_map_set(map: GcRef, key: GcRef, val: GcRef) pie "std::map::set"[Map, Any, Any] {
    // extract key string from a PIE GcBox (string or int)
    let k = key.0.borrow().to_string();
    if let Value::Map(m) = &mut *map.0.borrow_mut() {
        m.insert(k, val.new_box());
    }
});

pie_native_fn!(pie_map_get(map: GcRef, key: GcRef) pie "std::map::get"[Map, Any] => Any -> Option<GcBox> {
    let k = key.0.borrow().to_string();
    if let Value::Map(m) = &*map.0.borrow() {
        return m.get(&k).cloned();
    }
    None
});
