use std::collections::HashMap;

use crate::{
    piestd::builtins::pie_native_fn,
    runtime::{GcBox, GcRef, Value},
};

pie_native_fn!(pie_map_new() pie "std::map::new"[] => Map -> GcBox {
    GcBox::new(Value::Map(HashMap::new()))
});

pie_native_fn!(pie_map_set(map: GcRef, key: GcRef, val: GcRef) pie "std::map::set"[Map, Any, Any] {
    if let Value::Map(m) = &mut *map.value_mut() {
        m.insert(key.value().as_str(), val.new_box());
    }
});

pie_native_fn!(pie_map_get(map: GcRef, key: GcRef) pie "std::map::get"[Map, Any] => Any -> Option<GcBox> {
    // extract key string from a PIE GcBox (string or int)
    if let Value::Map(m) = &*map.try_value()? {
        return m.get(&key.try_value()?.as_str()).cloned();
    }
    None
});
