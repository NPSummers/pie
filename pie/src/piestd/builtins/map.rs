use serde_json::{Map as JsonMap, Number as JsonNumber, Value as JsonValue};
use std::borrow::Cow;
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

fn pie_value_to_json_value(val: &Value) -> JsonValue {
    match val {
        Value::Int(i) => JsonValue::Number(JsonNumber::from(*i)),
        Value::Float(f) => JsonNumber::from_f64(*f)
            .map(JsonValue::Number)
            .unwrap_or(JsonValue::Null),
        Value::Bool(b) => JsonValue::Bool(*b),
        Value::Str(s) => JsonValue::String(s.to_string()),
        Value::List(list) => {
            let mut arr = Vec::with_capacity(list.len());
            for gc in list.iter() {
                let gc_ref = gc.as_ref();
                let v_ref = gc_ref.value();
                let jv = pie_value_to_json_value(&*v_ref);
                arr.push(jv);
            }
            JsonValue::Array(arr)
        }
        Value::Map(map) => {
            let mut obj = JsonMap::with_capacity(map.len());
            for (k, v) in map.iter() {
                let v_ref = v.as_ref();
                let vv = v_ref.value();
                obj.insert(k.to_string(), pie_value_to_json_value(&*vv));
            }
            JsonValue::Object(obj)
        }
        // Iterators are represented as strings
        Value::Iterator(_) => JsonValue::String(val.to_string()),
    }
}

fn json_value_to_pie(v: JsonValue) -> Option<GcBox> {
    match v {
        JsonValue::Null => Some("null".into()),
        JsonValue::Bool(b) => Some(b.into()),
        JsonValue::Number(n) => {
            if let Some(i) = n.as_i64() {
                Some(i.into())
            } else if let Some(f) = n.as_f64() {
                Some(f.into())
            } else {
                None
            }
        }
        JsonValue::String(s) => Some(s.into()),
        JsonValue::Array(arr) => {
            let mut out = Vec::with_capacity(arr.len());
            for el in arr {
                let gc = json_value_to_pie(el)?;
                out.push(gc);
            }
            Some(out.into())
        }
        JsonValue::Object(obj) => {
            let mut map: HashMap<Cow<'static, str>, GcBox> = HashMap::with_capacity(obj.len());
            for (k, v) in obj {
                let vb = json_value_to_pie(v)?;
                map.insert(Cow::Owned(k), vb);
            }
            Some(GcBox::new(Value::Map(map)))
        }
    }
}

pie_native_fn!(pie_map_to_json(map: GcRef) pie "std::map::to_json"[Map] => String -> GcBox {
    let mut json = "{}".to_owned();
    if let Some(vref) = map.try_value() {
        if let Value::Map(_) = &*vref {
            let jv = pie_value_to_json_value(&*vref);
            json = serde_json::to_string(&jv).unwrap_or_else(|_| "{}".to_owned());
        }
    }
    json.into()
});

pie_native_fn!(pie_map_from_json(json: GcRef) pie "std::map::from_json"[String] => Map -> Option<GcBox> {
    let s = json.try_value()?.as_str();
    let Ok(parsed) = serde_json::from_str::<JsonValue>(&s) else { return None; };
    match parsed {
        JsonValue::Object(_) => json_value_to_pie(parsed),
        JsonValue::Array(arr) => {
            let mut map: HashMap<Cow<'static, str>, GcBox> = HashMap::with_capacity(arr.len());
            for (i, v) in arr.into_iter().enumerate() {
                if let Some(gb) = json_value_to_pie(v) {
                    map.insert(Cow::Owned(i.to_string()), gb);
                }
            }
            Some(GcBox::new(Value::Map(map)))
        }
        other => {
            // Wrap primitive into a map under key "value"
            let mut map: HashMap<Cow<'static, str>, GcBox> = HashMap::new();
            if let Some(gb) = json_value_to_pie(other) {
                map.insert(Cow::Borrowed("value"), gb);
            }
            Some(GcBox::new(Value::Map(map)))
        }
    }
});
