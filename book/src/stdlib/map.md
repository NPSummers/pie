### Maps

- **std::map::new() -> map**
- **std::map::set(map, any key, any val)**: keys are stringified
- **std::map::get(map, any key) -> any | null**
- **std::map::to_json(map) -> string**
- **std::map::from_json(string) -> map | null**

Example:

```pie
use pie/std;

def void main() {
    let map m = std::map::new();
    std::map::set(m, "a", 1);
    std::print(std::map::get(m, "a"));
}
```
