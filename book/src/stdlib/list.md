### Lists

- **std::list::new() -> list**
- **std::list::len(list) -> int**
- **std::list::get(list, int idx) -> any | null**
- **std::list::set(list, int idx, any val)**
- **std::list::push(list, any val)**
- **std::list::pop(list) -> any | null**
- **std::list::remove(list, int idx) -> any | null**
- **std::list::add_in_place(list, int idx, int delta)**: adds to an int element in place

Example:

```pie
use pie/std;

def void main() {
    let list xs = std::list::new();
    std::list::push(xs, 1);
    std::list::push(xs, 2);
    std::print(std::list::len(xs));
}
```
