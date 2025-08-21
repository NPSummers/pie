### Strings

- **std::to_string(any) -> string**: converts a value to a string.
- **std::to_int(any) -> int | null**: parses/convert to int where possible (string, int, bool).

Notes:

- Pie concatenates strings with numbers via `+`.

Example:

```pie
use pie/std;

def void main() {
    std::print("num: " + 42);
    let int x = std::to_int("12");
    std::print(x);
}
```
