### Iterators

- **std::iter::new(any x) -> iterator | null**: wraps a string, list, or map into an iterator.
- **std::iter::next(iterator) -> any | null**: advances the iterator.
- **std::iter::range(int start, int stop, int step) -> iterator**: numeric range.

Use with `for`:

```pie
use pie/std;

def void main() {
    let any it = std::iter::range(0, 3, 1);
    for i in it { std::print(i); }
}
```
